use std::borrow::Borrow;
use std::collections::HashMap;
use std::{collections::HashSet};

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree, Group};

use quote::ToTokens;
use syn::{Block, Stmt, ExprMacro, ItemMacro2};
use syn::parse::{Parse, ParseBuffer};
use syn::{Expr, Pat, Path, Type, parse::ParseStream, parse2, punctuated::Punctuated, spanned::Spanned};


pub fn block_get_vars(block: &Block) -> Vec<Ident> {
   let mut bound_vars = HashSet::new();
   let mut used_vars = vec![];
   for stmt in block.stmts.iter() {
      let (stmt_bound_vars, stmt_used_vars) = stmt_get_vars(stmt);
      for used_var in stmt_used_vars.into_iter() {
         if !bound_vars.contains(&used_var) {
            used_vars.push(used_var);
         }
      }
      bound_vars.extend(stmt_bound_vars);
   }
   used_vars
}

pub fn pattern_get_vars(pat: &Pat) -> Vec<Ident> {
   let mut res = vec![];
   match pat {
      Pat::Box(pat_box) => res.extend(pattern_get_vars(&pat_box.pat)),
      Pat::Ident(pat_ident) => {
         res.push(pat_ident.ident.clone()); 
         if let Some(subpat) = &pat_ident.subpat {
            res.extend(pattern_get_vars(&subpat.1))
         }
      },
      Pat::Lit(_) => {},
      Pat::Macro(_) => {},
      Pat::Or(or_pat) => {
         let cases_vars = or_pat.cases.iter().map(pattern_get_vars).map(into_set);
         let intersection = cases_vars.fold1(|case_vars, accu| collect_set(case_vars.intersection(&accu).cloned()));
         if let Some(intersection) = intersection {
            res.extend(intersection);
         }
      },
      Pat::Path(_) => {},
      Pat::Range(_) => {},
      Pat::Reference(ref_pat) => res.extend(pattern_get_vars(&ref_pat.pat)),
      Pat::Rest(_) => {},
      Pat::Slice(slice_pat) => {
         for sub_pat in slice_pat.elems.iter(){
            res.extend(pattern_get_vars(sub_pat));
         }
      },
      Pat::Struct(struct_pat) => {
         for field_pat in struct_pat.fields.iter() {
            res.extend(pattern_get_vars(&field_pat.pat));
         }
      },
      Pat::Tuple(tuple_pat) => {
         for elem_pat in tuple_pat.elems.iter() {
            res.extend(pattern_get_vars(elem_pat));
         }
      }
      Pat::TupleStruct(tuple_strcut_pat) => {
         for elem_pat in tuple_strcut_pat.pat.elems.iter(){
            res.extend(pattern_get_vars(elem_pat));
         }
      },
      Pat::Type(type_pat) => {
         res.extend(pattern_get_vars(&type_pat.pat));
      },
      Pat::Verbatim(_) => {},
      Pat::Wild(_) => {},
      _ => {}
   }
   // println!("pattern vars {} : {}", pat.to_token_stream(), res.iter().map(|ident| ident.to_string()).join(", "));
   res
}


pub fn pattern_visit_vars_mut(pat: &mut Pat, visitor: &mut dyn FnMut(&mut Ident)) {
   macro_rules! visit {
      ($e: expr) => {
         pattern_visit_vars_mut($e, visitor)
      };
   }
   match pat {
      Pat::Box(pat_box) => visit!(&mut pat_box.pat),
      Pat::Ident(pat_ident) => {
         visitor(&mut pat_ident.ident); 
         if let Some(subpat) = &mut pat_ident.subpat {
            visit!(&mut subpat.1);
         }
      },
      Pat::Lit(_) => {},
      Pat::Macro(_) => {},
      Pat::Or(or_pat) => {
         // let cases_vars = or_pat.cases.iter().map(pattern_get_vars).map(into_set);
         // let intersection = cases_vars.fold1(|case_vars, accu| collect_set(case_vars.intersection(&accu).cloned()));
         // if let Some(intersection) = intersection {
         //    (intersection);
         // }
         for case in or_pat.cases.iter_mut() {
            visit!(case)
         }
      },
      Pat::Path(_) => {},
      Pat::Range(_) => {},
      Pat::Reference(ref_pat) => (visit!(&mut ref_pat.pat)),
      Pat::Rest(_) => {},
      Pat::Slice(slice_pat) => {
         for sub_pat in slice_pat.elems.iter_mut(){
            visit!(sub_pat);
         }
      },
      Pat::Struct(struct_pat) => {
         for field_pat in struct_pat.fields.iter_mut() {
            visit!(&mut field_pat.pat);
         }
      },
      Pat::Tuple(tuple_pat) => {
         for elem_pat in tuple_pat.elems.iter_mut() {
            visit!(elem_pat);
         }
      }
      Pat::TupleStruct(tuple_strcut_pat) => {
         for elem_pat in tuple_strcut_pat.pat.elems.iter_mut(){
            visit!(elem_pat);
         }
      },
      Pat::Type(type_pat) => {
         visit!(&mut type_pat.pat);
      },
      Pat::Verbatim(_) => {},
      Pat::Wild(_) => {},
      _ => {}
   }
}

#[test]
fn test_pattern_get_vars(){
   let pattern = quote! {
      SomePair(x, (y, z))
   };
   let pat : syn::Pat = parse2(pattern).unwrap();
   assert_eq!(collect_set(["x", "y", "z"].iter().map(ToString::to_string)), 
              pattern_get_vars(&pat).into_iter().map(|id| id.to_string()).collect());

}

/// if the expression is a let expression (for example in `if let Some(foo) = bar {..}`),
/// returns the variables bound by the let expression
pub fn expr_get_let_bound_vars(expr: &Expr) -> Vec<Ident> {
   match expr {
      Expr::Let(l) => pattern_get_vars(&l.pat),
      _ => vec![]
   }
}

pub fn stmt_get_vars(stmt: &Stmt) -> (Vec<Ident>, Vec<Ident>) {
   let mut bound_vars = vec![];
   let mut used_vars = vec![];
   match stmt {
      Stmt::Local(l) => {
         bound_vars.extend(pattern_get_vars(&l.pat));
         if let Some(init) = &l.init {used_vars.extend(expr_get_vars(&init.1))}
      },
      Stmt::Item(_) => {},
      Stmt::Expr(e) => used_vars.extend(expr_get_vars(&e)),
      Stmt::Semi(e, _) => used_vars.extend(expr_get_vars(&e))
   }
   (bound_vars, used_vars)
}

pub fn stmt_visit_free_vars_mut(stmt: &mut Stmt, visitor: &mut dyn FnMut(&mut Ident)) {
   match stmt {
      Stmt::Local(l) => {
         if let Some(init) = &mut l.init {expr_visit_free_vars_mut(&mut init.1, visitor)}
      },
      Stmt::Item(_) => {},
      Stmt::Expr(e) |
      Stmt::Semi(e, _) => expr_visit_free_vars_mut(e, visitor)
   }
}

pub fn stmt_visit_free_vars(stmt: &Stmt, visitor: &mut dyn FnMut(& Ident)) {
   match stmt {
      Stmt::Local(l) => {
         if let Some(init) = &l.init {expr_visit_free_vars(&init.1, visitor)}
      },
      Stmt::Item(_) => {},
      Stmt::Expr(e) |
      Stmt::Semi(e, _) => expr_visit_free_vars(e, visitor)
   }
}

pub fn block_visit_free_vars_mut(block: &mut Block, visitor: &mut dyn FnMut(&mut Ident)) {
   let mut bound_vars = HashSet::new();
   for stmt in block.stmts.iter_mut() {
      let (stmt_bound_vars, _) = stmt_get_vars(stmt);
      stmt_visit_free_vars_mut(stmt, &mut |ident| if !bound_vars.contains(ident) {visitor(ident)});
      bound_vars.extend(stmt_bound_vars);
   }
}

pub fn block_visit_free_vars(block: &Block, visitor: &mut dyn FnMut(&Ident)) {
   let mut bound_vars = HashSet::new();
   for stmt in block.stmts.iter() {
      let (stmt_bound_vars, _) = stmt_get_vars(stmt);
      stmt_visit_free_vars(stmt, &mut |ident| if !bound_vars.contains(ident) {visitor(ident)});
      bound_vars.extend(stmt_bound_vars);
   }
}


use duplicate::duplicate_item;

use crate::utils::{collect_set, into_set};

// all this nonsense to have two versions of this function:
//expr_visit_free_vars and expr_visit_free_vars_mut
#[duplicate_item(
   reft(type)  expr_visit_free_vars_mbm   block_visit_free_vars_mbm   iter_mbm   path_get_ident_mbm   deref_mbm;
   [&mut type] [expr_visit_free_vars_mut] [block_visit_free_vars_mut] [iter_mut] [path_get_ident_mut] [deref_mut];
   [&type]     [expr_visit_free_vars]     [block_visit_free_vars]     [iter]     [Path::get_ident]    [deref];
 )]
/// visits free variables in the expr
pub fn expr_visit_free_vars_mbm(expr: reft([Expr]), visitor: &mut dyn FnMut(reft([Ident]))) {
   use std::ops::DerefMut;
   use std::ops::Deref;
   macro_rules! visit {
      ($e: expr) => { expr_visit_free_vars_mbm(reft([$e]), visitor)};
   }
   macro_rules! visitor_except {
      ($excluded: expr) => {
         &mut |ident| {if ! $excluded.contains(ident) {visitor(ident)}}
      };
   }
   macro_rules! visit_except {
      ($e: expr, $excluded: expr) => { expr_visit_free_vars_mbm($e, visitor_except!($excluded))};
   }
   match expr {
      Expr::Array(arr) => {
         for elem in arr.elems.iter_mbm() {
            expr_visit_free_vars_mbm(elem, visitor);
         }
      }
      Expr::Assign(assign) => {
         visit!(assign.left);
         visit!(assign.right)
      },
      Expr::AssignOp(assign_op) => {
         visit!(assign_op.left);
         visit!(assign_op.right)
      },
      Expr::Async(a) => block_visit_free_vars_mbm(reft([a.block]), visitor),
      Expr::Await(a) => visit!(a.base),
      Expr::Binary(b) => {
         visit!(b.left);
         visit!(b.right)
      }
      Expr::Block(b) => block_visit_free_vars_mbm(reft([b.block]), visitor),
      Expr::Box(e) => expr_visit_free_vars_mbm(e.expr.deref_mbm(), visitor),
      Expr::Break(b) => if let Some(b_e) = reft([b.expr]) {expr_visit_free_vars_mbm(b_e, visitor)},
      Expr::Call(c) => {
         visit!(c.func);
         for arg in c.args.iter_mbm() {
            expr_visit_free_vars_mbm(arg, visitor)
         }
      }
      Expr::Cast(c) => visit!(c.expr),
      Expr::Closure(c) => {
         let input_vars : HashSet<_> = c.inputs.iter().flat_map(pattern_get_vars).collect();
         visit_except!(reft([c.body]), input_vars);
      },
      Expr::Continue(c) => {}
      Expr::Field(f) => visit!(f.base),
      Expr::ForLoop(f) => {
         let pat_vars: HashSet<_> = pattern_get_vars(&f.pat).into_iter().collect();
         visit!(f.expr);
         block_visit_free_vars_mbm(reft([f.body]), visitor_except!(pat_vars));
      },
      Expr::Group(g) => visit!(g.expr),
      Expr::If(e) => {
         let bound_vars = expr_get_let_bound_vars(&e.cond).into_iter().collect::<HashSet<_>>();
         visit!(e.cond);
         block_visit_free_vars_mbm(reft([e.then_branch]), visitor_except!(bound_vars));
         if let Some(eb) = reft([e.else_branch]) {
            visit!(eb.1)
         }
      }
      Expr::Index(i) => {
         visit!(i.expr);
         visit!(i.index)
      }
      Expr::Let(l) => visit!(l.expr),
      Expr::Lit(_) => {}
      Expr::Loop(l) => block_visit_free_vars_mbm(reft([l.body]), visitor),
      Expr::Macro(m) => {
         eprintln!("WARNING: cannot determine free varaibles of macro invocations. macro invocation:\n{}", 
                   expr.to_token_stream().to_string())
      },
      Expr::Match(m) => {
         visit!(m.expr);
         for arm in m.arms.iter_mbm() {
            if let Some(g) = reft([arm.guard]) {
               visit!(g.1);
            }
            let arm_vars = pattern_get_vars(&arm.pat).into_iter().collect::<HashSet<_>>();
            visit_except!(reft([arm.body]), arm_vars);
         }
      }
      Expr::MethodCall(c) => {
         visit!(c.receiver);
         for arg in c.args.iter_mbm() {
            expr_visit_free_vars_mbm(arg, visitor)
         }
      }
      Expr::Paren(p) => visit!(p.expr),
      Expr::Path(p) => {
         if let Some(ident) = path_get_ident_mbm(reft([p.path])) {
            visitor(ident)
         }
      }
      Expr::Range(r) => {
         if let Some(from) = reft([r.from]) {
            expr_visit_free_vars_mbm(from, visitor)
         };
         if let Some(to) = reft([r.to]) {
            expr_visit_free_vars_mbm(to, visitor)
         };
      }
      Expr::Reference(r) => (visit!(r.expr)),
      Expr::Repeat(r) => {
         visit!(r.expr);
         visit!(r.len)
      }
      Expr::Return(r) => {
         if let Some(e) = reft([r.expr]) { expr_visit_free_vars_mbm(e, visitor) }
      }
      Expr::Struct(s) => {
         for f in s.fields.iter_mbm() {
            visit!(f.expr)
         }
         if let Some(rest) = reft([s.rest]) {
            expr_visit_free_vars_mbm(rest.deref_mbm(), visitor)
         }
      }
      Expr::Try(t) => visit!(t.expr),
      Expr::TryBlock(t) => block_visit_free_vars_mbm(reft([t.block]), visitor),
      Expr::Tuple(t) => {
         for e in t.elems.iter_mbm() {
            expr_visit_free_vars_mbm(e, visitor)
         }
      }
      Expr::Type(t) => visit!(t.expr),
      Expr::Unary(u) => visit!(u.expr),
      Expr::Unsafe(u) => block_visit_free_vars_mbm(reft([u.block]), visitor),
      Expr::Verbatim(_) => {}
      Expr::While(w) => {
         let bound_vars = expr_get_let_bound_vars(&w.cond).into_iter().collect::<HashSet<_>>();
         visit!(w.cond);
         block_visit_free_vars_mbm(reft([w.body]), visitor_except!(bound_vars))
      }
      Expr::Yield(y) => {
         if let Some(e) = reft([y.expr]) {
            expr_visit_free_vars_mbm(e.deref_mbm(), visitor)
         }
      }
      _ => {}
   }
}

/// like `Path::get_ident(&self)`, but `mut`
pub fn path_get_ident_mut(path: &mut Path) -> Option<&mut Ident> {
   if path.segments.len() != 1 || path.leading_colon.is_some() {return None}
   let res = path.segments.first_mut()?;
   if res.arguments.is_empty() {
      Some(&mut res.ident)
   } else {
      None
   }
}
// TODO delete
/// returns the free variables contained in `expr`
pub fn expr_get_vars_old(expr: &Expr) -> Vec<Ident> {
   let mut res = vec![];
   match expr {
      Expr::Array(arr) => {
         for vs in arr.elems.iter() {
            res.extend(expr_get_vars(vs))
         }
      }
      Expr::Assign(assign) => res.extend(expr_get_vars(&assign.right)),
      Expr::AssignOp(assign_op) => res.extend(expr_get_vars(&assign_op.right)),
      Expr::Async(a) => res.extend(block_get_vars(&a.block)),
      Expr::Await(a) => res.extend(expr_get_vars(&a.base)),
      Expr::Binary(b) => {
         res.extend(expr_get_vars(&b.left));
         res.extend(expr_get_vars(&b.right))
      }
      Expr::Block(b) => res.extend(block_get_vars(&b.block)),
      Expr::Box(e) => res.extend(expr_get_vars(&e.expr)),
      Expr::Break(b) => if let Some(b_e) = &b.expr {res.extend(expr_get_vars(b_e))},
      Expr::Call(c) => {
         res.extend(expr_get_vars(&c.func));
         for arg in c.args.iter() {
            res.extend(expr_get_vars(arg))
         }
      }
      Expr::Cast(c) => res.extend(expr_get_vars(&c.expr)),
      Expr::Closure(c) => {
         let block_vars = expr_get_vars(&c.body);
         let input_vars : HashSet<_> = c.inputs.iter().flat_map(pattern_get_vars).map(|v| v.to_string()).collect();
         res.extend(block_vars.into_iter().filter(|v| !input_vars.contains(&v.to_string())));
         // TODO is to_string required?
      },
      Expr::Continue(c) => {}
      Expr::Field(f) => res.extend(expr_get_vars(&f.base)),
      Expr::ForLoop(f) => {
         res.extend(expr_get_vars(&f.expr));
         let pat_vars: HashSet<_> = pattern_get_vars(&f.pat).into_iter().collect();
         let block_vars = block_get_vars(&f.body);
         res.extend(block_vars.into_iter().filter(|v| !pat_vars.contains(v)));
      },
      Expr::Group(g) => res.extend(expr_get_vars(&g.expr)),
      Expr::If(e) => {
         res.extend(expr_get_vars(&e.cond));
         res.extend(block_get_vars(&e.then_branch));
         if let Some(eb) = &e.else_branch {
            res.extend(expr_get_vars(&eb.1))
         }
      }
      Expr::Index(i) => {
         res.extend(expr_get_vars(&i.expr));
         res.extend(expr_get_vars(&i.index))
      }
      Expr::Let(l) => res.extend(expr_get_vars(&l.expr)),
      Expr::Lit(_) => {}
      Expr::Loop(l) => res.extend(block_get_vars(&l.body)),
      Expr::Macro(m) => {
         eprintln!("WARNING: cannot determine free varaibles of macro invocations. macro invocation:\n{}", 
                   expr.to_token_stream().to_string())
      },
      Expr::Match(m) => {
         res.extend(expr_get_vars(&m.expr));
         for arm in m.arms.iter() {
            res.extend(expr_get_vars(&arm.body));
         }
      }
      Expr::MethodCall(c) => {
         res.extend(expr_get_vars(&c.receiver));
         for arg in c.args.iter() {
            res.extend(expr_get_vars(arg));
         }
      }
      Expr::Paren(p) => res.extend(expr_get_vars(&p.expr)),
      Expr::Path(p) => {
         if let Some(ident) = p.path.get_ident() {
            res.push(ident.clone())
         }
      }
      Expr::Range(r) => {
         if let Some(from) = &r.from {
            res.extend(expr_get_vars(from))
         };
         if let Some(to) = &r.to {
            res.extend(expr_get_vars(to))
         };
      }
      Expr::Reference(r) => res.extend(expr_get_vars(&r.expr)),
      Expr::Repeat(r) => {
         res.extend(expr_get_vars(&r.expr));
         res.extend(expr_get_vars(&r.len))
      }
      Expr::Return(r) => {
         if let Some(e) = &r.expr {
            res.extend(expr_get_vars(e))
         }
      }
      Expr::Struct(s) => {
         for f in s.fields.iter() {
            res.extend(expr_get_vars(&f.expr))
         }
         if let Some(rest) = &s.rest {
            res.extend(expr_get_vars(rest))
         }
      }
      Expr::Try(t) => res.extend(expr_get_vars(&t.expr)),
      Expr::TryBlock(t) => res.extend(block_get_vars(&t.block)),
      Expr::Tuple(t) => {
         for e in t.elems.iter() {
            res.extend(expr_get_vars(e))
         }
      }
      Expr::Type(t) => res.extend(expr_get_vars(&t.expr)),
      Expr::Unary(u) => res.extend(expr_get_vars(&u.expr)),
      Expr::Unsafe(u) => res.extend(block_get_vars(&u.block)),
      Expr::Verbatim(_) => {}
      Expr::While(w) => {
         res.extend(expr_get_vars(&w.cond));
         res.extend(block_get_vars(&w.body));
      }
      Expr::Yield(y) => {
         if let Some(e) = &y.expr {
            res.extend(expr_get_vars(e))
         }
      }
      _ => {}
   }
   res
}

pub fn expr_get_vars(expr: &Expr) -> Vec<Ident> {
   let mut res = vec![];
   expr_visit_free_vars(expr, &mut |ident| res.push(ident.clone()));
   res
}

#[test]
fn test_expr_get_vars(){
   let test_cases = [
      (quote! {
         {
            let res = 0;
            for i in [0..10] {
               let x = i + a;
               res += x / {|m, (n, o)| m + n - o}(2, (b, 42))
            }
            res
         } 
      }, vec!["a", "b"]),
      (quote! {
         |x1: u32, x2: u32| {
            if y > x1 {
               if let Some(z) = foo(x1)  {
                  z + w
               } else {
                  t = 42;
                  x2 
               }
            }
         }
      }, vec!["y", "foo", "w", "t"])
   ];

   for (expr, expected) in test_cases {
      let mut expr = parse2(expr).unwrap();
      let result = expr_get_vars(&mut expr);
      let result   =   result.into_iter().map(|v| v.to_string()).collect::<HashSet<_>>();
      let expected = expected.into_iter().map(|v| v.to_string()).collect::<HashSet<_>>();
      println!("result: {:?}", result);
      println!("expected: {:?}\n", expected);
      assert_eq!(result, expected)
   }
}


pub fn token_stream_replace_ident(ts: TokenStream, visitor: &mut dyn FnMut(&mut Ident)) -> TokenStream {

   fn token_tree_replace_ident(mut tt: TokenTree, visitor: &mut dyn FnMut(&mut Ident)) -> TokenTree {
      match tt {
         TokenTree::Group(grp) => {
            let updated_ts = token_stream_replace_ident(grp.stream(), visitor);
            let new_grp = Group::new(grp.delimiter(), updated_ts);
            TokenTree::Group(new_grp)
         },
         TokenTree::Ident(ref mut ident) =>{visitor(ident); tt},
         TokenTree::Punct(_) => tt,
         TokenTree::Literal(_) => tt,
      }
   }

   let mut new_tts = vec![];
   for tt in ts.into_iter() {
      new_tts.push(token_tree_replace_ident(tt, visitor));
   }
   TokenStream::from_iter(new_tts.into_iter())
}

// pub fn expr_visit_free_vars_mut(expr: &mut Expr, visitor: &mut dyn FnMut(&mut Ident)) {
//    macro_rules! visit {
//       ($e: expr) => { expr_visit_free_vars_mut($e, visitor)};
//    }
//    macro_rules! visitor_except {
//       ($excluded: expr) => {
//          &mut |ident| {if ! $excluded.contains(ident) {visitor(ident)}}
//       };
//    }
//    macro_rules! visit_except {
//       ($e: expr, $excluded: expr) => { expr_visit_free_vars_mut($e, visitor_except!($excluded))};
//    }
//    match expr {
//       Expr::Array(arr) => {
//          for elem in arr.elems.iter_mut() {
//             visit!(elem);
//          }
//       }
//       Expr::Assign(assign) => {
//          visit!(&mut assign.left);
//          visit!(&mut assign.right)
//       },
//       Expr::AssignOp(assign_op) => {
//          visit!(&mut assign_op.left);
//          visit!(&mut assign_op.right)
//       },
//       Expr::Async(a) => block_visit_free_vars_mut(&mut a.block, visitor),
//       Expr::Await(a) => visit!(&mut a.base),
//       Expr::Binary(b) => {
//          visit!(&mut b.left);
//          visit!(&mut b.right)
//       }
//       Expr::Block(b) => block_visit_free_vars_mut(&mut b.block, visitor),
//       Expr::Box(e) => visit!(&mut e.expr),
//       Expr::Break(b) => if let Some(b_e) = &mut b.expr {visit!(b_e)},
//       Expr::Call(c) => {
//          visit!(&mut c.func);
//          for arg in c.args.iter_mut() {
//             visit!(arg)
//          }
//       }
//       Expr::Cast(c) => visit!(&mut c.expr),
//       Expr::Closure(c) => {
//          let input_vars : HashSet<_> = c.inputs.iter().flat_map(pattern_get_vars).collect();
//          visit_except!(&mut c.body, input_vars);
//       },
//       Expr::Continue(c) => {}
//       Expr::Field(f) => visit!(&mut f.base),
//       Expr::ForLoop(f) => {
//          let pat_vars: HashSet<_> = pattern_get_vars(&f.pat).into_iter().collect();
//          visit!(&mut f.expr);
//          block_visit_free_vars_mut(&mut f.body, visitor_except!(pat_vars));
//       },
//       Expr::Group(g) => visit!(&mut g.expr),
//       Expr::If(e) => {
//          let bound_vars = expr_get_let_bound_vars(&e.cond).into_iter().collect::<HashSet<_>>();
//          visit!(&mut e.cond);
//          block_visit_free_vars_mut(&mut e.then_branch, visitor_except!(bound_vars));
//          if let Some(eb) = &mut e.else_branch {
//             visit!(&mut eb.1)
//          }
//       }
//       Expr::Index(i) => {
//          visit!(&mut i.expr);
//          visit!(&mut i.index)
//       }
//       Expr::Let(l) => visit!(&mut l.expr),
//       Expr::Lit(_) => {}
//       Expr::Loop(l) => block_visit_free_vars_mut(&mut l.body, visitor),
//       Expr::Macro(m) => {
//          eprintln!("WARNING: cannot determine free varaibles of macro invocations. macro invocation:\n{}", 
//                    expr.to_token_stream().to_string())
//       },
//       Expr::Match(m) => {
//          visit!(&mut m.expr);
//          for arm in m.arms.iter_mut() {
//             if let Some(g) = &mut arm.guard {
//                visit!(&mut g.1);
//             }
//             let arm_vars = pattern_get_vars(&arm.pat).into_iter().collect::<HashSet<_>>();
//             visit_except!(&mut arm.body, arm_vars);
//          }
//       }
//       Expr::MethodCall(c) => {
//          visit!(&mut c.receiver);
//          for arg in c.args.iter_mut() {
//             visit!(arg)
//          }
//       }
//       Expr::Paren(p) => visit!(&mut p.expr),
//       Expr::Path(p) => {
//          if let Some(ident) = path_get_ident_mut(&mut p.path) {
//             visitor(ident)
//          }
//       }
//       Expr::Range(r) => {
//          if let Some(from) = &mut r.from {
//             visit!(from)
//          };
//          if let Some(to) = &mut r.to {
//             visit!(to)
//          };
//       }
//       Expr::Reference(r) => (visit!(&mut r.expr)),
//       Expr::Repeat(r) => {
//          visit!(&mut r.expr);
//          visit!(&mut r.len)
//       }
//       Expr::Return(r) => {
//          if let Some(e) = &mut r.expr { visit!(e) }
//       }
//       Expr::Struct(s) => {
//          for f in s.fields.iter_mut() {
//             visit!(&mut f.expr)
//          }
//          if let Some(rest) = &mut s.rest {
//             visit!(rest)
//          }
//       }
//       Expr::Try(t) => visit!(&mut t.expr),
//       Expr::TryBlock(t) => block_visit_free_vars_mut(&mut t.block, visitor),
//       Expr::Tuple(t) => {
//          for e in t.elems.iter_mut() {
//             visit!(e)
//          }
//       }
//       Expr::Type(t) => visit!(&mut t.expr),
//       Expr::Unary(u) => visit!(&mut u.expr),
//       Expr::Unsafe(u) => block_visit_free_vars_mut(&mut u.block, visitor),
//       Expr::Verbatim(_) => {}
//       Expr::While(w) => {
//          let bound_vars = expr_get_let_bound_vars(&w.cond).into_iter().collect::<HashSet<_>>();
//          visit!(&mut w.cond);
//          block_visit_free_vars_mut(&mut w.body, visitor_except!(bound_vars))
//       }
//       Expr::Yield(y) => {
//          if let Some(e) = &mut y.expr {
//             visit!(e)
//          }
//       }
//       _ => {}
//    }
// }
