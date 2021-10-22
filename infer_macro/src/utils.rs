use std::borrow::Borrow;
use std::{collections::HashSet};

use itertools::Itertools;
use proc_macro2::{Ident, Span};

use quote::ToTokens;
use syn::{Block, Stmt};
use syn::parse::{Parse, ParseBuffer};
use syn::{Expr, Pat, Path, Type, parse::ParseStream, parse2, punctuated::Punctuated, spanned::Spanned};



pub fn tuple_type(types: &[Type]) -> Type {
   let res = if types.len() == 0 {
      quote! {()}
   } else if types.len() == 1 {
      let ty = &types[0];
      quote! { ( #ty, ) }
   } else {
      quote! { ( #(#types),* ) }
   };
   syn::parse2(res).unwrap()
}

pub fn tuple(exprs: &[Expr]) -> Expr {
   let span = if exprs.len() > 0 {exprs[0].span()} else {Span::call_site()};
   tuple_spanned(exprs, span)
}
pub fn tuple_spanned(exprs: &[Expr], span: Span) -> Expr {
   let res = if exprs.len() == 0 {
      quote_spanned! {span=>()}
   } else if exprs.len() == 1 {
      let exp = &exprs[0];
      quote_spanned! {span=> ( #exp, ) }
   } else {
      quote_spanned! {span=> ( #(#exprs),* ) }
   };
   syn::parse2(res).unwrap()
}

pub fn exp_cloned(exp: &Expr) -> Expr {
   let exp_span = exp.span();
   let res = match exp {
      Expr::Path(_) |
      Expr::Field(_) |
      Expr::Paren(_) => quote_spanned! {exp_span=> #exp.clone()},
      _ => quote_spanned! {exp_span=> (#exp).clone()}
   };
   syn::parse2(res).unwrap()
}

pub fn collect_set<T: Eq + std::hash::Hash>(iter : impl Iterator<Item = T>) -> HashSet<T> {
   iter.collect()
}

pub fn into_set<T: Eq + std::hash::Hash>(iter : impl IntoIterator<Item = T>) -> HashSet<T> {
   iter.into_iter().collect()
}

pub fn map_punctuated<T, P, U>(punc: Punctuated<T,P>, mut f: impl FnMut (T) -> U) -> Punctuated<U,P> {
   let mut res = Punctuated::new();
   for pair in punc.into_pairs() {
      let (t, p) = pair.into_tuple();
      res.push_value(f(t));
      if let Some(p) = p {res.push_punct(p)}
   };
   res
}
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

pub fn expr_get_vars(expr: &Expr) -> Vec<Ident> {
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

#[test]
fn test_pattern_get_vars(){
   let pattern = quote! {
      SomePair(x, (y, z))
   };
   let pat : syn::Pat = parse2(pattern).unwrap();
   assert_eq!(collect_set(["x", "y", "z"].iter().map(ToString::to_string)), 
              pattern_get_vars(&pat).into_iter().map(|id| id.to_string()).collect());
}

#[test]
fn test_expr_get_vars(){
   let expr = quote! {
      {
         let res = 0;
         for i in [0..10] {
            let x = i + a;
            res += x / {|m, (n, o)| m + n - o}(2, (b, 42))
         }
         res
      } 
   };
   let expected = vec!["a", "b"];
   let result = expr_get_vars(& parse2(expr).unwrap());
   let result   =   result.into_iter().map(|v| v.to_string()).collect::<HashSet<_>>();
   let expected = expected.into_iter().map(|v| v.to_string()).collect::<HashSet<_>>();

   println!("result: {:?}", result);
   assert_eq!(result, expected)
}


pub fn expr_to_ident(expr: &Expr) -> Option<Ident> {
   match expr {
      Expr::Path(p) if p.path.get_ident().is_some() => p.path.get_ident().cloned(),
      _ => None
   }
}

pub fn pat_to_ident(pat: &Pat) -> Option<Ident> {
   match pat {
      Pat::Ident(ident) => Some(ident.ident.clone()),
      _ => None
   }
}