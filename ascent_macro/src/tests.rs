#![cfg(test)]
use std::{clone, cmp::max, rc::Rc};

use petgraph::dot::{Config, Dot};
use proc_macro2::{TokenStream, TokenTree, Group, Span};
use syn::{Ident, parse2};

use crate::{ascent_impl, utils::token_stream_replace_macro_ident};


#[test]
fn test_macro0() {
   let inp = quote!{
      relation subset(i32, i32, i32);
      relation subset_error(i32, i32, i32);
      relation placeholder_origin(i32);
      relation known_placeholder_subset(i32, i32) = vec![(2, 3), (3, 4)];

      placeholder_origin(o1),
      known_placeholder_subset(p1, p2) <--
         subset(p1, p2, o1);

      subset(o1, o2, 42) <--
         placeholder_origin(o1),
         placeholder_origin(o2),
         known_placeholder_subset(o1, o2);

      subset_error(*origin1, *origin2, *point) <--
         subset(origin1, origin2, point),
         placeholder_origin(origin1),
         placeholder_origin(origin2),
         !known_placeholder_subset(origin1, origin2),
         if origin1 != origin2;
   };
   write_ascent_run_to_scratchpad(inp);
}
#[test]
fn test_macro1() {
   let inp = quote!{
      struct TC<TNode> where TNode: Clone + std::cmp::Eq + std::hash::Hash;
      relation edge(TNode, TNode);
      relation path(TNode, TNode);

      path(x.clone(), y.clone()) <-- edge(x,y), path(x, y);
      path(x.clone(), z.clone()) <-- edge(x,y), path(y, z);
      path(x.clone(), z.clone()) <-- path(x,y), path(y, z);
   };

   write_to_scratchpad(inp);
}

#[test]
fn test_macro2() {
   let input = quote! {
      relation foo(i32);
      relation bar(i32, i32);
      relation res(i32);
      relation bar_refl(i32);
      relation bar3(i32, i32, i32);
      relation bar3_res(i32);

      foo(3);
      bar(2, 1);
      bar(1, 1);
      bar(3, 3);

      bar_refl(*x) <-- bar(x, x);

      res(*x) <-- foo(x), bar(x, x);

      bar3(10,10,11);
      bar3(1,1,1);
      bar3(1,2,3);
      bar3(2,1,3);

      bar3_res(*x) <-- bar3(x, y, z), res(x), if x > y, bar3_res(y);

      foo(x), bar(x, x + 1) <-- bar3_res(x), let y = x - 2, bar_refl(x);
   };

   write_to_scratchpad(input);
}

#[test]
fn test_macro3() {
   let input = quote! {
      relation bar(i32, i32);
      relation foo(i32, i32);
      relation baz(i32, i32);

      foo(1, 2);
      foo(10, 2);
      bar(2, 3);
      bar(2, 1);

      baz(*x, *z) <-- foo(x, y) if *x != 10, bar(y, z), if x != z;
      foo(*x, *y), bar(*x, *y) <-- baz(x, y);
   };

   write_to_scratchpad(input);
}

#[test]
fn test_macro_agg() {
   let inp = quote!{
      relation foo(i32, i32);
      relation bar(i32, i32, i32);
      relation baz(i32, i32, i32);

      baz(x, y, min_z) <--
         foo(x, y),
         agg min_z = min(z) in bar(x, y, z);
   };
   write_to_scratchpad(inp);
}

#[test]
fn test_macro_generator() {
   let input = quote! {
      relation edge(i32, i32);
      relation path(i32, i32);
      edge(x, x + 1) <-- for x in 0..100;
      path(*x, *y) <-- edge(x,y);
      path(*x, *z) <-- edge(x,y), path(y, z);
   };

   write_to_scratchpad(input);
}

#[test]
fn test_macro_patterns() {
   let input = quote! {
      relation foo(i32, Option<i32>);
      relation bar(i32, i32);
      foo(1, None);
      foo(2, Some(2));
      foo(3, Some(30));
      bar(*x, *y) <-- foo(x, ?Some(y)) if y != x;
      bar(*x, *y) <-- foo(x, y_opt) if let Some(y) = y_opt if y != x;
   };

   write_to_scratchpad(input);
}

#[test]
fn test_macro_lattices(){
   let input = quote!{
      lattice shortest_path(i32, i32, Dual<u32>);
      relation edge(i32, i32, u32);

      shortest_path(*x,*y, Dual(*w)) <-- edge(x,y,w);

      // edge(1,2, 3);
      // edge(2,3, 5);
      // edge(1,3, 4);
      // edge(2,4, 10);

   };
   write_to_scratchpad(input);
}

#[test]
fn exp_borrowing(){
   // let mut v: Vec<i32> = vec![];
   // let mut u: Vec<i32> = vec![];
   // for i in 0..v.len(){
   //    let v_row = &v[i];

   //    for j in 0..u.len(){
   //       let u_row = &u[j];
   //       let new_row = *u_row + *v_row;
   //       v.push(new_row);
   //    }
   // }

   // let x: Vec<i32> = vec![42];
   // let y: Vec<i32> = Convert::convert(&x);
   // let z: Vec<i32> = Convert::convert(x);
}

#[test]
fn exp_condensation() {
   use petgraph::algo::condensation;
   use petgraph::prelude::*;
   use petgraph::Graph;

   let mut graph: Graph<&'static str, (), Directed> = Graph::new();
   let a = graph.add_node("a"); // node with no weight
   let b = graph.add_node("b");
   let c = graph.add_node("c");
   let d = graph.add_node("d");
   let e = graph.add_node("e");
   let f = graph.add_node("f");
   let g = graph.add_node("g");
   let h = graph.add_node("h");

   // a ----> b ----> e ----> f
   // ^       |       ^       |
   // |       v       |       v
   // d <---- c       h <---- g
   graph.extend_with_edges(&[(a, b), (b, c), (c, d), (d, a), (b, e), (e, f), (f, g), (g, h), (h, e)]);
   let acyclic_condensed_graph = condensation(graph.clone(), true);
   #[allow(non_snake_case)]
   let (A, B) = (NodeIndex::new(0), NodeIndex::new(1));
   assert_eq!(acyclic_condensed_graph.node_count(), 2);
   assert_eq!(acyclic_condensed_graph.edge_count(), 1);
   assert_eq!(acyclic_condensed_graph.neighbors(B).collect::<Vec<_>>(), vec![A]);

   println!("{:?}", Dot::with_config(&acyclic_condensed_graph, &[Config::EdgeNoLabel]));

   let sccs = petgraph::algo::tarjan_scc(&graph);
   println!("sccs ordered:");
   for scc in sccs.iter(){
      println!("{:?}", scc);
   }
}

#[test]
fn exp_items_in_fn(){
   let mut p = Default::default();
   for i in 0..10 {
      p = {
         #[derive(Debug, Default)]
         struct Point{x: i32, y: i32}
         impl Point {
            pub fn size(&self) -> i32 {self.x * self.x + self.y * self.y}
         }
         Point{x:i, y: i+1}
      };
   }
   println!("point is {:?}, with size {}", p, p.size());
}

fn write_to_scratchpad_base(tokens: TokenStream, is_ascent_run: bool) -> TokenStream {
   let code = ascent_impl(tokens, is_ascent_run);
   let code = code.unwrap();
   let template = std::fs::read_to_string("src/scratchpad_template.rs").unwrap();
   let code_in_template = template.replace("todo!(\"here\");", &code.to_string());
   std::fs::write("src/scratchpad.rs", code_in_template).unwrap();
   std::process::Command::new("rustfmt").args(&["src/scratchpad.rs"]).spawn().unwrap().wait().unwrap();
   code
}

fn write_to_scratchpad(tokens: TokenStream) -> TokenStream {
   write_to_scratchpad_base(tokens, false)
}

fn write_ascent_run_to_scratchpad(tokens: TokenStream) -> TokenStream {
   write_to_scratchpad_base(tokens, true)
}


#[test]
fn test_macro_lambda_calc(){
   let inp = quote!{
      relation output(LambdaCalcExpr);
      relation input(LambdaCalcExpr);
      relation do_eval(LambdaCalcExpr);
      relation eval(LambdaCalcExpr, LambdaCalcExpr);

      input(app(U(), I()));
      do_eval(exp.clone()) <-- input(exp);
      output(res.clone()) <-- input(exp), eval(exp, res);

      eval(exp.clone(), exp.clone()) <-- do_eval(?exp @Ref(_));

      eval(exp.clone(), exp.clone()) <-- do_eval(exp), if let Lam(_,_) = exp;

      do_eval(ef.as_ref().clone()) <-- do_eval(?App(ef,_ea));

      do_eval(sub(fb, fx, ea)) <-- 
         do_eval(?App(ef, ea)), 
         eval(ef.deref(), ?Lam(fx, fb));
      
      eval(exp.clone(), final_res.clone()) <-- 
         do_eval(?exp @ App(ef, ea)), // this requires nightly
         eval(ef.deref(), ?Lam(fx, fb)),
         eval(sub(fb, fx, ea), final_res);
   };
   write_to_scratchpad(inp);
}

#[test]
fn test_macro_in_macro() {
   let inp = quote!{
      relation foo(i32, i32);
      relation bar(i32, i32);

      macro foo_($x: expr, $y: expr) {
         foo($x, $y)
      }

      macro foo($x: expr, $y: expr) {
         let _x = $x, let _y = $y, foo_!(_x, _y)
      }

      foo(0, 1);
      foo(1, 2);
      foo(2, 3);
      foo(3, 4);

      bar(x, y) <-- foo(x, y), foo!(x + 1, y + 1), foo!(x + 2, y + 2), foo!(x + 3, y + 3);
      
   };

   write_to_scratchpad(inp);
}

#[test]
fn test_token_stream_replace_macro_ident() {
   let macro_def: syn::ItemMacro2 = syn::parse_quote!{
      macro foo($i: ident) {
         println!("{}", $i);
      }
   };

   fn tt_print_idents(tt: &TokenTree) {
      match tt {
         proc_macro2::TokenTree::Group(grp) => {
            ts_print_idents(&grp.stream());
         },
         proc_macro2::TokenTree::Ident(ident) => println!("{}", ident),
         proc_macro2::TokenTree::Punct(punct) => println!("punct {}", punct),
         proc_macro2::TokenTree::Literal(_) => (),
      }
   }

   fn ts_print_idents(ts: &TokenStream) {
      for tt in ts.clone() {
         tt_print_idents(&tt);
      }
   }

   let ts = quote! {
      println!("{}", $i);
   };

   fn ident(name: &str) -> Ident {Ident::new("i", Span::call_site())}

   let test_cases = [
      (quote!{{println!("{}", $i);}}, "i", quote! { "hello"}, quote!{ {println!("{}", "hello");}}),
      (quote!{{println!("{}{}{}", $$i, $j, $$);}}, "i", quote! { "hello"}, quote!{ {println!("{}{}{}", $"hello", $j, $$);}}),
      (quote!{ foo($$$j, $i, $$i)}, "i", quote! { "hello"}, quote!{ foo($$$j, "hello", $"hello")}),
   ];

   for (ts, var, replacement, expected) in test_cases {
      assert_eq!(token_stream_replace_macro_ident(ts, &ident(var), &replacement).to_string(), expected.to_string());
   }

}

macro_rules! hygiene_test {
   ($e: expr) => {
      // x = $e;
      println!("{}", XXX);
   };
}

const XXX: i32 = 42;

fn test_macro_hygiene() {
   let mut x = "baz";
   hygiene_test!("foo");
   hygiene_test!("bar");
}

#[test]
fn test_macro_item() {
   let def = quote! {
      macro foo($x: expr, $y: expr) {
         $x + $y
      }
   };

   let parsed = parse2::<syn::ItemMacro2>(def).unwrap();
   
   println!("rules: {}", parsed.rules);
}