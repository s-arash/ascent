use std::collections::HashMap;

use petgraph::dot::{Config, Dot};
use proc_macro2::TokenStream;

use crate::{dl_impl};

#[test]
fn test_macro1() {
   // let inp = quote!{
   //     relation foo(i32);
   //     relation bar(i32, i32);

   //     foo(x) <-- bar(x,y);
   // };

   // let inp = quote! {
   //     relation foo(i32);
   //     relation bar(i32, i32);
   //     relation baz(i32, i32);


   //     foo(*x) <-- bar(x,y) if *x > 42, baz(y, z) if *x < *z;
   //     baz(*x - 1, *x + 1) <-- foo(x) if *x > 42;
   // };

   // let inp = quote! {
   //     relation bar(i32, i32);
   //     relation foo1(i32, i32, i32);
   //     relation foo2(i32, Option<i32>);

   //     relation baz(i32, Vec<i32>);
   //     relation baz2(i32, Vec<i32>, i32);


   //     // bar(x, y + z) <-- foo1(x, y, z), foo2(z) when x < y;
   //     bar(*x, y + z + w.unwrap_or(0)) <-- foo1(x, y, z), foo2(x, w) if *z != 4;

   //     baz2(*x, exp.clone(), exp.len() as i32) <-- baz(x, exp) if *x < 100;
   // };

   let inp = quote!{
      relation bar(i32, i32);
      relation foo1(i32, i32);
      relation foo2(i32, i32);
      foo1(1,2);
      foo1(10,20);

      bar(*x, y + z) <-- foo1(x, y), foo2(y, z) if *z > *y;
   };

   write_to_scratchpad(inp);
}

fn write_to_scratchpad(tokens: TokenStream) -> TokenStream {
   let code = dl_impl(tokens);
   assert!(code.is_ok());

   let code = code.unwrap();

   std::fs::write("src/scratchpad.rs", code.to_string());
   std::process::Command::new("rustfmt").args(&["src/scratchpad.rs"]).spawn().unwrap().wait();
   code
}


#[test]
fn test_macro_tc() {
   let input = quote! {
       relation edge(i32, i32);
       relation path(i32, i32);

       path(*x, *y) <-- edge(x,y);
       path(*x, *z) <-- edge(x,y), path(y, z);
      //  path(*x, *z) <-- path(x,y), edge(y, z);

   };

   write_to_scratchpad(input);
}


#[test]
fn test_macro_lambda(){
   let input = quote!{
      relation output(LambdaCalcExpr);
      relation input(LambdaCalcExpr);
      relation eval(LambdaCalcExpr, LambdaCalcExpr);
      relation do_eval(LambdaCalcExpr);

      eval(exp.clone(), exp.clone()) <-- do_eval(exp) if exp.is_ref();
      eval(exp.clone(), exp.clone()) <-- do_eval(exp) if exp.is_lam();
      do_eval(get_app(exp).0.clone()) <-- do_eval(exp) if exp.is_app();
      eval(exp, sub(get_lam(f_res).1, get_lam(f_res).0, get_app(exp).1)) <-- 
         do_eval(exp) if exp.is_app(), 
         eval(f, f_res) if f_res.is_lam() && f == get_app(exp).0 ;
   };
   write_to_scratchpad(input);
}

#[test]
fn test_macro_generator() {
   let input = quote! {
      relation edge(i32, i32);
      relation path(i32, i32);
      edge(x, x + 1) <-- for x in (0..100);
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
      bar(*x, *y) <-- foo(x, y_opt) if let Some(y) = y_opt if y != x;
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
}

#[test]
fn exp_condensation() {
   use petgraph::algo::condensation;
   use petgraph::prelude::*;
   use petgraph::Graph;

   let mut graph: Graph<&'static str, (), Directed> = Graph::new();
   let a = graph.add_node(("a")); // node with no weight
   let b = graph.add_node(("b"));
   let c = graph.add_node(("c"));
   let d = graph.add_node(("d"));
   let e = graph.add_node(("e"));
   let f = graph.add_node(("f"));
   let g = graph.add_node(("g"));
   let h = graph.add_node(("h"));

   // a ----> b ----> e ----> f
   // ^       |       ^       |
   // |       v       |       v
   // d <---- c       h <---- g
   graph.extend_with_edges(&[(a, b), (b, c), (c, d), (d, a), (b, e), (e, f), (f, g), (g, h), (h, e)]);
   let acyclic_condensed_graph = condensation(graph.clone(), true);
   let A = NodeIndex::new(0);
   let B = NodeIndex::new(1);
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
