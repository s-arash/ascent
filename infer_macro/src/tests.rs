use std::collections::HashMap;

use petgraph::dot::{Config, Dot};
use proc_macro2::TokenStream;

use crate::{dl_impl, dl_impl_hir_to_code};

#[test]
fn test_macro() {
   // let inp = quote!{
   //     relation foo(i32);
   //     relation bar(i32, i32);

   //     foo(x) <-- bar(x,y);
   // };

   let inp = quote! {
       relation foo(i32);
       relation bar(i32, i32);
       relation baz(i32, i32);


       foo(x) <-- bar(x,y), baz(y, z) when x < z;
       baz(x - 1, x + 1) <-- foo(x) when x > 42;
   };

   let inp = quote! {
       relation bar(i32, i32);
       relation foo1(i32, i32, i32);
       relation foo2(i32, Option<i32>);

       relation baz(i32, Vec<i32>);
       relation baz2(i32, Vec<i32>, i32);


       // bar(x, y + z) <-- foo1(x, y, z), foo2(z) when x < y;
       bar(*x, y + z + w.unwrap_or(0)) <-- foo1(x, y, z), foo2(x, w) when *z != 4;

       baz2(*x, exp.clone(), exp.len() as i32) <-- baz(x, exp) when *x < 100;
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
   };

   write_to_scratchpad(input);
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
