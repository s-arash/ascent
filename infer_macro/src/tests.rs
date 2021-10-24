#![cfg(test)]
use std::clone;

use petgraph::dot::{Config, Dot};
use proc_macro2::TokenStream;

use crate::{infer_impl};


#[test]
fn test_macro() {
   let inp = quote!{
      lattice shortest_path(i32, i32, u32);
      relation edge(i32, i32, u32);

      shortest_path(*x,*y, {println!("adding sp({},{},{})?", x, y, w ); *w}) <-- edge(x, y, w);
      shortest_path(*x, *z, {println!("adding sp({},{},{})?", x, z, w + l); (w + l)}) <-- edge(x, y, w), shortest_path(y, z, l);

      edge(1, 2, 30);
      edge(2, 3, 50);
      edge(1, 3, 40);
      edge(2, 4, 100);
      edge(4, 1, 1000);

   };
   write_infer_run_to_scratchpad(inp);
}
#[test]
fn test_macro1() {
   let inp = quote!{
      relation foo(i32, i32);
      relation bar(i32, i32);
      relation baz(i32, i32, i32);
      foo(1, 2);
      foo(2, 3);
      foo(3, 5);

      bar(3, 6);
      bar(5, 10);

      baz(*x, *y, *z) <-- foo(x, y), bar(x + y , z);
   };

   write_to_scratchpad(inp);
}

fn write_to_scratchpad_base(tokens: TokenStream, is_infer_run: bool) -> TokenStream {
   let code = infer_impl(tokens, is_infer_run);
   let code = code.expect("code is not ok!");
   let template = std::fs::read_to_string("src/scratchpad_template.rs").unwrap();
   let code_in_template = template.replace("todo!(\"here\");", &code.to_string());
   std::fs::write("src/scratchpad.rs", code_in_template).unwrap();
   std::process::Command::new("rustfmt").args(&["src/scratchpad.rs"]).spawn().unwrap().wait().unwrap();
   code
}

fn write_to_scratchpad(tokens: TokenStream) -> TokenStream {
   write_to_scratchpad_base(tokens, false)
}

fn write_infer_run_to_scratchpad(tokens: TokenStream) -> TokenStream {
   write_to_scratchpad_base(tokens, true)
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

   let x: i32 = 42;
   let y: i32 = <i32 as Convert<_>>::convert(&x);
}

trait Convert<TSource> {
   #[inline]
   fn convert(source: TSource) -> Self;
}

impl<T> Convert<T> for T {
   #[inline(always)]
   fn convert(source: T) -> T {source}
}

impl<T> Convert<&T> for T where T: Clone {
   #[inline(always)]
   fn convert(source: &T) -> T {source.clone()}
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
