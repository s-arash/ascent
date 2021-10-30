// #![allow(warnings)]
#![feature(decl_macro)]
#![allow(unused_imports)]

mod tests;
mod utils;
mod se;
mod exps;

use std::{any::Any, cmp::max, rc::Rc};
use stopwatch::Stopwatch;
use infer::infer;


infer!{
   struct TC;
   relation edge(i32, i32);
   relation path(i32, i32);

   path(*x, *y) <-- edge(x,y);
   path(*x, *z) <-- edge(x,y), path(y, z);
}

fn main(){
   let mut tc = TC::default();

   for i in 0..1000 {
      tc.edge.push((i, i + 1));
   }
   tc.update_indices();

   let mut stopwatch = Stopwatch::start_new();
   tc.run();
   stopwatch.stop();

   println!("tc took {:?}", stopwatch.elapsed());
   println!("path size: {}", tc.path.len());

}