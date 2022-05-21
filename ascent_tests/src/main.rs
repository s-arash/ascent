// #![allow(warnings)]
// #![feature(decl_macro)]
#![allow(unused_imports)]
#![allow(confusable_idents)]

mod tests;
pub mod utils;
mod se;
mod exps;
mod analysis_exp;
mod agg_tests;
mod example_tests;
mod macros_tests;

use std::{any::Any, cmp::max, rc::Rc, collections::HashSet};
use stopwatch::Stopwatch;
use ascent::{ascent};


ascent!{
   struct TC;
   relation edge(i32, i32);
   relation path(i32, i32);

   path(*x, *y) <-- edge(x,y);
   // path(*x, *z) <-- edge(x,y), path(y, z);
   //path(*x, *z) <-- path(y, z), edge(x, y);
   path(*x, *z) <-- edge(x, y), path(y, z);
}

fn _run_tc_bench(){
   let mut tc = TC::default();

   for i in 0..1000 {
      tc.edge.push((i, i + 1));
   }

   let mut stopwatch = Stopwatch::start_new();
   tc.run();
   stopwatch.stop();

   println!("tc took {:?}", stopwatch.elapsed());
   println!("path size: {}", tc.path.len());
}

fn main(){
   analysis_exp::analysis_exp();
}