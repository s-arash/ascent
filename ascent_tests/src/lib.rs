// #![allow(warnings)]
// #![feature(decl_macro)]
#![allow(unused_imports)]
#![allow(confusable_idents)]

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Deref;
use std::{clone, cmp::max, rc::Rc};

mod agg_tests;
mod analysis_exp;
mod ascent_maybe_par;
mod example_tests;
mod exps;
mod macros_tests;
mod se;
mod tests;
pub mod utils;

#[allow(dead_code)]
pub trait Atom:
   From<usize> + Into<usize> + Copy + Clone + std::fmt::Debug + Eq + Ord + Hash + Sync + Send + 'static
{
   fn index(self) -> usize;
}

#[allow(dead_code)]
pub trait FactTypes: Copy + Clone + Debug {
   type Origin: Atom;
   type Loan: Atom;
   type Point: Atom;
   type Variable: Atom;
   type Path: Atom;
}
