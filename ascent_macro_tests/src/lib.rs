// See "tests" directory for macro expansion tests.

use std::fmt::Debug;
use std::hash::Hash;

#[allow(dead_code)]
pub trait Atom: From<usize> + Into<usize> + Copy + Clone + Debug + Eq + Ord + Hash + Sync + Send + 'static {
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
