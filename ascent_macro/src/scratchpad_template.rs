use std::{clone, cmp::max, rc::Rc};
use std::ops::Deref;
use std::hash::Hash;
use std::fmt::Debug;


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

#[warn(warnings)]
#[allow(unused_imports)]
#[allow(dead_code)]
#[allow(redundant_semicolons)]
#[cfg(test)]
fn _test<T: FactTypes>() {
   use ascent::aggregators::*;
   use ascent::lattice::set::Set;
   use ascent::Dual;

   todo!("here");
   ;
}