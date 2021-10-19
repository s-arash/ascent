use std::cmp::Ordering;
use super::{BoundedLattice, Lattice};


#[derive(PartialEq, Eq)]
pub enum ConstPropagation<T> {
   Bottom,
   Constant(T),
   Top,
}

impl<T: PartialEq> PartialOrd for ConstPropagation<T> {
   fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
      use ConstPropagation::*;
      match (self, other) {
         (Bottom, Bottom) => Some(Ordering::Equal),
         (Bottom, _) => Some(Ordering::Less),
         (Constant(_x), Bottom) => Some(Ordering::Greater),
         (Constant(x), Constant(y)) => if x == y {Some(Ordering::Equal)} else {None},
         (Constant(_), Top) => Some(Ordering::Less),
         (Top, Top) => Some(Ordering::Equal),
         (Top, _) => Some(Ordering::Greater),
      }
   }
}

impl<T: PartialEq> Lattice for ConstPropagation<T> {
   fn meet(self, other: Self) -> Self {
      use ConstPropagation::*;
      match (self, other) {
         (Bottom, _) => Self::Bottom,
         (Constant(_x), Bottom) => Self::Bottom,
         (Constant(x), Constant(y)) => if x == y {Constant(x)} else {Self::Bottom},
         (Constant(x), Top) => Constant(x),
         (Top, other) => other,
      }
   }

   fn join(self, other: Self) -> Self {
      use ConstPropagation::*;
      match (self, other) {
         (Bottom, other) => other,
         (Constant(x), Bottom) => Constant(x),
         (Constant(x), Constant(y)) => if x == y {Constant(x)} else {Self::Top},
         (Constant(_x), Top) => Top,
         (Top, _) => Top,
      }
   }
}

impl<T: Lattice> BoundedLattice for ConstPropagation<T> {
   fn top() -> Self { Self::Top }
   fn bottom() -> Self { Self::Bottom }
}
