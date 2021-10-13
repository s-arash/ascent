use std::{
   cmp::Ordering,
   collections::{BTreeSet, HashSet},
};

pub trait Lattice: PartialOrd {
   fn top() -> Self;
   fn bottom() -> Self;
   fn meet(self, other: Self) -> Self;
   fn join(self, other: Self) -> Self;
}

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
         (Constant(x), Bottom) => Some(Ordering::Greater),
         (Constant(x), Constant(y)) => if x == y {Some(Ordering::Equal)} else {None},
         (Constant(_), Top) => Some(Ordering::Less),
         (Top, Top) => Some(Ordering::Equal),
         (Top, _) => Some(Ordering::Greater),
      }
   }
}

impl<T: PartialEq> Lattice for ConstPropagation<T> {
   fn top() -> Self {
      Self::Top
   }

   fn bottom() -> Self {
      Self::Bottom
   }

   fn meet(self, other: Self) -> Self {
      use ConstPropagation::*;
      match (self, other) {
         (Bottom, _) => Self::Bottom,
         (Constant(x), Bottom) => Self::Bottom,
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
         (Constant(x), Top) => Top,
         (Top, _) => Top,
      }
   }
}
