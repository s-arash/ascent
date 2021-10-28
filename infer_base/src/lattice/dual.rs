use std::{cmp::Ordering, fmt::Debug, fmt::Display, fmt::Formatter, ops::Deref};

use crate::Lattice;

use super::{BoundedLattice};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
/// A wrapper that inverts `<=` and `>=` (or `partial_cmp` for `PartialOrd` types), `meet` and `join` for `ProtoLattice`s, 
/// and `top` and `bottom` for `BoundedLattice`s.
/// 
/// # Example 
/// ```
/// # use infer_base::lattice::Dual;
/// assert!(Dual(2) < Dual(1));
/// ```
pub struct Dual<T>(pub T);

impl<T> Deref for Dual<T>{
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Debug> Debug for Dual<T> {
   fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { self.0.fmt(f) }
}
 
impl<T: Display> Display for Dual<T> {
   fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { self.0.fmt(f) }
}


impl<T> PartialOrd for Dual<T> where T: PartialOrd {
   fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      other.0.partial_cmp(&self.0)
   }
}

impl<T> Ord for Dual<T> where T: Ord {
   fn cmp(&self, other: &Self) -> Ordering { other.0.cmp(&self.0) }
}

impl<T> Lattice for Dual<T> where T: Lattice{
   fn meet(self, other: Self) -> Self { Dual(self.0.join(other.0)) }
   fn join(self, other: Self) -> Self { Dual(self.0.meet(other.0)) }

   fn meet_mut(&mut self, other: Self) -> bool {
      self.0.join_mut(other.0)
   }

   fn join_mut(&mut self, other: Self) -> bool {
      self.0.meet_mut(other.0)
   }
}


impl<T> BoundedLattice for Dual<T> where T: BoundedLattice, Dual<T>: Lattice{
   fn top() -> Self { Dual(T::bottom()) }
   fn bottom() -> Self { Dual(T::top()) }
}
