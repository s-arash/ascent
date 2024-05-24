use paste::paste;

use super::{BoundedLattice, Lattice};

macro_rules! tuple_lattice_impl{
   ($($i:tt),*) => {
      paste!(
      impl< $([<T $i>]),* > Lattice for ($([<T $i>]),*,) where ($([<T $i>]),*,): Ord {
         fn meet_mut(&mut self, other: Self) -> bool {
            use std::cmp::Ordering::*;
            match (&*self).cmp(&other) {
               Less | Equal => false,
               Greater => {
                  *self = other;
                  true
               }
            }
         }
         
         fn join_mut(&mut self, other: Self) -> bool {
            use std::cmp::Ordering::*;
            match (&*self).cmp(&other) {
               Greater | Equal => false,
               Less => {
                  *self = other;
                  true
               }
            }
         }

         fn meet(self, other: Self) -> Self {
            self.min(other)
         }
      
         fn join(self, other: Self) -> Self {
            self.max(other)
         }
      }
      
      impl< $([<T $i>]),* > BoundedLattice for ($([<T $i>]),*,) where $([<T $i>]: BoundedLattice + Ord),* {
         fn bottom() -> Self {
            ($([<T $i>]::bottom(),)*)
         }
      
         fn top() -> Self {
            ($([<T $i>]::top(),)*)
         }
      }
      );
   };
}

tuple_lattice_impl!(0);
tuple_lattice_impl!(0, 1);
tuple_lattice_impl!(0, 1, 2);
tuple_lattice_impl!(0, 1, 2, 3);
tuple_lattice_impl!(0, 1, 2, 3, 4);
tuple_lattice_impl!(0, 1, 2, 3, 4, 5);
tuple_lattice_impl!(0, 1, 2, 3, 4, 5, 6);
tuple_lattice_impl!(0, 1, 2, 3, 4, 5, 6, 7);
tuple_lattice_impl!(0, 1, 2, 3, 4, 5, 6, 7, 8);
tuple_lattice_impl!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
tuple_lattice_impl!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

impl super::Lattice for () {
   fn meet_mut(&mut self, _other: Self) -> bool { false }
   fn join_mut(&mut self, _other: Self) -> bool { false }
   fn meet(self, _other: Self) -> Self {}
   fn join(self, _other: Self) -> Self {}
}

impl BoundedLattice for () {
   fn bottom() -> Self {}
   fn top() -> Self {}
}
