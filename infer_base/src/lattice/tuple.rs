use paste::paste;

use super::{BoundedLattice, Lattice};
use std::cmp::Ordering::*;

macro_rules! tuple_lattice_impl{
   ($($i:tt),*) => {
      paste!(
      impl< $([<T $i>]: BoundedLattice),* > super::ProtoLattice for ($([<T $i>]),*,) {
         #[allow(unused_assignments)]
         fn meet(self, other: Self) -> Self {
            let mut state = Some(Equal);
            $(
               let [<comp $i>] = match state{
                  Some(Equal) => {
                     let comp_res = self.$i.partial_cmp(&other.$i);
                     state = comp_res;
                     match comp_res {
                        Some(Equal) => self.$i,
                        Some(Greater) => other.$i,
                        Some(Less) => self.$i,
                        None => self.$i.meet(other.$i),
                     }
                  },
                  Some(Greater) => other.$i,
                  Some(Less) => self.$i,
                  None => [<T $i>]::top()
               };
            )*
            ($([<comp $i>],)*)
         }
      
         #[allow(unused_assignments)]
         fn join(self, other: Self) -> Self {
            let mut state = Some(Equal);
            $(
               let [<comp $i>] = match state{
                  Some(Equal) => {
                     let comp_res = self.$i.partial_cmp(&other.$i);
                     state = comp_res;
                     match comp_res {
                        Some(Equal) => self.$i,
                        Some(Greater) => self.$i,
                        Some(Less) => other.$i,
                        None => self.$i.join(other.$i),
                     }
                  },
                  Some(Greater) => self.$i,
                  Some(Less) => other.$i,
                  None => [<T $i>]::bottom()
               };
            )*
            ($([<comp $i>],)*)
         }
      }
      
      impl< $([<T $i>]: BoundedLattice),* > BoundedLattice for ($([<T $i>]),*,) where ($([<T $i>]),*,): Lattice  {
         fn bottom() -> Self {
               ($([<T $i>]::bottom()),*,)
         }
      
         fn top() -> Self {
            ($([<T $i>]::top()),*,)
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

impl super::ProtoLattice for () {
   fn meet(self, _other: Self) -> Self { () }
   fn join(self, _other: Self) -> Self { () }
}

impl BoundedLattice for () {
   fn bottom() -> Self { () }
   fn top() -> Self { () }
}


#[test]
fn test_tuple_lattice(){
   #[derive(PartialEq, Eq, Clone, Copy, Debug)]
   enum Diamond{
      Top,
      A, B,
      Bottom
   }
   use Diamond::*;
   impl PartialOrd for Diamond {
      fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
         match (self, other) {
            (Top, _) => Some(Greater),
            (_, Top) => Some(Less),
            (B, A) => None,
            (B, B) => Some(Equal),
            (B, Bottom) => Some(Greater),
            (A, A) => Some(Equal),
            (A, B) => None,
            (A, Bottom) => Some(Greater),
            (Bottom, _) => Some(Less)
         }
      }
   }
   impl super::ProtoLattice for Diamond {
      fn meet(self, other: Self) -> Self {
         match (self, other) {
            (Bottom, _) => Bottom,
            (_, Bottom) => Bottom,
            (Top, x) => x,
            (x, Top) => x,
            (A, B) => Bottom,
            (B, A) => Bottom,
            (A, A) => A,
            (B, B) => B,
         }
      }

      fn join(self, other: Self) -> Self {
         match (self, other) {
            (Bottom, x) => x,
            (x, Bottom) => x,
            (Top, _) => Top,
            (_, Top) => Top,
            (A, B) => Top,
            (B, A) => Top,
            (A, A) => A,
            (B, B) => B,
         }
      }
   }
   impl BoundedLattice for Diamond {
      fn bottom() -> Self { Bottom }
      fn top() -> Self { Top }
   }

   assert_eq!((A, 1).join((Top, 2)), (Top, 2));
   assert_eq!((A, 1).meet((Top, 2)), (A, 1));

   assert_eq!((A, 1).join((B, 2)), (Top, i32::MIN));
   assert_eq!((A, 1).meet((B, 2)), (Bottom, u32::MAX));

   assert_eq!((1, B).join((10, A)), (10, A));
   
   assert_eq!((1, B, 4).join((1, A, 2)), (1, Top, i32::MIN));
   assert_eq!((1, B, 4).meet((1, A, 2)), (1, Bottom, i32::MAX));

   assert_eq!((1, Top, 4).join((1, A, 2)), (1, Top, 4));
   assert_eq!((1, Top, 4).meet((1, A, 2)), (1, A, 2));

   assert_eq!((1,2).join((2,0)), (2,0));
}

