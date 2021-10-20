pub mod constant_propagation;
mod set;
use std::{cmp::Ordering, ops::Deref, rc::Rc};

use paste::paste;

/// an easy-to-implement trait that provides an implementation of the `Lattice` trait
/// through a blanket implementation (`impl<T: ProtoLattice + Clone> Lattice for T`)
pub trait ProtoLattice: PartialOrd + Sized{
   fn meet(self, other: Self) -> Self;
   fn join(self, other: Self) -> Self;
}

pub trait Lattice: PartialOrd + Sized{

   fn meet_mut(&mut self, other: Self) -> bool;

   fn join_mut(&mut self, other: Self) -> bool;

   fn meet(mut self, other: Self) -> Self{
      self.meet_mut(other);
      self
   }

   fn join(mut self, other: Self) -> Self{
      self.join_mut(other);
      self
   }
}

impl<T: ProtoLattice + Clone> Lattice for T {
   fn meet_mut(&mut self, other: Self) -> bool {
      let new_self = ProtoLattice::meet(self.clone(), other);
      let res = self != &new_self;
      *self = new_self;
      res
   }

   fn join_mut(&mut self, other: Self) -> bool {
      let new_self = ProtoLattice::join(self.clone(), other);
      let res = self != &new_self;
      *self = new_self;
      res
   }
}


pub trait BoundedLattice: Lattice {
   fn bottom() -> Self;   
   fn top() -> Self;
}


#[derive(PartialEq, Eq, Clone, Copy, Hash)]
/// A wrapper that inverts `<=` and `>=` (or `partial_cmp` for `PartialOrd` types), `meet` and `join` for `ProtoLattice`s, 
/// and `top` and `bottom` for `BoundedLattice`s.
/// 
/// # Example 
/// ```
/// assert!(Dual(2) < Dual(1));
/// ```
struct Dual<T>(pub T);

impl<T> PartialOrd for Dual<T> where T: PartialOrd {
   fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      other.0.partial_cmp(&self.0)
   }
}

impl<T> Ord for Dual<T> where T: Ord {
   fn cmp(&self, other: &Self) -> Ordering { other.0.cmp(&self.0) }
}

impl<T> ProtoLattice for Dual<T> where T: ProtoLattice {
   fn meet(self, other: Self) -> Self { Dual(self.0.join(other.0)) }
   fn join(self, other: Self) -> Self { Dual(self.0.meet(other.0)) }
}

// impl<T> Lattice for Dual<T> where T: Lattice{
//    fn meet(self, other: Self) -> Self { Dual(self.0.join(other.0)) }
//    fn join(self, other: Self) -> Self { Dual(self.0.meet(other.0)) }
// }


impl<T> BoundedLattice for Dual<T> where T: BoundedLattice, Dual<T>: Lattice{
   fn top() -> Self { Dual(T::bottom()) }
   fn bottom() -> Self { Dual(T::top()) }
}


impl ProtoLattice for bool {
   fn meet(self, other: Self) -> Self { self & other }
   fn join(self, other: Self) -> Self { self | other }
}

impl BoundedLattice for bool {
   fn bottom() -> Self { false }
   fn top() -> Self { true }
}

macro_rules! num_lattice_impl {
   ($int:ty) => {
      impl ProtoLattice for $int {
         fn meet(self, other: Self) -> Self { self.min(other) }
         fn join(self, other: Self) -> Self { self.max(other) }
      }
      impl BoundedLattice for $int {
         fn bottom() -> Self { Self::MIN }
         fn top() -> Self { Self::MAX }
      }
   };
}
num_lattice_impl!(i8);
num_lattice_impl!(u8);
num_lattice_impl!(i16);
num_lattice_impl!(u16);
num_lattice_impl!(i32);
num_lattice_impl!(u32);
num_lattice_impl!(i64);
num_lattice_impl!(u64);
num_lattice_impl!(isize);
num_lattice_impl!(usize);
num_lattice_impl!(f32);
num_lattice_impl!(f64);


macro_rules! tuple_lattice_impl{
   ($($i:tt),*) => {
      paste!(
      impl< $([<T $i>]: ProtoLattice),* > ProtoLattice for ($([<T $i>]),*,) {
         fn meet(self, other: Self) -> Self {
            ($(self.$i.meet(other.$i)),*,)
         }
      
         fn join(self, other: Self) -> Self {
            ($(self.$i.join(other.$i)),*,)
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

impl ProtoLattice for () {
   fn meet(self, _other: Self) -> Self { () }
   fn join(self, _other: Self) -> Self { () }
}

impl BoundedLattice for () {
   fn bottom() -> Self { () }
   fn top() -> Self { () }
}

impl<T: ProtoLattice> ProtoLattice for Option<T> {
   fn meet(self, other: Self) -> Self {
      match (self, other) {
         (Some(x), Some(y)) => Some(x.meet(y)),
         _ => None,
      }
   }

   fn join(self, other: Self) -> Self {
      match (self, other) {
         (None, y) => y,
         (x, None) => x,
         (Some(x), Some(y)) => Some(x.join(y))
      }
   }
}

impl<T: BoundedLattice + Eq> BoundedLattice for Option<T> where Option<T> : Lattice {
   fn bottom() -> Self { None }
   fn top() -> Self { Some(T::top()) }
}


#[test]
fn test_tuple_lattice(){
   let t1 = (1, 3);
   let t2 = (0, 10);

   assert_eq!(Lattice::meet(t1, t2), (0, 3));
   assert_eq!(Lattice::join(t1, t2), (1, 10));

   assert!((1,3) < (2,3));
}

impl<T: ProtoLattice + Clone> ProtoLattice for Rc<T> {
   fn meet(self, other: Self) -> Self {
      Rc::new(self.deref().clone().meet(other.deref().clone()))
   }

   fn join(self, other: Self) -> Self {
      Rc::new(self.deref().clone().join(other.deref().clone()))
   }
}

impl<T: ProtoLattice + Sized> ProtoLattice for Box<T> {
   fn meet(self, other: Self) -> Self {
      Box::new((*self).meet(*other))
   }

   fn join(self, other: Self) -> Self {
      Box::new((*self).join(*other))
   }
}