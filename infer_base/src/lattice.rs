pub mod constant_propagation;
pub mod set;
pub mod product;
pub mod ord_lattice;
pub use product::Product;
pub mod tuple;
use std::{ops::Deref, rc::Rc};
mod dual;
pub use dual::Dual;

/// an easy-to-implement trait that provides an implementation of the `Lattice` trait
/// through a blanket implementation (`impl<T: ProtoLattice + Clone> Lattice for T`)
pub trait ProtoLattice: PartialOrd + Sized{
   fn meet(self, other: Self) -> Self;
   fn join(self, other: Self) -> Self;
}

pub trait Lattice: PartialOrd + Sized{

   /// potentially updates `self` to be the join of `self` and `other`. 
   ///
   /// Returns true if `self` was updated.
   fn meet_mut(&mut self, other: Self) -> bool;

   /// potentially updates `self` to be the meet of `self` and `other`. 
   ///
   /// Returns true if `self` was updated.
   fn join_mut(&mut self, other: Self) -> bool;

   #[inline]
   fn meet(mut self, other: Self) -> Self{
      self.meet_mut(other);
      self
   }

   #[inline]
   fn join(mut self, other: Self) -> Self{
      self.join_mut(other);
      self
   }
}

impl<T: ProtoLattice + Clone> Lattice for T {
   #[inline]
   fn meet_mut(&mut self, other: Self) -> bool {
      let new_self = ProtoLattice::meet(self.clone(), other);
      let res = self != &new_self;
      *self = new_self;
      res
   }

   #[inline]
   fn join_mut(&mut self, other: Self) -> bool {
      let new_self = ProtoLattice::join(self.clone(), other);
      let res = self != &new_self;
      *self = new_self;
      res
   }

   #[inline]
   fn meet(self, other: Self) -> Self{
      ProtoLattice::meet(self, other)     
   }

   #[inline]
   fn join(self, other: Self) -> Self{
      ProtoLattice::join(self, other)
   }
}


pub trait BoundedLattice: Lattice {
   fn bottom() -> Self;   
   fn top() -> Self;
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