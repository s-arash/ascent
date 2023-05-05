//! Defines the `Lattice` trait and provides implementations for standard types

pub mod constant_propagation;
pub mod set;
pub mod product;
pub mod ord_lattice;
pub mod bounded_set;
pub use product::Product;
pub mod tuple;
use std::{ops::Deref, rc::Rc};
mod dual;
pub use dual::Dual;

/// A `Lattice` is a `PartialOrd` where each pair of elements has a least upper bound (`join`) and a greatest lower bound (`meet`)
pub trait Lattice: PartialOrd + Sized {

   /// ensures `self` is the join of `self` and `other`
   ///
   /// Returns true if `self` was changed.
   fn meet_mut(&mut self, other: Self) -> bool {
      let res = !(*self <= other);
      crate::util::update(self, |x| x.meet(other));
      res
   }

   /// ensures `self` is the meet of `self` and `other`. 
   ///
   /// Returns true if `self` was changed.
   fn join_mut(&mut self, other: Self) -> bool {
      let res = !(*self >= other);
      crate::util::update(self, |x| x.join(other));
      res
   }

   /// the greatest lower bound of two elements. `meet(x, y)` is the largest value x
   /// s.t. `z <= x` and `z <= y`
   fn meet(self, other: Self) -> Self;
   
   /// The least upper bound of two elements. `join(x, y)` is the smallest value z
   /// s.t. `z >= x` and `z >= y`.
   fn join(self, other: Self) -> Self;
}

pub trait BoundedLattice: Lattice {
   fn bottom() -> Self;   
   fn top() -> Self;
}


impl Lattice for bool {
   fn meet(self, other: Self) -> Self { self & other }
   fn join(self, other: Self) -> Self { self | other }
}

impl BoundedLattice for bool {
   fn bottom() -> Self { false }
   fn top() -> Self { true }
}

macro_rules! num_lattice_impl {
   ($int:ty) => {
      impl Lattice for $int {
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


impl Lattice for String {
   fn meet(self, other: Self)-> Self {self.min(other)}
   fn join(self, other: Self) -> Self {self.max(other)}
}

impl<T: Lattice> Lattice for Option<T> {
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


impl<T: Lattice + Clone> Lattice for Rc<T> {
   fn meet(self, other: Self) -> Self {
      let cmp = self.partial_cmp(&other);
      match cmp {
         Some(cmp) => if cmp.is_le() {self} else {other},
         None => Rc::new(self.deref().clone().meet(other.deref().clone())),
      }
   }

   fn join(self, other: Self) -> Self {
      let cmp = self.partial_cmp(&other);
      match cmp {
         Some(cmp) => if cmp.is_ge() {self} else {other},
         None => Rc::new(self.deref().clone().join(other.deref().clone())),
      }
   }
}

impl<T: Lattice + Sized> Lattice for Box<T> {
   fn meet(self, other: Self) -> Self {
      Box::new((*self).meet(*other))
   }

   fn join(self, other: Self) -> Self {
      Box::new((*self).join(*other))
   }
}