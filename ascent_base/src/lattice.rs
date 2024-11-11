//! Defines the `Lattice` trait and provides implementations for standard types

pub mod constant_propagation;
pub mod set;
pub mod product;
pub mod ord_lattice;
pub mod bounded_set;
pub use product::Product;
pub mod tuple;
use std::cmp::{Ordering, Reverse};
use std::rc::Rc;
use std::sync::Arc;
mod dual;
pub use dual::Dual;

/// A `Lattice` is a `PartialOrd` where each pair of elements has a least upper bound (`join`) and a greatest lower bound (`meet`)
pub trait Lattice: PartialOrd + Sized {
   /// ensures `self` is the join of `self` and `other`
   ///
   /// Returns true if `self` was changed.
   fn meet_mut(&mut self, other: Self) -> bool;

   /// ensures `self` is the meet of `self` and `other`.
   ///
   /// Returns true if `self` was changed.
   fn join_mut(&mut self, other: Self) -> bool;

   /// The greatest lower bound of two elements. `meet(x, y)` is the biggest value `z`
   /// s.t. `z <= x` and `z <= y`
   fn meet(mut self, other: Self) -> Self {
      self.meet_mut(other);
      self
   }

   /// The least upper bound of two elements. `join(x, y)` is the smallest value `z`
   /// s.t. `z >= x` and `z >= y`.
   fn join(mut self, other: Self) -> Self {
      self.join_mut(other);
      self
   }
}

pub trait BoundedLattice: Lattice {
   fn bottom() -> Self;
   fn top() -> Self;
}

macro_rules! ord_lattice_impl {
   ($t: ty) => {
      impl Lattice for $t {
         fn meet_mut(&mut self, other: Self) -> bool {
            #[allow(clippy::neg_cmp_op_on_partial_ord)]
            let changed = !(*self <= other);
            if changed {
               *self = other;
            }
            changed
         }

         fn join_mut(&mut self, other: Self) -> bool {
            #[allow(clippy::neg_cmp_op_on_partial_ord)]
            let changed = !(*self >= other);
            if changed {
               *self = other;
            }
            changed
         }
      }
   };
}

ord_lattice_impl!(bool);

impl BoundedLattice for bool {
   #[inline]
   fn bottom() -> Self { false }
   #[inline]
   fn top() -> Self { true }
}

macro_rules! num_lattice_impl {
   ($int:ty) => {
      ord_lattice_impl!($int);
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
num_lattice_impl!(i128);
num_lattice_impl!(u128);

num_lattice_impl!(isize);
num_lattice_impl!(usize);

impl<T: Lattice> Lattice for Option<T> {
   fn meet_mut(&mut self, other: Self) -> bool {
      match (self, other) {
         (Some(x), Some(y)) => x.meet_mut(y),
         (this @ Some(_), None) => {
            *this = None;
            true
         },
         (None, _) => false,
      }
   }

   fn join_mut(&mut self, other: Self) -> bool {
      match (self, other) {
         (Some(x), Some(y)) => x.join_mut(y),
         (this @ None, Some(y)) => {
            *this = Some(y);
            true
         },
         (_, None) => false,
      }
   }
}

impl<T: BoundedLattice> BoundedLattice for Option<T> {
   #[inline]
   fn bottom() -> Self { None }
   #[inline]
   fn top() -> Self { Some(T::top()) }
}

impl<T: Lattice + Clone> Lattice for Rc<T> {
   fn meet_mut(&mut self, other: Self) -> bool {
      match self.as_ref().partial_cmp(&other) {
         Some(Ordering::Less | Ordering::Equal) => false,
         Some(Ordering::Greater) => {
            *self = other;
            true
         },
         // Stable in 1.76:
         // None => Rc::make_mut(self).meet_mut(Rc::unwrap_or_clone(other))
         None => Rc::make_mut(self).meet_mut(Rc::try_unwrap(other).unwrap_or_else(|rc| (*rc).clone())),
      }
   }

   fn join_mut(&mut self, other: Self) -> bool {
      match self.as_ref().partial_cmp(&other) {
         Some(Ordering::Greater | Ordering::Equal) => false,
         Some(Ordering::Less) => {
            *self = other;
            true
         },
         // Stable in 1.76:
         // None => Rc::make_mut(self).join_mut(Rc::unwrap_or_clone(other))
         None => Rc::make_mut(self).join_mut(Rc::try_unwrap(other).unwrap_or_else(|rc| (*rc).clone())),
      }
   }
}

impl<T: Lattice + Clone> Lattice for Arc<T> {
   fn meet_mut(&mut self, other: Self) -> bool {
      match self.as_ref().partial_cmp(&other) {
         Some(Ordering::Less | Ordering::Equal) => false,
         Some(Ordering::Greater) => {
            *self = other;
            true
         },
         // Stable in 1.76:
         // None => Arc::make_mut(self).meet_mut(Arc::unwrap_or_clone(other))
         None => Arc::make_mut(self).meet_mut(Arc::try_unwrap(other).unwrap_or_else(|rc| (*rc).clone())),
      }
   }

   fn join_mut(&mut self, other: Self) -> bool {
      match self.as_ref().partial_cmp(&other) {
         Some(Ordering::Greater | Ordering::Equal) => false,
         Some(Ordering::Less) => {
            *self = other;
            true
         },
         // Stable in 1.76:
         // None => Arc::make_mut(self).join_mut(Arc::unwrap_or_clone(other))
         None => Arc::make_mut(self).join_mut(Arc::try_unwrap(other).unwrap_or_else(|rc| (*rc).clone())),
      }
   }
}

impl<T: Lattice + Sized> Lattice for Box<T> {
   fn meet_mut(&mut self, other: Self) -> bool { self.as_mut().meet_mut(*other) }

   fn join_mut(&mut self, other: Self) -> bool { self.as_mut().join_mut(*other) }
}

impl<T: Lattice> Lattice for Reverse<T> {
   #[inline]
   fn meet(self, other: Self) -> Self { Reverse(self.0.join(other.0)) }

   #[inline]
   fn join(self, other: Self) -> Self { Reverse(self.0.meet(other.0)) }

   #[inline]
   fn meet_mut(&mut self, other: Self) -> bool { self.0.join_mut(other.0) }

   #[inline]
   fn join_mut(&mut self, other: Self) -> bool { self.0.meet_mut(other.0) }
}

impl<T: BoundedLattice> BoundedLattice for Reverse<T> {
   #[inline]
   fn bottom() -> Self { Reverse(T::top()) }

   #[inline]
   fn top() -> Self { Reverse(T::bottom()) }
}

#[cfg(test)]
mod tests {
   use std::sync::Arc;

   use crate::Lattice;

   #[test]
   fn test_arc_lattice() {
      let x = Arc::new(42);
      let y = Arc::new(17);
      assert_eq!(*x.clone().meet(y.clone()), 17);
      assert_eq!(*x.meet(y), 17);

      let x = Arc::new(42);
      let y = Arc::new(17);
      assert_eq!(*x.clone().join(y.clone()), 42);
      assert_eq!(*x.join(y), 42);
   }
}
