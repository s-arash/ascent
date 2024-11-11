use crate::Lattice;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct OrdLattice<T>(pub T);

impl<T: Ord> Lattice for OrdLattice<T> {
   #[inline(always)]
   fn meet(self, other: Self) -> Self { self.min(other) }

   #[inline(always)]
   fn join(self, other: Self) -> Self { self.max(other) }

   fn meet_mut(&mut self, other: Self) -> bool {
      if self.0 > other.0 {
         self.0 = other.0;
         true
      } else {
         false
      }
   }
   fn join_mut(&mut self, other: Self) -> bool {
      if self.0 < other.0 {
         self.0 = other.0;
         true
      } else {
         false
      }
   }
}

#[test]
fn test_ord_lattice() {
   assert_eq!(OrdLattice(42).meet(OrdLattice(22)), OrdLattice(22));

   let mut x = OrdLattice(42);
   assert!(!x.join_mut(OrdLattice(42)));
   assert_eq!(x.0, 42);
   assert!(!x.meet_mut(OrdLattice(42)));
   assert_eq!(x.0, 42);
}
