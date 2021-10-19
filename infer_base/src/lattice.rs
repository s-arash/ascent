pub mod constant_propagation;

pub trait Lattice: PartialOrd {
   fn meet(self, other: Self) -> Self;
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
   fn top() -> Self { true }
   fn bottom() -> Self { false }
}

macro_rules! num_lattice_impl {
   ($int:ty) => {
      impl Lattice for $int {
         fn meet(self, other: Self) -> Self { self.min(other) }
         fn join(self, other: Self) -> Self { self.max(other) }
      }
      impl BoundedLattice for $int {
         fn top() -> Self { Self::MAX }
         fn bottom() -> Self { Self::MIN }
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

#[derive(PartialEq, PartialOrd, Eq)]
struct Dual<T>(pub T);

impl<T> Lattice for Dual<T> where T: Lattice {
   fn meet(self, other: Self) -> Self { Dual(self.0.join(other.0)) }
   fn join(self, other: Self) -> Self { Dual(self.0.meet(other.0)) }
}

impl<T> BoundedLattice for Dual<T> where T: BoundedLattice {
   fn top() -> Self { Dual(T::bottom()) }
   fn bottom() -> Self { Dual(T::top()) }
}