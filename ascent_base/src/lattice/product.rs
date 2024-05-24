use paste::paste;

use super::{BoundedLattice, Lattice};
use std::cmp::Ordering;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// A wrapper for tuple types and arrays that implements `PartialOrd` using
/// [product-order](https://en.wikipedia.org/wiki/Product_order) semantics. 
/// 
/// `Lattice` and `BoundedLattice` traits are also implemented.
/// 
/// Difference from lexicographical ordering (the `PartialOrd` implementation for tuple types):
/// ```
/// # use ascent_base::lattice::Product;
/// assert!(!{Product((1,4)) < Product((2,3))});
/// assert!((1,4) < (2,3));
/// 
/// ```
pub struct Product<T>(pub T);

#[inline]
fn combine_orderings(ord1: Ordering, ord2: Ordering) -> Option<Ordering>{
   use Ordering::*;
   match (ord1, ord2) {
      (Equal, _) => Some(ord2),
      (_, Equal) => Some(ord1),
      (Less, Less) => Some(Less),
      (Greater, Greater) => Some(Greater),
      _ => None
   }
}

macro_rules! tuple_lattice_impl{
   ($($i:tt),*) => { paste!(
      impl< $([<T $i>]: PartialOrd),* > PartialOrd for Product<($([<T $i>]),*,)> {
         fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            let mut res = Ordering::Equal;
            $(
               match self.0.$i.partial_cmp(&other.0.$i) {
                  None => return None,
                  Some(ord) => {
                     match combine_orderings(ord, res) {
                        None => return None,
                        Some(new_res) => res = new_res,
                     }
                  }
               };
            )*
            Some(res)
         }
      }
      impl< $([<T $i>]: Lattice),* > Lattice for Product<($([<T $i>]),*,)> {
         fn meet_mut(&mut self, other: Self) -> bool {
            let mut changed = false;
            $(changed |= self.0.$i.meet_mut(other.0.$i);)*
            changed
         }

         fn join_mut(&mut self, other: Self) -> bool {
            let mut changed = false;
            $(changed |= self.0.$i.join_mut(other.0.$i);)*
            changed
         }

         fn meet(self, other: Self) -> Self {
            Product(($(self.0.$i.meet(other.0.$i)),*,))
         }

         fn join(self, other: Self) -> Self {
            Product(($(self.0.$i.join(other.0.$i)),*,))
         }
      }

      impl< $([<T $i>]: BoundedLattice),* > BoundedLattice for Product<($([<T $i>]),*,)> where Product<($([<T $i>]),*,)>: Lattice  {
         fn bottom() -> Self {
            Product(($([<T $i>]::bottom()),*,))
         }

         fn top() -> Self {
            Product(($([<T $i>]::top()),*,))
         }
      }
   );};
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


#[test]
fn test_product_lattice(){
   let t1 = Product((1, 3));
   let t2 = Product((0, 10));

   assert_eq!(Lattice::meet(t1, t2), Product((0, 3)));
   assert_eq!(Lattice::join(t1, t2), Product((1, 10)));
   assert_eq!(Product::<(u32, u32)>::bottom(), Product((0,0)));


   assert!(Product((1,3)) < Product((2,3)));
   assert!(!{Product((1,4)) < Product((2,3))});
   assert!(Product((1,4)).partial_cmp(&Product((2,3))).is_none());
}


impl <const N: usize, T: PartialOrd> PartialOrd for Product<[T; N]> {
   fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      let mut ord = Ordering::Equal;
      for i in 0..N {
         let ith_ord = self.0[i].partial_cmp(&other.0[i]);
         match ith_ord {
            None => return None,
            Some(ith_ord) => match combine_orderings(ith_ord, ord) {
               Some(new_ord) => ord = new_ord,
               None => return None,
            },
         }
      }
      Some(ord)
   }
}

#[test]
fn test_product_of_array_partial_ord() {
   let a1 = Product([1, 2, 3]);
   let a2 = Product([1, 3, 4]);
   assert_eq!(a1.partial_cmp(&a2), Some(Ordering::Less));
   assert_eq!(a2.partial_cmp(&a1), Some(Ordering::Greater));

   let a3 = Product([0, 2, 4]);
   assert_eq!(a1.partial_cmp(&a3), None);
   assert_eq!(a3.partial_cmp(&a1), None);

   assert_eq!(a2.partial_cmp(&a3), Some(Ordering::Greater));
   assert_eq!(a3.partial_cmp(&a2), Some(Ordering::Less));
}

impl <const N: usize, T: Lattice> Lattice for Product<[T; N]> {
   fn meet_mut(&mut self, other: Self) -> bool {
      let mut changed = false;
      for (l, r) in self.0.iter_mut().zip(other.0){
         changed |= l.meet_mut(r);
      }
      changed
   }
 
   fn join_mut(&mut self, other: Self) -> bool {
      let mut changed = false;
      for (l, r) in self.0.iter_mut().zip(other.0){
         changed |= l.join_mut(r);
      }
      changed
   }
}

impl <const N: usize, T: BoundedLattice> BoundedLattice for Product<[T; N]> {
   fn bottom() -> Self {
      // unstable:
      // Product(std::array::from_fn(|_| T::bottom()))
      Product([(); N].map(|_| T::bottom()))
   }

   fn top() -> Self {
      Product([(); N].map(|_| T::top()))
   }
}

#[test]
fn test_product_of_array_lattice() {
   let a1 = Product([1, 5, 3]);
   let a2 = Product([1, 3, 4]);
   let a1_a2_meet = Product([1,3,3]);
   let a1_a2_join = Product([1,5,4]);
   assert_eq!(a1.meet(a2), a1_a2_meet);
   assert_eq!(a1.join(a2), a1_a2_join);

   assert_eq!(Product([0; 3]), Product::<[u32; 3]>::bottom());
   assert_eq!(Product([true; 4]), Product::<[bool; 4]>::top());
}