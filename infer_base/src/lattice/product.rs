use paste::paste;

use super::{BoundedLattice, Lattice, ProtoLattice};
use std::cmp::Ordering;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// A wrapper for tuple types that provides an implementation of `PartialOrd` using
/// [product-order](https://en.wikipedia.org/wiki/Product_order) semantics. In addition, 
/// `Lattice` and `BoundedLattice` traits are also implemented.
/// 
/// Difference from lexicographical ordering (the `PartialOrd` implementation for tuple types):
/// ```
/// assert!(!{Product((1,4)) < Product((2,3))});
/// assert!((1,4) < (2,3));
/// 
/// ```
struct Product<T>(pub T);

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
      impl< $([<T $i>]: Lattice),* > ProtoLattice for Product<($([<T $i>]),*,)> {
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
   assert!(Product((1,4)).partial_cmp(&Product((2,3))) == None);
}
