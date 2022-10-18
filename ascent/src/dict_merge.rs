use crate::experimental_dict::{DictMerge, DictWrite, DictDrain, MultiDict, Dict, MultiDictDedup};
use std::hash::Hash;

pub struct DictOfDictMergeAsDictMerge0<'a, TDict>(pub &'a mut TDict);

impl<'a, TDict> DictMerge<'a> for DictOfDictMergeAsDictMerge0<'a, TDict> 
where 
for<'aa> TDict: DictMerge<'aa>,
TDict: Default
{
   fn merge(&'a mut self, other: &'a mut Self) {
      self.0.merge(other.0)
   }
}

// What we need:

// pub struct DictOfDictMergeAsDictMerge2<'a, TDict>(pub &'a mut TDict);

// impl<'a, TDict, TDict2, K> DictMerge<'a> for DictOfDictMergeAsDictMerge2<'a, TDict> 
// where 
// for<'aa> TDict: DictWrite<'aa, K = K, V = TDict2>, for<'aa> &'aa mut TDict: DictDrain<'aa, K = K, V = TDict2>, 
// for<'aa> TDict2: DictMerge<'aa>,
// TDict2: Default
// {
//    fn merge(&'a mut self, other: &'a mut Self) {
//       for (k, mut v) in (&mut other.0).drain() {
//          let mut other = &mut v;
//          (self.0.get_mut_or_insert(k, TDict2::default)).merge(&mut other);
//       }
//    }
// }

// pub struct DictOfDictMergeAsDictMerge3<'a, TDict>(pub &'a mut TDict);

// impl<'a, TDict, TDict2, TDict3, K, K2> DictMerge<'a> for DictOfDictMergeAsDictMerge3<'a, TDict> 
// where 
// for<'aa> TDict : DictWrite<'aa, K = K , V = TDict2>, for<'aa> &'aa mut TDict : DictDrain<'aa, K = K , V = TDict2>, 
// for<'aa> TDict2: DictWrite<'aa, K = K2, V = TDict3>, for<'aa> &'aa mut TDict2: DictDrain<'aa, K = K2, V = TDict3>, 
// TDict3: DictMerge2,
// TDict2: Default, TDict3: Default
// {
//    fn merge(&'a mut self, other: &'a mut Self) {
//       for (k2, mut other2) in (&mut other.0).drain() {
//          let self2 = self.0.get_mut_or_insert(k2, TDict2::default);
//          for (k3, mut other3) in (&mut other2).drain() {
//             self2.get_mut_or_insert(k3, TDict3::default).merge2(&mut other3)
//          }
//       }
//    }
// }

macro_rules! dict_of_dict_merge_as_dict_merge {
   ($n: expr, [$($rest: expr),*], [$($shftl: expr),*]) => { paste!(
      pub struct [<DictOfDictMergeAsDictMerge $n>]<'a, TDict>(pub &'a mut TDict);

      impl<'a, [<TDict $n>], $([<TDict $rest>],)*  $([<K $rest>]),*> DictMerge<'a> for [<DictOfDictMergeAsDictMerge $n>]<'a, TDict0> 
      where
      $(for<'aa> [<TDict $rest>]: DictWrite<'aa, K = [<K $rest>], V = [<TDict $shftl>]>, for<'aa> &'aa mut [<TDict $rest>]: DictDrain<'aa, K = [<K $rest>], V = [<TDict $shftl>]>,)*
      for<'aa> [<TDict $n>]: DictMerge<'aa>,
      $([<TDict $shftl>]: Default),*
      {
         fn merge(&'a mut self, other: &'a mut Self) {
            for (k, mut other) in other.0.drain() {
               let self_ = self.0.get_mut_or_insert(k, Default::default);
               dict_of_dict_merge_as_dict_merge_body!(self_, other, k, $($rest,)*)
            }
         }
      }


   );};
}

macro_rules! dict_of_dict_merge_as_dict_merge_body {
   ($self: ident, $other: ident, $k: ident, $n: expr,) => {
      $self.merge(&mut $other)
   };
   ($self: ident, $other: ident, $k: ident, $n: expr, $($rest: expr,)*) => {paste!{
      for (k, mut other) in (&mut $other).drain() {
         let self_ = $self.get_mut_or_insert(k, Default::default);
         dict_of_dict_merge_as_dict_merge_body!(self_, other, k, $($rest,)*)
      }
   }};
   
}

use paste::paste;
dict_of_dict_merge_as_dict_merge!(10, [9, 8, 7, 6, 5, 4, 3, 2, 1, 0], [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(9 , [8, 7, 6, 5, 4, 3, 2, 1, 0], [9, 8, 7, 6, 5, 4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(8 , [7, 6, 5, 4, 3, 2, 1, 0], [8, 7, 6, 5, 4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(7 , [6, 5, 4, 3, 2, 1, 0], [7, 6, 5, 4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(6 , [5, 4, 3, 2, 1, 0], [6, 5, 4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(5 , [4, 3, 2, 1, 0], [5, 4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(4 , [3, 2, 1, 0], [4, 3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(3 , [2, 1, 0], [3, 2, 1]);
dict_of_dict_merge_as_dict_merge!(2 , [1, 0], [2, 1]);
dict_of_dict_merge_as_dict_merge!(1 , [0], [1]);


#[test]
fn test_dict_write_merge() {
   use std::collections::HashMap;
   use crate::experimental_dict::Dict;

   type Concrete = Dict<u8, Dict<u16, Dict<u32, usize>>>;
   type TDict<'a> = DictOfDictMergeAsDictMerge2<'a, Concrete>;
   let mut concrete: Concrete = Dict::default();
   concrete.insert(1, Dict(HashMap::from_iter([(2, Dict(HashMap::from_iter([(3, 4)])))])));
   concrete.insert(10, Dict(HashMap::from_iter([(20, Dict(HashMap::from_iter([(30, 40)])))])));

   let mut transformed: TDict<'_> = DictOfDictMergeAsDictMerge2(&mut concrete);
   let mut concrete2: Concrete = Dict::default();
   concrete2.insert(1,   Dict(HashMap::from_iter([(2,   Dict(HashMap::from_iter([(  4,   5)])))])));
   concrete2.insert(10,  Dict(HashMap::from_iter([(20,  Dict(HashMap::from_iter([( 40,  50)])))])));
   concrete2.insert(100, Dict(HashMap::from_iter([(200, Dict(HashMap::from_iter([(300, 400)])))])));

   let mut transformed2: TDict<'_> = DictOfDictMergeAsDictMerge2(&mut concrete2);

   transformed.merge(&mut transformed2);

   println!("{:?}", concrete);

   assert_eq!(concrete.0[&1].0[&2].0[&3], 4);
   assert_eq!(concrete.0[&1].0[&2].0[&4], 5);
   assert_eq!(concrete.0[&100].0[&200].0[&300], 400);
}

mod simpler {
   //! This is a simpler impl of `DictOfDictMergeAsDictMergeN` types.
   //! Unfortunately, they require Dict Values to be `'static`. Maybe GATs in Rust will
   //! someday help get around that limitation.
   //! https://sabrinajewson.org/blog/the-better-alternative-to-lifetime-gats
    use crate::experimental_dict::{DictMerge};

    use super::DictMerge2;

   #[allow(unused_macros)]
   macro_rules! dict_of_dict_merge_as_dict_merge {
      ($n: expr, $p: expr) => { paste!(
         pub struct [<DictOfDictMergeAsDictMerge $n>]<'a, TDict>(pub &'a mut TDict);
   
         impl<'a, K, TDict, TDict2: 'a> DictMerge<'a> for [<DictOfDictMergeAsDictMerge $n>]<'a, TDict>
         where for<'aa> TDict: DictWrite<'aa, K = K, V = TDict2>, TDict: DictDrain<'a, K = K, V = TDict2>, 
               for<'aa> [<DictOfDictMergeAsDictMerge $p>]<'aa, TDict2>: DictMerge<'aa>,
               TDict2: Default
         {
            fn merge(&'a mut self, other: &'a mut Self) {
               for (k, mut v) in other.0.drain() {
                  let mut other = [<DictOfDictMergeAsDictMerge $p>](&mut v);
                  [<DictOfDictMergeAsDictMerge $p>](self.0.get_mut_or_insert(k, TDict2::default)).merge(&mut other);
               }
            }
         }
         
         impl<'a, K, TDict, TDict2: 'a> DictMerge2 for [<DictOfDictMergeAsDictMerge $n>]<'a, TDict>
         where for<'aa> TDict: DictWrite<'aa, K = K, V = TDict2>, for<'aa> TDict: DictDrain<'aa, K = K, V = TDict2>, 
               for<'aa> [<DictOfDictMergeAsDictMerge $p>]<'aa, TDict2>: DictMerge2,
               TDict2: Default
         {
            fn merge2(&mut self, other: &mut Self) {
               for (k, mut v) in other.0.drain() {
                  let mut other = [<DictOfDictMergeAsDictMerge $p>](&mut v);
                  [<DictOfDictMergeAsDictMerge $p>](self.0.get_mut_or_insert(k, TDict2::default)).merge2(&mut other);
               }
            }
         }  
      );};
   }
   
   pub struct DictOfDictMergeAsDictMerge0<'a, TDict>(pub &'a mut TDict);
   
   impl<'a, TDict> DictMerge<'a> for DictOfDictMergeAsDictMerge0<'a, TDict>
   where TDict: DictMerge<'a> 
   {
      fn merge(&'a mut self, other: &'a mut Self) {
         self.0.merge(other.0);
      }
   }  
   
   impl<'a, TDict> DictMerge2 for DictOfDictMergeAsDictMerge0<'a, TDict>
   where TDict: DictMerge2 
   {
      fn merge2(&mut self, other: &mut Self) {
         self.0.merge2(other.0);
      }
   }  
   
   // use paste::paste;
   // dict_of_dict_merge_as_dict_merge!(1, 0);
   // dict_of_dict_merge_as_dict_merge!(2, 1);
   // dict_of_dict_merge_as_dict_merge!(3, 2);
   // dict_of_dict_merge_as_dict_merge!(4, 3);
   // dict_of_dict_merge_as_dict_merge!(5, 4);
   // dict_of_dict_merge_as_dict_merge!(6, 5);
   // dict_of_dict_merge_as_dict_merge!(7, 6);
   // dict_of_dict_merge_as_dict_merge!(8, 7);
   // dict_of_dict_merge_as_dict_merge!(9, 8);
   // dict_of_dict_merge_as_dict_merge!(10, 9);
}

pub trait DictMerge2: Sized {
   fn merge2(&mut self, other: &mut Self);
}

impl<K: Clone + Eq + Hash, V> DictMerge2 for MultiDict<K, V> {
   fn merge2(&mut self, other: &mut Self) {
      use std::collections::hash_map::Entry::*;
      use crate::experimental_dict::MultiDictRead;
      if other.len() > self.len() {
         std::mem::swap(self, other);
      }
      for (k, mut v) in other.0.drain() {
         match self.0.entry(k) {
            Occupied(existing) => {
               let existing = existing.into_mut();
               if v.len() > existing.len() {
                  std::mem::swap(&mut v, existing);
               }
               existing.append(&mut v);
            },
            Vacant(vacant) => {
               vacant.insert(v);
            },
         }
      }
   }
}

impl<'a, K: Clone + Eq + Hash, V> DictMerge2 for Dict<K, V> {
   fn merge2(&mut self, other: &mut Self) {
      use crate::experimental_dict::DictRead;
      if other.0.len() > self.0.len() {
         std::mem::swap(other, self);
      }
      self.0.reserve(other.len());
      for (k, v) in other.0.drain() {
         self.0.insert(k, v);
      }
   }
}

impl<K: Eq + Hash, V: Eq + Hash> DictMerge2 for MultiDictDedup<K, V> {
   fn merge2(&mut self, other: &mut Self) {
      for (k, mut v) in other.0.drain(){
         use std::collections::hash_map::Entry::*;
         match self.0.entry(k) {
            Occupied(mut occ) => {
               let existing = occ.get_mut();
               if existing.len() < v.len() {
                  std::mem::swap(existing, &mut v);
               }
               existing.extend(v)
            },
            Vacant(vac) => {
               vac.insert(v);
            },
         }
      }
   }
}

impl<'a, T> DictMerge2 for &'a mut T where T: DictMerge2 {
   #[inline(always)]
   fn merge2(&mut self, other: &mut Self) {
      (*self).merge2(*other)
   }
}