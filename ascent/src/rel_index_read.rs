
use nohash_hasher::BuildNoHashHasher;

use crate::internal::*;

use std::collections::HashSet;
use core::slice::Iter;
use std::iter::Chain;

pub trait RelIndexRead<'a>{
   type Key;
   type IteratorType: Iterator<Item = usize> + Clone + 'a;
   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType>;
   fn len(&self) -> usize;
}

pub trait RelIndexReadAll<'a>{
   type Key: 'a;
   type ValueIteratorType: Iterator<Item = usize> + 'a;
   type AllIteratorType: Iterator<Item = (&'a Self::Key, Self::ValueIteratorType)> + 'a;
   fn iter_all(&'a self) -> Self::AllIteratorType;
}


impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexRead<'a> for RelIndexType1<K> {
   type IteratorType = std::iter::Cloned<core::slice::Iter<'a, usize>>;
   type Key = K;

   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      self.get(key).map(|vec| vec.iter().cloned())
   }

   #[inline(always)]
   fn len(&self) -> usize { self.len() }
}

// #[inline]
fn rel_index_type_iter_all_mapper<'a,'b,K>((k,v): (&'a K, &'b Vec<usize>)) -> (&'a K, std::iter::Cloned<Iter<'b, usize>>) {
   (k, v.iter().cloned())
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexReadAll<'a> for RelIndexType1<K> {

   type Key = K;
   type ValueIteratorType = std::iter::Cloned<core::slice::Iter<'a, usize>>;

   type AllIteratorType = std::iter::Map<std::collections::hash_map::Iter<'a, K, Vec<usize>>, for <'aa, 'bb> fn ((&'aa K, &'bb Vec<usize>)) -> (&'aa K, std::iter::Cloned<Iter<'bb, usize>>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: std::iter::Map<std::collections::hash_map::Iter<'a, K, Vec<usize>>, for <'aa, 'bb> fn ((&'aa K, &'bb Vec<usize>)) -> (&'aa K, std::iter::Cloned<Iter<'bb, usize>>)> 
         = self.iter().map(rel_index_type_iter_all_mapper);
      res
   }
}


impl<'a, K: Eq + std::hash::Hash> RelIndexRead<'a> for HashBrownRelFullIndexType<K> {
   type IteratorType = std::iter::Once<usize>;
   type Key = K;

   // #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let res: Option<std::iter::Once<usize>> = self.get(key).cloned().map(std::iter::once);
      res
   }

   #[inline(always)]
   fn len(&self) -> usize { self.len() }
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexReadAll<'a> for HashBrownRelFullIndexType<K> {

   type Key = K;
   type ValueIteratorType = std::iter::Once<usize>;

   type AllIteratorType = std::iter::Map<hashbrown::hash_map::Iter<'a, K, usize>, for <'aa, 'bb> fn ((&'aa K, &'bb usize)) -> (&'aa K, std::iter::Once<usize>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: std::iter::Map<hashbrown::hash_map::Iter<K, usize>, for <'aa, 'bb> fn ((&'aa K, &'bb usize)) -> (&'aa K, std::iter::Once<usize>)> = 
         self.iter().map(rel_full_inedx_type_iter_all_mapper);
      res
   }
}


// #[inline]
fn rel_full_inedx_type_iter_all_mapper<'a, 'b, K>((k, v): (&'a K, &'b usize)) -> (&'a K, std::iter::Once<usize>) {
   (k, std::iter::once(*v))
}

// #[inline]
fn lattice_index_type_iter_all_mapper<'a, 'b, K>((k,v): (&'a K, &'b std::collections::HashSet<usize, BuildNoHashHasher<usize>>)) 
-> (&'a K, std::iter::Cloned<std::collections::hash_set::Iter<'b, usize>>) {
   (k, v.iter().cloned())
}

impl<'a, K: Eq + std::hash::Hash> RelIndexRead<'a> for LatticeIndexType<K> {
   type IteratorType = std::iter::Cloned<std::collections::hash_set::Iter<'a, usize>>;
   type Key = K;

   #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let res: Option<std::iter::Cloned<std::collections::hash_set::Iter<usize>>> = 
         self.get(key).map(HashSet::iter).map(Iterator::cloned);
      res
   }

   #[inline(always)]
   fn len(&self) -> usize { self.len() }
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexReadAll<'a> for LatticeIndexType<K> {

   type Key = K;
   type ValueIteratorType = std::iter::Cloned<std::collections::hash_set::Iter<'a, usize>>;

   type AllIteratorType = std::iter::Map<std::collections::hash_map::Iter<'a, K, HashSet<usize, BuildNoHashHasher<usize>>>, for <'aa, 'bb> fn ((&'aa K, &'bb std::collections::HashSet<usize, BuildNoHashHasher<usize>>)) -> (&'aa K, std::iter::Cloned<std::collections::hash_set::Iter<'bb, usize>>)>;

   #[inline]
   fn iter_all(&'a self) -> Self::AllIteratorType {
      use std::collections::hash_map::Iter;
      type H = std::hash::BuildHasherDefault<nohash_hasher::NoHashHasher<usize>>;
      let res: std::iter::Map<Iter<K, HashSet<usize, H>>, for <'aa, 'bb> fn ((&'aa K, &'bb HashSet<usize, H>)) -> (&'aa K, std::iter::Cloned<std::collections::hash_set::Iter<'bb, usize>>)> 
         = self.iter().map(lattice_index_type_iter_all_mapper);
      res
   }
}


pub struct RelIndexCombined<'a, Ind1, Ind2> {
   pub ind1: &'a Ind1,
   pub ind2: &'a Ind2,
}

impl <'a, Ind1, Ind2> RelIndexCombined<'a, Ind1, Ind2> {
   #[inline]
   pub fn new(ind1: &'a Ind1, ind2: &'a Ind2) -> Self { Self { ind1, ind2 } }
} 

impl <'a, Ind1, Ind2, K> RelIndexRead<'a> for RelIndexCombined<'a, Ind1, Ind2> 
where Ind1: RelIndexRead<'a, Key = K>,  Ind2: RelIndexRead<'a, Key = K>, {
   type Key = K;

   type IteratorType = Chain<std::iter::Flatten<std::option::IntoIter<Ind1::IteratorType>>, 
                             std::iter::Flatten<std::option::IntoIter<Ind2::IteratorType>>>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      match (self.ind1.index_get(key), self.ind2.index_get(key)) {
         (None, None) => None,
         (iter1, iter2) => {
            let res = iter1.into_iter().flatten().chain(iter2.into_iter().flatten());
            Some(res)
         }
      }
   }

   #[inline(always)]
   fn len(&self) -> usize { self.ind1.len() + self.ind2.len() }
}

// impl <'a, Ind> RelIndexRead<'a> for RelIndexCombined<'a, Ind, Ind> 
// where Ind: RelIndexTrait2<'a> {
//     type Key = Ind::Key;

//     type IteratorType = EitherIter<Ind::IteratorType, Chain<Ind::IteratorType, Ind::IteratorType>>;

//    fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
//       let res: Option<EitherIter<<Ind as RelIndexTrait2>::IteratorType, Chain<<Ind as RelIndexTrait2>::IteratorType, <Ind as RelIndexTrait2>::IteratorType>>> = 
//       match (self.ind1.index_get(key), self.ind2.index_get(key)) {
//          (None, None) => None,
//          (Some(it), None) | (None, Some(it)) => Some(EitherIter::Left(it)),
//          (Some(it1), Some(it2)) => Some(EitherIter::Right(it1.chain(it2)))
//       };
//       res
//    }

//    #[inline(always)]
//    fn len(&self) -> usize { self.ind1.len() + self.ind2.len() }
// }

// #[derive(Clone)]
// pub enum EitherIter<L, R> {
//    Left(L),
//    Right(R)
// }

// impl <L: Iterator<Item = T>, R: Iterator<Item = T>, T> Iterator for EitherIter<L, R> {
//    type Item = T;

//    fn next(&mut self) -> Option<Self::Item> {
//       match self {
//          EitherIter::Left(l) => l.next(),
//          EitherIter::Right(r) => r.next(),
//       }
//    }
// }

impl <'a, Ind1, Ind2, K: 'a, VTI: Iterator<Item = usize> + 'a> RelIndexReadAll<'a> for RelIndexCombined<'a, Ind1, Ind2> 
where Ind1: RelIndexReadAll<'a, Key = K, ValueIteratorType = VTI>,  Ind2: RelIndexReadAll<'a, Key = K, ValueIteratorType = VTI>
{
    type Key = K;

   type ValueIteratorType = VTI;

   type AllIteratorType = Chain<Ind1::AllIteratorType, Ind2::AllIteratorType>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self.ind1.iter_all().chain(self.ind2.iter_all());
      res
   }
}