use core::slice::Iter;
use std::collections::HashSet;
use std::hash::BuildHasherDefault;
use std::iter::Chain;

use rustc_hash::FxHasher;

use crate::internal::*;

#[allow(clippy::len_without_is_empty)]
pub trait RelIndexRead<'a> {
   type Key;
   type Value;
   type IteratorType: Iterator<Item = Self::Value> + Clone + 'a;
   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType>;
   fn len(&'a self) -> usize;
}

pub trait RelIndexReadAll<'a> {
   type Key: 'a;
   type Value;
   type ValueIteratorType: Iterator<Item = Self::Value> + 'a;
   type AllIteratorType: Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a;
   fn iter_all(&'a self) -> Self::AllIteratorType;
}

impl<'a, K: Eq + std::hash::Hash + 'a, V: Clone + 'a> RelIndexRead<'a> for RelIndexType1<K, V> {
   type IteratorType = core::slice::Iter<'a, V>;
   type Key = K;
   type Value = &'a V;

   #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let v = self.get(key)?;
      Some(v.iter())
   }

   #[inline(always)]
   fn len(&self) -> usize { Self::len(self) }
}

impl<'a, K: Eq + std::hash::Hash + 'a, V: 'a + Clone> RelIndexReadAll<'a> for RelIndexType1<K, V> {
   type Key = &'a K;
   type Value = &'a V;
   type ValueIteratorType = core::slice::Iter<'a, V>;

   type AllIteratorType = std::iter::Map<
      std::collections::hash_map::Iter<'a, K, Vec<V>>,
      for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, Iter<'bb, V>),
   >;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.iter().map(|(k, v)| (k, v.iter()));
      res
   }
}

impl<'a, K: Eq + std::hash::Hash, V: 'a + Clone> RelIndexRead<'a> for HashBrownRelFullIndexType<K, V> {
   type IteratorType = std::iter::Once<&'a V>;
   type Key = K;
   type Value = &'a V;

   #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let res = self.get(key)?;
      Some(std::iter::once(res))
   }

   #[inline(always)]
   fn len(&self) -> usize { Self::len(self) }
}

impl<'a, K: Eq + std::hash::Hash + 'a, V: 'a + Clone> RelIndexReadAll<'a> for HashBrownRelFullIndexType<K, V> {
   type Key = &'a K;
   type Value = &'a V;
   type ValueIteratorType = std::iter::Once<&'a V>;

   type AllIteratorType = std::iter::Map<
      hashbrown::hash_map::Iter<'a, K, V>,
      for<'aa, 'bb> fn((&'aa K, &'bb V)) -> (&'aa K, std::iter::Once<&'bb V>),
   >;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.iter().map(|(k, v)| (k, std::iter::once(v)));
      res
   }
}

impl<'a, K: Eq + std::hash::Hash, V: 'a + Clone> RelIndexRead<'a> for LatticeIndexType<K, V> {
   type IteratorType = std::collections::hash_set::Iter<'a, V>;
   type Key = K;
   type Value = &'a V;

   #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let res: Option<std::collections::hash_set::Iter<'a, V>> = self.get(key).map(HashSet::iter);
      res
   }

   #[inline(always)]
   fn len(&self) -> usize { Self::len(self) }
}

impl<'a, K: Eq + std::hash::Hash + 'a, V: 'a + Clone> RelIndexReadAll<'a> for LatticeIndexType<K, V> {
   type Key = &'a K;
   type Value = &'a V;
   type ValueIteratorType = std::collections::hash_set::Iter<'a, V>;

   type AllIteratorType = std::iter::Map<
      std::collections::hash_map::Iter<'a, K, HashSet<V, BuildHasherDefault<FxHasher>>>,
      for<'aa, 'bb> fn(
         (&'aa K, &'bb std::collections::HashSet<V, BuildHasherDefault<FxHasher>>),
      ) -> (&'aa K, std::collections::hash_set::Iter<'bb, V>),
   >;

   #[inline]
   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.iter().map(|(k, v)| (k, v.iter()));
      res
   }
}

pub struct RelIndexCombined<'a, Ind1, Ind2> {
   pub ind1: &'a Ind1,
   pub ind2: &'a Ind2,
}

impl<'a, Ind1, Ind2> RelIndexCombined<'a, Ind1, Ind2> {
   #[inline]
   pub fn new(ind1: &'a Ind1, ind2: &'a Ind2) -> Self { Self { ind1, ind2 } }
}

impl<'a, Ind1, Ind2, K, V> RelIndexRead<'a> for RelIndexCombined<'a, Ind1, Ind2>
where
   Ind1: RelIndexRead<'a, Key = K, Value = V>,
   Ind2: RelIndexRead<'a, Key = K, Value = V>,
{
   type Key = K;
   type Value = V;

   type IteratorType = Chain<
      std::iter::Flatten<std::option::IntoIter<Ind1::IteratorType>>,
      std::iter::Flatten<std::option::IntoIter<Ind2::IteratorType>>,
   >;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      match (self.ind1.index_get(key), self.ind2.index_get(key)) {
         (None, None) => None,
         (iter1, iter2) => {
            let res = iter1.into_iter().flatten().chain(iter2.into_iter().flatten());
            Some(res)
         },
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

impl<'a, Ind1, Ind2, K: 'a, V: 'a, VTI: Iterator<Item = V> + 'a> RelIndexReadAll<'a>
   for RelIndexCombined<'a, Ind1, Ind2>
where
   Ind1: RelIndexReadAll<'a, Key = K, ValueIteratorType = VTI>,
   Ind2: RelIndexReadAll<'a, Key = K, ValueIteratorType = VTI>,
{
   type Key = K;
   type Value = V;

   type ValueIteratorType = VTI;

   type AllIteratorType = Chain<Ind1::AllIteratorType, Ind2::AllIteratorType>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self.ind1.iter_all().chain(self.ind2.iter_all());
      res
   }
}
