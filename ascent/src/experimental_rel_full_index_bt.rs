use crate::internal::{RelIndexWrite, MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME, RelFullIndexWrite, RelFullIndexRead};
use crate::rel_index_read::{RelIndexRead, RelIndexReadAll};

use instant::Instant;
use rustc_hash::FxHasher;
use smallvec::{SmallVec, smallvec};

use std::collections::BTreeMap;
use std::hash::Hasher;
use std::hash::Hash;


pub struct RelFullIndexBT<K>(BTreeMap<u64, SmallVec<[(K, usize); 1]>>);

impl<K> Default for RelFullIndexBT<K> {
   fn default() -> Self { Self(Default::default()) }
}

fn hash<K: Hash>(k: &K) -> u64 {
   let mut hasher = FxHasher::default();
   k.hash(&mut hasher);
   hasher.finish()
}

impl<K: Eq + Hash> RelIndexWrite for RelFullIndexBT<K>{
   type Key = K;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      // to.0.append(&mut from.0);
      if from.len() > to.len() {
         std::mem::swap(from, to);
      }
      let from_iter = std::mem::replace(from, Default::default()).0.into_iter();
      for (h, mut kv) in from_iter {
         match to.0.entry(h) {
            std::collections::btree_map::Entry::Vacant(vacant) => {vacant.insert(kv);},
            std::collections::btree_map::Entry::Occupied(mut occupied) => {occupied.get_mut().append(&mut kv);},
         }
      }
      unsafe {
         MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }

   }

   #[inline(always)]
   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      let hash = hash(&key);
      match hm.0.entry(hash) {
         std::collections::btree_map::Entry::Vacant(vacant) => {vacant.insert(smallvec![(key, tuple_index)]);},
         std::collections::btree_map::Entry::Occupied(mut occupied) => {occupied.get_mut().push((key, tuple_index));},
      }
   }
}

impl<K: Hash + Eq> RelFullIndexRead for RelFullIndexBT<K> {
   type Key = K;

   fn contains_key(&self, key: &Self::Key) -> bool {
      if let Some(pairs) = self.0.get(&hash(key)){
         for (k, _v) in pairs {
            if k == key {
               return true
            }
         }
         false
      } else {false}
   }
}

impl <K: Clone + Hash + Eq> RelFullIndexWrite for RelFullIndexBT<K> {  
   type Key = K;
   #[inline]
   fn insert_if_not_present(&mut self, key: &Self::Key, v: usize) -> bool {
      let hash = hash(key);
      match self.0.entry(hash) {
         std::collections::btree_map::Entry::Vacant(vacant) => {
            vacant.insert(smallvec![(key.clone(), v)]);
            true
         },
         std::collections::btree_map::Entry::Occupied(mut occupied) => {
            for (k, _v) in occupied.get().iter() {
               if k == key { return false }
            }
            occupied.get_mut().push((key.clone(), v));
            true
         },
      }
   }
}

impl<'a, K: Eq + std::hash::Hash> RelIndexRead<'a> for RelFullIndexBT<K> {
   type IteratorType = std::iter::Once<usize>;
   type Key = K;

   // #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      if let Some(pairs) = self.0.get(&hash(key)){
         for (k, v) in pairs {
            if k == key {
               return Some(std::iter::once(*v))
            }
         }
         None
      } else {None}
   }

   #[inline(always)]
   fn len(&self) -> usize { self.0.len() }
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexReadAll<'a> for RelFullIndexBT<K> {

   type Key = K;
   type ValueIteratorType = std::iter::Once<usize>;

   type AllIteratorType = std::iter::FlatMap<std::collections::btree_map::Iter<'a, u64, SmallVec<[(K, usize); 1]>>, std::iter::Map<std::slice::Iter<'a, (K, usize)>, fn(&(K, usize)) -> (&K, std::iter::Once<usize>)>, for <'aa> fn ((&u64, &'aa SmallVec<[(K, usize); 1]>)) -> std::iter::Map<std::slice::Iter<'aa, (K, usize)>, fn(&(K, usize)) -> (&K, std::iter::Once<usize>)>>;

   // #[inline]
   fn iter_all(&'a self) -> Self::AllIteratorType {
      // let res: std::iter::Map<hashbrown::hash_map::Iter<K, usize>, for <'aa, 'bb> fn ((&'aa K, &'bb usize)) -> (&'aa K, std::iter::Once<usize>)> = 
      //    self.iter().map(rel_full_inedx_type_iter_all_mapper);
      // res
      let res: std::iter::FlatMap<std::collections::btree_map::Iter<u64, SmallVec<[(K, usize); 1]>>, std::iter::Map<std::slice::Iter<(K, usize)>, fn(&(K, usize)) -> (&K, std::iter::Once<usize>)>, for <'aa> fn ((&u64, &'aa SmallVec<[(K, usize); 1]>)) -> std::iter::Map<std::slice::Iter<'aa, (K, usize)>, fn(&(K, usize)) -> (&K, std::iter::Once<usize>)>> 
         = self.0.iter().flat_map(rel_full_index_bt_read_all_mapper);
      res
   }
}
fn rel_full_index_bt_read_all_mapper<'a, K>(kv: (&u64, &'a SmallVec<[(K, usize); 1]>)) -> std::iter::Map<std::slice::Iter<'a, (K, usize)>, fn (&(K, usize)) -> (&K, std::iter::Once<usize>)> {
   let res: std::iter::Map<std::slice::Iter<(K, usize)>, fn (&(K, usize)) -> (&K, std::iter::Once<usize>)> 
      = kv.1.iter().map(rel_full_index_bt_read_all_mapper_inner);
   res
}

fn rel_full_index_bt_read_all_mapper_inner<K>(kv: &(K, usize)) -> (&K, std::iter::Once<usize>) {
   (&kv.0, std::iter::once(kv.1))
}
