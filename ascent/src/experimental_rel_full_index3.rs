use crate::internal::{RelIndexWrite, MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME, RelFullIndexWrite, RelFullIndexRead};
use crate::rel_index_read::{RelIndexRead, RelIndexReadAll};

use bitvec::prelude::Msb0;
use bitvec::slice::IterOnes;
use hashbrown::HashMap;
use instant::Instant;
use rustc_hash::FxHasher;

use std::hash::{Hash, Hasher, BuildHasherDefault, BuildHasher};

const SIZE: usize = 64;

fn hash<K: Hash>(k: &K) -> u64 {
   let mut hasher = FxHasher::default();
   k.hash(&mut hasher);
   hasher.finish()
}

pub struct RelFullIndex3<K> { 
   mask: bitvec::array::BitArray<[u8; SIZE / 8], Msb0>,
   data: Vec<hashbrown::HashMap<K, usize, BuildHasherDefault<FxHasher>>>,
   len: usize
}

impl<K> Default for RelFullIndex3<K> {
   fn default() -> Self { 
      let mut res = Vec::with_capacity(SIZE);
      for _ in 0..SIZE {res.push(Default::default())}
      Self { mask: Default::default(), data: res, len: 0 }
   }
}

impl<K: Eq + Hash> RelIndexWrite for RelFullIndex3<K>{
   type Key = K;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      
      to.len += from.len;
      to.mask |= from.mask;
      for i in from.mask.iter_ones() {
         hm_append(&mut to.data[i], &mut from.data[i]);
      }

      from.mask = Default::default();
      from.len = 0;

      unsafe {
         MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }
   }

   #[inline(always)]
   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      let hash = hash(&key);
      let ind = hash as usize % SIZE;
      hm.mask.set(ind, true);
      hm.data[ind as usize].raw_entry_mut().from_key_hashed_nocheck(hash, &key).insert(key, tuple_index);
      hm.len += 1;
   }
}

impl <K: Clone + Hash + Eq> RelFullIndexWrite for RelFullIndex3<K> {  
   type Key = K;
   
   fn insert_if_not_present(&mut self, key: &Self::Key, v: usize) -> bool {
      let hash = hash(key);
      let ind = hash as usize % SIZE;

      match self.data[ind as usize].raw_entry_mut().from_key_hashed_nocheck(hash, key) {
         hashbrown::hash_map::RawEntryMut::Occupied(_) => false,
         hashbrown::hash_map::RawEntryMut::Vacant(vacant) => {
            self.mask.set(ind, true);
            vacant.insert_hashed_nocheck(hash, key.clone(), v);
            self.len += 1;
            true
         }
      }
   }
}

impl<'a, K: Eq + std::hash::Hash> RelFullIndexRead for RelFullIndex3<K> {
   type Key = K;
   
   fn contains_key(&self, key: &Self::Key) -> bool {
      let hash = hash(key);
      let ind = hash as usize % SIZE;
      self.data[ind].raw_entry().from_key_hashed_nocheck(hash, key).is_some()
   }
}

impl<'a, K: Eq + std::hash::Hash> RelIndexRead<'a> for RelFullIndex3<K> {
   type IteratorType = std::iter::Once<usize>;
   type Key = K;

   // #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let hash = hash(key);
      let ind = hash as usize % SIZE;
      let res: Option<std::iter::Once<usize>> = 
         self.data[ind].raw_entry().from_key_hashed_nocheck(hash, key).map(|(_k, v)| std::iter::once(*v));
      res
   }

   #[inline(always)]
   fn len(&self) -> usize { self.len }
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexReadAll<'a> for RelFullIndex3<K> {
   type Key = K;

   type ValueIteratorType = std::iter::Once<usize>;

   type AllIteratorType = std::iter::FlatMap<DataIter<'a, K>, std::iter::Map<hashbrown::hash_map::Iter<'a, K, usize>, for <'aa> fn((&'aa K, &usize)) -> (&'aa K, std::iter::Once<usize>)>, fn (&HashMap<K, usize, BuildHasherDefault<FxHasher>>) -> std::iter::Map<hashbrown::hash_map::Iter<K, usize>, for<'aaa> fn((&'aaa K, &usize)) -> (&'aaa K, std::iter::Once<usize>)>>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: std::iter::FlatMap<DataIter<'a, K>, std::iter::Map<hashbrown::hash_map::Iter<'a, K, usize>, for <'aa> fn((&'aa K, &usize)) -> (&'aa K, std::iter::Once<usize>)>, fn (&HashMap<K, usize, BuildHasherDefault<FxHasher>>) -> std::iter::Map<hashbrown::hash_map::Iter<K, usize>, for<'aaa> fn((&'aaa K, &usize)) -> (&'aaa K, std::iter::Once<usize>)>> 
         = DataIter::new(self).flat_map(read_all_mapper);
      res
   }
}

fn read_all_mapper<K>(hm: &HashMap<K, usize, BuildHasherDefault<FxHasher>>) -> std::iter::Map<hashbrown::hash_map::Iter<K, usize>, for <'aa> fn((&'aa K, &usize)) -> (&'aa K, std::iter::Once<usize>)> {
   let res: std::iter::Map<hashbrown::hash_map::Iter<K, usize>, for<'aa> fn((&'aa K, &usize)) -> (&'aa K, std::iter::Once<usize>)> 
      = hm.iter().map(|(k, v)| (k, std::iter::once(*v)));
   res
}

pub struct DataIter<'a, K> {
   ind: &'a RelFullIndex3<K>,
   ones: IterOnes<'a, u8, Msb0>
}

impl<'a, K> DataIter<'a, K> {
   fn new(ind: &'a RelFullIndex3<K>) -> Self {
      Self { ind, ones: ind.mask.iter_ones() }
   }
}

impl<'a, K> Iterator for DataIter<'a, K> {
   type Item = &'a hashbrown::HashMap<K, usize, BuildHasherDefault<FxHasher>>;

   fn next(&mut self) -> Option<Self::Item> {
      self.ones.next().map(|i| &self.ind.data[i])
   }
}


fn hm_append<K: Eq + Hash, V, H: BuildHasher>(to: &mut HashMap<K, V, H>, from: &mut HashMap<K, V, H>) {
   if from.len() > to.len() {
      std::mem::swap(from, to);
   }
   to.reserve(from.len());
   for (k, v) in from.drain() {
      to.insert(k, v);
   }
}
