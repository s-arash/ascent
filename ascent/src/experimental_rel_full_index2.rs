use crate::internal::{RelIndexWrite, MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME, RelFullIndexWrite};
use crate::rel_index_read::RelIndexRead;

use instant::Instant;
use rustc_hash::FxHasher;

use std::hash::BuildHasherDefault;
use std::hash::Hash;


pub(crate) struct RelFullIndex2<K>(hashbrown::HashMap<K, usize, BuildHasherDefault<FxHasher>>);

impl<K> Default for RelFullIndex2<K> {
   fn default() -> Self { Self(Default::default()) }
}

impl<K: Eq + Hash> RelIndexWrite for RelFullIndex2<K>{
   type Key = K;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      if from.len() > to.len() {
         std::mem::swap(from, to);
      }
      to.0.reserve(from.len());
      
      for (k, v) in from.0.drain() {
         to.0.insert_unique_unchecked(k, v); // TODO could be improved
      }
      unsafe {
         MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }

   }

   #[inline(always)]
   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      hm.0.insert(key, tuple_index);
   }
}

impl <K: Clone + Hash + Eq> RelFullIndexWrite for RelFullIndex2<K> {  
   type Key = K;
   #[inline]
   fn insert_if_not_present(&mut self, key: &Self::Key, v: usize) -> bool {
      match self.0.raw_entry_mut().from_key(key) {
         hashbrown::hash_map::RawEntryMut::Occupied(_) => false,
         hashbrown::hash_map::RawEntryMut::Vacant(vacant) => {vacant.insert(key.clone(), v); true},
      }
   }
}

impl<'a, K: Eq + std::hash::Hash> RelIndexRead<'a> for RelFullIndex2<K> {
   type IteratorType = std::iter::Once<usize>;
   type Key = K;

   // #[inline]
   fn index_get(&'a self, key: &K) -> Option<Self::IteratorType> {
      let res: Option<std::iter::Once<usize>> = self.0.get(key).cloned().map(std::iter::once);
      res
   }

   #[inline(always)]
   fn len(&self) -> usize { self.0.len() }
}