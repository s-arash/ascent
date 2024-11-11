use std::collections::HashSet;
use std::hash::{BuildHasherDefault, Hash};

use ascent_base::util::update;
use dashmap::{DashMap, SharedValue};
use instant::Instant;
use rustc_hash::FxHasher;

use crate::c_rel_index::{DashMapViewParIter, shards_count};
use crate::internal::{
   CRelIndexRead, CRelIndexReadAll, CRelIndexWrite, Freezable, RelIndexMerge, RelIndexRead, RelIndexReadAll,
   RelIndexWrite,
};

type SetType<T> = HashSet<T>;
pub enum CLatIndex<K, V> {
   Unfrozen(DashMap<K, SetType<V>, BuildHasherDefault<FxHasher>>),
   Frozen(dashmap::ReadOnlyView<K, SetType<V>, BuildHasherDefault<FxHasher>>),
}

impl<K: Clone + Hash + Eq, V: Clone + Hash + Eq> Freezable for CLatIndex<K, V> {
   fn freeze(&mut self) {
      update(self, |_self| match _self {
         CLatIndex::Unfrozen(dm) => Self::Frozen(dm.into_read_only()),
         CLatIndex::Frozen(_) => _self,
      })
   }

   fn unfreeze(&mut self) {
      update(self, |_self| match _self {
         CLatIndex::Frozen(v) => Self::Unfrozen(v.into_inner()),
         CLatIndex::Unfrozen(_) => _self,
      })
   }
}

impl<K: Clone + Hash + Eq, V: Clone + Hash + Eq> CLatIndex<K, V> {
   #[inline]
   pub fn unwrap_frozen(&self) -> &dashmap::ReadOnlyView<K, SetType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CLatIndex::Frozen(v) => v,
         CLatIndex::Unfrozen(_) => panic!("CRelIndex::unwrap_frozen(): object is Unfrozen"),
      }
   }

   #[inline]
   pub fn unwrap_unfrozen(&self) -> &DashMap<K, SetType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CLatIndex::Unfrozen(dm) => dm,
         CLatIndex::Frozen(_) => panic!("CRelIndex::unwrap_unfrozen(): object is Frozen"),
      }
   }

   #[inline]
   pub fn unwrap_mut_unfrozen(&mut self) -> &mut DashMap<K, SetType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CLatIndex::Unfrozen(dm) => dm,
         CLatIndex::Frozen(_) => panic!("CRelIndex::unwrap_unfrozen(): object is Frozen"),
      }
   }

   pub fn into_read_only(self) -> dashmap::ReadOnlyView<K, SetType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CLatIndex::Unfrozen(dm) => dm.into_read_only(),
         CLatIndex::Frozen(f) => f,
      }
   }

   #[inline]
   fn insert(&self, key: K, value: V) {
      match self.unwrap_unfrozen().entry(key) {
         dashmap::mapref::entry::Entry::Occupied(mut occ) => {
            occ.get_mut().insert(value);
         },
         dashmap::mapref::entry::Entry::Vacant(vac) => {
            let mut set = SetType::default();
            set.insert(value);
            vac.insert(set);
         },
      }
   }

   #[inline]
   pub fn hash_usize(&self, k: &K) -> usize { self.unwrap_unfrozen().hash_usize(k) }
}

impl<K: Clone + Hash + Eq, V> Default for CLatIndex<K, V> {
   fn default() -> Self {
      // Self::Unfrozen(Default::default())
      Self::Unfrozen(DashMap::with_hasher_and_shard_amount(Default::default(), shards_count()))
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Clone + Hash + Eq> RelIndexRead<'a> for CLatIndex<K, V> {
   type Key = K;
   type Value = &'a V;

   type IteratorType = std::collections::hash_set::Iter<'a, V>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let vals = &self.unwrap_frozen().get(key)?;
      let res = vals.iter();
      Some(res)
   }

   fn len(&self) -> usize { self.unwrap_frozen().len() }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Clone + Hash + Eq + Sync> CRelIndexRead<'a> for CLatIndex<K, V> {
   type Key = K;
   type Value = &'a V;

   type IteratorType = rayon::collections::hash_set::Iter<'a, V>;

   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      use rayon::prelude::*;
      let vals = &self.unwrap_frozen().get(key)?;
      let res = vals.par_iter();
      Some(res)
   }
}

impl<'a, K: 'a + Clone + Hash + Eq + Send + Sync, V: 'a + Clone + Hash + Eq + Send + Sync> RelIndexWrite
   for CLatIndex<K, V>
{
   type Key = K;
   type Value = V;

   fn index_insert(&mut self, key: Self::Key, value: Self::Value) {
      let dm = self.unwrap_mut_unfrozen();
      // let shard = dm.determine_map(&key);
      // let entry = dm.shards_mut()[shard].get_mut().entry(key).or_insert(SharedValue::new(Default::default()));
      // entry.get_mut().push(value);

      let hash = dm.hash_usize(&key);
      let shard = dm.determine_shard(hash);
      let entry = dm.shards_mut()[shard]
         .get_mut()
         .raw_entry_mut()
         .from_key_hashed_nocheck(hash as u64, &key)
         .or_insert(key, SharedValue::new(Default::default()));
      entry.1.get_mut().insert(value);
   }
}

impl<'a, K: 'a + Clone + Hash + Eq + Send + Sync, V: 'a + Clone + Hash + Eq + Send + Sync> RelIndexMerge
   for CLatIndex<K, V>
{
   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      let from = from.unwrap_mut_unfrozen();
      let to = to.unwrap_mut_unfrozen();

      use rayon::prelude::*;
      assert_eq!(from.shards().len(), to.shards().len());
      to.shards_mut().par_iter_mut().zip(from.shards_mut().par_iter_mut()).for_each(|(to, from)| {
         let from = from.get_mut();
         let to = to.get_mut();

         if from.len() > to.len() {
            std::mem::swap(from, to);
         }

         for (k, mut v) in from.drain() {
            match to.entry(k) {
               hashbrown::hash_map::Entry::Occupied(mut occ) => {
                  let occ = occ.get_mut().get_mut();
                  let v_mut = v.get_mut();
                  if v_mut.len() > occ.len() {
                     std::mem::swap(occ, v_mut);
                  }
                  let v = v.into_inner();
                  occ.reserve(v.len());
                  occ.extend(&mut v.into_iter());
               },
               hashbrown::hash_map::Entry::Vacant(vac) => {
                  vac.insert(v);
               },
            }
         }
      });
      unsafe {
         crate::internal::MOVE_REL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Clone + Hash + Eq> RelIndexReadAll<'a> for CLatIndex<K, V> {
   type Key = &'a K;
   type Value = V;

   type ValueIteratorType = std::iter::Cloned<std::collections::hash_set::Iter<'a, V>>;
   type AllIteratorType = Box<dyn Iterator<Item = (&'a K, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      // let res = DashMapViewParIter::new(self.unwrap_frozen()).map(|(k, v)| (k, v.iter().cloned()));
      let res = self.unwrap_frozen().iter().map(|(k, v)| (k, v.iter().cloned()));
      Box::new(res) as _
   }
}

impl<'a, K: 'a + Clone + Hash + Eq + Sync + Send, V: 'a + Clone + Hash + Eq + Sync + Send> CRelIndexReadAll<'a>
   for CLatIndex<K, V>
{
   type Key = &'a K;
   type Value = &'a V;

   type ValueIteratorType = rayon::collections::hash_set::Iter<'a, V>;

   type AllIteratorType = rayon::iter::Map<
      DashMapViewParIter<'a, K, SetType<V>, BuildHasherDefault<FxHasher>>,
      for<'aa, 'bb> fn((&'aa K, &'bb SetType<V>)) -> (&'aa K, rayon::collections::hash_set::Iter<'bb, V>),
   >;

   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      use rayon::prelude::*;
      let res: Self::AllIteratorType = DashMapViewParIter::new(self.unwrap_frozen()).map(|(k, v)| (k, v.par_iter()));
      res
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Clone + Hash + Eq> CRelIndexWrite for CLatIndex<K, V> {
   type Key = K;
   type Value = V;

   #[inline(always)]
   fn index_insert(&self, key: Self::Key, value: Self::Value) {
      // let before = Instant::now();
      self.insert(key, value);
      // unsafe {
      //    crate::internal::INDEX_INSERT_TOTAL_TIME += before.elapsed();
      // }
   }
}
