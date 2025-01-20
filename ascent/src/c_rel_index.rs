use std::hash::{BuildHasher, BuildHasherDefault, Hash};

use ascent_base::util::update;
use dashmap::{DashMap, ReadOnlyView, RwLock, SharedValue};
use instant::Instant;
use rayon::iter::IntoParallelIterator;
use rayon::iter::plumbing::UnindexedConsumer;
use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};
use rustc_hash::FxHasher;

use crate::internal::{
   CRelIndexRead, CRelIndexReadAll, CRelIndexWrite, Freezable, RelIndexMerge, RelIndexRead, RelIndexReadAll,
   RelIndexWrite,
};

type VecType<T> = Vec<T>;
pub enum CRelIndex<K, V> {
   Unfrozen(DashMap<K, VecType<V>, BuildHasherDefault<FxHasher>>),
   Frozen(dashmap::ReadOnlyView<K, VecType<V>, BuildHasherDefault<FxHasher>>),
}

impl<K: Clone + Hash + Eq, V> Freezable for CRelIndex<K, V> {
   fn freeze(&mut self) {
      update(self, |_self| match _self {
         CRelIndex::Unfrozen(dm) => Self::Frozen(dm.into_read_only()),
         CRelIndex::Frozen(_) => _self,
      })
   }

   fn unfreeze(&mut self) {
      update(self, |_self| match _self {
         CRelIndex::Frozen(v) => Self::Unfrozen(v.into_inner()),
         CRelIndex::Unfrozen(_) => _self,
      })
   }
}

impl<K: Clone + Hash + Eq, V> CRelIndex<K, V> {
   #[inline]
   pub fn unwrap_frozen(&self) -> &dashmap::ReadOnlyView<K, VecType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CRelIndex::Frozen(v) => v,
         CRelIndex::Unfrozen(_) => panic!("CRelIndex::unwrap_frozen(): object is Unfrozen"),
      }
   }

   #[inline]
   pub fn unwrap_unfrozen(&self) -> &DashMap<K, VecType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CRelIndex::Unfrozen(dm) => dm,
         CRelIndex::Frozen(_) => panic!("CRelIndex::unwrap_unfrozen(): object is Frozen"),
      }
   }

   #[inline]
   pub fn unwrap_mut_unfrozen(&mut self) -> &mut DashMap<K, VecType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CRelIndex::Unfrozen(dm) => dm,
         CRelIndex::Frozen(_) => panic!("CRelIndex::unwrap_unfrozen(): object is Frozen"),
      }
   }

   pub fn into_read_only(self) -> dashmap::ReadOnlyView<K, VecType<V>, BuildHasherDefault<FxHasher>> {
      match self {
         CRelIndex::Unfrozen(dm) => dm.into_read_only(),
         CRelIndex::Frozen(f) => f,
      }
   }

   #[inline]
   #[allow(dead_code)]
   // TODO remove if not used
   fn insert(&self, key: K, value: V) {
      match self.unwrap_unfrozen().entry(key) {
         dashmap::mapref::entry::Entry::Occupied(mut occ) => {
            occ.get_mut().push(value);
         },
         dashmap::mapref::entry::Entry::Vacant(vac) => {
            vac.insert(vec![value]);
         },
      }
   }

   #[inline]
   #[allow(dead_code)]
   // TODO remove if not used
   fn insert2(&self, key: K, value: V) {
      use std::hash::Hasher;

      use dashmap::Map;

      let dm = self.unwrap_unfrozen();
      let mut hasher = dm.hasher().build_hasher();
      key.hash(&mut hasher);
      let hash = hasher.finish();

      let idx = dm.determine_shard(hash as usize);
      let mut shard = unsafe { dm._yield_write_shard(idx) };

      match shard.raw_entry_mut().from_key_hashed_nocheck(hash, &key) {
         hashbrown::hash_map::RawEntryMut::Occupied(mut occ) => {
            occ.get_mut().get_mut().push(value);
         },
         hashbrown::hash_map::RawEntryMut::Vacant(vac) => {
            vac.insert(key, SharedValue::new(vec![value]));
         },
      }
   }

   #[inline]
   pub fn hash_usize(&self, k: &K) -> usize { self.unwrap_unfrozen().hash_usize(k) }
}

impl<K: Clone + Hash + Eq, V> Default for CRelIndex<K, V> {
   fn default() -> Self {
      // Self::Unfrozen(Default::default())
      Self::Unfrozen(DashMap::with_hasher_and_shard_amount(Default::default(), shards_count()))
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a> RelIndexRead<'a> for CRelIndex<K, V> {
   type Key = K;
   type Value = &'a V;

   type IteratorType = std::slice::Iter<'a, V>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let vals = &self.unwrap_frozen().get(key)?;
      let res = vals.iter();
      Some(res)
   }

   fn len_estimate(&self) -> usize {
      let sample_size = 4;
      let shards = self.unwrap_frozen().shards();
      let (count, sum) = shards.iter().take(sample_size).fold((0, 0), |(c, s), shard| (c + 1, s + shard.read().len()));
      sum * shards.len() / count
   }

   fn is_empty(&'a self) -> bool {
      let shards = self.unwrap_frozen().shards();
      shards.iter().all(|s| s.read().is_empty())
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Sync> CRelIndexRead<'a> for CRelIndex<K, V> {
   type Key = K;
   type Value = &'a V;

   type IteratorType = rayon::slice::Iter<'a, V>;

   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      use rayon::prelude::*;
      let vals = &self.unwrap_frozen().get(key)?;
      let res = vals.as_slice().par_iter();
      Some(res)
   }
}

impl<K: Clone + Hash + Eq + Send + Sync, V: Send + Sync> RelIndexWrite for CRelIndex<K, V> {
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
      entry.1.get_mut().push(value);
   }
}

impl<K: Clone + Hash + Eq + Send + Sync, V: Send + Sync> RelIndexMerge for CRelIndex<K, V> {
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
                  occ.append(&mut v.into_inner());
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

impl<'a, K: 'a + Clone + Hash + Eq, V: Clone + 'a> RelIndexReadAll<'a> for CRelIndex<K, V> {
   type Key = &'a K;
   type Value = &'a V;

   type ValueIteratorType = std::slice::Iter<'a, V>;

   type AllIteratorType = Box<dyn Iterator<Item = (&'a K, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self.unwrap_frozen().iter().map(|(k, v)| (k, v.iter()));
      Box::new(res) as _
   }
}
pub struct DashMapViewParIter<'a, K, V, S> {
   shards: &'a [RwLock<hashbrown::HashMap<K, SharedValue<V>, S>>],
}

impl<K, V, S> Clone for DashMapViewParIter<'_, K, V, S> {
   fn clone(&self) -> Self { Self { shards: self.shards } }
}

impl<'a, K: Eq + Hash, V, S: BuildHasher + Clone> DashMapViewParIter<'a, K, V, S> {
   pub fn new(v: &'a ReadOnlyView<K, V, S>) -> Self { Self { shards: v.shards() } }
}

// taken from DashMap rayon::map::Iter ParallelIterator impl
impl<'a, K, V, S> ParallelIterator for DashMapViewParIter<'a, K, V, S>
where
   K: Send + Sync + Eq + Hash,
   V: Send + Sync,
   S: Send + Sync + Clone + BuildHasher,
{
   type Item = (&'a K, &'a V);

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where C: UnindexedConsumer<Self::Item> {
      self
         .shards
         .into_par_iter()
         .flat_map(|shard| {
            let sref = unsafe { shard.data_ptr().as_ref().unwrap() };
            sref.par_iter().map(move |(k, v)| (k, v.get()))
         })
         .drive_unindexed(consumer)
   }
}

type CRelIndexReadAllParIterShard<K, V, S> = hashbrown::HashMap<K, SharedValue<VecType<V>>, S>;

pub struct CRelIndexReadAllParIter<'a, K, V, S> {
   shards: &'a [RwLock<CRelIndexReadAllParIterShard<K, V, S>>],
}

impl<'a, K, V, S> ParallelIterator for CRelIndexReadAllParIter<'a, K, V, S>
where
   K: Send + Sync + Eq + Hash,
   V: Send + Sync,
   S: Send + Sync + Clone + BuildHasher,
{
   type Item = (&'a K, rayon::slice::Iter<'a, V>);

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where C: UnindexedConsumer<Self::Item> {
      self
         .shards
         .into_par_iter()
         .flat_map(|shard| {
            let sref = unsafe { shard.data_ptr().as_ref().unwrap() };
            sref.par_iter().map(|(k, v)| (k, v.get().par_iter()))
         })
         .drive_unindexed(consumer)
   }
}

impl<'a, K: 'a + Clone + Hash + Eq + Sync + Send, V: Clone + 'a + Sync + Send> CRelIndexReadAll<'a>
   for CRelIndex<K, V>
{
   type Key = &'a K;
   type Value = &'a V;

   type ValueIteratorType = rayon::slice::Iter<'a, V>;

   type AllIteratorType = CRelIndexReadAllParIter<'a, K, V, BuildHasherDefault<FxHasher>>;

   #[inline]
   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      CRelIndexReadAllParIter { shards: self.unwrap_frozen().shards() }
   }
}

impl<K: Clone + Hash + Eq, V> CRelIndexWrite for CRelIndex<K, V> {
   type Key = K;
   type Value = V;

   #[inline(always)]
   fn index_insert(&self, key: Self::Key, value: Self::Value) {
      // let before = Instant::now();
      self.insert(key, value);
      // ind.insert2(key, value);
      // unsafe {
      //    crate::internal::INDEX_INSERT_TOTAL_TIME += before.elapsed();
      // }
   }
}

pub fn shards_count() -> usize {
   static RES: once_cell::sync::Lazy<usize> = once_cell::sync::Lazy::new(|| {
      (rayon::current_num_threads() * 4).next_power_of_two()
      // (std::thread::available_parallelism().map_or(1, usize::from) * 4).next_power_of_two()
   });
   *RES
}
