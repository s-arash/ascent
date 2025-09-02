use std::hash::{BuildHasherDefault, Hash};

use ascent_base::util::update;
use dashmap::{DashMap, SharedValue};
use instant::Instant;
use rustc_hash::FxHasher;

use crate::c_rel_index::{DashMapViewParIter, shards_count};
use crate::internal::{
   CRelFullIndexWrite, CRelIndexRead, CRelIndexReadAll, CRelIndexWrite, Freezable, RelFullIndexRead, RelFullIndexWrite,
   RelIndexMerge, RelIndexRead, RelIndexReadAll, RelIndexWrite,
};

pub enum CRelFullIndex<K, V> {
   Unfrozen(DashMap<K, V, BuildHasherDefault<FxHasher>>),
   Frozen(dashmap::ReadOnlyView<K, V, BuildHasherDefault<FxHasher>>),
}

impl<K: Clone + Hash + Eq, V> Freezable for CRelFullIndex<K, V> {
   fn freeze(&mut self) {
      update(self, |_self| match _self {
         Self::Unfrozen(dm) => Self::Frozen(dm.into_read_only()),
         Self::Frozen(_) => _self,
      })
   }

   fn unfreeze(&mut self) {
      update(self, |_self| match _self {
         Self::Frozen(v) => Self::Unfrozen(v.into_inner()),
         Self::Unfrozen(dm) => Self::Unfrozen(dm),
      })
   }
}

impl<K: Clone + Hash + Eq, V> CRelFullIndex<K, V> {
   pub fn exact_len(&self) -> usize {
      match self {
         Self::Unfrozen(uf) => uf.len(),
         Self::Frozen(f) => f.len(),
      }
   }

   #[inline]
   pub fn unwrap_frozen(&self) -> &dashmap::ReadOnlyView<K, V, BuildHasherDefault<FxHasher>> {
      match self {
         Self::Frozen(v) => v,
         Self::Unfrozen(_) => panic!("CRelFullIndex::unwrap_frozen(): object is Unfrozen"),
      }
   }

   #[inline]
   pub fn unwrap_unfrozen(&self) -> &DashMap<K, V, BuildHasherDefault<FxHasher>> {
      match self {
         Self::Unfrozen(dm) => dm,
         Self::Frozen(_) => panic!("CRelFullIndex::unwrap_unfrozen(): object is Frozen"),
      }
   }

   #[inline]
   pub fn unwrap_mut_unfrozen(&mut self) -> &mut DashMap<K, V, BuildHasherDefault<FxHasher>> {
      match self {
         Self::Frozen(_) => panic!("CRelFullIndex::unwrap_mut_unfrozen(): object is Frozen"),
         Self::Unfrozen(dm) => dm,
      }
   }

   pub fn into_read_only(self) -> dashmap::ReadOnlyView<K, V, BuildHasherDefault<FxHasher>> {
      match self {
         Self::Unfrozen(dm) => dm.into_read_only(),
         Self::Frozen(f) => f,
      }
   }

   #[inline]
   fn insert(&self, key: K, value: V) { self.unwrap_unfrozen().insert(key, value); }

   pub fn hash_usize(&self, k: &K) -> usize { self.unwrap_unfrozen().hash_usize(k) }

   pub fn get_cloned(&self, key: &K) -> Option<V>
   where V: Clone {
      match self {
         Self::Unfrozen(uf) => uf.get(key).map(|x| x.value().clone()),
         Self::Frozen(f) => f.get(key).cloned(),
      }
   }

   pub fn insert_if_not_present2(&self, key: &K, value: V) -> bool {
      let dm = self.unwrap_unfrozen();

      let hash = dm.hash_usize(&key);

      let idx = dm.determine_shard(hash);
      use dashmap::Map;
      let mut shard = unsafe { dm._yield_write_shard(idx) };

      match shard.raw_entry_mut().from_key_hashed_nocheck(hash as u64, key) {
         hashbrown::hash_map::RawEntryMut::Occupied(_) => false,
         hashbrown::hash_map::RawEntryMut::Vacant(vac) => {
            vac.insert(key.clone(), SharedValue::new(value));
            true
         },
      }
   }
}

impl<K: Clone + Hash + Eq, V> Default for CRelFullIndex<K, V> {
   fn default() -> Self {
      // Self::Unfrozen(Default::default())
      Self::Unfrozen(DashMap::with_hasher_and_shard_amount(Default::default(), shards_count()))
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a> RelIndexRead<'a> for CRelFullIndex<K, V> {
   type Key = K;
   type Value = &'a V;

   type IteratorType = std::iter::Once<&'a V>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let val = self.unwrap_frozen().get(key)?;
      let res = std::iter::once(val);
      Some(res)
   }

   fn len_estimate(&self) -> usize { self.unwrap_frozen().len() }

   fn is_empty(&'a self) -> bool { self.unwrap_frozen().len() == 0 }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Sync> CRelIndexRead<'a> for CRelFullIndex<K, V> {
   type Key = K;

   type Value = &'a V;

   type IteratorType = rayon::iter::Once<&'a V>;

   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let val = self.unwrap_frozen().get(key)?;
      let res = rayon::iter::once(val);
      Some(res)
   }
}

impl<K: Clone + Hash + Eq, V> RelFullIndexRead<'_> for CRelFullIndex<K, V> {
   type Key = K;

   #[inline(always)]
   fn contains_key(&self, key: &Self::Key) -> bool { self.unwrap_frozen().contains_key(key) }
}

impl<K: Clone + Hash + Eq, V> RelFullIndexWrite for CRelFullIndex<K, V> {
   type Key = K;
   type Value = V;

   fn insert_if_not_present(&mut self, key: &Self::Key, v: Self::Value) -> bool {
      self.unfreeze();
      match self.unwrap_mut_unfrozen().entry(key.clone()) {
         dashmap::mapref::entry::Entry::Occupied(_) => false,
         dashmap::mapref::entry::Entry::Vacant(vac) => {
            vac.insert(v);
            true
         },
      }
   }
}

impl<K: Clone + Hash + Eq, V> CRelFullIndexWrite for CRelFullIndex<K, V> {
   type Key = K;
   type Value = V;

   fn insert_if_not_present(&self, key: &Self::Key, v: Self::Value) -> bool {
      // TODO decide what to do here
      // let before = Instant::now();

      // let res = match self.unwrap_unfrozen().entry(key.clone()) {
      //    dashmap::mapref::entry::Entry::Occupied(_) => false,
      //    dashmap::mapref::entry::Entry::Vacant(vac) => {
      //       vac.insert_quick(v);
      //       true
      //    },
      // };
      let res = self.insert_if_not_present2(key, v);
      // unsafe {
      //    crate::internal::INDEX_INSERT_TOTAL_TIME += before.elapsed();
      // }

      #[allow(clippy::let_and_return)]
      res
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a + Clone> RelIndexReadAll<'a> for CRelFullIndex<K, V> {
   type Key = &'a K;
   type Value = V;

   type ValueIteratorType = std::iter::Once<V>;
   type AllIteratorType = Box<dyn Iterator<Item = (&'a K, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self.unwrap_frozen().iter().map(|(k, v)| (k, std::iter::once(v.clone())));
      Box::new(res) as _
   }
}

impl<'a, K: 'a + Clone + Hash + Eq + Send + Sync, V: 'a + Clone + Send + Sync> CRelIndexReadAll<'a>
   for CRelFullIndex<K, V>
{
   type Key = &'a K;
   type Value = &'a V;

   type ValueIteratorType = rayon::iter::Once<&'a V>;
   // type AllIteratorType = Box<dyn Iterator<Item = (&'a K, Self::ValueIteratorType)> + 'a>;

   type AllIteratorType = rayon::iter::Map<
      DashMapViewParIter<'a, K, V, BuildHasherDefault<FxHasher>>,
      for<'aa, 'bb> fn((&'aa K, &'bb V)) -> (&'aa K, rayon::iter::Once<&'bb V>),
   >;

   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      use rayon::prelude::*;
      let res: Self::AllIteratorType =
         DashMapViewParIter::new(self.unwrap_frozen()).map(|(k, v)| (k, rayon::iter::once(v)));
      res
   }
}

impl<K: Clone + Hash + Eq + Send + Sync, V: Send + Sync> RelIndexWrite for CRelFullIndex<K, V> {
   type Key = K;
   type Value = V;

   fn index_insert(&mut self, key: Self::Key, value: Self::Value) {
      let dm = self.unwrap_mut_unfrozen();

      // let shard = dm.determine_map(&key);
      // dm.shards_mut()[shard].get_mut().insert(key, SharedValue::new(value));

      let hash = dm.hash_usize(&key);
      let shard = dm.determine_shard(hash);
      dm.shards_mut()[shard]
         .get_mut()
         .raw_entry_mut()
         .from_key_hashed_nocheck(hash as u64, &key)
         .insert(key, SharedValue::new(value));
   }
}

impl<K: Clone + Hash + Eq + Send + Sync, V: Send + Sync> RelIndexMerge for CRelFullIndex<K, V> {
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
         to.reserve(from.len());
         for (k, v) in from.drain() {
            to.insert(k, v);
         }
      });

      unsafe {
         crate::internal::MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }
   }
}

impl<K: Clone + Hash + Eq, V> CRelIndexWrite for CRelFullIndex<K, V> {
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
