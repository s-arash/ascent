use ascent_base::util::update;
use dashmap::{DashMap, RwLock, SharedValue, ReadOnlyView};
use instant::Instant;
use rayon::iter::plumbing::UnindexedConsumer;
use rayon::prelude::ParallelIterator;
use rustc_hash::FxHasher;
use std::hash::{Hash, BuildHasherDefault, BuildHasher};

use crate::internal::{RelIndexWrite, CRelIndexWrite, RelFullIndexRead, RelFullIndexWrite, CRelFullIndexWrite};
use crate::rel_index_read::{RelIndexRead, RelIndexReadAll, CRelIndexRead, CRelIndexReadAll};

use rayon::iter::IntoParallelIterator;


type VecType<T> = Vec<T>;
pub enum CRelIndex<K, V> {
   Unfrozen(DashMap<K, VecType<V>, BuildHasherDefault<FxHasher>>),
   Frozen(dashmap::ReadOnlyView<K, VecType<V>, BuildHasherDefault<FxHasher>>)
}

impl<K: Clone + Hash + Eq, V> CRelIndex<K, V> {
   pub fn freeze(&mut self) {
      update(self, |_self| match _self {
         CRelIndex::Unfrozen(dm) => Self::Frozen(dm.into_read_only()),
         CRelIndex::Frozen(_) => _self,
      })
   }

   pub fn unfreeze(&mut self) {
      update(self, |_self| match _self {
         CRelIndex::Frozen(v) => Self::Unfrozen(v.into_inner()),
         CRelIndex::Unfrozen(_) => _self,
      })
   }

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
   fn insert(&self, key: K, value: V) {
      match self.unwrap_unfrozen().entry(key) {
         dashmap::mapref::entry::Entry::Occupied(mut occ) => {occ.get_mut().push(value);},
         dashmap::mapref::entry::Entry::Vacant(vac) => {vac.insert(vec![value]);},
      }
   }

   #[inline]
   pub fn hash_usize(&self, k: &K) -> usize {
      self.unwrap_unfrozen().hash_usize(k)
   }
}

impl<K: Clone + Hash + Eq, V> Default for CRelIndex<K, V> {
   fn default() -> Self {
      Self::Unfrozen(Default::default())
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

   fn len(&self) -> usize {
      self.unwrap_frozen().len()
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

impl<'a, K: 'a + Clone + Hash + Eq + Send + Sync, V: 'a + Send + Sync> RelIndexWrite for CRelIndex<K, V> {
   type Key = K;
   type Value = V;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      let from = from.unwrap_mut_unfrozen();
      let to = to.unwrap_mut_unfrozen();

      if from.len() > to.len() {
         std::mem::swap(from, to);
      }

      let from = std::mem::take(from);

      use rayon::prelude::*;
      assert_eq!(from.shards().len(), to.shards().len());
      to.shards_mut().par_iter_mut().zip(from.into_shards().into_vec().into_par_iter()).for_each(|(to, from)| {
         let mut from = from.into_inner();
         let to = to.get_mut();

         if from.len() > to.len() {
            std::mem::swap(&mut from, to);
         }

         for (k, mut v) in from.into_iter() {
            match to.entry(k) {
               hashbrown::hash_map::Entry::Occupied(mut occ) => {
                  let occ = occ.get_mut().get_mut();
                  if v.get().len() > occ.len() {
                     std::mem::swap(occ, v.get_mut());
                  }
                  occ.append(&mut v.into_inner());
               },
               hashbrown::hash_map::Entry::Vacant(vac) => {vac.insert(v);},
            }
         }
      
      });
      // from.into_par_iter().for_each(|(k, mut v)| {
      //    match to.entry(k) {
      //       dashmap::mapref::entry::Entry::Occupied(mut occ) => {
      //          let occ = occ.get_mut();
      //          if occ.len() < v.len() {
      //             std::mem::swap(occ, &mut v);
      //          }
      //          occ.append(&mut v);
      //       },
      //       dashmap::mapref::entry::Entry::Vacant(vac) => {vac.insert(v);},
      //    }
      // });
      unsafe {
         crate::internal::MOVE_REL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }

   }

   fn index_insert(ind: &mut Self, key: Self::Key, value: Self::Value) {
      let mut entry = ind.unwrap_unfrozen().entry(key).or_default();
      entry.push(value);
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: Clone + 'a> RelIndexReadAll<'a> for CRelIndex<K, V> {
   type Key = K;
   type Value = V;

   type ValueIteratorType = std::iter::Cloned<std::slice::Iter<'a, V>>;
   type AllIteratorType = Box<dyn Iterator<Item = (&'a K, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      // let res = DashMapViewParIter::new(self.unwrap_frozen()).map(|(k, v)| (k, v.iter().cloned()));
      let res = self.unwrap_frozen().iter().map(|(k, v)| (k, v.iter().cloned()));
      Box::new(res) as _
   }
}
pub struct DashMapViewParIter<'a, K, V, S> {
   shards: &'a [RwLock<hashbrown::HashMap<K, SharedValue<V>, S>>]
}

impl<'a, K: Eq + Hash, V, S: BuildHasher + Clone> DashMapViewParIter<'a, K, V, S> {
   pub fn new(v: &'a ReadOnlyView<K, V, S>) -> Self {
      Self {
         shards: v.shards()
      }
   }
}

impl<'a, K, V, S> ParallelIterator for DashMapViewParIter<'a, K, V, S>
where
   K: Send + Sync + Eq + Hash,
   V: Send + Sync,
   S: Send + Sync + Clone + BuildHasher,
{
   type Item = (&'a K, &'a V);

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where
      C: UnindexedConsumer<Self::Item>,
   {
      self.shards
         .into_par_iter()
         .flat_map_iter(|shard| {
               let sref = unsafe { shard.data_ptr().as_ref().unwrap() };
               // let guard = shard.read();
               // let sref: &'a HashMap<K, V, S> = guard; //unsafe { util::change_lifetime_const(&*guard) };

               // let guard = Arc::new(guard);
               sref.iter().map(move |(k, v)| {
                  // let guard = Arc::clone(&guard);
                  // unsafe { RefMulti::new(guard, k, v.get()) }
                  (k, v.get())
               })
         })
         .drive_unindexed(consumer)
   }
}


impl<'a, K: 'a + Clone + Hash + Eq + Sync + Send, V: Clone + 'a + Sync + Send> CRelIndexReadAll<'a> for CRelIndex<K, V> {
   type Key = K;

   type Value = &'a V;

   type ValueIteratorType = rayon::slice::Iter<'a, V>;

   type AllIteratorType = 
      rayon::iter::Map<DashMapViewParIter<'a, K, Vec<V>, BuildHasherDefault<FxHasher>>, for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, rayon::slice::Iter<'bb, V>)>;

   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      use rayon::prelude::*;
      let res: rayon::iter::Map<DashMapViewParIter<'a, K, Vec<V>, BuildHasherDefault<FxHasher>>, for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, rayon::slice::Iter<'bb, V>)>
         = DashMapViewParIter::new(self.unwrap_frozen()).map(|(k, v)| (k, v.par_iter()));
      res
   }
}


impl<'a, K: 'a + Clone + Hash + Eq, V: 'a> CRelIndexWrite for CRelIndex<K, V> {
   type Key = K;
   type Value = V;

   #[inline(always)]
   fn index_insert(ind: & Self, key: Self::Key, value: Self::Value) {
      let before = Instant::now();
      ind.insert(key, value);
      unsafe {
         crate::internal::INDEX_INSERT_TOTAL_TIME += before.elapsed();
      }
   }
}


// TODO RelFullIndexRead and CRelFullIndexWrite need a dedicated type
impl<'a, K: 'a + Clone + Hash + Eq, V: 'a> RelFullIndexRead for CRelIndex<K, V> {
   type Key = K;

   #[inline(always)]
   fn contains_key(&self, key: &Self::Key) -> bool {
      match self {
         CRelIndex::Frozen(f) => f.contains_key(key),
         CRelIndex::Unfrozen(_uf) => panic!("contains_key: CRelIndex object is unfrozen"),
      }
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a> RelFullIndexWrite for CRelIndex<K, V> {
   type Key = K;
   type Value = V;

   fn insert_if_not_present(&mut self, key: &Self::Key, v: Self::Value) -> bool {
      self.unfreeze();
      match self.unwrap_mut_unfrozen().entry(key.clone()) {
         dashmap::mapref::entry::Entry::Occupied(_) => false,
         dashmap::mapref::entry::Entry::Vacant(vac) => {
            vac.insert(vec![v]);
            true
         },
      }
   }
}

impl<'a, K: 'a + Clone + Hash + Eq, V: 'a> CRelFullIndexWrite for CRelIndex<K, V> {
   type Key = K;
   type Value = V;

   fn insert_if_not_present(&self, key: &Self::Key, v: Self::Value) -> bool {
      let before = Instant::now();

      let res = match self.unwrap_unfrozen().entry(key.clone()) {
         dashmap::mapref::entry::Entry::Occupied(_) => false,
         dashmap::mapref::entry::Entry::Vacant(vac) => {
            vac.insert(vec![v]);
            true
         },
      };
      unsafe {
         crate::internal::INDEX_INSERT_TOTAL_TIME += before.elapsed();
      }
      res
   }
}