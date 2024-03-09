use instant::Instant;
use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};

use crate::internal::{RelIndexWrite, CRelIndexWrite};
use crate::rel_index_read::{RelIndexRead, RelIndexReadAll, CRelIndexRead, CRelIndexReadAll};
use dashmap::RwLock;

pub struct CRelNoIndex<V> {
   // TODO remove pub
   pub vec: Vec<RwLock<Vec<V>>>,
   // vec: [RwLock<Vec<V>>; 32],
   frozen: bool,
}

impl<V> Default for CRelNoIndex<V> {
   #[inline]
   fn default() -> Self {
      let threads = rayon::current_num_threads().max(1);
      let mut vec = Vec::with_capacity(threads);
      for _ in 0..threads {
         vec.push(RwLock::new(vec![]));
      }
      Self { vec, frozen: false }

      // Self { vec: array_init::array_init(|_| RwLock::new(vec![])), frozen: false }
   }
}

impl<V> CRelNoIndex<V> {
   pub fn freeze(&mut self) { self.frozen = true; }
   pub fn unfreeze(&mut self) { self.frozen = false; }
   pub fn hash_usize(&self, _key: &()) -> usize { 0 }
}

impl<'a, V: 'a> RelIndexRead<'a> for CRelNoIndex<V> {
   type Key = ();
   type Value = &'a V;

   type IteratorType = std::iter::FlatMap<std::slice::Iter<'a, RwLock<Vec<V>>>, std::slice::Iter<'a, V>, fn(&RwLock<Vec<V>>) -> std::slice::Iter<V>>;

   fn index_get(&'a self, _key: &Self::Key) -> Option<Self::IteratorType> {
      assert!(self.frozen);
      let res: Self::IteratorType = self.vec.iter().flat_map(|v| {
         let data = unsafe { &*v.data_ptr()};
         data.iter()
      });
      Some(res)
   }

   #[inline(always)]
   fn len(&self) -> usize { 1 }
}

impl<'a, V: 'a + Sync + Send> CRelIndexRead<'a> for CRelNoIndex<V> {
   type Key = ();
   type Value = &'a V;

   type IteratorType = rayon::iter::FlatMap<rayon::slice::Iter<'a, RwLock<Vec<V>>>, fn(&RwLock<Vec<V>>) -> rayon::slice::Iter<V>>;

   fn c_index_get(&'a self, _key: &Self::Key) -> Option<Self::IteratorType> {
      assert!(self.frozen);
      let res: Self::IteratorType = self.vec.par_iter().flat_map(|v| {
         let data = unsafe {&* v.data_ptr()};
         data.par_iter()
      });
      Some(res)
   }
}

impl<'a, V: 'a> RelIndexWrite for CRelNoIndex<V> {
   type Key = ();
   type Value = V;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      assert_eq!(from.len(), to.len());
      // not necessary because we have a mut reference
      // assert!(!from.frozen);
      // assert!(!to.frozen);

      from.vec.iter_mut().zip(to.vec.iter_mut()).for_each(|(from, to)| {
         let from = from.get_mut();
         let to = to.get_mut();

         if from.len() > to.len() {
            std::mem::swap(from, to);
         }
         to.append(from);
      });
      unsafe {
         crate::internal::MOVE_NO_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }
   }

   fn index_insert(ind: &mut Self, _key: Self::Key, value: Self::Value) {
      // not necessary because we have a mut reference
      // assert!(!ind.frozen);
      let shard_idx = rayon::current_thread_index().unwrap_or(0) % ind.vec.len();
      ind.vec[shard_idx].get_mut().push(value);
   }
}

impl<'a, V: 'a> CRelIndexWrite for CRelNoIndex<V> {
   type Key = ();
   type Value = V;

   fn index_insert(ind: &Self, _key: Self::Key, value: Self::Value) {
      assert!(!ind.frozen);
      let shard_idx = rayon::current_thread_index().unwrap_or(0) % ind.vec.len();
      ind.vec[shard_idx].write().push(value);
   }
}

impl<'a, V: 'a> RelIndexReadAll<'a> for CRelNoIndex<V> {
   type Key = ();
   type Value = &'a V;

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;

   type AllIteratorType = std::iter::Once<(&'a (), Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      std::iter::once((&(), self.index_get(&()).unwrap()))
   }
}

impl<'a, V: 'a + Sync + Send> CRelIndexReadAll<'a> for CRelNoIndex<V> {
   type Key = ();
   type Value = &'a V;

   type ValueIteratorType = <Self as CRelIndexRead<'a>>::IteratorType;

   type AllIteratorType = rayon::iter::Once<(&'a (), Self::ValueIteratorType)>;

   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      rayon::iter::once((&(), self.c_index_get(&()).unwrap()))
   }
}