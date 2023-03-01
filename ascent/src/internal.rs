// Provides definitions required for the `ascent` macro. Not intended for direct use.
#![doc(hidden)]

pub use crate::convert::*;

use std::time::Duration;
use std::hash::{BuildHasherDefault, Hash};
use std::collections::{HashMap, HashSet};

pub use instant::Instant;

use ascent_base::{Lattice};
use rustc_hash::FxHasher;

pub use crate::rel_index_read::RelIndexCombined;
pub use crate::rel_index_read::RelIndexRead;
pub use crate::rel_index_read::RelIndexReadAll;

pub use crate::rel_index_read::CRelIndexRead;
pub use crate::rel_index_read::CRelIndexReadAll;

pub type RelIndexType<K> = RelIndexType1<K, usize>;

pub type LatticeIndexType<K, V> = HashMap<K, HashSet<V, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>;

pub(crate) type HashBrownRelFullIndexType<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type RelFullIndexType<K, V> = HashBrownRelFullIndexType<K, V>;

pub type RelNoIndexType = Vec<usize>;

pub use crate::c_rel_index::CRelIndex;
pub use crate::c_rel_full_index::CRelFullIndex;
pub use crate::c_lat_index::CLatIndex;
pub use crate::c_rel_no_index::CRelNoIndex;

pub use crate::c_rel_index::shards_count;

pub trait Freezable {
   fn freeze(&mut self);
   fn unfreeze(&mut self);
}

pub trait RelIndexWrite: Sized {
   type Key;
   type Value;
   fn index_insert(&mut self, key: Self::Key, value: Self::Value);
}


pub trait RelIndexMerge: Sized {
   fn move_index_contents(from: &mut Self, to: &mut Self);
   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      Self::move_index_contents(delta, total);
      std::mem::swap(new, delta);
   }
}

pub trait CRelIndexWrite{
   type Key;
   type Value;
   fn index_insert(&self, key: Self::Key, value: Self::Value);
}

pub trait RelFullIndexRead {
   type Key;
   fn contains_key(&self, key: &Self::Key) -> bool;
}


pub trait RelFullIndexWrite {
   type Key: Clone;
   type Value;
   /// if an entry for `key` does not exist, inserts `v` for it and returns true.
   fn insert_if_not_present(&mut self, key: &Self::Key, v: Self::Value) -> bool;
}

pub trait CRelFullIndexWrite {
   type Key: Clone;
   type Value;
   /// if an entry for `key` does not exist, inserts `v` for it and returns true.
   fn insert_if_not_present(&self, key: &Self::Key, v: Self::Value) -> bool;
}


pub type RelIndexType1<K, V> = HashMap<K, Vec<V>, BuildHasherDefault<FxHasher>>;

pub static mut MOVE_REL_INDEX_CONTENTS_TOTAL_TIME : Duration = Duration::ZERO;
pub static mut INDEX_INSERT_TOTAL_TIME : Duration = Duration::ZERO;

impl<K: Eq + Hash, V> RelIndexWrite for RelIndexType1<K, V>{
   type Key = K;
   type Value = V;

   fn index_insert(&mut self, key: K, value: V) {
      // let before = Instant::now();
      use std::collections::hash_map::Entry::*;
      match self.entry(key){
         Occupied(mut vec) => vec.get_mut().push(value),
         Vacant(vacant) => {
            let mut vec = Vec::with_capacity(4);
            vec.push(value);
            vacant.insert(vec);
         },
      }
      // unsafe {
      //    INDEX_INSERT_TOTAL_TIME += before.elapsed();
      // }
   }
}

impl<K: Eq + Hash, V> RelIndexMerge for RelIndexType1<K, V> {
   fn move_index_contents(from: &mut RelIndexType1<K, V>, to: &mut RelIndexType1<K, V>) {
      let before = Instant::now();
      if from.len() > to.len() {
         std::mem::swap(from, to);
      }
      use std::collections::hash_map::Entry::*;
      for (k, mut v) in from.drain() {
         match to.entry(k) {
            Occupied(existing) => {
               let existing = existing.into_mut();
               if v.len() > existing.len() {
                  std::mem::swap(&mut v, existing);
               }
               existing.append(&mut v);
            },
            Vacant(vacant) => {
               vacant.insert(v);
            },
         }
      }
      unsafe {
         MOVE_REL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }
   }
}

impl RelIndexWrite for RelNoIndexType {
   type Key = ();
   type Value = usize;

   fn index_insert(&mut self, _key: Self::Key, tuple_index: usize) {
      self.push(tuple_index);
   }
}

impl RelIndexMerge for RelNoIndexType {
   fn move_index_contents(ind1: &mut Self, ind2: &mut Self) {
      ind2.append(ind1);
   }
}

impl<K: Eq + Hash, V: Hash + Eq> RelIndexWrite for LatticeIndexType<K, V>{
   type Key = K;
   type Value = V;

   #[inline(always)]
   fn index_insert(&mut self, key: Self::Key, tuple_index: V) {
      self.entry(key).or_default().insert(tuple_index);
   }
}

impl<K: Eq + Hash, V: Hash + Eq> RelIndexMerge for LatticeIndexType<K, V>{
   #[inline(always)]
   fn move_index_contents(hm1: &mut LatticeIndexType<K, V>, hm2: &mut LatticeIndexType<K, V>) {
      for (k,v) in hm1.drain(){
         let set = hm2.entry(k).or_default();
         set.extend(v);
      }
   }
}


pub static mut MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME : Duration = Duration::ZERO;
pub static mut MOVE_NO_INDEX_CONTENTS_TOTAL_TIME : Duration = Duration::ZERO;

impl<K: Eq + Hash, V> RelIndexWrite for HashBrownRelFullIndexType<K, V>{
    type Key = K;
    type Value = V;

   #[inline(always)]
   fn index_insert(&mut self, key: Self::Key, value: V) {
      self.insert(key, value);
      // TODO undo this
      // assert!(hm.insert(key, tuple_index).is_none(), 
      //    "inserting duplicate index into RelFullIndexType, index:{}", tuple_index);
   }
}

impl<K: Eq + Hash, V> RelIndexMerge for HashBrownRelFullIndexType<K, V> {
   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      if from.len() > to.len() {
         std::mem::swap(from, to);
      }
      to.reserve(from.len());
      for (k, v) in from.drain() {
         to.insert(k, v); // TODO could be improved
      }
      unsafe {
         MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      }
   }
}

impl <K: Clone + Hash + Eq, V> RelFullIndexWrite for HashBrownRelFullIndexType<K, V> {
   type Key = K;
   type Value = V;
   #[inline]
   fn insert_if_not_present(&mut self, key: &K, v: V) -> bool {
      match self.raw_entry_mut().from_key(key) {
         hashbrown::hash_map::RawEntryMut::Occupied(_) => false,
         hashbrown::hash_map::RawEntryMut::Vacant(vacant) => {vacant.insert(key.clone(), v); true},
      }
   }
}

impl<K: Hash + Eq, V> RelFullIndexRead for HashBrownRelFullIndexType<K, V> {
    type Key = K;

   fn contains_key(&self, key: &Self::Key) -> bool {
      self.contains_key(key)
   }
}


/// type constraints for relation columns
pub struct TypeConstraints<T> where T : Clone + Eq + Hash{_t: ::core::marker::PhantomData<T>}
/// type constraints for a lattice
pub struct LatTypeConstraints<T> where T : Clone + Eq + Hash + Lattice{_t: ::core::marker::PhantomData<T>}

/// type contraints for parallel Ascent
pub struct ParTypeConstraints<T> where T: Send + Sync {_t: ::core::marker::PhantomData<T>}

#[inline(always)]
pub fn comment(_: &str){}
