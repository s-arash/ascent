// Provides definitions required for the `ascent` macro. Not intended for direct use.
#![doc(hidden)]

pub use crate::convert::*;

use std::time::Duration;
use std::hash::{BuildHasherDefault, Hash};
use std::collections::{HashMap, HashSet};

pub use instant::Instant;

use ascent_base::{Lattice};
use nohash_hasher::BuildNoHashHasher;
use rustc_hash::FxHasher;

pub use crate::rel_index_read::RelIndexCombined;
pub use crate::rel_index_read::RelIndexRead;
pub use crate::rel_index_read::RelIndexReadAll;

pub type RelIndexType<K> = RelIndexType1<K>;

pub type LatticeIndexType<K> = HashMap<K, HashSet<usize, BuildNoHashHasher<usize>>, BuildHasherDefault<FxHasher>>;

pub(crate) type HashBrownRelFullIndexType<K> = hashbrown::HashMap<K, usize, BuildHasherDefault<FxHasher>>;
pub type RelFullIndexType<K> = HashBrownRelFullIndexType<K>;

pub type RelNoIndexType = Vec<usize>;

pub trait RelIndexWrite{
   type Key;
   fn move_index_contents(from: &mut Self, to: &mut Self);
   fn index_insert(ind: &mut Self, key: Self::Key, tuple_index: usize);
}

pub trait RelFullIndexRead {
   type Key;
   fn contains_key(&self, key: &Self::Key) -> bool;
}

pub trait RelFullIndexWrite: Default {
   type Key: Clone;
   /// if an entry for `key` does not exist, inserts `v` for it and returns true.
   fn insert_if_not_present(&mut self, key: &Self::Key, v: usize) -> bool;
}


pub(crate) type RelIndexType1<K> = HashMap<K, Vec<usize>, BuildHasherDefault<FxHasher>>;

pub static mut MOVE_REL_INDEX_CONTENTS_TOTAL_TIME : Duration = Duration::ZERO;
pub static mut INDEX_INSERT_TOTAL_TIME : Duration = Duration::ZERO;

impl<K: Eq + Hash> RelIndexWrite for RelIndexType1<K>{
   type Key = K;

   fn move_index_contents(from: &mut RelIndexType1<K>, to: &mut RelIndexType1<K>) {
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

   fn index_insert(hm: &mut RelIndexType1<K>, key: K, tuple_index: usize) {
      // let before = Instant::now();
      use std::collections::hash_map::Entry::*;
      match hm.entry(key){
         Occupied(mut vec) => vec.get_mut().push(tuple_index),
         Vacant(vacant) => {
            let mut vec = Vec::with_capacity(4);
            vec.push(tuple_index);
            vacant.insert(vec);
         },
      }
      // unsafe {
      //    INDEX_INSERT_TOTAL_TIME += before.elapsed();
      // }
   }
}

impl RelIndexWrite for RelNoIndexType {
   type Key = ();

   fn move_index_contents(ind1: &mut Self, ind2: &mut Self) {
      ind2.append(ind1);
   }

   fn index_insert(ind: &mut Self, _key: Self::Key, tuple_index: usize) {
      ind.push(tuple_index);
   }
}

impl<K: Eq + Hash> RelIndexWrite for LatticeIndexType<K>{
   type Key = K;

   #[inline(always)]
   fn move_index_contents(hm1: &mut LatticeIndexType<K>, hm2: &mut LatticeIndexType<K>) {
      for (k,v) in hm1.drain(){
         let set = hm2.entry(k).or_default();
         set.extend(v);
      }
   }

   #[inline(always)]
   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      hm.entry(key).or_default().insert(tuple_index);
   }
}

pub static mut MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME : Duration = Duration::ZERO;
impl<K: Eq + Hash> RelIndexWrite for HashBrownRelFullIndexType<K>{
    type Key = K;

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

   #[inline(always)]
   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      hm.insert(key, tuple_index);
      // TODO undo this
      // assert!(hm.insert(key, tuple_index).is_none(), 
      //    "inserting duplicate index into RelFullIndexType, index:{}", tuple_index);
   }
}

impl <K: Clone + Hash + Eq> RelFullIndexWrite for HashBrownRelFullIndexType<K> {
   type Key = K;
   #[inline]
   fn insert_if_not_present(&mut self, key: &Self::Key, v: usize) -> bool {
      match self.raw_entry_mut().from_key(key) {
         hashbrown::hash_map::RawEntryMut::Occupied(_) => false,
         hashbrown::hash_map::RawEntryMut::Vacant(vacant) => {vacant.insert(key.clone(), v); true},
      }
   }
}

impl<K: Hash + Eq> RelFullIndexRead for HashBrownRelFullIndexType<K> {
    type Key = K;

   fn contains_key(&self, key: &Self::Key) -> bool {
      self.contains_key(key)
   }
}


/// type constraints for relation columns
pub struct TypeConstraints<T> where T : Clone + Eq + Hash{_t: ::core::marker::PhantomData<T>}
/// type constraints for a lattice
pub struct LatTypeConstraints<T> where T : Clone + Eq + Hash + Lattice{_t: ::core::marker::PhantomData<T>}

#[inline(always)]
pub fn comment(_: &str){}
