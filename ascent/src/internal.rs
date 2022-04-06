//! Provides definitions required for the `ascent` macro. Not intended for direct use.

use std::{collections::{HashMap, HashSet}, hash::{BuildHasherDefault, Hash}, rc::Rc, time::{Duration, Instant}};

use ascent_base::Lattice;
use nohash_hasher::BuildNoHashHasher;
use rustc_hash::FxHasher;

// pub use hashbrown;
pub type RelIndexType<K> = HashMap<K, Vec<usize>, BuildHasherDefault<FxHasher>>;
pub type LatticeIndexType<K> = HashMap<K, HashSet<usize, BuildNoHashHasher<usize>>, BuildHasherDefault<FxHasher>>;
// pub type RelFullIndexType<K> = HashMap<K, usize, BuildHasherDefault<FxHasher>>;
pub type RelFullIndexType<K> = hashbrown::HashMap<K, usize, BuildHasherDefault<FxHasher>>;
pub type RelNoIndexType = Vec<usize>;

pub trait RelIndexTrait{
   type Key;
   fn move_index_contents(from: &mut Self, to: &mut Self);
   fn index_insert(ind: &mut Self, key: Self::Key, tuple_index: usize);
}

// pub trait RelIndexIterAllTrait<'a>{
//    type IteratorType: Iterator<Item = &'a usize>;
//    fn iter_all(&'a self) -> Self::IteratorType;
// }
// use std::collections::hash_map::Values;
// use core::slice::Iter;
// use std::iter::FlatMap;
// impl<'a, K: 'a> RelIndexIterAllTrait<'a> for RelIndexType<K> {
//    type IteratorType = FlatMap<Values<'a, K, Vec<usize>>, Iter<'a, usize>, fn(& Vec<usize>) -> Iter<usize>>;
//    // type IteratorType = Iter<'a, usize>;
//    fn iter_all(&'a self) -> Self::IteratorType {
//       fn vec_iter(vec: &Vec<usize>) -> Iter<usize> {
//          vec.iter()
//       }
//       let res: FlatMap<Values<K, Vec<usize>>, Iter<usize>, fn (&Vec<usize>) -> Iter<usize>> = self.values().flat_map(vec_iter);
//       // let res = self.values().next().unwrap().iter();
//       res
//       // todo!()
//    }
// }

pub static mut MOVE_REL_INDEX_CONTENTS_TOTAL_TIME : Duration = Duration::ZERO;
pub static mut INDEX_INSERT_TOTAL_TIME : Duration = Duration::ZERO;

impl<K: Eq + Hash> RelIndexTrait for RelIndexType<K>{
   type Key = K;

   #[inline(always)]
   fn move_index_contents(from: &mut RelIndexType<K>, to: &mut RelIndexType<K>) {
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

   #[inline(always)]
   fn index_insert(hm: &mut RelIndexType<K>, key: K, tuple_index: usize) {
      // let before = Instant::now();
      // hm.entry(key).or_insert_with(|| Vec::with_capacity(8)).push(tuple_index);
      use std::collections::hash_map::Entry::*;
      match hm.entry(key){
         Occupied(mut vec) => vec.get_mut().push(tuple_index),
         Vacant(vacant) => {vacant.insert(vec![tuple_index]);},
      }
      // hm.entry(key).or_default().push(tuple_index);
      // unsafe {
      //    INDEX_INSERT_TOTAL_TIME += before.elapsed();
      // }
   }
}
impl RelIndexTrait for RelNoIndexType {
    type Key = ();

    fn move_index_contents(ind1: &mut Self, ind2: &mut Self) {
        ind2.append(ind1);
    }

    fn index_insert(ind: &mut Self, _key: Self::Key, tuple_index: usize) {
        ind.push(tuple_index);
    }
}

impl<K: Eq + Hash> RelIndexTrait for LatticeIndexType<K>{
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
impl<K: Eq + Hash> RelIndexTrait for RelFullIndexType<K>{
    type Key = K;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      // let before = Instant::now();
      if from.len() > to.len() {
         std::mem::swap(from, to);
      }
      to.reserve(from.len());
      for (k, v) in from.drain() {
         to.insert(k, v); // TODO could be improved
      }
      // unsafe {
      //    MOVE_FULL_INDEX_CONTENTS_TOTAL_TIME += before.elapsed();
      // }

   }

   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      hm.insert(key, tuple_index);
      // TODO undo this
      // assert!(hm.insert(key, tuple_index).is_none(), 
      //    "inserting duplicate index into RelFullIndexType, index:{}", tuple_index);
   }
}

#[inline]
/// if an entry for `key` does not exsit, inserts `v` for it and returns true.
pub fn full_index_insert_if_not_present<K: Eq + Hash + Clone>(hm: &mut RelFullIndexType<K>, key: &K, v: usize) -> bool {

   match hm.raw_entry_mut().from_key(key) {
    hashbrown::hash_map::RawEntryMut::Occupied(_) => false,
    hashbrown::hash_map::RawEntryMut::Vacant(vacant) => {vacant.insert(key.clone(), v); true},
   }
   // match hm.entry(key) {
   //    Occupied(_) => false,
   //    Vacant(vacant) => {vacant.insert(v); true},
   // }
}


/// type constraints for relation columns
pub struct TypeConstraints<T> where T : Clone + Eq + Hash{_t: ::core::marker::PhantomData<T>}
/// type constraints for a lattice
pub struct LatTypeConstraints<T> where T : Clone + Eq + Hash + Lattice{_t: ::core::marker::PhantomData<T>}

#[inline(always)]
pub fn comment(_: &str){}


pub trait Convert<TSource> {
   fn convert(source: TSource) -> Self;
}

impl<T> Convert<T> for T {
   #[inline(always)]
   fn convert(source: T) -> T {source}
}

impl<T> Convert<&T> for T where T: Clone {
   #[inline(always)]
   fn convert(source: &T) -> T {source.clone()}
}

impl Convert<&str> for String {
   fn convert(source: &str) -> Self {
      source.to_string()
   }
}

impl<T: Clone> Convert<&Rc<T>> for T {
   fn convert(source: &Rc<T>) -> Self {
      source.as_ref().clone()
   }
}

#[inline]
pub fn rel_ind_val_option_to_iter<'a>(val_option: Option<&'a Vec<usize>>) -> std::slice::Iter<'a, usize> {
   match val_option {
      Some(v) => v.iter(),
      None => [].iter()
   }
}

#[inline(always)]
pub fn rel_full_ind_val_option_to_iter<'a>(val_option: Option<&'a usize>) -> std::option::IntoIter<&'a usize>  {
   val_option.into_iter()
}

static mut EMPTY_LAT_IND_VAL: Option<HashSet<usize, BuildNoHashHasher<usize>>> = None;
pub fn lat_ind_val_option_to_iter<'a>(
   val_option: Option<&'a HashSet<usize, BuildNoHashHasher<usize>>>,
) -> std::collections::hash_set::Iter<'_, usize> {
   match val_option {
      Some(v) => v.iter(),
      None => unsafe {
         if EMPTY_LAT_IND_VAL.is_none() {
            EMPTY_LAT_IND_VAL = Some(HashSet::default());
         }
         EMPTY_LAT_IND_VAL.as_ref().unwrap().iter()
      },
   }
}
