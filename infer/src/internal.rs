use std::{collections::{HashMap, HashSet}, hash::{BuildHasherDefault, Hash}};

use infer_base::Lattice;
use nohash_hasher::BuildNoHashHasher;
use rustc_hash::FxHasher;

pub type RelIndexType<K> = HashMap<K, Vec<usize>, BuildHasherDefault<FxHasher>>;
pub type LatticeIndexType<K> = HashMap<K, HashSet<usize, BuildNoHashHasher<usize>>, BuildHasherDefault<FxHasher>>;
pub type RelFullIndexType<K> = HashMap<K, usize, BuildHasherDefault<FxHasher>>;
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
// impl<'a, K: 'a> RelIndexIterAllTrait<'a> for RelIndexType<K> {
//    type IteratorType = FlatMap<Values<'a, K, Vec<usize>>, Iter<'a, usize>, fn(& Vec<usize>) -> Iter<usize>>;
//    // type IteratorType = Iter<'a, usize>;
//    fn iter_all(&'a self) -> Self::IteratorType {
//       fn vec_iter(vec: &Vec<usize>) -> Iter<usize> {
//          vec.iter()
//       }
//       let res = self.values().flat_map(vec_iter);
//       // let res = self.values().next().unwrap().iter();
//       res
//       // todo!()
//    }
// }

impl<K: Eq + Hash> RelIndexTrait for RelIndexType<K>{
   type Key = K;

   #[inline(always)]
   fn move_index_contents(from: &mut RelIndexType<K>, to: &mut RelIndexType<K>) {
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
   }

   #[inline(always)]
   fn index_insert(hm: &mut RelIndexType<K>, key: K, tuple_index: usize) {
      hm.entry(key).or_insert_with(|| Vec::with_capacity(8)).push(tuple_index);
      // hm.entry(key).or_default().push(tuple_index);
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

impl<K: Eq + Hash> RelIndexTrait for RelFullIndexType<K>{
    type Key = K;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      if from.len() > to.len() {
         std::mem::swap(from, to);
      }
      for (k, v) in from.drain() {
         to.insert(k, v); // TODO could be improved
      }
   }

   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize) {
      assert!(hm.insert(key, tuple_index).is_none(), 
         "inserting duplicate index into RelFullIndexType, index:{}", tuple_index);
   }
}

/// type constraints for relation columns
pub struct TypeConstraints<T> where T : Clone + Eq + Hash{_t: ::core::marker::PhantomData<T>}
/// type constraints for a lattice
pub struct LatTypeConstraints<T> where T : Clone + Eq + Hash + Lattice{_t: ::core::marker::PhantomData<T>}