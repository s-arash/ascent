use std::{collections::{HashMap, HashSet}, hash::{BuildHasherDefault, Hash}};

use infer_base::Lattice;
use nohash_hasher::BuildNoHashHasher;
use rustc_hash::FxHasher;

pub type RelIndexType<K> = HashMap<K, Vec<usize>, BuildHasherDefault<FxHasher>>;
pub type LatticeIndexType<K> = HashMap<K, HashSet<usize, BuildNoHashHasher<usize>>, BuildHasherDefault<FxHasher>>;

pub trait RelIndexTrait{
   type Key;
   fn move_index_contents(hm1: &mut Self, hm2: &mut Self);
   fn index_insert(hm: &mut Self, key: Self::Key, tuple_index: usize);
}

impl<K: Eq + Hash> RelIndexTrait for RelIndexType<K>{
   type Key = K;

   #[inline]
   fn move_index_contents(hm1: &mut RelIndexType<K>, hm2: &mut RelIndexType<K>) {
      for (k, mut v) in hm1.drain() {
         let set = hm2.entry(k).or_default();
         set.append(&mut v);
      }
   }

   #[inline(always)]
   fn index_insert(hm: &mut RelIndexType<K>, key: K, tuple_index: usize) {
      hm.entry(key).or_insert_with(|| Vec::with_capacity(8)).push(tuple_index);
      // hm.entry(key).or_default().push(tuple_index);
   }
}

impl<K: Eq + Hash> RelIndexTrait for LatticeIndexType<K>{
   type Key = K;

   #[inline]
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


/// type constraints for relation columns
pub struct TypeConstraints<T> where T : Clone + Eq + Hash{_t: ::core::marker::PhantomData<T>}
/// type constraints for a lattice
pub struct LatTypeConstraints<T> where T : Clone + Eq + Hash + Lattice{_t: ::core::marker::PhantomData<T>}