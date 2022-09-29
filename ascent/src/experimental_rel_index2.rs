use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use std::hash::Hash;
use instant::Instant;
use rustc_hash::FxHasher;

use crate::internal::MOVE_REL_INDEX_CONTENTS_TOTAL_TIME;
use crate::internal::RelIndexWrite;
use crate::rel_index_read::RelIndexRead;
use crate::rel_index_read::RelIndexReadAll;



pub struct RelIndexType2<K>(HashMap<K, Vec<Vec<usize>>, BuildHasherDefault<FxHasher>>);

impl<K> Default for RelIndexType2<K> {
   fn default() -> Self { Self(Default::default()) }
}

impl<K: Eq + Hash> RelIndexWrite for RelIndexType2<K> {
   type Key = K;

   fn move_index_contents(from: &mut Self, to: &mut Self) {
      let before = Instant::now();
      if from.0.len() > to.0.len() {
         std::mem::swap(from, to);
      }
      use std::collections::hash_map::Entry::*;
      for (k, mut v) in from.0.drain() {
         match to.0.entry(k) {
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

   fn index_insert(ind: &mut Self, key: Self::Key, tuple_index: usize) {
      use std::collections::hash_map::Entry::*;
      match ind.0.entry(key){
         Occupied(mut vec) => vec.get_mut()[0].push(tuple_index),
         Vacant(vacant) => {
            let mut inner_vec = Vec::with_capacity(8);
            inner_vec.push(tuple_index);
            vacant.insert(vec![inner_vec]);
         },
      }
   }
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexRead<'a> for RelIndexType2<K> {
   type Key = K;

   type IteratorType = std::iter::Cloned<std::iter::Flatten<std::slice::Iter<'a, Vec<usize>>>>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      if let Some(v) = self.0.get(key) {
         let res: std::iter::Cloned<std::iter::Flatten<std::slice::Iter<Vec<usize>>>> = v.iter().flatten().cloned();
         Some(res)
      } else { None }
   }

   #[inline(always)]
   fn len(&self) -> usize { self.0.len() }
}

impl<'a, K: Eq + std::hash::Hash + 'a> RelIndexReadAll<'a> for RelIndexType2<K> {
   type Key = K;

   type ValueIteratorType  = std::iter::Cloned<std::iter::Flatten<std::slice::Iter<'a, Vec<usize>>>>;

   type AllIteratorType= std::iter::Map<std::collections::hash_map::Iter<'a, K, Vec<Vec<usize>>>, for<'aa, 'bb> fn ((&'aa K, &'bb Vec<Vec<usize>>)) -> (&'aa K, std::iter::Cloned<std::iter::Flatten<std::slice::Iter<'bb, Vec<usize>>>>)>;


   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: std::iter::Map<std::collections::hash_map::Iter<K, Vec<Vec<usize>>>, for<'aa, 'bb> fn ((&'aa K, &'bb Vec<Vec<usize>>)) -> (&'aa K, std::iter::Cloned<std::iter::Flatten<std::slice::Iter<'bb, Vec<usize>>>>)> 
         = self.0.iter().map(rel_index_type2_iter_all_mapper);
      res
   }
}

fn rel_index_type2_iter_all_mapper<'a, 'b, K>((k, v): (&'a K, &'b Vec<Vec<usize>>)) -> (&'a K, std::iter::Cloned<std::iter::Flatten<std::slice::Iter<'b, Vec<usize>>>>) {
   (k, v.iter().flatten().cloned())
}

#[test]
fn test_vec() {
   let v = vec![42];
   println!("{}", v.capacity());
}