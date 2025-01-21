use std::hash::{BuildHasherDefault, Hash};

use hashbrown::HashMap;
use rustc_hash::FxHasher;

pub type MyHashSetIter<'a, T> = hashbrown::hash_set::Iter<'a, T>;
pub type MyHashSet<T, S> = hashbrown::HashSet<T, S>;

pub struct TrRel<T: Clone + Hash + Eq> {
   pub(crate) map: HashMap<T, MyHashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>,
   pub(crate) reverse_map: HashMap<T, MyHashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>,
   // pub(crate) precursor_map: HashMap<T, HashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>,
   // pub(crate) precursor_set: HashSet<(T, T), BuildHasherDefault<FxHasher>>,
   pub(crate) precursor_set: Vec<(T, T)>,
   pub anti_reflexive: bool,
}

impl<T: Clone + Hash + Eq> Default for TrRel<T> {
   fn default() -> Self {
      Self {
         map: Default::default(),
         reverse_map: Default::default(),
         // precursor_map: Default::default(),
         precursor_set: Default::default(),
         anti_reflexive: true,
      }
   }
}

impl<T: Clone + Hash + Eq> TrRel<T> {
   /// returns true if this tuple did not exist in the transitive relation
   pub fn insert(&mut self, x: T, y: T) -> bool {
      // TODO is this correct?

      if x == y {
         return false;
      }

      if self.map.get(&x).is_some_and(|s| s.contains(&y)) {
         return false;
      }

      // if !self.precursor_map.entry(x.clone()).or_default().insert(y.clone()) {
      //    return false;
      // }
      // if !self.precursor_set.insert((x.clone(), y.clone())) {
      //    return false;
      // }
      self.precursor_set.push((x.clone(), y.clone()));

      let mut x_reverse_map = std::mem::take(self.reverse_map.entry(x.clone()).or_default());
      let mut y_map = std::mem::take(self.map.entry(y.clone()).or_default());
      // let y_map2 = y_map.iter().chain([&x]).map(|elem| (hash_one(self.map.hasher(), elem), elem.clone())).collect_vec();
      for x_prime in x_reverse_map.iter().chain([&x]) {
         if x_prime != &y {
            let x_prime_map = self.map.entry(x_prime.clone()).or_default();
            x_prime_map.extend(y_map.iter().chain([&y]).filter(|&a| a != x_prime).cloned());
            // set_extend_with_hash_no_check(x_prime_map, y_map2.iter().cloned());
            // for y_prime in y_map.iter().chain([&y]) {
            //    self.reverse_map.entry(y_prime.clone()).or_default().insert(x_prime.clone());
            // }
         }
      }

      // let x_reverse_map2 = x_reverse_map.iter().chain([&x]).map(|elem| (hash_one(self.map.hasher(), elem), elem.clone())).collect_vec();
      for y_prime in y_map.iter().chain([&y]) {
         if y_prime != &x {
            let y_prime_reverse_map = self.reverse_map.entry(y_prime.clone()).or_default();
            y_prime_reverse_map.extend(x_reverse_map.iter().chain([&x]).filter(|&a| a != y_prime).cloned());
            // set_extend_with_hash_no_check(y_prime_reverse_map, x_reverse_map2.iter().cloned());
         }
      }
      if x == y {
         x_reverse_map.insert(y.clone());
         y_map.insert(x.clone());
      }
      self.reverse_map.insert(x.clone(), x_reverse_map);
      self.map.insert(y.clone(), y_map);
      true
   }

   pub fn iter_all(&self) -> impl Iterator<Item = (&T, &T)> + '_ {
      self.map.iter().flat_map(|(x, x_set)| x_set.iter().map(move |y| (x, y)))
   }

   #[inline]
   pub fn contains(&self, x: &T, y: &T) -> bool { self.map.get(x).is_some_and(|s| s.contains(y)) }

   pub fn count_estimate(&self) -> usize {
      let sample_size = 3;
      let sum = self.map.values().take(sample_size).map(|x| x.len()).sum::<usize>();
      sum * self.map.len() / sample_size.min(self.map.len()).max(1)
   }

   pub fn count_exact(&self) -> usize { self.map.values().map(|x| x.len()).sum() }
}
