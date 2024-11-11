//! A data structure for union-find based reflexive transitive relations.
//!
//! This is the backing data strcuture for [`trrel_uf`](crate::trrel_uf) in Ascent.
use std::fmt::Debug;
use std::hash::{BuildHasher, BuildHasherDefault, Hash};
use std::time::Duration;

use ascent::internal::Instant;
use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use rustc_hash::FxHasher;

use crate::binary_rel;
use crate::utils::merge_sets;

pub static mut DEPTH_COUNT: usize = 0;
pub static mut DEPTH_SUM: usize = 0;
pub static mut MERGE_MULTIPLE_TIME: Duration = Duration::ZERO;
pub static mut ADD_SET_CONNECTION_TIME: Duration = Duration::ZERO;

#[derive(Clone, Debug)]
pub struct TrRelUnionFind<T: Clone + Hash + Eq> {
   pub(crate) sets: Vec<HashSet<T, BuildHasherDefault<FxHasher>>>,
   pub(crate) elem_ids: HashMap<T, usize, BuildHasherDefault<FxHasher>>,
   pub(crate) set_subsumptions: HashMap<usize, usize, BuildHasherDefault<FxHasher>>,
   pub(crate) set_connections: binary_rel::Map<usize>,
   pub(crate) reverse_set_connections: binary_rel::Map<usize>,
}

impl<T: Clone + Hash + Eq> Default for TrRelUnionFind<T> {
   fn default() -> Self {
      Self {
         sets: Default::default(),
         elem_ids: Default::default(),
         set_subsumptions: Default::default(),
         set_connections: Default::default(),
         reverse_set_connections: Default::default(),
      }
   }
}

impl<T: Clone + Hash + Eq> TrRelUnionFind<T> {
   pub(crate) fn get_dominant_id(&self, id: usize) -> usize {
      match self.set_subsumptions.get(&id) {
         Some(dom_id) => self.get_dominant_id(*dom_id),
         None => id,
      }
   }

   // TODO decide which one to use
   #[allow(dead_code)]
   pub(crate) fn get_dominant_id_mut_halving(&mut self, id: usize) -> usize {
      match self.set_subsumptions.get(&id).copied() {
         Some(dom_id) => {
            debug_assert_ne!(id, dom_id);
            // Path halving
            match self.set_subsumptions.get(&dom_id).copied() {
               Some(grandparent) => {
                  debug_assert_ne!(dom_id, grandparent);
                  debug_assert_ne!(id, grandparent);

                  *self.set_subsumptions.get_mut(&id).unwrap() = grandparent;
                  self.get_dominant_id_mut(grandparent)
               },
               None => dom_id,
            }
         },
         None => id,
      }
   }

   pub(crate) fn get_dominant_id_mut_with_depth(&mut self, id: usize) -> (usize, usize) {
      match self.set_subsumptions.get(&id) {
         Some(&parent_id) => {
            let (dom_id, depth) = self.get_dominant_id_mut_with_depth(parent_id);
            if dom_id != parent_id {
               self.set_subsumptions.insert(id, dom_id);
            }
            (dom_id, depth + 1)
         },
         None => (id, 0),
      }
   }

   pub(crate) fn get_dominant_id_mut(&mut self, id: usize) -> usize {
      let (res, depth) = self.get_dominant_id_mut_with_depth(id);
      unsafe {
         DEPTH_COUNT += 1;
         DEPTH_SUM += depth;
      }
      res
   }

   pub(crate) fn elem_set(&self, elem: &T) -> Option<usize> {
      self.elem_ids.get(elem).map(|id| self.get_dominant_id(*id))
   }

   pub(crate) fn elem_set_update(&mut self, elem: &T) -> Option<usize> {
      let id = *self.elem_ids.get(elem)?;
      let dominant_id = self.get_dominant_id_mut(id);
      if id != dominant_id {
         self.elem_ids.insert(elem.clone(), dominant_id);
      }
      Some(dominant_id)
   }

   pub(crate) fn add_node_new(&mut self, x: T) -> (usize, bool) {
      let res = match self.elem_set_update(&x) {
         Some(set_id) => (set_id, false),
         None => {
            let elem_id = self.sets.len();
            self.sets.push(HashSet::from_iter([x.clone()]));
            self.elem_ids.insert(x.clone(), elem_id);
            (elem_id, true)
         },
      };

      #[cfg(debug_assertions)]
      self.assert_disjoint_invariant();
      res
   }

   pub(crate) fn add_node(&mut self, x: T) -> usize { self.add_node_new(x).0 }

   pub fn add(&mut self, x: T, y: T) -> bool {
      let (x_set, x_new) = self.add_node_new(x.clone());
      let (y_set, y_new) = self.add_node_new(y.clone());
      if x_new || y_new {
         self.add_set_connection(x_set, y_set);
         return true;
      }
      if x_set == y_set {
         return false;
      }
      // We have different sets, neither is new

      if self.set_connections.get(&y_set).map_or(false, |y_set| y_set.contains(&x_set)) {
         // There exists a back-edge, collapse for anti-symmetry
         let mut to_be_merged = self.set_connections[&y_set]
            .intersection(&self.reverse_set_connections[&x_set])
            .cloned()
            .collect::<hashbrown::HashSet<usize, BuildHasherDefault<FxHasher>>>();
         to_be_merged.remove(&x_set);
         to_be_merged.insert(y_set);

         keep_difference(self.reverse_set_connections.get_mut(&x_set).unwrap(), &to_be_merged);
         keep_difference(self.set_connections.get_mut(&y_set).unwrap(), &to_be_merged);

         self.set_connections.get_mut(&y_set).unwrap().remove(&x_set);
         self.reverse_set_connections.get_mut(&x_set).unwrap().remove(&y_set);

         self.add_set_connection(x_set, y_set);

         to_be_merged.remove(&y_set);
         let merged_set = self.merge_multiple(x_set, y_set, &to_be_merged);

         *self.elem_ids.get_mut(&x).unwrap() = merged_set;
         *self.elem_ids.get_mut(&y).unwrap() = merged_set;
      } else {
         self.add_set_connection(x_set, y_set);
      }
      true
   }

   fn merge_multiple(
      &mut self, from: usize, to: usize, in_between: &hashbrown::HashSet<usize, BuildHasherDefault<FxHasher>>,
   ) -> usize {
      let before = Instant::now();
      // TODO is this right? Aren't we doing too much?
      for s in [from, to] {
         if let Some(s_connections) = self.set_connections.get(&s) {
            for z in s_connections.difference(in_between) {
               let z_rev_connections = self.reverse_set_connections.entry(*z).or_default();
               keep_difference(z_rev_connections, in_between);
               z_rev_connections.remove(&to);
               z_rev_connections.insert(from);
            }
         }

         if let Some(s_rev_connections) = self.reverse_set_connections.get(&s) {
            for z in s_rev_connections.difference(in_between) {
               let z_connections = self.set_connections.entry(*z).or_default();
               keep_difference(z_connections, in_between);
               z_connections.insert(from);
               z_connections.remove(&to);
            }
         }
      }

      for s in in_between.iter().cloned().chain([to]) {
         assert!(from != s);

         self.set_connections.remove(&s);
         self.reverse_set_connections.remove(&s);

         let s_taken = std::mem::take(&mut self.sets[s]);
         merge_sets(&mut self.sets[from], s_taken);
         self.set_subsumptions.insert(s, from);
      }

      // TODO this looks unnecessary
      let from_set_connections = self.set_connections.get_mut(&from).unwrap();
      from_set_connections.remove(&to);
      keep_difference(from_set_connections, in_between);

      let from_rev_set_connection = self.reverse_set_connections.get_mut(&from).unwrap();
      from_rev_set_connection.remove(&to);
      keep_difference(from_rev_set_connection, in_between);

      #[cfg(debug_assertions)]
      self.assert_disjoint_invariant();

      unsafe {
         MERGE_MULTIPLE_TIME += before.elapsed();
      }
      from
   }

   fn add_one_connection(&mut self, from: usize, to: usize) -> bool {
      if !self.set_connections.entry(from).or_default().insert(to) {
         return false;
      }
      self.reverse_set_connections.entry(to).or_default().insert(from);
      true
   }

   fn add_set_connection(&mut self, from: usize, to: usize) -> bool {
      let before = Instant::now();
      if !self.set_connections.entry(from).or_default().insert(to) {
         return false;
      }
      self.reverse_set_connections.entry(to).or_default().insert(from);

      let from_reverse_connections = std::mem::take(self.reverse_set_connections.entry(from).or_default());
      let to_connections = std::mem::take(self.set_connections.entry(to).or_default());

      // TODO perf can be improved
      // TODO not ideal perf-wise
      let new_to_connections: HashSet<usize, BuildHasherDefault<FxHasher>> =
         to_connections.difference(&self.set_connections[&from]).cloned().collect();
      let new_from_reverse_connections: HashSet<usize, BuildHasherDefault<FxHasher>> =
         from_reverse_connections.difference(&self.reverse_set_connections[&to]).cloned().collect();

      for x_prime in new_from_reverse_connections.iter() {
         for y_prime in new_to_connections.iter() {
            self.add_one_connection(*x_prime, *y_prime);
         }
      }
      for x_prime in new_from_reverse_connections.iter() {
         self.set_connections.entry(*x_prime).or_default().insert(to);
      }
      for y_prime in new_to_connections.iter() {
         self.reverse_set_connections.entry(*y_prime).or_default().insert(from);
      }

      self.reverse_set_connections.entry(to).or_default().extend(from_reverse_connections.iter().cloned());
      self.set_connections.entry(from).or_default().extend(to_connections.iter().cloned());

      self.reverse_set_connections.insert(from, from_reverse_connections);
      self.set_connections.insert(to, to_connections);

      unsafe {
         ADD_SET_CONNECTION_TIME += before.elapsed();
      }
      true
   }

   pub fn set_of<'a>(&'a self, x: &T) -> Option<impl Iterator<Item = &'a T>> {
      let set_id = self.elem_set(x)?;
      Some(self.set_of_by_set_id(x, set_id))
   }

   pub(crate) fn set_of_by_set_id<'a>(&'a self, _x: &T, id: usize) -> impl Iterator<Item = &'a T> {
      let id = self.get_dominant_id(id);
      let res = self
         .set_connections
         .get(&id)
         .into_iter()
         .flatten()
         .cloned()
         .filter(move |&s_id| s_id != id)
         .chain([id])
         .flat_map(|s| self.sets[s].iter());
      res
   }

   pub fn rev_set_of<'a>(&'a self, x: &T) -> Option<impl Iterator<Item = &'a T>> {
      let set_id = self.elem_set(x)?;
      Some(self.rev_set_of_by_set_id(x, set_id))
   }

   pub(crate) fn rev_set_of_by_set_id<'a>(&'a self, _x: &T, id: usize) -> impl Iterator<Item = &'a T> {
      let id = self.get_dominant_id(id);
      let res = self
         .reverse_set_connections
         .get(&id)
         .into_iter()
         .flatten()
         .cloned()
         .filter(move |&s_id| s_id != id)
         .chain([id])
         .flat_map(|s| self.sets[s].iter());
      res
   }

   pub fn iter_all(&self) -> impl Iterator<Item = (&'_ T, &'_ T)> {
      self.elem_ids.iter().flat_map(|(x, &x_set_id)| self.set_of_by_set_id(x, x_set_id).map(move |y| (x, y)))
   }

   pub fn contains(&self, x: &T, y: &T) -> bool {
      self.elem_set(x).map_or(false, |set| {
         self.sets[set].contains(y) || {
            // TODO
            self.get_set_connections(set).into_iter().flatten().any(|s| self.sets[s].contains(y))
         }
      })
   }

   #[inline]
   pub fn is_empty(&self) -> bool { self.sets.is_empty() }

   // TODO `set_connections` and `reverse_set_connections` should be guaranteed to contain only dominant ids
   pub fn get_set_connections(&self, set: usize) -> Option<impl Iterator<Item = usize> + '_> {
      let connections = self.set_connections.get(&set)?;
      let res = connections.iter().map(|&x| self.get_dominant_id(x)).dedup();
      Some(res)
   }

   pub fn get_reverse_set_connections(&self, set: usize) -> Option<impl Iterator<Item = usize> + '_> {
      let connections = self.reverse_set_connections.get(&set)?;
      let res = connections.iter().map(|&x| self.get_dominant_id(x)).dedup();
      Some(res)
   }
   pub fn count_exact(&self) -> usize {
      let dominant_sets: HashSet<usize> = (0..self.sets.len()).map(|s| self.get_dominant_id(s)).collect();
      let mut res = 0;
      for &s in dominant_sets.iter() {
         let s_len = self.sets[s].len();
         res += s_len * s_len;
         for s2 in self.get_set_connections(s).into_iter().flatten() {
            // TODO remove this line
            if s == s2 {
               continue;
            }
            assert_ne!(s2, s);
            res += s_len * self.sets[s2].len();
         }
      }

      res
   }

   pub fn assert_disjoint_invariant(&self) {
      let mut set = HashSet::default();

      for (i, s) in self.sets.iter().enumerate() {
         assert!(s.is_disjoint(&set), "found duplicate in set {i}");
         set.extend(s.iter().cloned());
      }
   }

   pub fn assert_set_connections_dominant_sets(&self) {
      let dominated_sets: HashSet<usize, BuildHasherDefault<FxHasher>> =
         (0..self.sets.len()).filter(|s| self.set_subsumptions.get(s).is_some()).collect();

      for (i, sc) in self.set_connections.iter() {
         let intersect = sc.intersection(&dominated_sets).collect_vec();
         assert!(intersect.is_empty(), "set {} has dominated connections: {:?}", i, intersect);
      }

      for (i, rsc) in self.reverse_set_connections.iter() {
         let intersect = rsc.intersection(&dominated_sets).collect_vec();
         assert!(intersect.is_empty(), "set {} has dominated reverse connections: {:?}", i, intersect);
      }
   }
}

fn keep_difference<T: Hash + Eq, S: BuildHasher>(set: &mut HashSet<T, S>, to_subtract: &HashSet<T, S>) {
   if set.len() > to_subtract.len() {
      for x in to_subtract.iter() {
         set.remove(x);
      }
   } else {
      set.retain(|x| !to_subtract.contains(x))
   }
}

#[allow(dead_code)]
fn extend_set<T: Hash + Eq + Clone, S: BuildHasher + Clone>(set1: &mut HashSet<T, S>, set2: &HashSet<T, S>) {
   if false && set2.len() > 3 * set1.len() {
      let set2_clone = set2.clone();
      merge_sets(set1, set2_clone);
   } else {
      set1.extend(set2.iter().cloned());
   }
}

#[cfg(test)]
mod tests {
   use std::collections::HashSet;
   use std::hash::Hash;

   use ascent::ascent;
   use itertools::Itertools;
   use proptest::prelude::{Strategy, any};
   use proptest::proptest;
   use proptest::strategy::Just;
   use rand::prelude::Distribution;
   use rand::thread_rng;

   use super::TrRelUnionFind;

   #[test]
   fn test_trrel_union_find() {
      let mut rel = TrRelUnionFind::<i32>::default();
      rel.add(1, 2);
      rel.add(11, 12);
      assert!(rel.contains(&1, &2));
      assert!(!rel.contains(&1, &12));
      rel.add(2, 3);
      rel.add(13, 12);
      assert!(!rel.contains(&2, &12));
      rel.add(3, 11);
      // assert!(rel.contains(&2, &12));
      let one_connected: HashSet<_> = rel.set_of(&1).unwrap().cloned().collect();
      println!("one_connected: {:?}", one_connected);
      assert_eq!(one_connected, HashSet::from([1, 2, 3, 11, 12]));
   }

   #[test]
   fn test_trrel_union_find2() {
      let mut rel = TrRelUnionFind::<i32>::default();
      for i in 0..10 {
         rel.add(i, i + 1);
      }

      assert_eq!(rel.count_exact(), 11 * 12 / 2);

      for i in 100..110 {
         rel.add(i, i + 1);
      }

      assert_eq!(rel.set_of(&0).unwrap().collect_vec().len(), 11);
      assert_eq!(rel.set_of(&100).unwrap().collect_vec().len(), 11);
      rel.add(10, 100);
      assert_eq!(rel.set_of(&0).unwrap().collect_vec().len(), 22);
      assert_eq!(rel.set_of(&100).unwrap().collect_vec().len(), 11);
      rel.add(110, 0);
      assert_eq!(rel.set_of(&0).unwrap().collect_vec().len(), 22);
      assert_eq!(rel.set_of(&100).unwrap().collect_vec().len(), 22);
      assert_eq!(rel.set_of(&5).unwrap().collect_vec().len(), 22);
      assert_eq!(rel.set_of(&105).unwrap().collect_vec().len(), 22);

      assert_eq!(rel.count_exact(), 22 * 22);
      rel.assert_set_connections_dominant_sets();
   }

   #[test]
   fn test_trrel_union_find3() {
      let mut rel = TrRelUnionFind::<i32>::default();
      for i in 10000..10009 {
         rel.add(i, i + 1);
      }
      rel.add(10009, 105);
      assert_eq!(rel.set_of(&10000).unwrap().collect_vec().len(), 11);

      for i in 1000..1009 {
         rel.add(i, i + 1);
      }

      for i in (5..9).chain(0..5) {
         rel.add(i, i + 1);
      }

      for i in (103..107).chain(100..103).chain(107..109) {
         rel.add(i, i + 1);
      }

      rel.add(5, 1000);

      assert_eq!(rel.set_of(&0).unwrap().collect_vec().len(), 20);
      assert_eq!(rel.set_of(&100).unwrap().collect_vec().len(), 10);
      rel.add(9, 100);
      assert_eq!(rel.set_of(&0).unwrap().collect_vec().len(), 30);
      assert_eq!(rel.set_of(&100).unwrap().collect_vec().len(), 10);
      rel.add(109, 0);
      assert_eq!(rel.set_of(&0).unwrap().collect_vec().len(), 30);
      assert_eq!(rel.set_of(&100).unwrap().collect_vec().len(), 30);
      assert_eq!(rel.set_of(&5).unwrap().collect_vec().len(), 30);
      assert_eq!(rel.set_of(&105).unwrap().collect_vec().len(), 30);

      println!("set of 10000: {:?}", rel.set_of(&10000).unwrap().collect_vec().iter().sorted());
      assert_eq!(rel.set_of(&10000).unwrap().collect_vec().len(), 40);
      rel.assert_set_connections_dominant_sets();
   }

   // `u8`s to generate collisions
   #[derive(Copy, Clone, Debug)]
   enum Op {
      Add(u8, u8),
      Contains(u8, u8),
      Count,
   }

   fn op_strat() -> impl Strategy<Value = Op> {
      proptest::prop_oneof![
         any::<(u8, u8)>().prop_map(|(a, b)| Op::Add(a, b)),
         any::<(u8, u8)>().prop_map(|(a, b)| Op::Contains(a, b)),
         Just(Op::Count),
      ]
   }

   proptest! {

      #[test]
      fn trrel_ok(ops in proptest::collection::vec(op_strat(), 1..100)) {

         // Perform a sequence of operations
         let mut rel = TrRelUnionFind::<u8>::default();
         for op in &ops {
            match *op {
                  Op::Add(x, y) => { rel.add(x, y); },
                  Op::Contains(x, y) => { rel.contains(&x, &y); },
                  Op::Count => { rel.count_exact(); },
            }
         }

         rel.assert_disjoint_invariant();
         rel.assert_set_connections_dominant_sets();

         for op in ops {
            match op {
                  Op::Add(x, y) => assert!(rel.contains(&x, &y)),
                  Op::Contains(_, _) => (),
                  Op::Count => (),
            }
         }
      }
   }

   ascent! {
      /// Reflexive Transitive Closure
      struct RTC<T: Clone + Hash + Eq>;

      relation r(T, T);
      relation rtc(T, T);
      rtc(x, y), rtc(x, x), rtc(y, y) <-- r(x, y);
      rtc(x, z) <-- r(x, y), rtc(y, z);
   }

   #[test]
   fn test_trrel_union_find_ground_truth() {
      use rand::distributions::Uniform;
      let values_distro = Uniform::new(0, 50);
      let tests_count = 15;
      for i in 0..tests_count {
         let tuples_count = Uniform::new(10, 70).sample(&mut thread_rng());
         let tuples = values_distro
            .sample_iter(&mut thread_rng())
            .take(tuples_count)
            .zip(values_distro.sample_iter(&mut thread_rng()).take(tuples_count))
            .collect_vec();

         let mut ground_truth_prog = RTC::default();
         ground_truth_prog.r = tuples.clone();
         ground_truth_prog.run();

         println!("test {i}: tuples_count: {tuples_count}, ground_truth_res tuples: {}", ground_truth_prog.rtc.len());
         let mut trrel_uf = TrRelUnionFind::default();
         for (i, (x, y)) in tuples.iter().cloned().enumerate() {
            trrel_uf.add(x, y);

            if i % 5 == 0 {
               trrel_uf.assert_disjoint_invariant();
               trrel_uf.assert_set_connections_dominant_sets();
            }
         }

         let trrel_uf_res: HashSet<_> = trrel_uf.iter_all().map(|(x, y)| (*x, *y)).collect();
         let ground_truth_res: HashSet<_> = ground_truth_prog.rtc.into_iter().collect();

         let count_exact = trrel_uf.count_exact();
         // println!("count_exact: {count_exact}");

         assert_eq!(
            trrel_uf_res,
            ground_truth_res,
            "input tuples: {:?}, ground_truth_res len: {:?}, trrel_uf_res len: {:?}",
            tuples,
            ground_truth_res.len(),
            trrel_uf_res.len()
         );

         assert_eq!(count_exact, ground_truth_res.len());
      }
   }
}
