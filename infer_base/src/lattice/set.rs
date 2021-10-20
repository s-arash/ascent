use std::cmp::Ordering;
use std::collections::HashSet;
use std::hash::Hash;

use super::Lattice;

#[derive(Clone, PartialEq, Eq)]
pub struct Set<T: PartialEq + Eq + Hash>(pub HashSet<T>);

impl<T: Eq + Hash> PartialOrd for Set<T> {
   fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
      if self.0 == other.0 {
         Some(Ordering::Equal)
      } else if self.0.is_subset(&other.0) {
         Some(Ordering::Less)
      } else if self.0.is_superset(&other.0) {
         Some(Ordering::Greater)
      } else {
         None
      }
   }
}

impl<T: Eq + Hash + Clone> Lattice for Set<T> {
   fn meet_mut(&mut self, mut other: Self) -> bool {
      let self_len = self.0.len();
      let mut new_self = HashSet::new();
      if self.0.len() > other.0.len() {
         std::mem::swap(self, &mut other);
      }
      for item in self.0.drain() {
         if other.0.contains(&item) {
            new_self.insert(item);
         }
      }
      *self = Set(new_self);
      self_len != self.0.len()
   }

   fn join_mut(&mut self, mut other: Self) -> bool {
      let self_len = self.0.len();
      if self_len < other.0.len() {
         std::mem::swap(self, &mut other);
      }
      for item in other.0.into_iter() {
         self.0.insert(item);
      }

      self_len != self.0.len()
   }
}
