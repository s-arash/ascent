use std::cmp::Ordering;
use std::collections::{BTreeSet};
use std::hash::Hash;
use std::ops::Deref;

use super::Lattice;

/// A set type that implements the `Lattice` trait
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Set<T: PartialEq + Eq + Hash + Ord>(pub BTreeSet<T>);

impl<T: PartialEq + Eq + Hash + Ord> Set<T> {

   /// Creates a `Set` containing only `item`
   pub fn singleton(item: T) -> Self {
      let mut set = BTreeSet::new();
      set.insert(item);
      Set(set)
   }
}

impl<T: PartialEq + Eq + Hash + Ord> Default for Set<T> {
   fn default() -> Self {Self(Default::default())}
}

impl<T: PartialEq + Eq + Hash + Ord> Deref for Set<T>{
   type Target = BTreeSet<T>;

   fn deref(&self) -> &Self::Target {
      &self.0
   }
}

impl<T: Eq + Hash + Ord> PartialOrd for Set<T> {
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

impl<T: Eq + Hash + Ord> Lattice for Set<T> {
   fn meet_mut(&mut self, mut other: Self) -> bool {
      let self_len = self.0.len();
      let mut old_self = BTreeSet::new();
      std::mem::swap(&mut self.0, &mut old_self);
      if self.0.len() > other.0.len() {
         std::mem::swap(self, &mut other);
      }
      for item in old_self.into_iter() {
         if other.0.contains(&item) {
            self.0.insert(item);
         }
      }
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

   fn meet(mut self, other: Self) -> Self {
       self.meet_mut(other);
       self
   }

   fn join(mut self, other: Self) -> Self {
      self.join_mut(other);
      self
   }
}
