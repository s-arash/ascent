use std::hash::Hash;

use super::BoundedLattice;
use super::set::Set;
use crate::Lattice;

/// `BoundedSet` is a generalization of the flat lattice.
///
/// A `BoundedSet` stores at most `BOUND` items, and if asked to store more, will go to `TOP`.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct BoundedSet<const BOUND: usize, T: PartialEq + Eq + Hash + Ord>(Option<Set<T>>);

impl<const BOUND: usize, T: PartialEq + Eq + Hash + Ord> Default for BoundedSet<BOUND, T> {
   fn default() -> Self { Self::new() }
}

impl<const BOUND: usize, T: PartialEq + Eq + Hash + Ord> BoundedSet<BOUND, T> {
   /// A set containing everything
   pub const TOP: Self = BoundedSet(None);

   /// Creates an empty `BoundedSet`
   pub fn new() -> Self { BoundedSet(Some(Set::default())) }

   /// Creates a `BoundedSet` containing only `item`
   pub fn singleton(item: T) -> Self { Self::from_set(Set::singleton(item)) }

   /// Creates a `BoundedSet` from a `Set`, ensuring the `BOUND` is not exceeded
   pub fn from_set(set: Set<T>) -> Self { if set.len() <= BOUND { BoundedSet(Some(set)) } else { BoundedSet(None) } }

   /// Returns the size of the set. In case of the set being `TOP`, returns `None`
   pub fn count(&self) -> Option<usize> { self.0.as_ref().map(|s| s.len()) }

   /// Returns `true` if the set contains the `item`. For a set that `is_top()`, always returns `true`.
   pub fn contains(&self, item: &T) -> bool {
      match &self.0 {
         Some(set) => set.0.contains(item),
         None => true,
      }
   }

   pub fn is_top(&self) -> bool { self.0.is_none() }
}

impl<const BOUND: usize, T: PartialEq + Eq + Hash + Ord> PartialOrd for BoundedSet<BOUND, T> {
   fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
      use std::cmp::Ordering;
      match (&self.0, &other.0) {
         (None, None) => Some(Ordering::Equal),
         (None, _) => Some(Ordering::Greater),
         (_, None) => Some(Ordering::Less),
         (Some(set1), Some(set2)) => set1.partial_cmp(set2),
      }
   }
}

impl<const BOUND: usize, T: PartialEq + Eq + Hash + Ord> Lattice for BoundedSet<BOUND, T> {
   fn meet_mut(&mut self, other: Self) -> bool {
      match (&mut self.0, other.0) {
         (None, None) => false,
         (this @ None, Some(set2)) => {
            *this = Some(set2);
            true
         },
         (Some(_), None) => false,
         (Some(set1), Some(set2)) => set1.meet_mut(set2),
      }
   }

   fn join_mut(&mut self, other: Self) -> bool {
      match (&mut self.0, other.0) {
         (None, _) => false,
         (this @ Some(_), None) => {
            *this = None;
            true
         },
         (Some(set1), Some(set2)) => {
            let changed = set1.join_mut(set2);
            if set1.len() > BOUND {
               self.0 = None;
               true
            } else {
               changed
            }
         },
      }
   }
   fn meet(self, other: Self) -> Self {
      match (self.0, other.0) {
         (None, None) => BoundedSet(None),
         (None, set2 @ Some(_)) => BoundedSet(set2),
         (set1 @ Some(_), None) => BoundedSet(set1),
         (Some(set1), Some(set2)) => {
            let res = set1.meet(set2);
            BoundedSet(Some(res))
         },
      }
   }

   fn join(self, other: Self) -> Self {
      match (self.0, other.0) {
         (None, _) => BoundedSet(None),
         (_, None) => BoundedSet(None),
         (Some(set1), Some(set2)) => {
            let res = set1.join(set2);
            if res.len() > BOUND { BoundedSet(None) } else { BoundedSet(Some(res)) }
         },
      }
   }
}

impl<const BOUND: usize, T: PartialEq + Eq + Hash + Ord> BoundedLattice for BoundedSet<BOUND, T> {
   fn bottom() -> Self { Self::new() }

   /// top is meant to represent a set containing everything
   fn top() -> Self { BoundedSet(None) }
}

#[test]
fn test_bounded_set() {
   let set1 = BoundedSet::<2, i32>::singleton(10);

   let mut set2_by_mut = set1.clone();
   assert!(set2_by_mut.join_mut(BoundedSet::singleton(11)));

   let set2 = set1.join(BoundedSet::singleton(11));

   assert!(set2_by_mut == set2);
   assert_eq!(set2.count(), Some(2));
   assert!(set2.contains(&10));
   assert!(!set2.contains(&20));

   let set3 = set2.join(BoundedSet::singleton(12));
   assert!(set3.is_top());
   assert!(set3 == BoundedSet::TOP);
   assert!(set3.contains(&15));
}
