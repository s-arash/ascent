use std::hash::{BuildHasherDefault, Hash};
use std::iter::{FlatMap, Repeat, Zip};

#[cfg(feature = "par")]
use ascent::rayon::prelude::{IntoParallelRefIterator, ParallelIterator};
use hashbrown::hash_set::Iter as HashSetIter;
use hashbrown::{HashMap, HashSet};
use rustc_hash::FxHasher;

use crate::utils::merge_sets;

#[derive(Clone, Debug)]
pub struct EqRel<T: Clone + Hash + Eq> {
   pub(crate) sets: Vec<HashSet<T, BuildHasherDefault<FxHasher>>>,
   pub(crate) elem_ids: HashMap<T, usize, BuildHasherDefault<FxHasher>>,
   pub(crate) set_subsumptions: HashMap<usize, usize, BuildHasherDefault<FxHasher>>,
}

impl<T: Clone + Hash + Eq> Default for EqRel<T> {
   fn default() -> Self {
      Self { sets: Default::default(), elem_ids: Default::default(), set_subsumptions: Default::default() }
   }
}

pub type IterAllIterator<'a, T> = FlatMap<
   std::slice::Iter<'a, HashSet<T, BuildHasherDefault<FxHasher>>>,
   FlatMap<
      Zip<HashSetIter<'a, T>, Repeat<HashSetIter<'a, T>>>,
      Zip<HashSetIter<'a, T>, Repeat<&'a T>>,
      for<'aa> fn((&'aa T, HashSetIter<'aa, T>)) -> Zip<HashSetIter<'aa, T>, Repeat<&'aa T>>,
   >,
   fn(
      &HashSet<T, BuildHasherDefault<FxHasher>>,
   ) -> FlatMap<
      Zip<HashSetIter<T>, Repeat<HashSetIter<T>>>,
      Zip<HashSetIter<T>, Repeat<&T>>,
      for<'aa> fn((&'aa T, HashSetIter<'aa, T>)) -> Zip<HashSetIter<'aa, T>, Repeat<&'aa T>>,
   >,
>;

#[cfg(feature = "par")]
pub struct IterAllParIterator<'a, T: Clone + Hash + Eq>(&'a EqRel<T>);
#[cfg(feature = "par")]
impl<'a, T: Clone + Hash + Eq + Sync> ParallelIterator for IterAllParIterator<'a, T> {
   type Item = (&'a T, &'a T);

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where C: ascent::rayon::iter::plumbing::UnindexedConsumer<Self::Item> {
      self
         .0
         .sets
         .par_iter()
         .flat_map::<fn(_) -> _, _>(|s| {
            s.par_iter().map_with(s, |s, x| s.par_iter().map_with(x, |x, y| (*x, y))).flatten()
         })
         .drive_unindexed(consumer)
   }
}

impl<T: Clone + Hash + Eq> EqRel<T> {
   fn get_dominant_id(&self, id: usize) -> usize {
      match self.set_subsumptions.get(&id) {
         Some(dom_id) => self.get_dominant_id(*dom_id),
         None => id,
      }
   }
   pub(crate) fn elem_set(&self, elem: &T) -> Option<usize> {
      self.elem_ids.get(elem).map(|id| self.get_dominant_id(*id))
   }

   fn get_dominant_id_update(&mut self, id: usize) -> usize {
      match self.set_subsumptions.get(&id) {
         Some(&parent_id) => {
            let dom_id = self.get_dominant_id_update(parent_id);
            if dom_id != parent_id {
               self.set_subsumptions.insert(id, dom_id);
            }
            dom_id
         },
         None => id,
      }
   }
   pub(crate) fn elem_set_update(&mut self, elem: &T) -> Option<usize> {
      let id = self.elem_ids.get(elem)?;
      Some(self.get_dominant_id_update(*id))
   }

   pub fn add(&mut self, x: T, y: T) -> bool {
      let x_set = self.elem_set_update(&x);
      let y_set = self.elem_set_update(&y);
      match (x_set, y_set) {
         (None, None) => {
            let id = self.sets.len();
            self.sets.push(HashSet::from_iter([x.clone(), y.clone()]));
            self.elem_ids.insert(x.clone(), id);
            self.elem_ids.insert(y.clone(), id);
            true
         },
         (None, Some(y_set)) => {
            self.sets[y_set].insert(x.clone());
            self.elem_ids.insert(x, y_set);
            true
         },
         (Some(x_set), None) => {
            self.sets[x_set].insert(y.clone());
            self.elem_ids.insert(y, x_set);
            true
         },
         (Some(x_set), Some(y_set)) =>
            if x_set != y_set {
               let y_set_taken = std::mem::take(&mut self.sets[y_set]);
               merge_sets(&mut self.sets[x_set], y_set_taken);
               self.set_subsumptions.insert(y_set, x_set);
               true
            } else {
               false
            },
      }
   }

   pub fn set_of(&self, x: &T) -> Option<HashSetIter<T>> {
      let set = self.elem_set(x)?;
      let res = Some(self.sets[set].iter());
      res
   }

   #[cfg(feature = "par")]
   pub fn c_set_of(&self, x: &T) -> Option<&'_ hashbrown::hash_set::HashSet<T, BuildHasherDefault<FxHasher>>>
   where T: Sync {
      let set = self.elem_set(x)?;
      Some(&self.sets[set])
   }

   // TODO not used
   #[allow(dead_code)]
   fn set_of_inc_x<'a>(&'a self, x: &'a T) -> impl Iterator<Item = &'a T> {
      let set = self.set_of(x);
      let x_itself = if set.is_none() { Some(x) } else { None };
      set.into_iter().flatten().chain(x_itself)
   }

   pub fn iter_all(&self) -> IterAllIterator<'_, T> {
      let res: IterAllIterator<'_, T> = self
         .sets
         .iter()
         .flat_map(|s| s.iter().zip(std::iter::repeat(s.iter())).flat_map(|(x, s)| s.zip(std::iter::repeat(x))));
      res
   }

   #[cfg(feature = "par")]
   pub fn c_iter_all(&self) -> IterAllParIterator<'_, T>
   where T: Sync {
      IterAllParIterator(self)
   }

   pub fn contains(&self, x: &T, y: &T) -> bool { self.elem_set(x).is_some_and(|set| self.sets[set].contains(y)) }

   pub fn combine(&mut self, other: Self) {
      for set in other.sets.into_iter() {
         #[allow(clippy::comparison_chain)]
         if set.len() == 1 {
            let repr = set.into_iter().next().unwrap();
            self.add(repr.clone(), repr);
         } else if set.len() > 1 {
            let mut set = set.into_iter();
            let repr = set.next().unwrap();
            for x in set {
               self.add(repr.clone(), x);
            }
         }
      }
   }

   pub fn count_exact(&self) -> usize { self.sets.iter().map(|s| s.len() * s.len()).sum() }
}

#[test]
fn test_eq_rel() {
   let mut eqrel = EqRel::<i32>::default();
   eqrel.add(1, 2);
   eqrel.add(11, 12);
   assert!(eqrel.contains(&1, &2));
   assert!(!eqrel.contains(&1, &12));
   eqrel.add(1, 3);
   eqrel.add(13, 12);
   assert!(!eqrel.contains(&2, &13));
   eqrel.add(3, 11);
   assert!(eqrel.contains(&2, &13));
}

#[test]
fn test_eq_rel_combine() {
   let mut eqrel1 = EqRel::<i32>::default();
   eqrel1.add(1, 2);
   eqrel1.add(1, 3);
   eqrel1.add(1, 10);

   let mut eqrel2 = EqRel::<i32>::default();
   eqrel2.add(10, 11);
   eqrel2.add(11, 12);
   eqrel2.add(13, 12);

   assert!(!eqrel1.contains(&1, &13));
   eqrel1.combine(eqrel2);
   assert!(eqrel1.contains(&1, &13));
}
