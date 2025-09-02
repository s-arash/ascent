use core::panic;
use std::hash::{BuildHasherDefault, Hash};
use std::rc::Rc;
use std::time::{Duration, Instant};

use ascent::internal::{RelIndexMerge, RelIndexRead, RelIndexReadAll};
use hashbrown::{HashMap, HashSet};
use rustc_hash::FxHasher;

use crate::adaptor::bin_rel::ByodsBinRel;
use crate::binary_rel::{self, MapRelIndexAdaptor};
use crate::iterator_from_dyn::IteratorFromDyn;
use crate::trrel_binary::MyHashSet;
use crate::trrel_union_find::TrRelUnionFind;
use crate::utils::move_hash_map_of_hash_set_contents_disjoint;

type NewSet<T> = HashSet<(T, T), BuildHasherDefault<FxHasher>>;
pub enum TrRelIndCommon<T: Clone + Hash + Eq> {
   New { rel: NewSet<T> },
   Delta { rel: TrRelDelta<T> },
   Total { rel: Rc<TrRelUnionFind<T>> },
}

pub struct TrRelDelta<T: Clone + Hash + Eq> {
   set_connections: binary_rel::Map<usize>,
   rev_set_connections: binary_rel::Map<usize>,
   precursor: NewSet<T>,
   total: Rc<TrRelUnionFind<T>>,
}

impl<T: Clone + Hash + Eq> Default for TrRelDelta<T> {
   fn default() -> Self {
      Self {
         set_connections: Default::default(),
         rev_set_connections: Default::default(),
         precursor: Default::default(),
         total: Default::default(),
      }
   }
}

impl<T: Clone + Hash + Eq> TrRelDelta<T> {
   fn ind_0_get(&self, x: &T) -> Option<IteratorFromDyn<&'_ T>> {
      let x_set = self.total.elem_set(x)?;
      let sets_of_x = self.set_connections.get(&x_set)?;

      let res = || sets_of_x.iter().flat_map(|&s| self.total.sets[s].iter());
      Some(IteratorFromDyn::new(res))
   }
   fn ind_1_get(&self, x: &T) -> Option<IteratorFromDyn<&'_ T>> {
      let x_set = self.total.elem_set(x)?;
      let sets_of_x = self.rev_set_connections.get(&x_set)?;

      let res = || sets_of_x.iter().flat_map(|&s| self.total.sets[s].iter());
      Some(IteratorFromDyn::new(res))
   }
   fn ind_0_iter_all(&self) -> IteratorFromDyn<(&T, IteratorFromDyn<&T>)> {
      let res = || {
         self.set_connections.iter().flat_map(|(set_id, set_connections)| {
            let xs = &self.total.sets[*set_id];
            xs.iter().map(|x| {
               let ys = || set_connections.iter().flat_map(|sc| self.total.sets[*sc].iter());
               (x, IteratorFromDyn::new(ys))
            })
         })
      };
      IteratorFromDyn::new(res)
   }
   fn ind_1_iter_all(&self) -> IteratorFromDyn<(&T, IteratorFromDyn<&T>)> {
      let res = || {
         self.rev_set_connections.iter().flat_map(|(set_id, rev_set_connections)| {
            let xs = &self.total.sets[*set_id];
            xs.iter().map(|x| {
               let ys = || rev_set_connections.iter().flat_map(|sc| self.total.sets[*sc].iter());
               (x, IteratorFromDyn::new(ys))
            })
         })
      };
      IteratorFromDyn::new(res)
   }

   fn contains(&self, x: &T, y: &T) -> bool { self.ind_0_1_get(x, y).is_some() }
   fn ind_0_1_get(&self, x: &T, y: &T) -> Option<std::iter::Once<()>> {
      let x_set = self.total.elem_set(x)?;
      let y_set = self.total.elem_set(y)?;
      if x_set == y_set {
         return None
      }
      let x_set_connections = self.set_connections.get(&x_set)?;
      if x_set_connections.contains(&y_set) { Some(std::iter::once(())) } else { None }
   }

   fn iter_all(&self) -> impl Iterator<Item = (&'_ T, &'_ T)> + '_ {
      let res = self.set_connections.iter().flat_map(move |(x_set, y_sets)| {
         self.total.sets[*x_set].iter().flat_map(move |x| {
            y_sets
               .iter()
               .filter(move |y_set| *y_set != x_set)
               .flat_map(move |y_set| self.total.sets[*y_set].iter().map(move |y| (x, y)))
         })
      });
      res
   }
   fn is_empty(&self) -> bool { self.precursor.is_empty() }
}

impl<T: Clone + Hash + Eq> Default for TrRelIndCommon<T> {
   #[inline]
   fn default() -> Self { Self::Total { rel: Default::default() } }
}

impl<T: Clone + Hash + Eq> TrRelIndCommon<T> {
   pub fn unwrap_new_mut(&mut self) -> &mut NewSet<T> {
      match self {
         Self::New { rel, .. } => rel,
         _ => {
            assert!(self.is_empty(), "unwrap_new_mut called on non-empty non-New");
            *self = Self::New { rel: Default::default() };
            self.unwrap_new_mut()
         },
      }
   }

   pub fn unwrap_total(&self) -> &TrRelUnionFind<T> {
      match self {
         Self::Total { rel, .. } => rel,
         _ => panic!("TrRelIndCommon: unwrap_total called on non-Total"),
      }
   }

   pub fn is_empty(&self) -> bool {
      match self {
         Self::New { rel, .. } => rel.is_empty(),
         Self::Delta { rel, .. } => rel.set_connections.is_empty(),
         Self::Total { rel, .. } => rel.elem_ids.is_empty(),
      }
   }

   pub fn count_exact(&self) -> usize { self.unwrap_total().count_exact() }
}

pub static mut MERGE_TIME: Duration = Duration::ZERO;
pub static mut MERGE_DELTA_CONSTRUCTION_TIME: Duration = Duration::ZERO;
pub static mut MERGE_TOTAL_UPDATE_TIME: Duration = Duration::ZERO;
pub static mut MERGE_COUNT: usize = 0;

impl<T: Clone + Hash + Eq> RelIndexMerge for TrRelIndCommon<T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      panic!("merge_delta_to_total_new_to_delta must be called instead.")
   }

   fn init(new: &mut Self, _delta: &mut Self, _total: &mut Self) {
      *new = Self::New { rel: Default::default() };
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      let before = Instant::now();

      if let Self::Total { .. } = delta {
         assert!(total.is_empty());
         *total = std::mem::take(delta);
         *delta = Self::Delta { rel: TrRelDelta::default() }
      }

      let mut delta_rel = match delta {
         Self::Delta { rel } => std::mem::take(rel),
         _ => panic!("expected Delta"),
      };
      delta_rel.total = Rc::new(Default::default());

      let mut total_rel_rc = match total {
         Self::Total { rel, .. } => std::mem::take(rel),
         _ => panic!("expected Total"),
      };

      let mut new_rel = std::mem::take(new.unwrap_new_mut());

      // optimization for when total will be empty
      if total_rel_rc.is_empty() && delta_rel.is_empty() {
         let mut new_delta = TrRelUnionFind::default();
         let before_total_update = Instant::now();

         for (x, y) in new_rel.drain() {
            new_delta.add(x.clone(), y);
         }
         unsafe {
            MERGE_TOTAL_UPDATE_TIME += before_total_update.elapsed();
         }
         *delta = Self::Total { rel: Rc::new(new_delta) };
         return;
      }
      let total_rel = Rc::get_mut(&mut total_rel_rc).unwrap();

      let before_total_update = Instant::now();
      for (x, y) in delta_rel.precursor.drain() {
         total_rel.add(x, y);
      }
      unsafe {
         MERGE_TOTAL_UPDATE_TIME += before_total_update.elapsed();
      }

      type RelMap<T> = HashMap<T, MyHashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>;

      let mut new_classes_map: RelMap<usize> = Default::default();
      let mut new_classes_rev_map: RelMap<usize> = Default::default();
      for (x, y) in new_rel.iter() {
         let x_id = total_rel.add_node(x.clone());
         let y_id = total_rel.add_node(y.clone());
         new_classes_map.entry(x_id).or_default().insert(y_id);
         new_classes_rev_map.entry(y_id).or_default().insert(x_id);
      }
      // println!("merge. new_classes_map.len(): {}, new_classes_rev_map.len(): {}", new_classes_map.len(), new_classes_rev_map.len());

      let mut delta_delta_map = new_classes_map.clone();
      let mut delta_delta_rev_map = new_classes_rev_map;

      let mut delta_total_map = RelMap::<usize>::default();
      let mut delta_total_rev_map = RelMap::<usize>::default();

      let mut delta_new_map = RelMap::<usize>::default();
      let mut delta_new_rev_map = RelMap::<usize>::default();

      fn join<
         'a,
         T: Clone + Hash + Eq + 'a,
         Rel1: RelIndexRead<'a, Key = T, Value = &'a T> + RelIndexReadAll<'a, Key = &'a T, Value = &'a T>,
         Rel2Rev: RelIndexRead<'a, Key = T, Value = &'a T> + RelIndexReadAll<'a, Key = &'a T, Value = &'a T>,
      >(
         target: &mut RelMap<T>, target_rev: &mut RelMap<T>, rel1: &'a Rel1, rel2_rev: &'a Rel2Rev,
         mut can_add: impl FnMut(&T, &T) -> bool, _name: &str,
      ) -> bool
      where
         Rel1::ValueIteratorType: Clone,
      {
         let mut changed = false;
         if rel1.len_estimate() < rel2_rev.len_estimate() {
            for (x, x_set) in rel1.iter_all() {
               if let Some(x_rev_set) = rel2_rev.index_get(x) {
                  for w in x_rev_set {
                     let entry = target.entry(w.clone()).or_default();
                     for y in x_set.clone() {
                        if !can_add(w, y) {
                           continue
                        }
                        if entry.insert(y.clone()) {
                           target_rev.entry(y.clone()).or_default().insert(w.clone());
                           changed = true;
                        }
                     }
                     if entry.is_empty() {
                        target.remove(w);
                     }
                  }
               }
            }
         } else {
            for (x, x_rev_set) in rel2_rev.iter_all() {
               if let Some(x_set) = rel1.index_get(x) {
                  for w in x_rev_set {
                     let entry = target.entry(w.clone()).or_default();
                     for y in x_set.clone() {
                        if !can_add(w, y) {
                           continue
                        }
                        if entry.insert(y.clone()) {
                           target_rev.entry(y.clone()).or_default().insert(w.clone());
                           changed = true;
                        }
                     }
                     if entry.is_empty() {
                        target.remove(w);
                     }
                  }
               }
            }
         }
         changed
      }
      let before_loop = Instant::now();
      loop {
         let mut cached_delta_delta_map_entry_for_can_add = None;
         let mut cached_delta_delta_map_x_for_can_add = None;
         let mut cached_delta_total_map_entry_for_can_add = None;
         let mut cached_delta_total_map_x_for_can_add = None;
         let mut cached_total_map_entry_for_can_add = None;
         let mut cached_total_map_x_for_can_add = None;
         let mut can_add = |x: &usize, y: &usize| {
            {
               if cached_delta_delta_map_x_for_can_add.as_ref() != Some(x) {
                  cached_delta_delta_map_entry_for_can_add = delta_delta_map.get(x);
                  cached_delta_delta_map_x_for_can_add = Some(*x);
               };
            }
            !cached_delta_delta_map_entry_for_can_add.is_some_and(|s| s.contains(y))
               && {
                  if cached_delta_total_map_x_for_can_add.as_ref() != Some(x) {
                     cached_delta_total_map_entry_for_can_add = delta_total_map.get(x);
                     cached_delta_total_map_x_for_can_add = Some(*x);
                  };
                  !cached_delta_total_map_entry_for_can_add.is_some_and(|s| s.contains(y))
               }
               && {
                  if cached_total_map_x_for_can_add.as_ref() != Some(x) {
                     cached_total_map_entry_for_can_add = total_rel.set_connections.get(x);
                     cached_total_map_x_for_can_add = Some(*x);
                  }
                  !cached_total_map_entry_for_can_add.is_some_and(|s| s.contains(y))
               }
         };

         let join1 = join(
            &mut delta_new_map,
            &mut delta_new_rev_map,
            &MapRelIndexAdaptor(&delta_delta_map),
            &MapRelIndexAdaptor(&total_rel.reverse_set_connections),
            &mut can_add,
            "join1",
         );
         let join2 = join(
            &mut delta_new_map,
            &mut delta_new_rev_map,
            &MapRelIndexAdaptor(&total_rel.set_connections),
            &MapRelIndexAdaptor(&delta_delta_rev_map),
            &mut can_add,
            "join2",
         );
         let join3 = join(
            &mut delta_new_map,
            &mut delta_new_rev_map,
            &MapRelIndexAdaptor(&new_classes_map),
            &MapRelIndexAdaptor(&delta_delta_rev_map),
            &mut can_add,
            "join3",
         );

         let changed = join1 | join2 | join3;

         move_hash_map_of_hash_set_contents_disjoint(&mut delta_delta_map, &mut delta_total_map);
         move_hash_map_of_hash_set_contents_disjoint(&mut delta_delta_rev_map, &mut delta_total_rev_map);

         assert!(delta_delta_map.is_empty());
         assert!(delta_delta_rev_map.is_empty());

         std::mem::swap(&mut delta_delta_map, &mut delta_new_map);
         std::mem::swap(&mut delta_delta_rev_map, &mut delta_new_rev_map);

         if !changed {
            break
         }
      }
      unsafe {
         MERGE_DELTA_CONSTRUCTION_TIME += before_loop.elapsed();
      }

      let new_delta: TrRelDelta<T> = TrRelDelta {
         set_connections: delta_total_map,
         rev_set_connections: delta_total_rev_map,
         precursor: new_rel,
         total: total_rel_rc.clone(),
      };
      *delta = Self::Delta { rel: new_delta };
      *total = Self::Total { rel: total_rel_rc };

      unsafe {
         MERGE_TIME += before.elapsed();
         MERGE_COUNT += 1;
      }
   }
}

impl<T: Clone + Hash + Eq> ByodsBinRel for TrRelIndCommon<T> {
   type T0 = T;
   type T1 = T;

   fn contains(&self, x0: &Self::T0, x1: &Self::T1) -> bool {
      match self {
         Self::Delta { rel, .. } => rel.contains(x0, x1),
         Self::Total { rel, .. } => rel.contains(x0, x1),
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   type AllIter<'a>
      = Box<dyn Iterator<Item = (&'a T, &'a T)> + 'a>
   where Self: 'a;

   fn iter_all(&self) -> Self::AllIter<'_> {
      match self {
         Self::Delta { rel, .. } => Box::new(rel.iter_all()),
         Self::Total { rel, .. } => Box::new(rel.iter_all()),
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   fn len_estimate(&self) -> usize {
      let sample_size = 3;
      match self {
         Self::Delta { rel, .. } => {
            let avg_set_connections = rel.set_connections.iter().take(3).map(|(_s, sets)| sets.len()).sum::<usize>()
               / sample_size.min(rel.set_connections.len()).max(1);
            let avg_set_size = rel.total.sets.iter().rev().take(sample_size).map(|s| s.len()).sum::<usize>()
               / sample_size.min(rel.total.sets.len()).max(1);
            avg_set_connections * avg_set_size
         },
         Self::Total { rel, .. } => {
            let avg_set_connections = rel.set_connections.iter().take(3).map(|(_s, sets)| sets.len()).sum::<usize>()
               / sample_size.min(rel.set_connections.len()).max(1);
            let avg_set_size = rel.sets.iter().rev().take(sample_size).map(|s| s.len()).sum::<usize>()
               / sample_size.min(rel.sets.len()).max(1);
            avg_set_connections * avg_set_size
         },
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   type Ind0AllIterValsIter<'a>
      = IteratorFromDyn<'a, &'a T>
   where Self: 'a;
   type Ind0AllIter<'a>
      = IteratorFromDyn<'a, (&'a T, Self::Ind0AllIterValsIter<'a>)>
   where Self: 'a;

   fn ind0_iter_all(&self) -> Self::Ind0AllIter<'_> {
      match self {
         Self::Delta { rel, .. } => rel.ind_0_iter_all(),
         Self::Total { rel, .. } => {
            let res = || {
               rel.elem_ids.iter().map(|(x, set_id)| {
                  let set = || rel.set_of_by_set_id(x, *set_id);
                  (x, IteratorFromDyn::new(set))
               })
            };
            IteratorFromDyn::new(res)
         },
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   fn ind0_len_estimate(&self) -> usize {
      let res = match self {
         Self::Delta { rel, .. } => {
            let sample_size = 5;
            let sum_set_size = rel.total.sets.iter().rev().take(sample_size).map(|s| s.len()).sum::<usize>();
            sum_set_size * rel.set_connections.len() / sample_size.min(rel.total.sets.len()).max(1)
         },
         Self::Total { rel, .. } => {
            let sample_size = 3;
            let sum_set_size = rel.sets.iter().rev().take(sample_size).map(|s| s.len()).sum::<usize>();
            sum_set_size * rel.set_connections.len() / sample_size.min(rel.sets.len()).max(1)
         },
         Self::New { .. } => panic!("unexpected New"),
      };
      res
   }

   type Ind0ValsIter<'a>
      = IteratorFromDyn<'a, &'a T>
   where Self: 'a;

   fn ind0_index_get<'a>(&'a self, key: &Self::T0) -> Option<Self::Ind0ValsIter<'a>> {
      match self {
         Self::Delta { rel, .. } => rel.ind_0_get(key),
         Self::Total { rel, .. } => {
            let (key, id) = rel.elem_ids.get_key_value(key)?;
            let id = rel.get_dominant_id(*id);
            let res = move || rel.set_of_by_set_id(key, id);
            Some(IteratorFromDyn::new(res))
         },
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   type Ind1AllIterValsIter<'a>
      = IteratorFromDyn<'a, &'a T>
   where Self: 'a;
   type Ind1AllIter<'a>
      = IteratorFromDyn<'a, (&'a T, Self::Ind1AllIterValsIter<'a>)>
   where Self: 'a;

   fn ind1_iter_all(&self) -> Self::Ind1AllIter<'_> {
      match self {
         Self::Delta { rel, .. } => rel.ind_1_iter_all(),
         Self::Total { rel, .. } => {
            let res = || {
               rel.elem_ids.iter().map(|(x, set_id)| {
                  let set = || rel.rev_set_of_by_set_id(x, *set_id);
                  (x, IteratorFromDyn::new(set))
               })
            };
            IteratorFromDyn::new(res)
         },
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   fn ind1_len_estimate(&self) -> usize {
      let res = match self {
         Self::Delta { rel, .. } => {
            let sample_size = 5;
            let sum_set_size = rel.total.sets.iter().rev().take(sample_size).map(|s| s.len()).sum::<usize>();
            sum_set_size * rel.rev_set_connections.len() / sample_size.min(rel.total.sets.len()).max(1)
         },
         Self::Total { rel, .. } => {
            let sample_size = 3;
            let sum_set_size = rel.sets.iter().rev().take(sample_size).map(|s| s.len()).sum::<usize>();
            sum_set_size * rel.reverse_set_connections.len() / sample_size.min(rel.sets.len()).max(1)
         },
         Self::New { .. } => panic!("unexpected New"),
      };
      res
   }

   type Ind1ValsIter<'a>
      = IteratorFromDyn<'a, &'a T>
   where Self: 'a;
   fn ind1_index_get<'a>(&'a self, key: &Self::T1) -> Option<Self::Ind1ValsIter<'a>> {
      match self {
         Self::Delta { rel, .. } => rel.ind_1_get(key),
         Self::Total { rel, .. } => {
            let (key, id) = rel.elem_ids.get_key_value(key)?;
            let id = rel.get_dominant_id(*id);
            let res = move || rel.rev_set_of_by_set_id(key, id);
            Some(IteratorFromDyn::new(res))
         },
         Self::New { .. } => panic!("unexpected New"),
      }
   }

   fn insert(&mut self, x0: Self::T0, x1: Self::T1) -> bool { self.unwrap_new_mut().insert((x0, x1)) }
}
