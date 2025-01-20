use std::hash::{BuildHasherDefault, Hash};
use std::iter::{FlatMap, Map, Repeat, Zip};
use std::marker::PhantomData;
use std::mem::transmute;
use std::sync::Mutex;

use ascent::internal::{
   CRelFullIndexWrite, CRelIndexRead, CRelIndexReadAll, CRelIndexWrite, Freezable, RelFullIndexRead, RelFullIndexWrite,
   RelIndexMerge, RelIndexRead, RelIndexReadAll, RelIndexWrite, ToRelIndex, ToRelIndex0,
};
use ascent::rayon;
use ascent::rayon::prelude::{IntoParallelRefIterator, ParallelIterator};
use hashbrown::HashSet;
use hashbrown::hash_set::Iter as HashSetIter;
use rustc_hash::FxHasher;

use crate::iterator_from_dyn::IteratorFromDyn;
use crate::union_find::EqRel;

pub struct EqRelInd0<'a, T: Clone + Hash + Eq>(pub(crate) &'a CEqRelIndCommon<T>);

pub struct ToEqRelIndNone<T>(PhantomData<T>);
impl<T> Freezable for ToEqRelIndNone<T> {}
impl<T> Default for ToEqRelIndNone<T> {
   fn default() -> Self { Self(PhantomData) }
}
impl<T: Clone + Hash + Eq> ToRelIndex<CEqRelIndCommon<T>> for ToEqRelIndNone<T> {
   type RelIndex<'a>
      = EqRelIndNone<'a, T>
   where T: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a CEqRelIndCommon<T>) -> Self::RelIndex<'a> { EqRelIndNone(rel) }

   type RelIndexWrite<'a>
      = EqRelIndNone<'a, T>
   where T: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut CEqRelIndCommon<T>) -> Self::RelIndexWrite<'a> {
      EqRelIndNone(rel)
   }
}

pub struct ToEqRelInd0<T>(PhantomData<T>);
impl<T> Freezable for ToEqRelInd0<T> {}

impl<T> Default for ToEqRelInd0<T> {
   fn default() -> Self { Self(Default::default()) }
}

impl<T: Clone + Hash + Eq> ToRelIndex<CEqRelIndCommon<T>> for ToEqRelInd0<T> {
   type RelIndex<'a>
      = EqRelInd0<'a, T>
   where T: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a CEqRelIndCommon<T>) -> Self::RelIndex<'a> { EqRelInd0(rel) }

   type RelIndexWrite<'a>
      = EqRelInd0<'a, T>
   where T: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut CEqRelIndCommon<T>) -> Self::RelIndexWrite<'a> {
      EqRelInd0(rel)
   }
}

pub struct ToEqRelInd0_1<T>(PhantomData<T>);
impl<T> Freezable for ToEqRelInd0_1<T> {}

impl<T> Default for ToEqRelInd0_1<T> {
   fn default() -> Self { Self(Default::default()) }
}

pub struct EqRelInd0_1<'a, T: Clone + Hash + Eq>(&'a CEqRelIndCommon<T>);
pub struct EqRelInd0_1Write<'a, T: Clone + Hash + Eq>(&'a mut CEqRelIndCommon<T>);
pub struct EqRelInd0_1CWrite<'a, T: Clone + Hash + Eq>(&'a CEqRelIndCommon<T>);

impl<T: Clone + Hash + Eq> RelIndexWrite for EqRelInd0_1Write<'_, T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, value: Self::Value) { self.0.index_insert(key, value) }
}

impl<T: Clone + Hash + Eq> CRelIndexWrite for EqRelInd0_1CWrite<'_, T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&self, key: Self::Key, (): Self::Value) {
      self.0.unwrap_unfrozen().lock().unwrap().add(key.0, key.1);
   }
}

impl<T: Clone + Hash + Eq> RelIndexMerge for EqRelInd0_1Write<'_, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { /* noop */
   }
}

impl<T: Clone + Hash + Eq> RelFullIndexWrite for CEqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   fn insert_if_not_present(&mut self, key: &Self::Key, _v: Self::Value) -> bool {
      self.unwrap_mut_unfrozen().add(key.0.clone(), key.1.clone())
   }
}

impl<T: Clone + Hash + Eq> CRelFullIndexWrite for CEqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   fn insert_if_not_present(&self, key: &Self::Key, _v: Self::Value) -> bool {
      self.unwrap_unfrozen().lock().unwrap().add(key.0.clone(), key.1.clone())
   }
}

impl<T: Clone + Hash + Eq> RelFullIndexWrite for EqRelInd0_1Write<'_, T> {
   type Key = <CEqRelIndCommon<T> as RelFullIndexWrite>::Key;
   type Value = <CEqRelIndCommon<T> as RelFullIndexWrite>::Value;
   fn insert_if_not_present(&mut self, key: &Self::Key, v: Self::Value) -> bool { self.0.insert_if_not_present(key, v) }
}

impl<T: Clone + Hash + Eq> CRelFullIndexWrite for EqRelInd0_1CWrite<'_, T> {
   type Key = <CEqRelIndCommon<T> as CRelFullIndexWrite>::Key;
   type Value = <CEqRelIndCommon<T> as CRelFullIndexWrite>::Value;
   fn insert_if_not_present(&self, key: &Self::Key, v: Self::Value) -> bool {
      CRelFullIndexWrite::insert_if_not_present(&self.0, key, v)
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for EqRelInd0_1<'a, T> {
   type Key = <CEqRelIndCommon<T> as RelIndexRead<'a>>::Key;
   type Value = <CEqRelIndCommon<T> as RelIndexRead<'a>>::Value;
   type IteratorType = <CEqRelIndCommon<T> as RelIndexRead<'a>>::IteratorType;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> { self.0.index_get(key) }

   fn len_estimate(&self) -> usize { self.0.len_estimate() }
}

impl<'a, T: Clone + Hash + Eq + Sync> CRelIndexRead<'a> for EqRelInd0_1<'a, T> {
   type Key = <CEqRelIndCommon<T> as CRelIndexRead<'a>>::Key;
   type Value = <CEqRelIndCommon<T> as CRelIndexRead<'a>>::Value;
   type IteratorType = <CEqRelIndCommon<T> as CRelIndexRead<'a>>::IteratorType;

   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> { self.0.c_index_get(key) }
}

impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRelInd0_1<'a, T> {
   type Key = <CEqRelIndCommon<T> as RelIndexReadAll<'a>>::Key;
   type Value = <CEqRelIndCommon<T> as RelIndexReadAll<'a>>::Value;
   type ValueIteratorType = <CEqRelIndCommon<T> as RelIndexReadAll<'a>>::ValueIteratorType;
   type AllIteratorType = <CEqRelIndCommon<T> as RelIndexReadAll<'a>>::AllIteratorType;
   fn iter_all(&'a self) -> Self::AllIteratorType { self.0.iter_all() }
}

impl<'a, T: Clone + Hash + Eq + Sync> CRelIndexReadAll<'a> for EqRelInd0_1<'a, T> {
   type Key = <CEqRelIndCommon<T> as CRelIndexReadAll<'a>>::Key;
   type Value = <CEqRelIndCommon<T> as CRelIndexReadAll<'a>>::Value;
   type ValueIteratorType = <CEqRelIndCommon<T> as CRelIndexReadAll<'a>>::ValueIteratorType;
   type AllIteratorType = <CEqRelIndCommon<T> as CRelIndexReadAll<'a>>::AllIteratorType;
   fn c_iter_all(&'a self) -> Self::AllIteratorType { self.0.c_iter_all() }
}

impl<'a, T: Clone + Hash + Eq> RelFullIndexRead<'a> for EqRelInd0_1<'a, T> {
   type Key = <CEqRelIndCommon<T> as RelFullIndexRead<'a>>::Key;
   fn contains_key(&self, key: &Self::Key) -> bool { self.0.contains_key(key) }
}

impl<T: Clone + Hash + Eq> ToRelIndex0<CEqRelIndCommon<T>> for ToEqRelInd0_1<T> {
   type RelIndex<'a>
      = EqRelInd0_1<'a, T>
   where T: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a CEqRelIndCommon<T>) -> Self::RelIndex<'a> { EqRelInd0_1(rel) }

   type RelIndexWrite<'a>
      = EqRelInd0_1Write<'a, T>
   where T: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut CEqRelIndCommon<T>) -> Self::RelIndexWrite<'a> {
      EqRelInd0_1Write(rel)
   }

   type CRelIndexWrite<'a>
      = EqRelInd0_1CWrite<'a, T>
   where
      Self: 'a,
      CEqRelIndCommon<T>: 'a;
   fn to_c_rel_index_write<'a>(&'a self, rel: &'a CEqRelIndCommon<T>) -> Self::CRelIndexWrite<'a> {
      EqRelInd0_1CWrite(rel)
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for EqRelInd0<'a, T> {
   type Key = (T,);
   type Value = (&'a T,);

   type IteratorType = IteratorFromDyn<'a, (&'a T,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let _ = self.0.set_of_added(&key.0)?;
      let key = key.clone();
      let producer = move || self.0.set_of_added(&key.0).unwrap().map(|x| (x,));

      Some(IteratorFromDyn::new(producer))
   }

   fn len_estimate(&self) -> usize { self.0.unwrap_frozen().combined.elem_ids.len() }
}

impl<'a, T: Clone + Hash + Eq + Sync> CRelIndexRead<'a> for EqRelInd0<'a, T> {
   type Key = (T,);
   type Value = (&'a T,);

   type IteratorType = rayon::iter::Map<SetOfAddedParIter<'a, T>, fn(&T) -> (&T,)>;

   fn c_index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let set = self.0.c_set_of_added(&key.0)?;
      let res: Self::IteratorType = set.map(|x| (x,));
      Some(res)
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRelInd0<'a, T> {
   type Key = &'a (T,);
   type Value = (&'a T,);

   type ValueIteratorType = Map<HashSetIter<'a, T>, for<'aa> fn(&'aa T) -> (&'aa T,)>;

   type AllIteratorType = FlatMap<
      std::slice::Iter<'a, HashSet<T, BuildHasherDefault<FxHasher>>>,
      Map<
         Zip<HashSetIter<'a, T>, Repeat<HashSetIter<'a, T>>>,
         for<'aa> fn(
            (&'aa T, HashSetIter<'aa, T>),
         ) -> (&'aa (T,), Map<HashSetIter<'aa, T>, for<'bb> fn(&'bb T) -> (&'bb T,)>),
      >,
      for<'aa> fn(
         &'aa HashSet<T, BuildHasherDefault<FxHasher>>,
      ) -> Map<
         Zip<HashSetIter<'aa, T>, Repeat<HashSetIter<'aa, T>>>,
         for<'cc> fn(
            (&'cc T, HashSetIter<'cc, T>),
         ) -> (&'cc (T,), Map<HashSetIter<'cc, T>, for<'dd> fn(&'dd T) -> (&'dd T,)>),
      >,
   >;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.0.unwrap_frozen().combined.sets.iter().flat_map(|s| {
         s.iter().zip(std::iter::repeat(s.iter())).map(|(x, s)| (ref_to_singleton_tuple_ref(x), s.map(|x| (x,))))
      });
      res
   }
}

pub struct EqRelInd0CRelIndexReadAllIter<'a, T: Clone + Hash + Eq + Sync>(&'a EqRelPair<T>);

impl<'a, T: Clone + Hash + Eq + Sync + Send> ParallelIterator for EqRelInd0CRelIndexReadAllIter<'a, T> {
   type Item = (&'a (T,), rayon::iter::Map<hashbrown::hash_set::rayon::ParIter<'a, T>, fn(&T) -> (&T,)>);

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where C: rayon::iter::plumbing::UnindexedConsumer<Self::Item> {
      self
         .0
         .combined
         .sets
         .par_iter()
         .flat_map(|s| {
            s.par_iter().map(|x| {
               let vals_iter: rayon::iter::Map<hashbrown::hash_set::rayon::ParIter<'a, T>, fn(&T) -> (&T,)> =
                  s.par_iter().map(|x| (x,));
               (ref_to_singleton_tuple_ref(x), vals_iter)
            })
         })
         .drive_unindexed(consumer)
   }
}

impl<'a, T: Clone + Hash + Eq + Sync + Send> CRelIndexReadAll<'a> for EqRelInd0<'a, T> {
   type Key = &'a (T,);
   type Value = (&'a T,);

   type AllIteratorType = EqRelInd0CRelIndexReadAllIter<'a, T>;

   type ValueIteratorType = rayon::iter::Map<hashbrown::hash_set::rayon::ParIter<'a, T>, fn(&T) -> (&T,)>;

   fn c_iter_all(&'a self) -> Self::AllIteratorType { EqRelInd0CRelIndexReadAllIter(self.0.unwrap_frozen()) }
}

impl<T: Clone + Hash + Eq> RelIndexWrite for EqRelInd0<'_, T> {
   type Key = (T,);
   type Value = (T,);
   fn index_insert(&mut self, _key: Self::Key, _value: Self::Value) { /* noop */
   }
}

impl<T: Clone + Hash + Eq> CRelIndexWrite for EqRelInd0<'_, T> {
   type Key = (T,);
   type Value = (T,);
   fn index_insert(&self, _key: Self::Key, _value: Self::Value) { /* noop */
   }
}

impl<T: Clone + Hash + Eq> RelIndexMerge for EqRelInd0<'_, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { /* noop */
   }
}

pub enum CEqRelIndCommon<T: Clone + Hash + Eq> {
   Unfrozen(Mutex<EqRel<T>>),
   Frozen(EqRelPair<T>),
}

#[derive(Clone)]
pub struct EqRelPair<T: Clone + Hash + Eq> {
   pub(crate) old: EqRel<T>,
   pub(crate) combined: EqRel<T>,
}

impl<T: Clone + Hash + Eq> Default for EqRelPair<T> {
   fn default() -> Self { Self { old: Default::default(), combined: Default::default() } }
}

impl<T: Clone + Hash + Eq> Freezable for CEqRelIndCommon<T> {}

impl<T: Clone + Hash + Eq> CEqRelIndCommon<T> {
   fn unwrap_frozen(&self) -> &EqRelPair<T> {
      match self {
         CEqRelIndCommon::Frozen(old) => old,
         CEqRelIndCommon::Unfrozen(_) => panic!("unwrap_frozen() called on Unfrozen"),
      }
   }
   fn unwrap_mut_frozen(&mut self) -> &mut EqRelPair<T> {
      match self {
         CEqRelIndCommon::Frozen(old) => old,
         CEqRelIndCommon::Unfrozen(_) => panic!("unwrap_mut_frozen() called on Unfrozen"),
      }
   }
   fn unwrap_mut_unfrozen(&mut self) -> &mut EqRel<T> {
      match self {
         CEqRelIndCommon::Unfrozen(uf) => uf.get_mut().unwrap(),
         CEqRelIndCommon::Frozen(_) => panic!("unwrap_mut_unfrozen called on Frozen"),
      }
   }
   fn unwrap_unfrozen(&self) -> &Mutex<EqRel<T>> {
      match self {
         CEqRelIndCommon::Unfrozen(uf) => uf,
         CEqRelIndCommon::Frozen(_) => panic!("unwrap_unfrozen called on Frozen"),
      }
   }
}

impl<T: Clone + Hash + Eq> Clone for CEqRelIndCommon<T> {
   fn clone(&self) -> Self {
      match self {
         CEqRelIndCommon::Unfrozen(new) => CEqRelIndCommon::Unfrozen(Mutex::new(new.lock().unwrap().clone())),
         CEqRelIndCommon::Frozen(old) => CEqRelIndCommon::Frozen(old.clone()),
      }
   }
}

#[derive(Clone)]
pub struct AllAddedParIter<'a, T: Clone + Hash + Eq + Sync>(&'a EqRelPair<T>);

impl<'a, T: Clone + Hash + Eq + Sync> ParallelIterator for AllAddedParIter<'a, T> {
   type Item = (&'a T, &'a T);

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where C: ascent::rayon::iter::plumbing::UnindexedConsumer<Self::Item> {
      self.0.combined.c_iter_all().filter(|(x, y)| !self.0.old.contains(x, y)).drive_unindexed(consumer)
   }
}

#[derive(Clone)]
pub struct SetOfAddedParIter<'a, T: Clone + Hash + Eq + Sync> {
   set: &'a hashbrown::HashSet<T, BuildHasherDefault<FxHasher>>,
   old_set: Option<&'a HashSet<T, BuildHasherDefault<FxHasher>>>,
}

impl<'a, T: Clone + Hash + Eq + Sync> ParallelIterator for SetOfAddedParIter<'a, T> {
   type Item = &'a T;

   fn drive_unindexed<C>(self, consumer: C) -> C::Result
   where C: ascent::rayon::iter::plumbing::UnindexedConsumer<Self::Item> {
      self.set.par_iter().filter(move |y| !self.old_set.is_some_and(|os| os.contains(*y))).drive_unindexed(consumer)
   }
}

impl<T: Clone + Hash + Eq> CEqRelIndCommon<T> {
   pub(crate) fn iter_all_added(&self) -> impl Iterator<Item = (&T, &T)> {
      let eqrel = self.unwrap_frozen();
      eqrel.combined.iter_all().filter(|(x, y)| !eqrel.old.contains(x, y))
   }

   pub(crate) fn c_iter_all_added(&self) -> AllAddedParIter<'_, T>
   where T: Sync {
      let eqrel = self.unwrap_frozen();
      AllAddedParIter(eqrel)
   }

   pub(crate) fn set_of_added(&self, x: &T) -> Option<impl Iterator<Item = &T>> {
      let self_ = self.unwrap_frozen();
      let set = self_.combined.set_of(x)?;
      // let old_set = self.old.set_of(x).into_iter().flatten();
      let old_set = self_.old.elem_set(x).map(|id| &self_.old.sets[id]);
      Some(set.filter(move |y| !old_set.is_some_and(|os| os.contains(*y))))
   }

   pub(crate) fn c_set_of_added(&self, x: &T) -> Option<SetOfAddedParIter<'_, T>>
   where T: Sync {
      let self_ = self.unwrap_frozen();
      let set = self_.combined.c_set_of(x)?;
      let old_set = self_.old.elem_set(x).map(|id| &self_.old.sets[id]);
      Some(SetOfAddedParIter { set, old_set })
   }

   #[allow(dead_code)]
   pub(crate) fn added_contains(&self, x: &T, y: &T) -> bool {
      let self_ = self.unwrap_frozen();
      self_.combined.contains(x, y) && !self_.old.contains(x, y)
   }
}

impl<T: Clone + Hash + Eq> Default for CEqRelIndCommon<T> {
   fn default() -> Self { Self::Frozen(Default::default()) }
}

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexRead<'a> for CEqRelIndCommon<T> {
   type Key = &'a (T, T);
   type Value = ();

   type IteratorType = std::iter::Once<()>;

   fn index_get(&'a self, (x, y): &Self::Key) -> Option<Self::IteratorType> {
      let self_ = self.unwrap_frozen();
      if self_.combined.contains(x, y) && !self_.old.contains(x, y) { Some(std::iter::once(())) } else { None }
   }

   fn len_estimate(&self) -> usize {
      let self_ = self.unwrap_frozen();
      let sample_size = 3;
      let sum: usize = self_.combined.sets.iter().take(sample_size).map(|s| s.len().pow(2)).sum();
      let sets_len = self_.combined.sets.len();
      sum * sets_len / sample_size.min(sets_len).max(1)
   }
}

impl<'a, T: Clone + Hash + Eq + Sync + 'a> CRelIndexRead<'a> for CEqRelIndCommon<T> {
   type Key = &'a (T, T);
   type Value = ();

   type IteratorType = ascent::rayon::iter::Once<()>;

   fn c_index_get(&'a self, (x, y): &Self::Key) -> Option<Self::IteratorType> {
      let self_ = self.unwrap_frozen();
      if self_.combined.contains(x, y) && !self_.old.contains(x, y) {
         Some(ascent::rayon::iter::once(()))
      } else {
         None
      }
   }
}

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexReadAll<'a> for CEqRelIndCommon<T> {
   type Key = (&'a T, &'a T);
   type Value = ();

   type ValueIteratorType = std::iter::Once<()>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType { Box::new(self.iter_all_added().map(|x| (x, std::iter::once(())))) }
}

impl<'a, T: Clone + Hash + Eq + Sync + 'a> CRelIndexReadAll<'a> for CEqRelIndCommon<T> {
   type Key = (&'a T, &'a T);
   type Value = ();

   type ValueIteratorType = ascent::rayon::iter::Once<()>;

   type AllIteratorType = ascent::rayon::iter::Map<
      AllAddedParIter<'a, T>,
      for<'aa, 'bb> fn((&'aa T, &'bb T)) -> ((&'aa T, &'bb T), ascent::rayon::iter::Once<()>),
   >;

   fn c_iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.c_iter_all_added().map(|x| (x, ascent::rayon::iter::once(())));
      res
   }
}

impl<'a, T: Clone + Hash + Eq> RelFullIndexRead<'a> for CEqRelIndCommon<T> {
   type Key = (T, T);

   fn contains_key(&'a self, (x, y): &Self::Key) -> bool {
      let self_ = self.unwrap_frozen();
      self_.combined.contains(x, y) && !self_.old.contains(x, y)
   }
}

impl<T: Clone + Hash + Eq> RelIndexWrite for CEqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, _value: Self::Value) { self.unwrap_mut_unfrozen().add(key.0, key.1); }
}

impl<T: Clone + Hash + Eq> CRelIndexWrite for CEqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&self, key: Self::Key, _value: Self::Value) {
      self.unwrap_unfrozen().lock().unwrap().add(key.0, key.1);
   }
}

impl<T: Clone + Hash + Eq> RelIndexMerge for CEqRelIndCommon<T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      unimplemented!("merge_delta_to_total_new_to_delta must be used instead")
   }

   fn init(new: &mut Self, _delta: &mut Self, _total: &mut Self) {
      *new = Self::Unfrozen(Mutex::new(Default::default()))
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      let total = total.unwrap_mut_frozen();
      let delta = delta.unwrap_mut_frozen();
      total.combined = delta.combined.clone();
      delta.old = total.combined.clone();

      // delta.combined.combine(new.combined.clone());
      delta.combined.combine(std::mem::take(new.unwrap_mut_unfrozen()));
   }
}

pub struct EqRelIndNone<'a, T: Clone + Hash + Eq>(&'a CEqRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for EqRelIndNone<'a, T> {
   type Key = ();
   type Value = (&'a T, &'a T);

   type IteratorType = IteratorFromDyn<'a, (&'a T, &'a T)>;

   fn index_get(&'a self, _key: &Self::Key) -> Option<Self::IteratorType> {
      Some(IteratorFromDyn::new(|| self.0.iter_all_added()))
   }

   fn len_estimate(&self) -> usize { 1 }
}

impl<'a, T: Clone + Hash + Eq + Sync> CRelIndexRead<'a> for EqRelIndNone<'a, T> {
   type Key = ();
   type Value = (&'a T, &'a T);

   type IteratorType = AllAddedParIter<'a, T>;

   fn c_index_get(&'a self, _key: &Self::Key) -> Option<Self::IteratorType> { Some(self.0.c_iter_all_added()) }
}

impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRelIndNone<'a, T> {
   type Key = ();

   type Value = (&'a T, &'a T);

   type ValueIteratorType = IteratorFromDyn<'a, (&'a T, &'a T)>;

   type AllIteratorType = std::option::IntoIter<(Self::Key, Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType { self.index_get(&()).map(|iter| ((), iter)).into_iter() }
}

impl<'a, T: Clone + Hash + Eq + Sync> CRelIndexReadAll<'a> for EqRelIndNone<'a, T> {
   type Key = ();
   type Value = (&'a T, &'a T);

   type ValueIteratorType = AllAddedParIter<'a, T>;

   type AllIteratorType = ascent::rayon::iter::Once<(Self::Key, Self::ValueIteratorType)>;

   fn c_iter_all(&'a self) -> Self::AllIteratorType { ascent::rayon::iter::once(((), self.0.c_iter_all_added())) }
}

impl<T: Clone + Hash + Eq> RelIndexWrite for EqRelIndNone<'_, T> {
   type Key = ();
   type Value = (T, T);
   fn index_insert(&mut self, _key: Self::Key, _value: Self::Value) { /* noop */
   }
}

impl<T: Clone + Hash + Eq> CRelIndexWrite for EqRelIndNone<'_, T> {
   type Key = ();
   type Value = (T, T);
   fn index_insert(&self, _key: Self::Key, _value: Self::Value) { /* noop */
   }
}

impl<T: Clone + Hash + Eq> RelIndexMerge for EqRelIndNone<'_, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { /* noop */
   }
}

// TODO this is not safe, and not required. Get rid of it.
pub(crate) fn ref_to_singleton_tuple_ref<T>(x: &T) -> &(T,) { unsafe { transmute(x) } }

#[test]
fn test_ref_to_singleton_tuple_ref() {
   use std::mem::size_of;
   println!("size_of::<Vec<usize>>(): {}", size_of::<Vec<usize>>());
   println!("size_of::<(Vec<usize>,)>(): {}", size_of::<(Vec<usize>,)>());

   let x = vec![1, 2, 3];
   let x2 = ref_to_singleton_tuple_ref(&x);
   assert_eq!(&x, &x2.0);
}
