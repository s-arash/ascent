use std::hash::{BuildHasherDefault, Hash};
use std::iter::{FlatMap, Map, Repeat, Zip};
use std::marker::PhantomData;
use std::mem::transmute;
use std::rc::Rc;

use ascent::internal::{
   RelFullIndexRead, RelFullIndexWrite, RelIndexMerge, RelIndexRead, RelIndexReadAll, RelIndexWrite, ToRelIndex,
};
use hashbrown::HashSet;
use hashbrown::hash_set::Iter as HashSetIter;
#[cfg(test)]
use itertools::Itertools;
use rustc_hash::FxHasher;

use crate::iterator_from_dyn::IteratorFromDyn;
use crate::union_find::EqRel;

pub struct EqRelInd0<'a, T: Clone + Hash + Eq>(pub(crate) &'a EqRelIndCommon<T>);

#[test]
fn test_eq_rel_ind_0_iter_all() {
   let mut eq_rel_old = EqRel::default();
   for x in 1..=10 {
      eq_rel_old.add(1, x);
   }
   for x in 101..=110 {
      eq_rel_old.add(101, x);
   }

   let mut eq_rel_new = EqRel::default();
   eq_rel_new.add(1, 110);
   eq_rel_new.combine(eq_rel_old.clone());

   let eq_rel_full_ind = EqRelIndCommon { old: Rc::new(eq_rel_old), combined: Rc::new(eq_rel_new) };
   let eq_rel_ind_0 = EqRelInd0(&eq_rel_full_ind);
   let iter = eq_rel_ind_0.iter_all().map(|x| (x.0, x.1.collect_vec())).collect_vec();

   for x in [1, 9, 103] {
      let iter_at_x = &iter.iter().find(|y| y.0.0 == x).unwrap().1;

      println!("x: {}, iter_at_x: {:?}", x, iter_at_x);
      assert_eq!(iter_at_x.len(), 20);
      assert_eq!(iter_at_x.into_iter().map(|x| *x.0).collect::<HashSet<_>>(), (1..=10).chain(101..=110).collect());
   }
}
pub struct ToEqRelIndNone<T>(PhantomData<T>);
impl<T> Default for ToEqRelIndNone<T> {
   fn default() -> Self { Self(PhantomData) }
}
impl<T: Clone + Hash + Eq> ToRelIndex<EqRelIndCommon<T>> for ToEqRelIndNone<T> {
   type RelIndex<'a>
      = EqRelIndNone<'a, T>
   where T: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a EqRelIndCommon<T>) -> Self::RelIndex<'a> { EqRelIndNone(rel) }

   type RelIndexWrite<'a>
      = EqRelIndNone<'a, T>
   where T: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut EqRelIndCommon<T>) -> Self::RelIndexWrite<'a> {
      EqRelIndNone(rel)
   }
}

pub struct ToEqRelInd0<T>(PhantomData<T>);

impl<T> Default for ToEqRelInd0<T> {
   fn default() -> Self { Self(Default::default()) }
}

impl<T: Clone + Hash + Eq> ToRelIndex<EqRelIndCommon<T>> for ToEqRelInd0<T> {
   type RelIndex<'a>
      = EqRelInd0<'a, T>
   where T: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a EqRelIndCommon<T>) -> Self::RelIndex<'a> { EqRelInd0(rel) }

   type RelIndexWrite<'a>
      = EqRelInd0<'a, T>
   where T: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut EqRelIndCommon<T>) -> Self::RelIndexWrite<'a> { EqRelInd0(rel) }
}

pub struct ToEqRelInd0_1<T>(PhantomData<T>);

impl<T> Default for ToEqRelInd0_1<T> {
   fn default() -> Self { Self(Default::default()) }
}

pub struct EqRelInd0_1<'a, T: Clone + Hash + Eq>(&'a EqRelIndCommon<T>);
pub struct EqRelInd0_1Write<'a, T: Clone + Hash + Eq>(&'a mut EqRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq> RelIndexWrite for EqRelInd0_1Write<'a, T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, value: Self::Value) { self.0.index_insert(key, value) }
}

impl<'a, T: Clone + Hash + Eq> RelIndexMerge for EqRelInd0_1Write<'a, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      //noop
   }
}

impl<T: Clone + Hash + Eq> RelFullIndexWrite for EqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   fn insert_if_not_present(&mut self, key: &Self::Key, _v: Self::Value) -> bool {
      Rc::get_mut(&mut self.combined).unwrap().add(key.0.clone(), key.1.clone())
   }
}

impl<'a, T: Clone + Hash + Eq> RelFullIndexWrite for EqRelInd0_1Write<'a, T> {
   type Key = <EqRelIndCommon<T> as RelFullIndexWrite>::Key;
   type Value = <EqRelIndCommon<T> as RelFullIndexWrite>::Value;
   fn insert_if_not_present(&mut self, key: &Self::Key, v: Self::Value) -> bool { self.0.insert_if_not_present(key, v) }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for EqRelInd0_1<'a, T> {
   type Key = <EqRelIndCommon<T> as RelIndexRead<'a>>::Key;
   type Value = <EqRelIndCommon<T> as RelIndexRead<'a>>::Value;
   type IteratorType = <EqRelIndCommon<T> as RelIndexRead<'a>>::IteratorType;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> { self.0.index_get(key) }

   fn len(&self) -> usize { self.0.len() }
}

impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRelInd0_1<'a, T> {
   type Key = <EqRelIndCommon<T> as RelIndexReadAll<'a>>::Key;
   type Value = <EqRelIndCommon<T> as RelIndexReadAll<'a>>::Value;
   type ValueIteratorType = <EqRelIndCommon<T> as RelIndexReadAll<'a>>::ValueIteratorType;
   type AllIteratorType = <EqRelIndCommon<T> as RelIndexReadAll<'a>>::AllIteratorType;
   fn iter_all(&'a self) -> Self::AllIteratorType { self.0.iter_all() }
}

impl<'a, T: Clone + Hash + Eq> RelFullIndexRead<'a> for EqRelInd0_1<'a, T> {
   type Key = <EqRelIndCommon<T> as RelFullIndexRead<'a>>::Key;
   fn contains_key(&self, key: &Self::Key) -> bool { self.0.contains_key(key) }
}

impl<T: Clone + Hash + Eq> ToRelIndex<EqRelIndCommon<T>> for ToEqRelInd0_1<T> {
   type RelIndex<'a>
      = EqRelInd0_1<'a, T>
   where T: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a EqRelIndCommon<T>) -> Self::RelIndex<'a> { EqRelInd0_1(rel) }

   type RelIndexWrite<'a>
      = EqRelInd0_1Write<'a, T>
   where T: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut EqRelIndCommon<T>) -> Self::RelIndexWrite<'a> {
      EqRelInd0_1Write(rel)
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

   fn len(&self) -> usize { self.0.combined.elem_ids.len() }
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
      let res: Self::AllIteratorType = self.0.combined.sets.iter().flat_map(|s| {
         s.iter().zip(std::iter::repeat(s.iter())).map(|(x, s)| (ref_to_singleton_tuple_ref(x), s.map(|x| (x,))))
      });
      res
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexWrite for EqRelInd0<'a, T> {
   type Key = (T,);
   type Value = (T,);
   fn index_insert(&mut self, _key: Self::Key, _value: Self::Value) {
      // noop
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexMerge for EqRelInd0<'a, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      //noop
   }
}

#[derive(Clone)]
pub struct EqRelIndCommon<T: Clone + Hash + Eq> {
   pub(crate) old: Rc<EqRel<T>>,
   pub(crate) combined: Rc<EqRel<T>>,
}

impl<T: Clone + Hash + Eq> EqRelIndCommon<T> {
   pub fn iter_all_added(&self) -> impl Iterator<Item = (&T, &T)> {
      self.combined.iter_all().filter(|(x, y)| !self.old.contains(x, y))
   }

   pub(crate) fn set_of_added(&self, x: &T) -> Option<impl Iterator<Item = &T>> {
      let set = self.combined.set_of(x)?;
      // let old_set = self.old.set_of(x).into_iter().flatten();
      let old_set = self.old.elem_set(x).map(|id| &self.old.sets[id]);
      Some(set.filter(move |y| !old_set.map_or(false, |os| os.contains(*y))))
   }

   pub(crate) fn added_contains(&self, x: &T, y: &T) -> bool {
      self.combined.contains(x, y) && !self.old.contains(x, y)
   }

   pub fn count_exact(&self) -> usize {
      // old must be a subset of combined
      self.combined.count_exact() - self.old.count_exact()
   }
}

impl<T: Clone + Hash + Eq> Default for EqRelIndCommon<T> {
   fn default() -> Self { Self { old: Default::default(), combined: Default::default() } }
}

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexRead<'a> for EqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   type IteratorType = std::iter::Once<()>;

   fn index_get(&'a self, (x, y): &Self::Key) -> Option<Self::IteratorType> {
      if self.combined.contains(x, y) && !self.old.contains(x, y) { Some(std::iter::once(())) } else { None }
   }

   fn len(&self) -> usize {
      let sample_size = 3;
      let sum: usize = self.combined.sets.iter().take(sample_size).map(|s| s.len().pow(2)).sum();
      let sets_len = self.combined.sets.len();
      sum * sets_len / sample_size.min(sets_len).max(1)
   }
}

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexReadAll<'a> for EqRelIndCommon<T> {
   type Key = (&'a T, &'a T);
   type Value = ();

   type ValueIteratorType = std::iter::Once<()>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType { Box::new(self.iter_all_added().map(|x| (x, std::iter::once(())))) }
}

impl<'a, T: Clone + Hash + Eq> RelFullIndexRead<'a> for EqRelIndCommon<T> {
   type Key = (T, T);

   fn contains_key(&'a self, (x, y): &Self::Key) -> bool { self.combined.contains(x, y) && !self.old.contains(x, y) }
}

impl<'a, T: Clone + Hash + Eq> RelIndexWrite for EqRelIndCommon<T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, _value: Self::Value) {
      Rc::get_mut(&mut self.combined).unwrap().add(key.0, key.1);
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexMerge for EqRelIndCommon<T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      unimplemented!("merge_delta_to_total_new_to_delta must be used instead")
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      total.combined = delta.combined.clone();
      delta.old = total.combined.clone();

      // delta.combined.combine(new.combined.clone());
      Rc::make_mut(&mut delta.combined).combine(std::mem::take(Rc::get_mut(&mut new.combined).unwrap()));
   }
}

pub struct EqRelIndNone<'a, T: Clone + Hash + Eq>(&'a EqRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for EqRelIndNone<'a, T> {
   type Key = ();

   type Value = (&'a T, &'a T);

   type IteratorType = IteratorFromDyn<'a, (&'a T, &'a T)>;

   fn index_get(&'a self, _key: &Self::Key) -> Option<Self::IteratorType> {
      Some(IteratorFromDyn::new(|| self.0.iter_all_added()))
   }

   fn len(&self) -> usize { 1 }
}

impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRelIndNone<'a, T> {
   type Key = ();

   type Value = (&'a T, &'a T);

   type ValueIteratorType = IteratorFromDyn<'a, (&'a T, &'a T)>;

   type AllIteratorType = std::option::IntoIter<(Self::Key, Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType { self.index_get(&()).map(|iter| ((), iter)).into_iter() }
}

impl<'a, T: Clone + Hash + Eq> RelIndexWrite for EqRelIndNone<'a, T> {
   type Key = ();
   type Value = (T, T);
   fn index_insert(&mut self, _key: Self::Key, _value: Self::Value) { /* noop */
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexMerge for EqRelIndNone<'a, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { /* noop */
   }
}

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
