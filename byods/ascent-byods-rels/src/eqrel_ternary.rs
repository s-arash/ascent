#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_ternary_rel {
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, ()) => {
      $crate::fake_vec::FakeVec<($col0, $col1, $col2)>
   };
}
pub use eqrel_ternary_rel as rel;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_rel_ternary_full_ind {
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2IndFull<$col0, $col1>
   };
}
pub use eqrel_rel_ternary_full_ind as rel_full_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_ternary_rel_ind {
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2IndNone<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2Ind0<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2Ind1<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [2], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2Ind2<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 1], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2Ind0_1<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 2], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2Ind0_1<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [1, 2], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2Ind1_2<$col0, $col1>
   };
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 1, 2], $key: ty, $val: ty) => {
      $crate::eqrel_ternary::ToEqRel2IndFull<$col0, $col1>
   };
}
pub use eqrel_ternary_rel_ind as rel_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_ternary_rel_ind_common {
   (($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, ()) => {
      $crate::eqrel_ternary::EqRel2IndCommonWithReverse<$col0, $col1>
   };
}
use std::hash::{BuildHasherDefault, Hash};
use std::iter::Map;
use std::marker::PhantomData;

use ascent::internal::{
   RelFullIndexRead, RelFullIndexWrite, RelIndexMerge, RelIndexRead, RelIndexReadAll, RelIndexWrite, ToRelIndex,
};
pub use eqrel_ternary_rel_ind_common as rel_ind_common;
use itertools::Itertools;
use rustc_hash::FxHasher;

use crate::eqrel_ind::{EqRelIndCommon, ref_to_singleton_tuple_ref};
use crate::iterator_from_dyn::IteratorFromDyn;
use crate::rel_boilerplate::NoopRelIndexWrite;

type FxHashSet<T> = hashbrown::hash_set::HashSet<T, BuildHasherDefault<FxHasher>>;
type FxHashMap<K, V> = hashbrown::hash_map::HashMap<K, V, BuildHasherDefault<FxHasher>>;

fn _test_macros() {
   let _x: rel!((u32, u64, u64), [[0, 1], [0]], ser, ());
   let _full_ind: rel_full_ind!((u32, u64, u64), [[0, 1], [0]], ser, (), (u32, u32), ());
   let _ind_0: rel_ind!((u32, u64, u64), [[0, 1], [0]], ser, (), [0], (u32,), (u32,));
}

#[derive(Clone)]
pub struct EqRel2IndWrapper<const WITH_REVERSE: bool, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(
   EqRel2IndCommon<T0, T1>,
);
pub type EqRel2IndCommonWithReverse<T0, T1> = EqRel2IndWrapper<true, T0, T1>;
pub type EqRel2IndCommonWithoutReverse<T0, T1> = EqRel2IndWrapper<false, T0, T1>;

impl<const WITH_REVERSE: bool, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexMerge
   for EqRel2IndWrapper<WITH_REVERSE, T0, T1>
{
   fn move_index_contents(from: &mut Self, to: &mut Self) {
      EqRel2IndCommon::move_index_contents(&mut from.0, &mut to.0)
   }
   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      EqRel2IndCommon::merge_delta_to_total_new_to_delta(&mut new.0, &mut delta.0, &mut total.0)
   }
}

impl<const WITH_REVERSE: bool, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> Default
   for EqRel2IndWrapper<WITH_REVERSE, T0, T1>
{
   fn default() -> Self {
      let reverse_map = if WITH_REVERSE { Some(Default::default()) } else { None };
      Self(EqRel2IndCommon { map: Default::default(), reverse_map })
   }
}

pub trait ToEqRel2IndCommon<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> {
   fn to_eq_rel2_ind_common(&self) -> &EqRel2IndCommon<T0, T1>;
   fn to_eq_rel2_ind_common_mut(&mut self) -> &mut EqRel2IndCommon<T0, T1>;
}

impl<const WITH_REVERSE: bool, T0, T1> ToEqRel2IndCommon<T0, T1> for EqRel2IndWrapper<WITH_REVERSE, T0, T1>
where
   T0: Clone + Hash + Eq,
   T1: Clone + Hash + Eq,
{
   fn to_eq_rel2_ind_common(&self) -> &EqRel2IndCommon<T0, T1> { &self.0 }
   fn to_eq_rel2_ind_common_mut(&mut self) -> &mut EqRel2IndCommon<T0, T1> { &mut self.0 }
}

#[derive(Clone)]
pub struct EqRel2IndCommon<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> {
   map: FxHashMap<T0, EqRelIndCommon<T1>>,
   reverse_map: Option<FxHashMap<T1, FxHashSet<T0>>>,
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> EqRel2IndCommon<T0, T1> {
   fn insert_into_reverse_map(&mut self, key: &(T0, T1, T1)) {
      if let Some(reverse_map) = &mut self.reverse_map {
         reverse_map
            .raw_entry_mut()
            .from_key(&key.1)
            .or_insert_with(|| (key.1.clone(), Default::default()))
            .1
            .insert(key.0.clone());
         reverse_map
            .raw_entry_mut()
            .from_key(&key.2)
            .or_insert_with(|| (key.2.clone(), Default::default()))
            .1
            .insert(key.0.clone());
      }
   }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelFullIndexWrite for EqRel2IndCommon<T0, T1> {
   type Key = (T0, T1, T1);
   type Value = ();

   fn insert_if_not_present(&mut self, key: &Self::Key, (): Self::Value) -> bool {
      self.insert_into_reverse_map(key);
      match self.map.entry(key.0.clone()) {
         hashbrown::hash_map::Entry::Occupied(mut occ) =>
            occ.get_mut().insert_if_not_present(&(key.1.clone(), key.2.clone()), ()),
         hashbrown::hash_map::Entry::Vacant(vac) => {
            let mut eqrel = EqRelIndCommon::default();
            eqrel.index_insert((key.1.clone(), key.2.clone()), ());
            vac.insert(eqrel);
            true
         },
      }
   }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexWrite for EqRel2IndCommon<T0, T1> {
   type Key = (T0, T1, T1);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, (): Self::Value) {
      self.insert_into_reverse_map(&key);
      self.map.entry(key.0).or_default().index_insert((key.1, key.2), ());
   }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexMerge for EqRel2IndCommon<T0, T1> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      unimplemented!("merge_delta_to_total_new_to_delta must be used instead")
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      for (t0, mut delta_eqrel) in delta.map.drain() {
         let mut new_eqrel = new.map.remove(&t0).unwrap_or_default();
         let total_eqrel = total.map.entry(t0).or_default();
         RelIndexMerge::merge_delta_to_total_new_to_delta(&mut new_eqrel, &mut delta_eqrel, total_eqrel);
      }
      for (t0, new_eqrel) in new.map.drain() {
         delta.map.insert(t0, new_eqrel);
      }

      // TODO not sure about this
      if delta.reverse_map.is_some() {
         crate::utils::move_hash_map_of_hash_set_contents(
            &mut delta.reverse_map.as_mut().unwrap(),
            total.reverse_map.as_mut().unwrap(),
         );
         crate::utils::move_hash_map_of_hash_set_contents(
            new.reverse_map.as_mut().unwrap(),
            delta.reverse_map.as_mut().unwrap(),
         );
      }
   }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> EqRel2IndCommon<T0, T1> {
   pub(crate) fn iter_all_added(&self) -> impl Iterator<Item = (&T0, &T1, &T1)> {
      self.map.iter().flat_map(|(t0, eqrel)| eqrel.iter_all_added().map(move |(t1, t2)| (t0, t1, t2)))
   }
}

pub struct EqRel2Ind0_1<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a EqRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for EqRel2Ind0_1<'a, T0, T1> {
   type Key = (T0, T1);
   type Value = (&'a T1,);

   type IteratorType = IteratorFromDyn<'a, (&'a T1,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let eqrel = self.0.map.get(&key.0)?;
      let _ = eqrel.set_of_added(&key.1)?;
      let key_1 = key.1.clone();
      let producer = move || eqrel.set_of_added(&key_1).unwrap().map(|x| (x,));

      Some(IteratorFromDyn::new(producer))
   }

   fn len(&self) -> usize {
      let sample_size = 4;
      let (count, sum) = self
         .0
         .map
         .values()
         .take(sample_size)
         .map(|eqrel| eqrel.combined.elem_ids.len())
         .fold((0, 0), |(c, s), x| (c + 1, s + x));

      sum * self.0.map.len() / count.max(1)
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRel2Ind0_1<'a, T0, T1> {
   type Key = (&'a T0, &'a T1);
   type Value = (&'a T1,);

   type ValueIteratorType = std::iter::Once<(&'a T1,)>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(
         self
            .0
            .map
            .iter()
            .flat_map(|(t0, eqrel)| eqrel.iter_all_added().map(move |(t1, t2)| ((t0, t1), std::iter::once((t2,))))),
      )
   }
}

pub struct EqRel2Ind0<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a EqRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRel2Ind0<'a, T0, T1> {
   type Key = &'a (T0,);
   type Value = (&'a T1, &'a T1);

   type ValueIteratorType = IteratorFromDyn<'a, Self::Value>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(
         self
            .0
            .map
            .iter()
            .map(|(t, eqrel)| (ref_to_singleton_tuple_ref(t), IteratorFromDyn::new(|| eqrel.iter_all_added()))),
      )
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for EqRel2Ind0<'a, T0, T1> {
   type Key = (T0,);
   type Value = (&'a T1, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;
   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let eqrel = self.0.map.get(&key.0)?;
      Some(IteratorFromDyn::new(|| eqrel.iter_all_added()))
   }

   fn len(&self) -> usize { self.0.map.len() }
}

pub struct EqRel2Ind1<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a EqRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRel2Ind1<'a, T0, T1> {
   type Key = &'a (T1,);
   type Value = (&'a T0, &'a T1);

   type ValueIteratorType = IteratorFromDyn<'a, Self::Value>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(
         self
            .0
            .reverse_map
            .as_ref()
            .unwrap()
            .keys()
            .map(ref_to_singleton_tuple_ref)
            .map(|k| (k, self.index_get(k).unwrap())),
      )
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for EqRel2Ind1<'a, T0, T1> {
   type Key = (T1,);
   type Value = (&'a T0, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let t0s = self.0.reverse_map.as_ref().unwrap().get(&key.0)?;
      let t1 = key.0.clone();
      let res = move || {
         t0s.iter().zip(std::iter::repeat(t1.clone())).flat_map(move |(t0, t1)| {
            self.0.map.get(t0).unwrap().set_of_added(&t1).into_iter().flatten().map(move |t2| (t0, t2))
         })
      };
      Some(IteratorFromDyn::new(res))
   }

   fn len(&self) -> usize { self.0.reverse_map.as_ref().unwrap().len() }
}

pub struct EqRel2Ind1_2<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a EqRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRel2Ind1_2<'a, T0, T1> {
   type Key = (&'a T1, &'a T1);
   type Value = (&'a T0,);

   type ValueIteratorType =
      Map<hashbrown::hash_set::Intersection<'a, T0, BuildHasherDefault<FxHasher>>, for<'aa> fn(&'aa T0) -> (&T0,)>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self
         .0
         .reverse_map
         .as_ref()
         .unwrap()
         .iter()
         .cartesian_product(self.0.reverse_map.as_ref().unwrap().iter())
         .map(|((t0, t0_set), (t1, t1_set))| {
            let intersection: Self::ValueIteratorType = t0_set.intersection(t1_set).map(|x| (x,));
            ((t0, t1), intersection)
         });
      Box::new(res)
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for EqRel2Ind1_2<'a, T0, T1> {
   type Key = (T1, T1);

   type Value = (&'a T0,);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let t0s = self.0.reverse_map.as_ref().unwrap().get(&key.0)?;

      let key = key.clone();
      let res = move || {
         t0s.iter()
            .zip(std::iter::repeat(key.clone()))
            .filter(|(t0, key)| self.0.map.get(*t0).unwrap().added_contains(&key.0, &key.1))
            .map(|(t0, _key)| (t0,))
      };

      Some(IteratorFromDyn::new(res))
   }

   fn len(&self) -> usize {
      let sample_size = 4;
      let sum = self.0.map.values().take(sample_size).map(|eqrel| eqrel.len()).sum::<usize>();
      let map_len = self.0.map.len();
      sum / sample_size.min(map_len).max(1)
   }
}

pub struct ToEqRel2IndFull<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(PhantomData<(T0, T1)>);

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> Default for ToEqRel2IndFull<T0, T1> {
   fn default() -> Self { Self(PhantomData) }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, Rel> ToRelIndex<Rel> for ToEqRel2IndFull<T0, T1>
where Rel: ToEqRel2IndCommon<T0, T1>
{
   type RelIndex<'a>
      = EqRel2IndFull<'a, T0, T1>
   where
      Self: 'a,
      Rel: 'a;
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { EqRel2IndFull(rel.to_eq_rel2_ind_common()) }

   type RelIndexWrite<'a>
      = &'a mut EqRel2IndCommon<T0, T1>
   where
      Self: 'a,
      Rel: 'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a> {
      rel.to_eq_rel2_ind_common_mut()
   }
}

pub struct EqRel2IndFull<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a EqRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRel2IndFull<'a, T0, T1> {
   type Key = (&'a T0, &'a T1, &'a T1);

   type Value = &'a ();

   type ValueIteratorType = std::iter::Once<&'a ()>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.iter_all_added().map(|t| (t, std::iter::once(&()))))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for EqRel2IndFull<'a, T0, T1> {
   type Key = (T0, T1, T1);
   type Value = &'a ();

   type IteratorType = std::iter::Once<&'a ()>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      if self.contains_key(key) { Some(std::iter::once(&())) } else { None }
   }

   fn len(&self) -> usize {
      let sample_size = 4;
      let sum = self.0.map.values().take(sample_size).map(|eqrel| eqrel.len()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelFullIndexRead<'a> for EqRel2IndFull<'a, T0, T1> {
   type Key = (T0, T1, T1);

   fn contains_key(&'a self, (t0, t1, t2): &Self::Key) -> bool {
      if let Some(eqrel) = self.0.map.get(t0) { eqrel.added_contains(t1, t2) } else { false }
   }
}

pub struct EqRel2IndNone<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a EqRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for EqRel2IndNone<'a, T0, T1> {
   type Key = &'a ();
   type Value = (&'a T0, &'a T1, &'a T1);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;

   type AllIteratorType = std::iter::Once<(Self::Key, Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType { std::iter::once((&(), self.index_get(&()).unwrap())) }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for EqRel2IndNone<'a, T0, T1> {
   type Key = ();
   type Value = (&'a T0, &'a T1, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;
   fn index_get(&'a self, (): &Self::Key) -> Option<Self::IteratorType> {
      Some(IteratorFromDyn::new(|| self.0.iter_all_added()))
   }

   fn len(&self) -> usize { 1 }
}

macro_rules! to_eq_rel2 {
   ($name: ident, $key: ty, $val: ty) => {paste::paste!{
      pub struct [<To $name>]<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(PhantomData<(T0, T1)>);

      impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> Default for [<To $name>]<T0, T1> {
         fn default() -> Self { Self(PhantomData) }
      }

      impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, Rel> ToRelIndex<Rel> for [<To $name>]<T0, T1>
      where Rel: ToEqRel2IndCommon<T0, T1>
      {
         type RelIndex<'a> = $name<'a, T0, T1> where Self: 'a, Rel: 'a;
         fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { $name(rel.to_eq_rel2_ind_common()) }

         type RelIndexWrite<'a> = NoopRelIndexWrite<$key, $val> where Self: 'a, Rel: 'a;
         fn to_rel_index_write<'a>(&'a mut self, _rel: &'a mut Rel) -> Self::RelIndexWrite<'a> {
            NoopRelIndexWrite::default()
         }
      }
   }};
}

to_eq_rel2!(EqRel2IndNone, (), (T0, T1, T1));
to_eq_rel2!(EqRel2Ind0, (T0,), (T1, T1));
to_eq_rel2!(EqRel2Ind0_1, (T0, T1), (T1,));
to_eq_rel2!(EqRel2Ind1, (T1,), (T0, T1));
to_eq_rel2!(EqRel2Ind1_2, (T1, T1), (T0,));
// to_eq_rel2!(EqRel2IndFull, (T0, T1, T1), ());
