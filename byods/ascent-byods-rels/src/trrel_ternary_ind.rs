use std::hash::{BuildHasher as _, BuildHasherDefault, Hash};
use std::iter::{Map, once};

use ascent::internal::{
   RelFullIndexRead, RelFullIndexWrite, RelIndexMerge, RelIndexRead, RelIndexReadAll, RelIndexWrite,
};
use derive_more::{Deref, DerefMut};
use hashbrown::HashMap;
use rustc_hash::FxHasher;

use crate::iterator_from_dyn::IteratorFromDyn;
use crate::trrel_binary::MyHashSetIter;
use crate::trrel_binary_ind::{TrRelInd0, TrRelInd1, TrRelIndCommon};
use crate::utils::{AltHashSet, AltHashSetIter};

#[derive(DerefMut, Deref)]
pub struct TrRel2IndCommonWrapper<
   const HAS_REVERSE_MAP1: bool,
   const HAS_REVERSE_MAP2: bool,
   T0: Clone + Hash + Eq,
   T1: Clone + Hash + Eq,
>(TrRel2IndCommon<T0, T1>);

impl<const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>
   RelIndexMerge for TrRel2IndCommonWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1>
{
   fn move_index_contents(from: &mut Self, to: &mut Self) {
      TrRel2IndCommon::move_index_contents(&mut from.0, &mut to.0)
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      TrRel2IndCommon::merge_delta_to_total_new_to_delta(&mut new.0, &mut delta.0, &mut total.0)
   }
}

impl<const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> Default
   for TrRel2IndCommonWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1>
{
   fn default() -> Self {
      let reverse_map1 = if HAS_REVERSE_MAP1 { Some(Default::default()) } else { None };
      let reverse_map2 = if HAS_REVERSE_MAP2 { Some(Default::default()) } else { None };
      Self(TrRel2IndCommon { map: Default::default(), reverse_map1, reverse_map2 })
   }
}

type RevMapHashSet<T, S> = AltHashSet<T, S>;
#[allow(dead_code)]
type RevMapHashSetIter<'a, T> = AltHashSetIter<'a, T>;

pub struct TrRel2IndCommon<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> {
   pub map: HashMap<T0, TrRelIndCommon<T1>, BuildHasherDefault<FxHasher>>,
   pub reverse_map1: Option<HashMap<T1, RevMapHashSet<T0, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>>,
   pub reverse_map2: Option<HashMap<T1, RevMapHashSet<T0, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>>,
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexMerge for TrRel2IndCommon<T0, T1> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      panic!("merge_delta_to_total_new_to_delta must be called instead");
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      let mut new_delta_map = HashMap::default();
      for (k, mut delta_trrel) in delta.map.drain() {
         let mut new_trrel = new.map.remove(&k).unwrap_or_else(|| TrRelIndCommon::make_new());
         match total.map.entry(k.clone()) {
            hashbrown::hash_map::Entry::Occupied(mut total_entry) => {
               TrRelIndCommon::merge_delta_to_total_new_to_delta(
                  &mut new_trrel,
                  &mut delta_trrel,
                  total_entry.get_mut(),
               );
               if !delta_trrel.is_empty() {
                  new_delta_map.insert(k, delta_trrel);
               }
            },
            hashbrown::hash_map::Entry::Vacant(total_vacant_entry) => {
               let mut new_total =
                  TrRelIndCommon::Old { rel: Default::default(), anti_reflexive: delta_trrel.anti_reflexive() };
               TrRelIndCommon::merge_delta_to_total_new_to_delta(&mut new_trrel, &mut delta_trrel, &mut new_total);
               total_vacant_entry.insert(new_total);
               if !delta_trrel.is_empty() {
                  new_delta_map.insert(k, delta_trrel);
               }
            },
         }
      }
      for (k, mut new_trrel) in new.map.drain() {
         let mut new_delta = Default::default();
         match total.map.entry(k.clone()) {
            hashbrown::hash_map::Entry::Occupied(mut total_entry) => {
               TrRelIndCommon::merge_delta_to_total_new_to_delta(&mut new_trrel, &mut new_delta, total_entry.get_mut());
               new_delta_map.insert(k, new_delta);
            },
            hashbrown::hash_map::Entry::Vacant(_) => {
               TrRelIndCommon::merge_delta_to_total_new_to_delta(
                  &mut new_trrel,
                  &mut new_delta,
                  &mut Default::default(),
               );
               new_delta_map.insert(k, new_delta);
            },
         }
      }
      delta.map = new_delta_map;

      if delta.reverse_map1.is_some() {
         crate::utils::move_hash_map_of_alt_hash_set_contents(
            delta.reverse_map1.as_mut().unwrap(),
            total.reverse_map1.as_mut().unwrap(),
         );
         std::mem::swap(delta.reverse_map1.as_mut().unwrap(), new.reverse_map1.as_mut().unwrap());
      }

      if delta.reverse_map2.is_some() {
         crate::utils::move_hash_map_of_alt_hash_set_contents(
            delta.reverse_map2.as_mut().unwrap(),
            total.reverse_map2.as_mut().unwrap(),
         );
         std::mem::swap(delta.reverse_map2.as_mut().unwrap(), new.reverse_map2.as_mut().unwrap());
      }
   }
}

pub struct TrRel2Ind0<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2Ind0<'a, T0, T1> {
   type Key = (&'a T0,);
   type Value = (&'a T1, &'a T1);

   type ValueIteratorType = Box<dyn Iterator<Item = Self::Value> + 'a>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.map.iter().map(|(k, v)| ((k,), Box::new(v.rel().iter_all()) as _)))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2Ind0<'a, T0, T1> {
   type Key = (T0,);
   type Value = (&'a T1, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let trrel = self.0.map.get(&key.0)?;
      Some(IteratorFromDyn::new(|| trrel.rel().iter_all()))
   }

   fn len_estimate(&self) -> usize {
      let sample_size = 4;
      let sum = self.0.map.values().map(|x| x.rel().count_estimate()).sum::<usize>();
      sum * self.0.map.len() / sample_size.min(self.0.map.len()).max(1)
   }
   fn is_empty(&'a self) -> bool { self.0.map.is_empty() }
}

pub struct TrRel2Ind0_1<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2Ind0_1<'a, T0, T1> {
   type Key = (&'a T0, &'a T1);

   type Value = (&'a T1,);

   type ValueIteratorType = Map<MyHashSetIter<'a, T1>, fn(&T1) -> (&T1,)>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.map.iter().flat_map(|(x0, v)| {
         v.rel().map.iter().map(move |(x1, x2_set)| {
            let iter: Self::ValueIteratorType = x2_set.iter().map(|x2| (x2,));
            ((x0, x1), iter)
         })
      }))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2Ind0_1<'a, T0, T1> {
   type Key = (T0, T1);

   type Value = (&'a T1,);

   type IteratorType = Map<MyHashSetIter<'a, T1>, fn(&T1) -> (&T1,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let trrel = self.0.map.get(&key.0)?;
      let res: Self::IteratorType = trrel.rel().map.get(&key.1)?.iter().map(|x| (x,));
      Some(res)
   }

   fn len_estimate(&self) -> usize {
      let sample_size = 3;
      let sum = self.0.map.values().take(sample_size).map(|trrel| TrRelInd0(trrel).len_estimate()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
   fn is_empty(&'a self) -> bool { self.0.map.is_empty() }
}

pub struct TrRel2Ind0_2<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2Ind0_2<'a, T0, T1> {
   type Key = (&'a T0, &'a T1);
   type Value = (&'a T1,);

   type ValueIteratorType = Map<std::slice::Iter<'a, T1>, fn(&T1) -> (&T1,)>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.map.iter().flat_map(|(x0, v)| {
         v.rel().reverse_map.iter().map(move |(x1, x2_set)| {
            let iter: Self::ValueIteratorType = x2_set.iter().map(|x2| (x2,));
            ((x0, x1), iter)
         })
      }))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2Ind0_2<'a, T0, T1> {
   type Key = (T0, T1);
   type Value = (&'a T1,);

   type IteratorType = Map<std::slice::Iter<'a, T1>, fn(&T1) -> (&T1,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let trrel = self.0.map.get(&key.0)?;
      let res: Self::IteratorType = trrel.rel().reverse_map.get(&key.1)?.iter().map(|x| (x,));
      Some(res)
   }

   fn len_estimate(&self) -> usize {
      let sample_size = 3;
      let sum = self.0.map.values().take(sample_size).map(|trrel| TrRelInd1(trrel).len_estimate()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
   fn is_empty(&'a self) -> bool { self.0.map.is_empty() }
}

pub struct TrRel2Ind1<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2Ind1<'a, T0, T1> {
   type Key = (&'a T1,);
   type Value = (&'a T0, &'a T1);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.reverse_map1.as_ref().unwrap().keys().map(|x1| ((x1,), self.get(x1).unwrap())))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> TrRel2Ind1<'a, T0, T1> {
   fn get(&'a self, x1: &T1) -> Option<<Self as RelIndexRead<'a>>::IteratorType> {
      let (x1, x0s) = self.0.reverse_map1.as_ref().unwrap().get_key_value(x1)?;
      let res = move || {
         x0s.iter()
            .filter_map(move |x0| Some(self.0.map.get(x0).unwrap().rel().map.get(x1)?.iter().map(move |x2| (x0, x2))))
            .flatten()
      };
      Some(IteratorFromDyn::new(res))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2Ind1<'a, T0, T1> {
   type Key = (T1,);

   type Value = (&'a T0, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (x1,): &Self::Key) -> Option<Self::IteratorType> { self.get(x1) }

   fn len_estimate(&self) -> usize { self.0.reverse_map1.as_ref().unwrap().len() }
   fn is_empty(&'a self) -> bool { self.0.reverse_map1.as_ref().unwrap().is_empty() }
}

pub struct TrRel2Ind2<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2Ind2<'a, T0, T1> {
   type Key = (&'a T1,);
   type Value = (&'a T0, &'a T1);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.reverse_map2.as_ref().unwrap().keys().map(|x2| ((x2,), self.get(x2).unwrap())))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> TrRel2Ind2<'a, T0, T1> {
   fn get(&'a self, x2: &T1) -> Option<<Self as RelIndexRead<'a>>::IteratorType> {
      let (x2, x0s) = self.0.reverse_map2.as_ref().unwrap().get_key_value(x2)?;
      let res = move || {
         x0s.iter().flat_map(move |x0| {
            self.0.map.get(x0).unwrap().rel().reverse_map.get(x2).unwrap().iter().map(move |x1| (x0, x1))
         })
      };
      Some(IteratorFromDyn::new(res))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2Ind2<'a, T0, T1> {
   type Key = (T1,);
   type Value = (&'a T0, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (x2,): &Self::Key) -> Option<Self::IteratorType> { self.get(x2) }

   fn len_estimate(&self) -> usize { self.0.reverse_map2.as_ref().unwrap().len() }
   fn is_empty(&'a self) -> bool { self.0.reverse_map2.as_ref().unwrap().is_empty() }
}

pub struct TrRel2Ind1_2<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2Ind1_2<'a, T0, T1> {
   type Key = (&'a T1, &'a T1);
   type Value = (&'a T0,);

   type ValueIteratorType = Box<dyn Iterator<Item = Self::Value> + 'a>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.reverse_map1.as_ref().unwrap().iter().flat_map(move |(x1, x0s_for_x1)| {
         self.0.reverse_map2.as_ref().unwrap().iter().map(move |(x2, x0s_for_x2)| {
            let x0s: Self::ValueIteratorType = Box::new(
               x0s_for_x1
                  .intersection(x0s_for_x2)
                  .filter(|&x0| self.0.map.get(x0).unwrap().rel().contains(x1, x2))
                  .map(|x0| (x0,)),
            );
            ((x1, x2), x0s)
         })
      }))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2Ind1_2<'a, T0, T1> {
   type Key = (T1, T1);

   type Value = (&'a T0,);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;
   fn index_get(&'a self, (x1, x2): &Self::Key) -> Option<Self::IteratorType> {
      let (x1, x1_map) = self.0.reverse_map1.as_ref().unwrap().get_key_value(x1)?;
      let (x2, x2_map) = self.0.reverse_map2.as_ref().unwrap().get_key_value(x2)?;

      let res = || {
         x1_map.intersection(x2_map).filter(|&x0| self.0.map.get(x0).unwrap().rel().contains(x1, x2)).map(|x0| (x0,))
      };
      Some(IteratorFromDyn::new(res))
   }

   fn len_estimate(&self) -> usize {
      // TODO random estimate, could be very wrong
      self.0.reverse_map1.as_ref().unwrap().len() * self.0.reverse_map2.as_ref().unwrap().len()
         / ((self.0.map.len() as f32).sqrt() as usize)
   }
}

pub struct TrRel2IndNone<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2IndNone<'a, T0, T1> {
   type Key = ();
   type Value = (&'a T0, &'a T1, &'a T1);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;
   type AllIteratorType = std::option::IntoIter<(Self::Key, Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType { self.index_get(&()).map(|x| ((), x)).into_iter() }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2IndNone<'a, T0, T1> {
   type Key = ();
   type Value = (&'a T0, &'a T1, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (): &Self::Key) -> Option<Self::IteratorType> {
      let res = || self.0.map.iter().flat_map(|(x0, trrel)| trrel.rel().iter_all().map(move |(x1, x2)| (x0, x1, x2)));
      Some(IteratorFromDyn::new(res))
   }

   fn len_estimate(&self) -> usize { 1 }
}

#[repr(transparent)]
pub struct TrRel2IndFull<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a TrRel2IndCommon<T0, T1>);

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelFullIndexRead<'a> for TrRel2IndFull<'a, T0, T1> {
   type Key = (T0, T1, T1);

   #[inline]
   fn contains_key(&'a self, (x0, x1, x2): &Self::Key) -> bool {
      match self.0.map.get(x0) {
         None => false,
         Some(rel) => rel.rel().contains(x1, x2),
      }
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRel2IndFull<'a, T0, T1> {
   type Key = (&'a T0, &'a T1, &'a T1);
   type Value = ();

   type ValueIteratorType = std::iter::Once<()>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let iter = self.0.map.iter().flat_map(|(x0, trrel)| trrel.rel().iter_all().map(move |(x1, x2)| (x0, x1, x2)));

      Box::new(iter.map(|t| (t, std::iter::once(()))))
   }
}

impl<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexRead<'a> for TrRel2IndFull<'a, T0, T1> {
   type Key = (T0, T1, T1);
   type Value = ();

   type IteratorType = std::iter::Once<Self::Value>;

   fn index_get(&'a self, (x0, x1, x2): &Self::Key) -> Option<Self::IteratorType> {
      if self.0.map.get(x0)?.rel().contains(x1, x2) { Some(once(())) } else { None }
   }

   fn len_estimate(&self) -> usize {
      let sample_size = 3;
      let sum = self.0.map.values().take(sample_size).map(|trrel| trrel.rel().count_estimate()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
   fn is_empty(&'a self) -> bool { self.0.map.is_empty() }
}

pub struct TrRel2IndFullWrite<'a, T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(&'a mut TrRel2IndCommon<T0, T1>);

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexMerge for TrRel2IndFullWrite<'_, T0, T1> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {} // noop
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelFullIndexWrite for TrRel2IndFullWrite<'_, T0, T1> {
   type Key = (T0, T1, T1);

   type Value = ();

   fn insert_if_not_present(&mut self, (x0, x1, x2): &Self::Key, (): Self::Value) -> bool {
      let x0_hash = self.0.map.hasher().hash_one(x0);

      if !self
         .0
         .map
         .raw_entry_mut()
         .from_key_hashed_nocheck(x0_hash, x0)
         .or_insert_with(|| (x0.clone(), TrRelIndCommon::make_new()))
         .1
         .insert_by_ref(x1, x2)
      {
         return false;
      }
      if let Some(reverse_map1) = self.0.reverse_map1.as_mut() {
         reverse_map1.entry(x1.clone()).or_default().insert_with_hash_no_check(x0_hash, x0.clone());
         // reverse_map1.entry(x1.clone()).or_default().raw_table().find(x0_hash, |a| &a.0 == x0).unwrap().copy_from_nonoverlapping(other).insert(x0.clone());
      }
      if let Some(reverse_map2) = self.0.reverse_map2.as_mut() {
         reverse_map2.entry(x2.clone()).or_default().insert_with_hash_no_check(x0_hash, x0.clone());
      }
      true
   }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> RelIndexWrite for TrRel2IndFullWrite<'_, T0, T1> {
   type Key = (T0, T1, T1);

   type Value = ();

   fn index_insert(&mut self, (x0, x1, x2): Self::Key, (): Self::Value) {
      if let Some(reverse_map1) = self.0.reverse_map1.as_mut() {
         reverse_map1.entry(x1.clone()).or_default().insert(x0.clone());
      }
      if let Some(reverse_map2) = self.0.reverse_map2.as_mut() {
         reverse_map2.entry(x2.clone()).or_default().insert(x0.clone());
      }
      self.0.map.entry(x0).or_insert_with(|| TrRelIndCommon::make_new()).insert(x1, x2);
   }
}

use std::marker::PhantomData;

use ascent::internal::ToRelIndex;

use crate::rel_boilerplate::NoopRelIndexWrite;

macro_rules! to_trrel2 {
   ($name: ident, $key: ty, $val: ty) => {paste::paste!{
      pub struct [<To $name>]<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(PhantomData<(T0, T1)>);

      impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> Default for [<To $name>]<T0, T1> {
         fn default() -> Self { Self(PhantomData) }
      }

      impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, Rel> ToRelIndex<Rel> for [<To $name>]<T0, T1>
      where Rel: std::ops::DerefMut<Target = TrRel2IndCommon<T0, T1>>
      {
         type RelIndex<'a> = $name<'a, T0, T1> where Self: 'a, Rel: 'a;
         #[inline(always)]
         fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { $name(rel.deref()) }

         type RelIndexWrite<'a> = NoopRelIndexWrite<$key, $val> where Self: 'a, Rel: 'a;
         #[inline(always)]
         fn to_rel_index_write<'a>(&'a mut self, _rel: &'a mut Rel) -> Self::RelIndexWrite<'a> {
            NoopRelIndexWrite::default()
         }
      }
   }};
}

to_trrel2!(TrRel2IndNone, (), (T0, T1, T1));
to_trrel2!(TrRel2Ind0, (T0,), (T1, T1));
to_trrel2!(TrRel2Ind1, (T1,), (T0, T1));
to_trrel2!(TrRel2Ind2, (T1,), (T0, T1));
to_trrel2!(TrRel2Ind0_1, (T0, T1), (T1,));
to_trrel2!(TrRel2Ind0_2, (T0, T1), (T1,));
to_trrel2!(TrRel2Ind1_2, (T1, T1), (T0,));
// to_trrel2!(TrRel2IndFull, (T0, T1, T1), ());

pub struct ToTrRel2IndFull<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq>(PhantomData<(T0, T1)>);

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq> Default for ToTrRel2IndFull<T0, T1> {
   fn default() -> Self { Self(PhantomData) }
}

impl<T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, Rel> ToRelIndex<Rel> for ToTrRel2IndFull<T0, T1>
where Rel: std::ops::DerefMut<Target = TrRel2IndCommon<T0, T1>>
{
   type RelIndex<'a>
      = TrRel2IndFull<'a, T0, T1>
   where
      Self: 'a,
      Rel: 'a;
   #[inline(always)]
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { TrRel2IndFull(rel.deref()) }

   type RelIndexWrite<'a>
      = TrRel2IndFullWrite<'a, T0, T1>
   where
      Self: 'a,
      Rel: 'a;
   #[inline(always)]
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a> {
      TrRel2IndFullWrite(rel.deref_mut())
   }
}
