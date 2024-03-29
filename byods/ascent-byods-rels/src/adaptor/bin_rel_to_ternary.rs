
use std::hash::BuildHasherDefault;
use std::hash::Hash;
use std::iter::Map;
use std::iter::once;
use std::marker::PhantomData;
use ascent::internal::RelFullIndexRead;
use ascent::internal::RelFullIndexWrite;
use ascent::internal::RelIndexMerge;
use ascent::internal::RelIndexWrite;
use ascent::rel_index_read::RelIndexRead;
use ascent::rel_index_read::RelIndexReadAll;
use ascent::to_rel_index::ToRelIndex;

use hashbrown::HashMap;
use rustc_hash::FxHasher;

use crate::iterator_from_dyn::IteratorFromDyn;
use crate::utils::AltHashSet;
use crate::utils::hash_one;

use super::bin_rel::ByodsBinRel;

pub struct BinRelToTernary<T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   pub map: HashMap<T0, TBinRel, BuildHasherDefault<FxHasher>>,
   pub reverse_map1: Option<HashMap<T1, AltHashSet<T0, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>>,
   pub reverse_map2: Option<HashMap<T2, AltHashSet<T0, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>>
}

impl<T0, T1, T2, TBinRel> RelIndexMerge for BinRelToTernary<T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      panic!("merge_delta_to_total_new_to_delta must be called instead");
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      let mut new_delta_map = HashMap::default();
      for (k, mut delta_trrel) in delta.map.drain() {
         let mut new_trrel = new.map.remove(&k).unwrap_or_default();
         match total.map.entry(k.clone()) {
            hashbrown::hash_map::Entry::Occupied(mut total_entry) => {
               TBinRel::merge_delta_to_total_new_to_delta(&mut new_trrel, &mut delta_trrel, total_entry.get_mut());
               if !delta_trrel.is_empty() {
                  new_delta_map.insert(k, delta_trrel);
               }
            },
            hashbrown::hash_map::Entry::Vacant(total_vacant_entry) => {
               let mut new_total = TBinRel::default();
               TBinRel::merge_delta_to_total_new_to_delta(&mut new_trrel, &mut delta_trrel, &mut new_total);
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
               TBinRel::merge_delta_to_total_new_to_delta(&mut new_trrel, &mut new_delta, total_entry.get_mut());
               new_delta_map.insert(k, new_delta);
            },
            hashbrown::hash_map::Entry::Vacant(_) => {
               TBinRel::merge_delta_to_total_new_to_delta(&mut new_trrel, &mut new_delta, &mut Default::default());
               new_delta_map.insert(k, new_delta);
            },
         }
      }
      delta.map = new_delta_map;

      if delta.reverse_map1.is_some() {
         crate::utils::move_hash_map_of_alt_hash_set_contents(delta.reverse_map1.as_mut().unwrap(), total.reverse_map1.as_mut().unwrap());
         std::mem::swap(delta.reverse_map1.as_mut().unwrap(), new.reverse_map1.as_mut().unwrap());
      }

      if delta.reverse_map2.is_some() {
         crate::utils::move_hash_map_of_alt_hash_set_contents(delta.reverse_map2.as_mut().unwrap(), total.reverse_map2.as_mut().unwrap());
         std::mem::swap(delta.reverse_map2.as_mut().unwrap(), new.reverse_map2.as_mut().unwrap());
      }
   }
}


pub struct BinRelToTernaryInd0<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd0<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T0, );
   type Value = (&'a T1, &'a T2);

   type ValueIteratorType = Box<dyn Iterator<Item = Self::Value> + 'a>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.map.iter().map(|(k, v)| {
         ((k, ), Box::new(v.iter_all()) as _)
      }))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd0<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T0, );
   type Value = (&'a T1, &'a T2);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let trrel = self.0.map.get(&key.0)?;
      Some(IteratorFromDyn::new(|| trrel.iter_all()))
   }

   fn len(&self) -> usize {
      let sample_size = 4;
      let sum = self.0.map.values().map(|x| x.len_estimate()).sum::<usize>();
      sum * self.0.map.len() / sample_size.min(self.0.map.len()).max(1)
   }
}


pub struct BinRelToTernaryInd0_1<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd0_1<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T0, &'a T1);

   type Value = (&'a T2, );

   type ValueIteratorType = Map<TBinRel::Ind0AllIterValsIter<'a>, fn(&T2) -> (&T2,)>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.map.iter().flat_map(|(x0, v)| v.ind0_iter_all().map(move |(x1, x2s)| {
         let iter: Self::ValueIteratorType = x2s.map(|x2| (x2, ));
         ((x0, x1), iter) 
      })))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd0_1<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T0, T1);

   type Value = (&'a T2, );

   type IteratorType = Map<TBinRel::Ind0ValsIter<'a>, fn(&T2) -> (&T2,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let trrel = self.0.map.get(&key.0)?;
      let res: Self::IteratorType = trrel.ind0_index_get(&key.1)?.map(|x| (x, ));
      Some(res)
   }

   fn len(&self) -> usize {
      let sample_size = 3;
      let sum = self.0.map.values().take(sample_size).map(|trrel| trrel.ind0_len_estimate()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)

   }
}


pub struct BinRelToTernaryInd0_2<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd0_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T0, &'a T2);

   type Value = (&'a T1, );

   type ValueIteratorType = Map<TBinRel::Ind1AllIterValsIter<'a>, fn(&T1) -> (&T1,)>;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.map.iter().flat_map(|(x0, v)| v.ind1_iter_all().map(move |(x2, x1s)| {
         let iter: Self::ValueIteratorType = x1s.map(|x2| (x2, ));
         ((x0, x2), iter) 
      })))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd0_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T0, T2);

   type Value = (&'a T1, );

   type IteratorType = Map<TBinRel::Ind1ValsIter<'a>, fn(&T1) -> (&T1,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let trrel = self.0.map.get(&key.0)?;
      let res: Self::IteratorType = trrel.ind1_index_get(&key.1)?.map(|x| (x, ));
      Some(res)
   }

   fn len(&self) -> usize {
      let sample_size = 3;
      let sum = self.0.map.values().take(sample_size).map(|trrel| trrel.ind1_len_estimate()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
}


pub struct BinRelToTernaryInd1<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd1<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T1, );
   type Value = (&'a T0, &'a T2);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.reverse_map1.as_ref().unwrap().keys().map(|x1| {
         ((x1, ), self.get(x1).unwrap())
      }))
   }
}

impl<'a, T0, T1, T2, TBinRel> BinRelToTernaryInd1<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn get(&'a self, x1: &T1) -> Option<<Self as RelIndexRead<'a>>::IteratorType> {
      let (x1, x0s) = self.0.reverse_map1.as_ref().unwrap().get_key_value(x1)?;
      let res = move || x0s.iter().filter_map(move |x0| {
         Some(self.0.map.get(x0).unwrap().ind0_index_get(x1)?.map(move |x2| (x0, x2)))
      }).flatten();
      Some(IteratorFromDyn::new(res))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd1<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T1, );

   type Value = (&'a T0, &'a T2);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (x1, ): &Self::Key) -> Option<Self::IteratorType> {
      self.get(x1)
   }

   fn len(&self) -> usize {
      self.0.reverse_map1.as_ref().unwrap().len()
   }
}


pub struct BinRelToTernaryInd2<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd2<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T2, );
   type Value = (&'a T0, &'a T1);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.reverse_map2.as_ref().unwrap().keys().map(|x2| {
         ((x2, ), self.get(x2).unwrap())
      }))
   }
}

impl<'a, T0, T1, T2, TBinRel> BinRelToTernaryInd2<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn get(&'a self, x2: &T2) -> Option<<Self as RelIndexRead<'a>>::IteratorType> {
      let (x2, x0s) = self.0.reverse_map2.as_ref().unwrap().get_key_value(x2)?;
      let res = move || x0s.iter().filter_map(move |x0| {
         Some(self.0.map.get(x0).unwrap().ind1_index_get(x2)?.map(move |x1| (x0, x1)))
      }).flatten();
      Some(IteratorFromDyn::new(res))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd2<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T2, );
   type Value = (&'a T0, &'a T1);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (x2, ): &Self::Key) -> Option<Self::IteratorType> {
      self.get(x2)
   }

   fn len(&self) -> usize {
      self.0.reverse_map2.as_ref().unwrap().len()
   }
}


pub struct BinRelToTernaryInd1_2<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;


impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd1_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T1, &'a T2);
   type Value = (&'a T0, );

   type ValueIteratorType = Box<dyn Iterator<Item = Self::Value> + 'a>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      Box::new(self.0.reverse_map1.as_ref().unwrap().iter().flat_map(move |(x1, x0s_for_x1)| {
         self.0.reverse_map2.as_ref().unwrap().iter().map(move |(x2, x0s_for_x2)| {
            let x0s: Self::ValueIteratorType = Box::new(x0s_for_x1.intersection(x0s_for_x2)
               .filter(|&x0| self.0.map.get(x0).unwrap().contains(x1, x2))
               .map(|x0| (x0, )));
            ((x1, x2), x0s)
         })
      }))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd1_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T1, T2);

   type Value = (&'a T0, );

   type IteratorType = IteratorFromDyn<'a, Self::Value>;
   fn index_get(&'a self, (x1, x2): &Self::Key) -> Option<Self::IteratorType> {
      let (x1, x1_map) = self.0.reverse_map1.as_ref().unwrap().get_key_value(x1)?;
      let (x2, x2_map) = self.0.reverse_map2.as_ref().unwrap().get_key_value(x2)?;

      let res = || x1_map.intersection(x2_map)
         .filter(|&x0| self.0.map.get(x0).unwrap().contains(x1, x2))
         .map(|x0| (x0, ));
      Some(IteratorFromDyn::new(res))
   }

   fn len(&self) -> usize {
      // TODO random estimate, could be very wrong
      self.0.reverse_map1.as_ref().unwrap().len() * self.0.reverse_map2.as_ref().unwrap().len()
         / ((self.0.map.len() as f32).sqrt() as usize)
   }
}


pub struct BinRelToTernaryIndNone<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryIndNone<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = ();
   type Value = (&'a T0, &'a T1, &'a T2);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;
   type AllIteratorType = std::option::IntoIter<(Self::Key, Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      self.index_get(&()).map(|x| ((), x)).into_iter()
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryIndNone<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = ();
   type Value = (&'a T0, &'a T1, &'a T2);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (): &Self::Key) -> Option<Self::IteratorType> {
      let res = || self.0.map.iter().flat_map(|(x0, rel)| {
         rel.iter_all().map(move |(x1, x2)| (x0, x1, x2))
      });
      Some(IteratorFromDyn::new(res))
   }

   fn len(&self) -> usize {
      1
   }
}

pub struct BinRelToTernaryInd0_1_2<'a, T0, T1, T2, TBinRel>(&'a BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelFullIndexRead<'a> for BinRelToTernaryInd0_1_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T0, T1, T2);

   #[inline]
   fn contains_key(&'a self, (x0, x1, x2): &Self::Key) -> bool {
      match self.0.map.get(x0) {
         None => false,
         Some(rel) => rel.contains(x1, x2),
      }
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexReadAll<'a> for BinRelToTernaryInd0_1_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (&'a T0, &'a T1, &'a T2);
   type Value = ();

   type ValueIteratorType = std::iter::Once<()>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let iter = self.0.map.iter().flat_map(|(x0, rel)| {
         rel.iter_all().map(move |(x1, x2)| (x0, x1, x2))
      });

      Box::new(iter.map(|t| (t, once(()))))
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexRead<'a> for BinRelToTernaryInd0_1_2<'a, T0, T1, T2, TBinRel> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T0, T1, T2);
   type Value = ();

   type IteratorType = std::iter::Once<Self::Value>;

   fn index_get(&'a self, (x0, x1, x2): &Self::Key) -> Option<Self::IteratorType> {
      if self.0.map.get(x0)?.contains(x1, x2) {
         Some(once(()))
      } else {
         None
      }
   }

   fn len(&self) -> usize {
      let sample_size = 3;
      let sum = self.0.map.values().take(sample_size).map(|rel| rel.len_estimate()).sum::<usize>();
      let map_len = self.0.map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
}


pub struct BinRelToTernaryInd0_1_2Write<'a, T0, T1, T2, TBinRel>(&'a mut BinRelToTernary<T0, T1, T2, TBinRel>) 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2, TBinRel> RelIndexMerge for BinRelToTernaryInd0_1_2Write<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { } // noop
}

impl<'a, T0, T1, T2, TBinRel> RelFullIndexWrite for BinRelToTernaryInd0_1_2Write<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   type Key = (T0, T1, T2);
   type Value = ();

   fn insert_if_not_present(&mut self, (x0, x1, x2): &Self::Key, (): Self::Value) -> bool {
      let x0_hash = hash_one(self.0.map.hasher(), x0);

      if !self.0.map.raw_entry_mut().from_key_hashed_nocheck(x0_hash, x0)
         .or_insert_with(|| (x0.clone(), TBinRel::default()))
         .1.insert(x1.clone(), x2.clone()) 
      {
         return false;
      }
      if let Some(reverse_map1) = self.0.reverse_map1.as_mut() {
         reverse_map1.entry(x1.clone()).or_default().insert_with_hash_no_check(x0_hash, x0.clone());
      }
      if let Some(reverse_map2) = self.0.reverse_map2.as_mut() {
         reverse_map2.entry(x2.clone()).or_default().insert_with_hash_no_check(x0_hash, x0.clone());
      }
      true
   }
}

impl<'a, T0, T1, T2, TBinRel> RelIndexWrite for BinRelToTernaryInd0_1_2Write<'a, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{   
   type Key = (T0, T1, T2);
   type Value = ();

   fn index_insert(&mut self, (x0, x1, x2): Self::Key, (): Self::Value) {
      if let Some(reverse_map1) = self.0.reverse_map1.as_mut() {
         reverse_map1.entry(x1.clone()).or_default().insert(x0.clone());
      }
      if let Some(reverse_map2) = self.0.reverse_map2.as_mut() {
         reverse_map2.entry(x2.clone()).or_default().insert(x0.clone());
      }
      self.0.map.entry(x0).or_insert_with(|| TBinRel::default()).insert(x1, x2);
   }
}



pub struct BinRelToTernaryWrapper<const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool, T0, T1, T2, TBinRel>
(pub BinRelToTernary<T0, T1, T2, TBinRel>)
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool, T0, T1, T2, TBinRel> RelIndexMerge for BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn move_index_contents(from: &mut Self, to: &mut Self) {
      RelIndexMerge::move_index_contents(&mut from.0, &mut to.0)
   }
   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      RelIndexMerge::merge_delta_to_total_new_to_delta(&mut new.0, &mut delta.0, &mut total.0)
   }
}

impl<const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool, T0, T1, T2, TBinRel> Default for BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn default() -> Self {
      let reverse_map1 = if HAS_REVERSE_MAP1 {Some(Default::default())} else {None};
      let reverse_map2 = if HAS_REVERSE_MAP2 {Some(Default::default())} else {None};
      Self(BinRelToTernary { map: Default::default(), reverse_map1, reverse_map2 })
   }
}


pub struct ToBinRelToTernaryInd0_1_2<T0, T1, T2>(PhantomData<(T0, T1, T2)>)
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq//, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
;

impl<'a, T0, T1, T2> Default for ToBinRelToTernaryInd0_1_2<T0, T1, T2> 
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq//, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
{
   fn default() -> Self { Self(PhantomData) }
}

impl<T0, T1, T2, TBinRel, const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool> 
ToRelIndex<BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>> for ToBinRelToTernaryInd0_1_2<T0, T1, T2>
where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>,
      // TWrapper: DerefMut<Target = BinRelToTernary<T0, T1, T2, TBinRel>>
{
   type RelIndex<'a>  = BinRelToTernaryInd0_1_2<'a, T0, T1, T2, TBinRel> where Self:'a, BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>: 'a;
   #[inline(always)]
   fn to_rel_index<'a>(&'a self, rel: &'a BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>) -> Self::RelIndex<'a> { BinRelToTernaryInd0_1_2(&rel.0) }
   
   type RelIndexWrite<'a>  = BinRelToTernaryInd0_1_2Write<'a, T0, T1, T2, TBinRel> where Self: 'a, BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>: 'a;
   #[inline(always)]
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>) -> Self::RelIndexWrite<'a> {
      BinRelToTernaryInd0_1_2Write(&mut rel.0)
   }
}


use crate::rel_boilerplate::NoopRelIndexWrite;
macro_rules! to_trrel2 {
   ($name: ident, $key: ty, $val: ty) => {paste::paste!{
      pub struct [<To $name>]<T0, T1, T2>(PhantomData<(T0, T1, T2)>)
      where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq//, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
      ;


      impl<'a, T0, T1, T2> Default for [<To $name>]<T0, T1, T2> 
      where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq//, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>
      {
         fn default() -> Self { Self(PhantomData) }
      }

      impl<T0, T1, T2, TBinRel, const HAS_REVERSE_MAP1: bool, const HAS_REVERSE_MAP2: bool> 
      ToRelIndex<BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>> for [<To $name>]<T0, T1, T2>
      where T0: Clone + Hash + Eq, T1: Clone + Hash + Eq, T2: Clone + Hash + Eq, TBinRel: ByodsBinRel<T0 = T1, T1 = T2>,
            //TWrapper: Deref<Target = BinRelToTernary<T0, T1, T2, TBinRel>>
      {

         type RelIndex<'a>  = $name<'a, T0, T1, T2, TBinRel> where Self:'a, BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>: 'a;
         #[inline(always)]
         fn to_rel_index<'a>(&'a self, rel: &'a BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>) -> Self::RelIndex<'a> { $name(& rel.0) }

         type RelIndexWrite<'a> = NoopRelIndexWrite<$key, $val> where Self: 'a, BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>: 'a;
         #[inline(always)]
         fn to_rel_index_write<'a>(&'a mut self, _rel: &'a mut BinRelToTernaryWrapper<HAS_REVERSE_MAP1, HAS_REVERSE_MAP2, T0, T1, T2, TBinRel>) -> Self::RelIndexWrite<'a> { 
            NoopRelIndexWrite::default() 
         }
      }
   }};
}

to_trrel2!(BinRelToTernaryIndNone, (), (T0, T1, T2));
to_trrel2!(BinRelToTernaryInd0, (T0, ), (T1, T2));
to_trrel2!(BinRelToTernaryInd1, (T1, ), (T0, T2));
to_trrel2!(BinRelToTernaryInd2, (T2, ), (T0, T1));
to_trrel2!(BinRelToTernaryInd0_1, (T0, T1), (T2, ));
to_trrel2!(BinRelToTernaryInd0_2, (T0, T2), (T1, ));
to_trrel2!(BinRelToTernaryInd1_2, (T1, T2), (T0, ));
