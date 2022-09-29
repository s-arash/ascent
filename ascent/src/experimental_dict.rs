use std::collections::{HashMap, HashSet};
use std::hash::{Hash, BuildHasherDefault};
use std::iter::{FlatMap, Map, Once, Zip, Repeat};
use nohash_hasher::BuildNoHashHasher;
use rustc_hash::FxHasher;

use crate::rel_index_read::RelIndexCombined;


#[derive(Clone, Debug)]
pub struct Dict<K, V>(pub HashMap<K, V, BuildHasherDefault<FxHasher>>);
impl<K, V> Default for Dict<K, V> {
   #[inline(always)]
   fn default() -> Self { Self(Default::default()) }
}

#[derive(Clone, Debug)]
pub struct MultiDict<K, V>(pub HashMap<K, Vec<V>, BuildHasherDefault<FxHasher>>);
impl<K, V> Default for MultiDict<K, V> {
   #[inline(always)]
   fn default() -> Self { Self(Default::default()) }
}

#[derive(Clone, Debug)]
pub struct MultiDictDedup<K, V>(pub HashMap<K, HashSet<V, BuildNoHashHasher<usize>>, BuildHasherDefault<FxHasher>>);
impl<K, V> Default for MultiDictDedup<K, V> {
   #[inline(always)]
   fn default() -> Self { Self(Default::default()) }
}


pub trait DictRead<'a> {
   type K: 'a;
   type V: 'a;
   fn get(&'a self, key: Self::K) -> Option<&'a Self::V>;

   fn contains_key(&'a self, key: Self::K) -> bool { self.get(key).is_some() }

   type TIter: Iterator<Item = (Self::K, &'a Self::V)> + Clone + 'a;
   fn iter(&'a self) -> Self::TIter;

   fn len(&'a self) -> usize;
}

pub trait DictDrain<'a> {
   type K;
   type V;

   type TDrain: Iterator<Item = (Self::K, Self::V)> + 'a;
   fn drain(&'a mut self) -> Self::TDrain;
}

pub trait MultiDictRead<'a> {
   type K: 'a;
   type V: 'a;

   type TValsIter: Iterator<Item = &'a Self::V> + Clone + 'a;
   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter>;
   fn contains_key(&'a self, key: Self::K) -> bool { self.get(key).is_some() }

   type TValsIterForTIter: Iterator<Item = &'a Self::V> + Clone + 'a;
   type TIter: Iterator<Item = (Self::K, Self::TValsIterForTIter)> + Clone + 'a;
   fn iter(&'a self) -> Self::TIter;

   fn len(&'a self) -> usize;
}

impl<'a, K: Hash + Eq +'a, V: 'a> DictRead<'a> for Dict<K, V> {
   type K = &'a K;
   type V = V;

   #[inline(always)]
   fn get(&'a self, key: Self::K) -> Option<&'a Self::V> {
      HashMap::get(&self.0, key)
   }

   type TIter = std::collections::hash_map::Iter<'a, K, V>;
   #[inline(always)]
   fn iter(&'a self) -> Self::TIter {
      let res: std::collections::hash_map::Iter<K, V> = self.0.iter();
      res
   }

   #[inline(always)]
   fn len(&self) -> usize { self.0.len() }
}

impl<'a, K: 'a, V: 'a> DictDrain<'a> for Dict<K, V> {
   type K = K;
   type V = V;

   type TDrain = std::collections::hash_map::Drain<'a, K, V>;

   #[inline(always)]
   fn drain(&'a mut self) -> Self::TDrain {
      let res: std::collections::hash_map::Drain<K, V> = self.0.drain();
      res
   }
}

impl<'a, K, V> DictDrain<'a> for &'a mut Dict<K, V> {
   type K = K;
   type V = V;

   type TDrain = std::collections::hash_map::Drain<'a, K, V>;

   #[inline(always)]
   fn drain(&'a mut self) -> Self::TDrain {
      let res: std::collections::hash_map::Drain<K, V> = self.0.drain();
      res
   }
}


pub struct DictOfDictUncurried<TDict>(pub TDict);

impl<'a, TDict, TDict2> DictRead<'a> for DictOfDictUncurried<TDict>
where TDict: DictRead<'a, V = TDict2> + 'a, TDict2: DictRead<'a> + 'a, TDict::K: Clone, TDict2::K: Clone {
   type K = (TDict::K, TDict2::K);
   type V = TDict2::V;

   #[inline]
   fn get(&'a self, key: Self::K) -> Option<&'a Self::V> {
      self.0.get(key.0)?.get(key.1)
   }

   type TIter = FlatMap<TDict::TIter, Map<Zip<TDict2::TIter, Repeat<TDict::K>>, for<'aa> fn(((TDict2::K, &'aa TDict2::V), TDict::K)) -> ((TDict::K, TDict2::K), &'aa TDict2::V)>, fn((TDict::K, &'a TDict2)) -> Map<Zip<TDict2::TIter, Repeat<TDict::K>>, fn(((TDict2::K, &TDict2::V), TDict::K)) -> ((TDict::K, TDict2::K), &TDict2::V)>>;

   fn iter(&'a self) -> Self::TIter {
      let res: FlatMap<TDict::TIter, Map<Zip<TDict2::TIter, Repeat<TDict::K>>, for<'aa> fn(((TDict2::K, &'aa TDict2::V), TDict::K)) -> ((TDict::K, TDict2::K), &'aa TDict2::V)>, fn((TDict::K, &'a TDict2)) -> Map<Zip<TDict2::TIter, Repeat<TDict::K>>, fn(((TDict2::K, &TDict2::V), TDict::K)) -> ((TDict::K, TDict2::K), &TDict2::V)>>
         = self.0.iter().flat_map(|(k1, v1): (TDict::K, &TDict::V)| v1.iter().zip(std::iter::repeat(k1))
            .map(|(kv2, k1): ((TDict2::K, &TDict2::V), TDict::K)| ((k1, kv2.0), kv2.1))
         );
      res
   }

   // not accurate
   fn len(&'a self) -> usize { 
      let (sub_count, sub_len_sum) = self.0.iter().take(3).map(|(_k, v)| v.len()).fold((0, 0), |(c, s), len| (c + 1, s + len));
      if sub_count != 0 {
         self.0.len() * sub_len_sum / sub_count
      } else { 0 }
   }

}

pub struct DictOfMultiDictUncurried<TDict>(pub TDict);

impl<'a, TDict, TDict2> MultiDictRead<'a> for DictOfMultiDictUncurried<TDict>
where TDict: DictRead<'a, V = TDict2> + 'a, TDict2: MultiDictRead<'a> + 'a, TDict::K: Clone 
{
   type K = (TDict::K, TDict2::K);
   type V = TDict2::V;

   type TValsIter = TDict2::TValsIter;

   #[inline]
   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      self.0.get(key.0)?.get(key.1)
   }

   type TValsIterForTIter = TDict2::TValsIterForTIter;
   type TIter = FlatMap<TDict::TIter, Map<Zip<TDict2::TIter, Repeat<TDict::K>>, fn(((TDict2::K, TDict2::TValsIterForTIter), TDict::K)) -> ((TDict::K, TDict2::K), TDict2::TValsIterForTIter)>, fn((TDict::K, &'a TDict2)) -> Map<Zip<TDict2::TIter, Repeat<TDict::K>>, fn(((TDict2::K, TDict2::TValsIterForTIter), TDict::K)) -> ((TDict::K, TDict2::K), TDict2::TValsIterForTIter)>>;

   fn iter(&'a self) -> Self::TIter {
      let res: FlatMap<TDict::TIter, Map<Zip<TDict2::TIter, Repeat<TDict::K>>, fn(((TDict2::K, TDict2::TValsIterForTIter), TDict::K)) -> ((TDict::K, TDict2::K), TDict2::TValsIterForTIter)>, fn((TDict::K, &'a TDict2)) -> Map<Zip<TDict2::TIter, Repeat<TDict::K>>, fn(((TDict2::K, TDict2::TValsIterForTIter), TDict::K)) -> ((TDict::K, TDict2::K), TDict2::TValsIterForTIter)>>
         = self.0.iter().flat_map(|(k1, v1)| v1.iter().zip(std::iter::repeat(k1)).map(|(kv2, k1)| ((k1, kv2.0), kv2.1)));
      res
   }

   // not accurate
   // fn len(&'a self) -> usize { self.0.len() }
   fn len(&'a self) -> usize { 
      let sub_len = self.0.iter().next().map(|(_k, v)| v.len());
      if let Some(sub_len) = sub_len {
         self.0.len() * sub_len
      } else { 0 }
   }
}

#[derive(Clone, Default)]
pub struct DictAsMultiDict<TDict>(pub TDict);

impl <'a, TDict> MultiDictRead<'a> for DictAsMultiDict<TDict>
where TDict: DictRead<'a>
{
   type K = TDict::K;
   type V = TDict::V;

   #[inline(always)]
   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      self.0.get(key).map(|v| std::iter::once(v))
   }

   type TValsIterForTIter = Self::TValsIter;
   type TIter = Map<TDict::TIter, for<'aa> fn((TDict::K, &'aa TDict::V)) -> (TDict::K, Once<&'aa TDict::V>)>;

   type TValsIter = std::iter::Once<&'a Self::V>;
   
   #[inline(always)]
   fn iter(&'a self) -> Self::TIter {
      let res: Self::TIter = self.0.iter().map(|(k, v)| (k, std::iter::once(v)));
      res
   }

   #[inline(always)]
   fn len(&'a self) -> usize { self.0.len() }
}

#[derive(Clone)]
pub struct DictOfDictAsMultiDict<TDict>(pub TDict);

impl <'a, TDict, TDict2> MultiDictRead<'a> for DictOfDictAsMultiDict<TDict>
where TDict: DictRead<'a, V = TDict2> + 'a, TDict2: DictRead<'a> + 'a, TDict::K: Clone {
   type K = TDict::K;
   type V = TDict2::V;

   #[inline]
   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      let dict2 = self.0.get(key)?;
      let res: Map<TDict2::TIter, fn((TDict2::K, &TDict2::V)) -> &TDict2::V> 
         = dict2.iter().map(|(_k2, v2)| v2);
      Some(res)
   }

   type TValsIterForTIter = Self::TValsIter;
   type TIter = Map<TDict::TIter, fn((TDict::K, &'a TDict2)) -> (TDict::K, Map<TDict2::TIter, for<'bb> fn((TDict2::K, &'bb TDict2::V)) -> &'bb TDict2::V>)>;

   type TValsIter = Map<TDict2::TIter, fn((TDict2::K, &TDict2::V)) -> &TDict2::V>;

   #[inline]
   fn iter(&'a self) -> Self::TIter {
      let res: Self::TIter = self.0.iter().map(|(k, v)| (k, v.iter().map(|(_k2, v2)| v2)));
      res
   }

   #[inline(always)]
   fn len(&'a self) -> usize { self.0.len() }
}

#[derive(Clone, Default)]
pub struct DictOfMultiDictAsMultiDict<TDict>(pub TDict);

impl <'a, TDict, TMultiDict2> MultiDictRead<'a> for DictOfMultiDictAsMultiDict<TDict>
where TDict: DictRead<'a, V = TMultiDict2> + 'a, TMultiDict2: MultiDictRead<'a> + 'a, TDict::K: Clone //, TDict::TIter: Clone 
{
   type K = TDict::K;
   type V = TMultiDict2::V;

   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      let dict2 = self.0.get(key)?;
      let res: FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter> 
         = dict2.iter().flat_map(|(_k2, v2)| v2);
      Some(res)
   }

   type TIter = Map<TDict::TIter, fn((TDict::K, &'a TMultiDict2)) -> (TDict::K, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>)>;
   type TValsIterForTIter = Self::TValsIter;
   type TValsIter = FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>;

   fn iter(&'a self) -> Self::TIter {
      let res: Self::TIter = self.0.iter().map(|(k, v)| (k, v.iter().flat_map(|(_k2,v2)| v2)));
      res
   }

   #[inline]
   fn len(&'a self) -> usize { self.0.len() }
}

#[derive(Clone, Default)]
pub struct MultiDictOfMultiDictAsMultiDict<TDict>(pub TDict);

impl <'a, TMultiDict, TMultiDict2> MultiDictRead<'a> for MultiDictOfMultiDictAsMultiDict<TMultiDict>
where TMultiDict: MultiDictRead<'a, V = TMultiDict2> + 'a, TMultiDict2: MultiDictRead<'a> + 'a, TMultiDict::K: Clone//, TMultiDict::TIter: Clone 
{
   type K = TMultiDict::K;
   type V = TMultiDict2::V;

   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      let dict2s = self.0.get(key)?;

      let res: FlatMap<TMultiDict::TValsIter, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>, fn(&'a TMultiDict2) -> FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>> 
         = dict2s.flat_map(|dict2: &TMultiDict2| dict2.iter().flat_map(|(_k, v): (TMultiDict2::K, TMultiDict2::TValsIterForTIter)| v));
      Some(res)
   }

   type TIter = std::iter::Map<TMultiDict::TIter, 
      fn((TMultiDict::K, TMultiDict::TValsIterForTIter)) -> (TMultiDict::K, FlatMap<TMultiDict::TValsIterForTIter, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>,
      fn(&'a TMultiDict2) -> FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>>) 
   >;
   type TValsIterForTIter = FlatMap<TMultiDict::TValsIterForTIter, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>,
      fn(&'a TMultiDict2) -> FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>>;
   type TValsIter = FlatMap<TMultiDict::TValsIter, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>, 
      fn(&'a TMultiDict2) -> FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>>;

   fn iter(&'a self) -> Self::TIter {
      let _map_dict2s = |dict2s: TMultiDict::TValsIterForTIter| {
         let res : FlatMap<TMultiDict::TValsIterForTIter, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter> ,
            fn(&'a TMultiDict2) -> FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>> 
            = dict2s.flat_map(|dict2| dict2.iter().flat_map(|(_k, v)| v));
         res
      };
      let res : std::iter::Map<TMultiDict::TIter, 
            fn((TMultiDict::K, TMultiDict::TValsIterForTIter)) -> (TMultiDict::K, FlatMap<TMultiDict::TValsIterForTIter, FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter> ,
            fn(&'a TMultiDict2) -> FlatMap<TMultiDict2::TIter, TMultiDict2::TValsIterForTIter, fn((TMultiDict2::K, TMultiDict2::TValsIterForTIter)) -> TMultiDict2::TValsIterForTIter>> ) > 
         = self.0.iter().map(|(k, dict2s)| (k, dict2s.flat_map(|dict2| dict2.iter().flat_map(|(_k, v)| v))));
      res
   }

   #[inline]
   fn len(&'a self) -> usize { self.0.len() }
}

#[derive(Clone, Default)]
pub struct MultiDictOfDictAsMultiDict<TDict>(pub TDict);

impl <'a, TMultiDict, TDict2> MultiDictRead<'a> for MultiDictOfDictAsMultiDict<TMultiDict>
where TMultiDict: MultiDictRead<'a, V = TDict2> + 'a, TDict2: DictRead<'a> + 'a, TMultiDict::K: Clone, TDict2::TIter: Clone 
{
   type K = TMultiDict::K;
   type V = TDict2::V;

   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      let dict2s = self.0.get(key)?;
      let res: FlatMap<TMultiDict::TValsIter, Map<TDict2::TIter, for<'aa> fn((TDict2::K, &'aa TDict2::V)) -> &'aa TDict2::V>, fn(&'a TDict2) -> Map<TDict2::TIter, for<'aaa> fn((TDict2::K, &'aaa TDict2::V)) -> &'aaa TDict2::V>>
         = dict2s.flat_map(|dict2| dict2.iter().map(|(_k, v)| v));
      
      Some(res)
   }

   type TIter = Map<TMultiDict::TIter, fn((TMultiDict::K, TMultiDict::TValsIterForTIter)) -> (TMultiDict::K, FlatMap<TMultiDict::TValsIterForTIter, Map<TDict2::TIter, for <'aa> fn((TDict2::K, &'aa TDict2::V)) -> &'aa TDict2::V>, for<'aa> fn(&'a TDict2) -> Map<TDict2::TIter, for<'aaa> fn((TDict2::K, &'aaa TDict2::V)) -> &'aaa TDict2::V>>)>;
   type TValsIterForTIter = FlatMap<TMultiDict::TValsIterForTIter, Map<TDict2::TIter, for<'aa> fn((TDict2::K, &'aa TDict2::V)) -> &'aa TDict2::V>, fn(&'a TDict2) -> Map<TDict2::TIter, for<'aaa> fn((TDict2::K, &'aaa TDict2::V)) -> &'aaa TDict2::V>>;
   type TValsIter =         FlatMap<TMultiDict::TValsIter        , Map<TDict2::TIter, for<'aa> fn((TDict2::K, &'aa TDict2::V)) -> &'aa TDict2::V>, fn(&'a TDict2) -> Map<TDict2::TIter, for<'aaa> fn((TDict2::K, &'aaa TDict2::V)) -> &'aaa TDict2::V>>;

   fn iter(&'a self) -> Self::TIter {
      let res : Map<TMultiDict::TIter, fn((TMultiDict::K, TMultiDict::TValsIterForTIter)) -> (TMultiDict::K, FlatMap<TMultiDict::TValsIterForTIter, Map<TDict2::TIter, for <'aa> fn((TDict2::K, &'aa TDict2::V)) -> &'aa TDict2::V>, for<'aa> fn(&'a TDict2) -> Map<TDict2::TIter, for<'aaa> fn((TDict2::K, &'aaa TDict2::V)) -> &'aaa TDict2::V>>)>
         = self.0.iter().map(|(k, dict2s)| (k, dict2s.flat_map(|dict2| dict2.iter().map(|(_k, v)| v))));
      res
   }

   #[inline]
   fn len(&'a self) -> usize { self.0.len() }
}

#[test]
fn test_uncurry() {
   let dict: HashMap<u32, Dict<u64, usize>, BuildHasherDefault<FxHasher>> = vec![
      (1, Dict(vec![(4, 5), (6, 7)].into_iter().collect())),
      (10, Dict(vec![(40, 50), (60, 70)].into_iter().collect())),
   ].into_iter().collect();
   let dict = Dict(dict);
   let uncurried = DictOfDictUncurried(&dict);
   assert_eq!(uncurried.get((&1, &4)), Some(&5));

   let all = uncurried.iter().collect::<Vec<_>>();
   let all_expected = vec![((&1, &4), &5), ((&1, &6), &7), ((&10, &40), &50), ((&10, &60), &70)];
   assert_eq!(all, all_expected);
   println!("{:?}", all);
}

pub trait DictWrite<'a> {
   type K;
   type V;
   fn set(&'a mut self, key: Self::K, value: Self::V);

   #[inline(always)]
   fn insert(&'a mut self, key: Self::K, value: Self::V) { self.set(key, value); }
   fn get_mut_or_insert(&'a mut self, key: Self::K, default: impl FnOnce() -> Self::V) -> &'a mut Self::V;

   #[inline]
   fn insert_if_not_present(&'a mut self, key: Self::K, value: Self::V) -> bool {
      let mut inserted = false;
      self.get_mut_or_insert(key, || { inserted = true; value });
      inserted
   }
}

impl<'a, K: Hash + Eq, V> DictWrite<'a> for Dict<K, V> {
   type K = K;
   type V = V;

   #[inline(always)]
   fn set(&mut self, key: Self::K, value: Self::V) {
      self.0.insert(key, value);
   }

   #[inline]
   fn get_mut_or_insert(&'a mut self, key: Self::K, default: impl FnOnce() -> Self::V) -> &mut Self::V {
      self.0.entry(key).or_insert_with(default)
   }

   fn insert_if_not_present(&'a mut self, key: Self::K, value: Self::V) -> bool {
      match self.0.entry(key) {
         std::collections::hash_map::Entry::Occupied(_) => false,
         std::collections::hash_map::Entry::Vacant(vac) => {
            vac.insert(value);
            true
         },
      }
   }
}

pub struct DictOfDictWriteUncurried<TDict>(pub TDict);

impl<'a, K, TDict, TDict2> DictWrite<'a> for DictOfDictWriteUncurried<TDict> 
where TDict: DictWrite<'a, K = K, V = TDict2>,
      TDict2: DictWrite<'a> + Default + 'a,
{
   type K = (K, TDict2::K);
   type V = TDict2::V;

   #[inline]
   fn set(&'a mut self, key: Self::K, value: Self::V) {
      self.0.get_mut_or_insert(key.0, TDict2::default).set(key.1, value);
   }

   #[inline]
   fn get_mut_or_insert(&'a mut self, key: Self::K, default: impl FnOnce() -> Self::V) -> &mut Self::V {
      let t = self.0.get_mut_or_insert(key.0, TDict2::default);
      t.get_mut_or_insert(key.1, default)
   }

   #[inline]
   fn insert_if_not_present(&'a mut self, key: Self::K, value: Self::V) -> bool {
      let t = self.0.get_mut_or_insert(key.0, TDict2::default);
      t.insert_if_not_present(key.1, value)
   }
}

pub trait MultiDictWrite<'a> {
   type K;
   type V;
   fn insert(&'a mut self, key: Self::K, value: Self::V);
}

impl<'a, K: Clone + Eq + Hash + 'a, V: 'a> MultiDictRead<'a> for MultiDict<K, V> {
   type K = &'a K;
   type V = V;

   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      self.0.get(key).map(|x| x.into_iter())
   }

   type TIter = Map<std::collections::hash_map::Iter<'a, K, Vec<V>>, for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, std::slice::Iter<'bb, V>)>;

   type TValsIter = std::slice::Iter<'a, V>;
   type TValsIterForTIter = Self::TValsIter;

   fn iter(&'a self) -> Self::TIter {
      let res: Map<std::collections::hash_map::Iter<K, Vec<V>>, for<'aa, 'bb> fn((&'aa K, &'bb Vec<V>)) -> (&'aa K, std::slice::Iter<'bb, V>)> 
         = self.0.iter().map(|(k, v)| (k, v.iter()));
      res
   }

   fn len(&self) -> usize { self.0.len() }
}

impl<'a, K: Clone + Eq + Hash, V> MultiDictWrite<'a> for MultiDict<K, V> {
   type K = K;
   type V = V;

   #[inline(always)]
   fn insert(&'a mut self, key: Self::K, value: Self::V) {
      self.0.entry(key).or_default().push(value);
   }
}

impl<'a, K: Clone + Eq + Hash + 'a, V: 'a> MultiDictRead<'a> for MultiDictDedup<K, V> {
   type K = &'a K;
   type V = V;

   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      self.0.get(key).map(|x| x.into_iter())
   }

   type TIter = Map<std::collections::hash_map::Iter<'a, K, HashSet<V, BuildNoHashHasher<usize>>>, for<'aa, 'bb> fn((&'aa K, &'bb HashSet<V, BuildNoHashHasher<usize>>)) -> (&'aa K, std::collections::hash_set::Iter<'bb, V>)>;

   type TValsIter = std::collections::hash_set::Iter<'a, V>;
   type TValsIterForTIter = Self::TValsIter;

   fn iter(&'a self) -> Self::TIter {
      let res: Map<std::collections::hash_map::Iter<'a, K, HashSet<V, BuildNoHashHasher<usize>>>, for<'aa, 'bb> fn((&'aa K, &'bb HashSet<V, BuildNoHashHasher<usize>>)) -> (&'aa K, std::collections::hash_set::Iter<'bb, V>)> 
         = self.0.iter().map(|(k, v)| (k, v.iter()));
      res
   }

   fn len(&self) -> usize { self.0.len() }
}

impl<'a, K: Clone + Eq + Hash, V: Eq + Hash> MultiDictWrite<'a> for MultiDictDedup<K, V> {
   type K = K;
   type V = V;

   fn insert(&'a mut self, key: Self::K, value: Self::V) {
      self.0.entry(key).or_default().insert(value);
   }
}

impl<'a, K, TDict, TDict2: 'a> MultiDictWrite<'a> for DictOfDictWriteUncurried<TDict> 
where TDict: DictWrite<'a, K = K, V = TDict2>,
      TDict2: MultiDictWrite<'a> + Default
{
   type K = (K, TDict2::K);
   type V = TDict2::V;

   fn insert(&'a mut self, key: Self::K, value: Self::V) {
      (&mut self.0).get_mut_or_insert(key.0, TDict2::default).insert(key.1, value)
   }
}

impl <'a, Ind1, Ind2, K: 'a, V: 'a> DictRead<'a> for RelIndexCombined<'a, Ind1, Ind2> 
where Ind1: DictRead<'a, K = K, V = V>,  Ind2: DictRead<'a, K = K, V = V>, K: Clone 
{
   type K = K;
   type V = V;

   #[inline]
   fn get(&'a self, key: Self::K) -> Option<&'a Self::V> {
      self.ind1.get(key.clone()).or_else(|| self.ind2.get(key))
   }
   
   type TIter = std::iter::Chain<Ind1::TIter, Ind2::TIter>; 
   
   fn iter(&'a self) -> Self::TIter {
      self.ind1.iter().chain(self.ind2.iter())
   }

   #[inline(always)]
   fn len(&self) -> usize { self.ind1.len() + self.ind2.len() }
}

impl <'a, Ind, K: 'a, V: 'a> MultiDictRead<'a> for RelIndexCombined<'a, Ind, Ind> 
where Ind: MultiDictRead<'a, K = K, V = V>, K: Clone {
   type K = K;
   type V = V;
   
   #[inline(always)]
   fn len(&self) -> usize { self.ind1.len() + self.ind2.len() }

   type TValsIter = std::iter::Chain<std::iter::Flatten<std::option::IntoIter<Ind::TValsIter>>, 
                                     std::iter::Flatten<std::option::IntoIter<Ind::TValsIter>>>;

   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      match (self.ind1.get(key.clone()), self.ind2.get(key)) {
         (None, None) => None,
         (iter1, iter2) => {
            let res = iter1.into_iter().flatten().chain(iter2.into_iter().flatten());
            Some(res)
         }
      }
   }

   type TIter = std::iter::Chain<Ind::TIter, Ind::TIter>;
   type TValsIterForTIter = Ind::TValsIterForTIter;
   fn iter(&'a self) -> Self::TIter {
      let res: std::iter::Chain<<Ind as MultiDictRead>::TIter, <Ind as MultiDictRead>::TIter> = self.ind1.iter().chain(self.ind2.iter());
      res
   }
}

pub trait DictMerge<'a> {
   fn merge(&'a mut self, other: &'a mut Self);
}

impl<'a, K: Clone + Eq + Hash, V> DictMerge<'a> for Dict<K, V> {
   fn merge(&'a mut self, other: &'a mut Self) {
      if other.0.len() > self.0.len() {
         std::mem::swap(other, self);
      }
      self.0.reserve(other.len());
      for (k, v) in other.0.drain() {
         self.0.insert(k, v);
      }
   }
}

impl<'a, K: Clone + Eq + Hash, V> DictMerge<'a> for MultiDict<K, V> {
   fn merge(&'a mut self, other: &'a mut Self) {
      use std::collections::hash_map::Entry::*;
      if other.0.len() > self.0.len() {
         std::mem::swap(self, other);
      }
      for (k, mut v) in other.0.drain() {
         match self.0.entry(k) {
            Occupied(existing) => {
               let existing = existing.into_mut();
               if v.len() > existing.len() {
                  std::mem::swap(&mut v, existing);
               }
               existing.append(&mut v);
            },
            Vacant(vacant) => {
               vacant.insert(v);
            },
         }
      }
   }
}

impl<'a, K: Eq + Hash, V: Eq + Hash> DictMerge<'a> for MultiDictDedup<K, V> {
   fn merge(&'a mut self, other: &'a mut Self) {
      if other.0.len() > self.0.len() {
         std::mem::swap(self, other);
      }
      for (k, mut v) in other.0.drain(){
         use std::collections::hash_map::Entry::*;
         match self.0.entry(k) {
            Occupied(mut occ) => {
               let existing = occ.get_mut();
               if existing.len() < v.len() {
                  std::mem::swap(existing, &mut v);
               }
               existing.extend(v)
            },
            Vacant(vac) => {
               vac.insert(v);
            },
         }
      }
   }
}

impl<'a, T> DictRead<'a> for &'a T where T: DictRead<'a> {
   type K = T::K;
   type V = T::V;

   #[inline(always)]
   fn get(&'a self, key: Self::K) -> Option<&'a Self::V> {
      (*self).get(key)
   }

   type TIter = T::TIter;

   #[inline(always)]
   fn iter(&'a self) -> Self::TIter {
      (*self).iter()
   }

   #[inline(always)]
   fn contains_key(&'a self, key: Self::K) -> bool { (*self).contains_key(key) }

   #[inline(always)]
   fn len(&self) -> usize { (*self).len() }
}

impl<'a, T> DictRead<'a> for &'a mut T where T: DictRead<'a> {
   type K = T::K;
   type V = T::V;

   #[inline(always)]
   fn get(&'a self, key: Self::K) -> Option<&'a Self::V> {
      (**self).get(key)
   }

   type TIter = T::TIter;

   #[inline(always)]
   fn iter(&'a self) -> Self::TIter {
      (**self).iter()
   }

   #[inline(always)]
   fn contains_key(&'a self, key: Self::K) -> bool { (**self).contains_key(key) }

   #[inline(always)]
   fn len(&'a self) -> usize { (**self).len() }
}

impl<'a, T> MultiDictRead<'a> for &'a T where T: MultiDictRead<'a> {
   type K = T::K;
   type V = T::V;

   #[inline(always)]
   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      (**self).get(key)
   }

   type TIter = T::TIter;
   type TValsIter = T::TValsIter;
   type TValsIterForTIter = T::TValsIterForTIter;

   #[inline(always)]
   fn iter(&'a self) -> Self::TIter {
      (**self).iter()
   }

   #[inline(always)]
   fn contains_key(&'a self, key: Self::K) -> bool { (**self).contains_key(key) }

   #[inline(always)]
   fn len(&self) -> usize { (**self).len() }
}

impl<'a, T> MultiDictRead<'a> for &'a mut T where T: MultiDictRead<'a> {
   type K = T::K;
   type V = T::V;

   #[inline(always)]
   fn get(&'a self, key: Self::K) -> Option<Self::TValsIter> {
      (**self).get(key)
   }

   type TIter = T::TIter;
   type TValsIter = T::TValsIter;
   type TValsIterForTIter = T::TValsIterForTIter;

   #[inline(always)]
   fn iter(&'a self) -> Self::TIter {
      (**self).iter()
   }

   #[inline(always)]
   fn contains_key(&'a self, key: Self::K) -> bool { (**self).contains_key(key) }

   #[inline(always)]
   fn len(&'a self) -> usize { (**self).len() }
}

impl<'a, 'b, T> DictWrite<'a> for &'b mut T where T: DictWrite<'a> {
   type K = T::K;

   type V = T::V;

   #[inline(always)]
   fn set(&'a mut self, key: Self::K, value: Self::V) {
      (*self).set(key, value)
   }

   #[inline(always)]
   fn get_mut_or_insert(&'a mut self, key: Self::K, default: impl FnOnce() -> Self::V) -> &mut Self::V {
      (*self).get_mut_or_insert(key, default)
   }

   #[inline(always)]
   fn insert(&'a mut self, key: Self::K, value: Self::V) { (*self).insert(key, value); }

   #[inline(always)]
   fn insert_if_not_present(&'a mut self, key: Self::K, value: Self::V) -> bool {
      (*self).insert_if_not_present(key, value)
   }
}

impl<'a, T> DictMerge<'a> for &'a mut T where T: DictMerge<'a> {
   #[inline(always)]
   fn merge(&'a mut self, other: &'a mut Self) {
      (*self).merge(*other)
   }
}



// impl<'a, 'b, T> DictDrain<'a> for &'b mut T where T: DictDrain<'a> {
//    type K = T::K;
//    type V = T::V;

//    type TDrain = T::TDrain;

//    fn drain(&'a mut self) -> Self::TDrain {
//       (*self).drain()   
//    }

//    fn next(&mut self) -> Option<(Self::K, Self::V)> {
//       (*self).next()
//    }
// }