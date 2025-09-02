use std::hash::{BuildHasherDefault, Hash};

use ascent::internal::{RelIndexRead, RelIndexReadAll};
use hashbrown::HashMap;
use hashbrown::hash_map::Iter;
use rustc_hash::FxHasher;

pub type MyHashSetIter<'a, T> = hashbrown::hash_set::Iter<'a, T>;
pub type MyHashSet<T, S> = hashbrown::HashSet<T, S>;

pub type Map<T> = HashMap<T, MyHashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>;
pub type RevMap<T> = HashMap<T, Vec<T>, BuildHasherDefault<FxHasher>>;

#[derive(Clone)]
pub struct BinaryRel<T: Clone + Hash + Eq> {
   pub(crate) map: Map<T>,
   pub(crate) reverse_map: RevMap<T>,
}

impl<T: Clone + Hash + Eq> Default for BinaryRel<T> {
   fn default() -> Self { Self { map: Default::default(), reverse_map: Default::default() } }
}

impl<T: Clone + Hash + Eq> BinaryRel<T> {
   /// returns true if this tuple did not exist in the binary relation
   pub fn insert(&mut self, x: T, y: T) -> bool {
      if self.map.entry(x.clone()).or_default().insert(y.clone()) {
         self.reverse_map.entry(y).or_default().push(x);
         true
      } else {
         false
      }
   }

   /// returns true if this tuple did not exist in the binary relation
   pub fn insert_by_ref(&mut self, x: &T, y: &T) -> bool {
      let added = match self.map.raw_entry_mut().from_key(x) {
         hashbrown::hash_map::RawEntryMut::Occupied(mut occ) => occ.get_mut().insert(y.clone()),
         hashbrown::hash_map::RawEntryMut::Vacant(vac) => {
            vac.insert(x.clone(), MyHashSet::from_iter([y.clone()]));
            true
         },
      };
      if added {
         match self.reverse_map.raw_entry_mut().from_key(y) {
            hashbrown::hash_map::RawEntryMut::Occupied(mut occ) => {
               occ.get_mut().push(x.clone());
            },
            hashbrown::hash_map::RawEntryMut::Vacant(vac) => {
               vac.insert(y.clone(), vec![x.clone()]);
            },
         };
         true
      } else {
         false
      }
      // if self.map.raw_entry_mut().from_key(x)..entry(x.clone()).or_default().insert(y.clone()) {
      //    self.reverse_map.entry(y).or_default().push(x);
      //    true
      // } else {
      //    false
      // }
   }

   pub fn iter_all(&self) -> impl Iterator<Item = (&T, &T)> + '_ {
      self.map.iter().flat_map(|(x, x_set)| x_set.iter().map(move |y| (x, y)))
   }

   #[inline]
   pub fn contains(&self, x: &T, y: &T) -> bool { self.map.get(x).is_some_and(|s| s.contains(y)) }

   pub fn count_estimate(&self) -> usize {
      let sample_size = 3;
      let sum = self.map.values().take(sample_size).map(|x| x.len()).sum::<usize>();
      sum * self.map.len() / sample_size.min(self.map.len()).max(1)
   }

   pub fn count_exact(&self) -> usize { self.map.values().map(|x| x.len()).sum() }
}

pub struct MapRelIndexAdaptor<'a, T: Clone + Hash + Eq>(
   pub &'a HashMap<T, MyHashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>,
);

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexReadAll<'a> for MapRelIndexAdaptor<'a, T> {
   type Key = &'a T;

   type Value = &'a T;

   type ValueIteratorType = MyHashSetIter<'a, T>;

   type AllIteratorType = std::iter::Map<
      Iter<'a, T, MyHashSet<T, BuildHasherDefault<FxHasher>>>,
      for<'aa> fn((&'aa T, &'a MyHashSet<T, BuildHasherDefault<FxHasher>>)) -> (&'aa T, Self::ValueIteratorType),
   >;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.0.iter().map(|(k, v)| {
         let v_iter = v.iter();
         (k, v_iter)
      });
      res
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for MapRelIndexAdaptor<'a, T> {
   type Key = T;

   type Value = &'a T;

   type IteratorType = hashbrown::hash_set::Iter<'a, T>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let set = self.0.get(key)?;
      let res = set.iter();
      Some(res)
   }

   fn len_estimate(&'a self) -> usize { self.0.len() }
   fn is_empty(&'a self) -> bool { self.0.is_empty() }
}

pub struct RelIndexValTransformer<T, F> {
   rel: T,
   f: F,
}

impl<'a, T: 'a, F: 'a, V: 'a, U: 'a> RelIndexRead<'a> for RelIndexValTransformer<T, F>
where
   T: RelIndexRead<'a, Value = V>,
   F: Fn(V) -> U,
{
   type Key = T::Key;
   type Value = U;

   type IteratorType = std::iter::Map<
      std::iter::Zip<T::IteratorType, std::iter::Repeat<&'a Self>>,
      for<'aa> fn((V, &'aa Self)) -> U,
   >;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let res: Self::IteratorType =
         self.rel.index_get(key)?.zip(std::iter::repeat(self)).map(|(x, _self)| (_self.f)(x));
      Some(res)
   }

   fn len_estimate(&'a self) -> usize { self.rel.len_estimate() }
   fn is_empty(&'a self) -> bool { self.rel.is_empty() }
}

impl<'a, T: 'a, F: 'a, V: 'a, U: 'a> RelIndexReadAll<'a> for RelIndexValTransformer<T, F>
where
   T: RelIndexReadAll<'a, Value = V>,
   F: Fn(V) -> U,
{
   type Key = T::Key;
   type Value = U;

   type ValueIteratorType = std::iter::Map<
      std::iter::Zip<T::ValueIteratorType, std::iter::Repeat<&'a Self>>,
      for<'aa> fn((V, &'aa Self)) -> U,
   >;

   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self.rel.iter_all().map(move |(k, vals_iter)| {
         let new_vals_iter: Self::ValueIteratorType =
            vals_iter.zip(std::iter::repeat(self)).map(|(x, _self)| (_self.f)(x));
         (k, new_vals_iter)
      });

      Box::new(res)
   }
}
