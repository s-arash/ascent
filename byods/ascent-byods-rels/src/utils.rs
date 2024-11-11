use std::hash::{BuildHasher, Hash, Hasher};

use hashbrown;
use hashbrown::{Equivalent, HashMap, HashSet};

use crate::iterator_from_dyn::IteratorFromDyn;
use crate::trrel_binary::MyHashSet;

pub(crate) fn move_hash_map_of_hash_set_contents<K, V, S1, S2>(
   from: &mut HashMap<K, HashSet<V, S2>, S1>, to: &mut HashMap<K, HashSet<V, S2>, S1>,
) where
   K: Clone + Hash + Eq,
   V: Clone + Hash + Eq,
   S1: BuildHasher,
   S2: BuildHasher,
{
   if from.len() > to.len() {
      std::mem::swap(from, to);
   }

   for (k, mut from_set) in from.drain() {
      match to.entry(k) {
         hashbrown::hash_map::Entry::Occupied(mut to_set) => {
            if from_set.len() > to_set.get().len() {
               std::mem::swap(&mut from_set, to_set.get_mut())
            }
            to_set.get_mut().extend(from_set.drain());
         },
         hashbrown::hash_map::Entry::Vacant(to_set_vac) => {
            to_set_vac.insert(from_set);
         },
      }
   }
}

pub(crate) fn move_hash_map_of_vec_contents<K, V, S1>(
   from: &mut HashMap<K, Vec<V>, S1>, to: &mut HashMap<K, Vec<V>, S1>,
) where
   K: Clone + Hash + Eq,
   V: Clone + Hash + Eq,
   S1: BuildHasher,
{
   if from.len() > to.len() {
      std::mem::swap(from, to);
   }

   for (k, mut from_vec) in from.drain() {
      match to.entry(k) {
         hashbrown::hash_map::Entry::Occupied(mut to_vec) => {
            if from_vec.len() > to_vec.get().len() {
               std::mem::swap(&mut from_vec, to_vec.get_mut())
            }
            to_vec.get_mut().append(&mut from_vec);
         },
         hashbrown::hash_map::Entry::Vacant(to_vec_vac) => {
            to_vec_vac.insert(from_vec);
         },
      }
   }
}

pub(crate) fn move_hash_map_of_hash_set_contents_disjoint<K, V, S1, S2>(
   from: &mut HashMap<K, HashSet<V, S2>, S1>, to: &mut HashMap<K, HashSet<V, S2>, S1>,
) where
   K: Clone + Hash + Eq,
   V: Clone + Hash + Eq,
   S1: BuildHasher,
   S2: BuildHasher,
{
   if from.len() > to.len() {
      std::mem::swap(from, to);
   }

   for (k, mut from_set) in from.drain() {
      match to.entry(k) {
         hashbrown::hash_map::Entry::Occupied(mut to_set) => {
            move_hash_set_contents_disjoint(&mut from_set, to_set.get_mut());
         },
         hashbrown::hash_map::Entry::Vacant(to_set_vac) => {
            to_set_vac.insert(from_set);
         },
      }
   }
}

pub fn move_hash_set_contents_disjoint<T: Hash + Eq, S: BuildHasher>(from: &mut HashSet<T, S>, to: &mut HashSet<T, S>) {
   if from.len() > to.len() {
      std::mem::swap(from, to);
   }
   to.reserve(from.len());
   for x in from.drain() {
      to.insert_unique_unchecked(x);
   }
}

pub(crate) fn move_hash_map_of_alt_hash_set_contents<K, V, S1, S2>(
   from: &mut HashMap<K, AltHashSet<V, S2>, S1>, to: &mut HashMap<K, AltHashSet<V, S2>, S1>,
) where
   K: Clone + Hash + Eq,
   V: Clone + Hash + Eq,
   S1: BuildHasher,
   S2: BuildHasher,
{
   if from.len() > to.len() {
      std::mem::swap(from, to);
   }

   for (k, mut from_set) in from.drain() {
      match to.entry(k) {
         hashbrown::hash_map::Entry::Occupied(mut to_set) => {
            if from_set.len() > to_set.get().len() {
               std::mem::swap(&mut from_set, to_set.get_mut())
            }
            to_set.get_mut().extend(from_set.drain());
         },
         hashbrown::hash_map::Entry::Vacant(to_set_vac) => {
            to_set_vac.insert(from_set);
         },
      }
   }
}

// #[allow(dead_code)]
#[inline]
pub fn hash_one<S: BuildHasher, T: Hash>(hahser: &S, x: &T) -> u64 {
   let mut hasher = hahser.build_hasher();
   x.hash(&mut hasher);
   hasher.finish()
}

pub struct AltHashSet<T, S>(pub(crate) HashMap<T, (), S>);

impl<T, S: Default> Default for AltHashSet<T, S> {
   #[inline(always)]
   fn default() -> Self { Self(Default::default()) }
}

impl<T: Clone + Hash + Eq, S: BuildHasher> AltHashSet<T, S> {
   #[inline(always)]
   pub fn contains<Q: ?Sized + Hash + Equivalent<T>>(&self, k: &Q) -> bool { self.0.contains_key(k) }

   #[inline(always)]
   pub fn iter(&self) -> AltHashSetIter<'_, T> { self.0.keys() }

   #[inline(always)]
   pub fn insert(&mut self, x: T) -> bool { self.0.insert(x, ()).is_none() }

   #[inline(always)]
   pub fn len(&self) -> usize { self.0.len() }

   pub fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
      self.0.extend(iter.into_iter().map(|x| (x, ())))
   }

   #[inline]
   pub fn insert_with_hash_no_check(&mut self, hash: u64, item: T) {
      self.0.raw_entry_mut().from_key_hashed_nocheck(hash, &item).or_insert(item, ());
   }

   pub fn drain(&mut self) -> impl Iterator<Item = T> + '_ { self.0.drain().map(|kv| kv.0) }

   pub fn intersection<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = &T> + 'a {
      let (small, big) = if self.len() < other.len() { (self, other) } else { (other, self) };
      small.iter().filter(|&x| big.contains(x))
   }
}
pub type AltHashSetIter<'a, T> = hashbrown::hash_map::Keys<'a, T, ()>;

// TODO remove if not used
fn _set_extend_with_hash_no_check<T, S, Iter>(set: &mut AltHashSet<T, S>, iter: Iter)
where
   T: Clone + Hash + Eq,
   S: BuildHasher,
   Iter: Iterator<Item = (u64, T)>,
{
   set.0.reserve(iter.size_hint().0);
   for (hash, item) in iter {
      set.0.raw_entry_mut().from_key_hashed_nocheck(hash, &item).insert(item, ());
   }
}

// TODO remove if not used
#[allow(dead_code)]
pub fn hash_map_hash_set_intersection<'a, K, V, S>(
   hm: &'a HashMap<K, V, S>, hs: &'a MyHashSet<K, S>,
) -> IteratorFromDyn<'a, &'a V>
where
   K: Clone + Hash + Eq,
   S: BuildHasher,
{
   if hm.len() < hs.len() {
      IteratorFromDyn::new(move || hm.iter().filter_map(move |(k, v)| if hs.contains(k) { Some(v) } else { None }))
   } else {
      IteratorFromDyn::new(|| hs.iter().filter_map(|k| hm.get(k)))
   }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
#[allow(dead_code)]
pub enum Either<L, R> {
   Left(L),
   Right(R),
}

impl<L, R, T> Iterator for Either<L, R>
where
   L: Iterator<Item = T>,
   R: Iterator<Item = T>,
{
   type Item = T;

   #[inline]
   fn next(&mut self) -> Option<Self::Item> {
      match self {
         Either::Left(l) => l.next(),
         Either::Right(r) => r.next(),
      }
   }
}

pub(crate) fn merge_sets<T: Hash + Eq, S: BuildHasher>(set1: &mut HashSet<T, S>, mut set2: HashSet<T, S>) {
   if set1.len() < set2.len() {
      std::mem::swap(set1, &mut set2);
   }
   set1.extend(set2);
}
