use std::hash::{Hash, BuildHasherDefault};
use std::iter::Map;
use std::marker::PhantomData;
use std::time::{Duration, Instant};
use ascent::internal::{RelIndexMerge, RelIndexReadAll, RelIndexRead, RelFullIndexWrite, RelIndexWrite, RelFullIndexRead};
use ascent::to_rel_index::ToRelIndex;
use hashbrown::HashMap;
use rustc_hash::FxHasher;
use crate::binary_rel::BinaryRel;
use crate::iterator_from_dyn::IteratorFromDyn;
use crate::rel_boilerplate::NoopRelIndexWrite;
use crate::trrel_binary::{MyHashSetIter, MyHashSet};
use crate::utils::{move_hash_map_of_hash_set_contents_disjoint, move_hash_map_of_vec_contents};

// TODO do we still need two variants?
pub enum TrRelIndCommon<T: Clone + Hash + Eq> {
   New { rel: BinaryRel<T>, anti_reflexive: bool },
   Old { rel: BinaryRel<T>, anti_reflexive: bool },
}

impl<T: Clone + Hash + Eq> TrRelIndCommon<T> {

   pub fn anti_reflexive(&self) -> bool {
      match self {
        TrRelIndCommon::New{ anti_reflexive, .. } => *anti_reflexive,
        TrRelIndCommon::Old{ anti_reflexive, .. } => *anti_reflexive,
      }
   }

   #[inline]
   pub fn make_new() -> Self { Self::New { rel: Default::default(), anti_reflexive: true } }

   #[inline]
   pub fn rel(&self) -> &BinaryRel<T> {
      match self {
         TrRelIndCommon::New { rel, .. } => rel,
         TrRelIndCommon::Old { rel, .. } => rel,
      }
   }

   pub fn unwrap_new_mut(&mut self) -> &mut BinaryRel<T> {
      match self {
         TrRelIndCommon::New{ rel, .. } => rel,
         TrRelIndCommon::Old{..} => panic!("TrRelIndCommon: unwrap_new_mut called on Old"),
      }
   }
   pub fn unwrap_new(&self) -> &BinaryRel<T> {
      match self {
         TrRelIndCommon::New{ rel, .. } => rel,
         TrRelIndCommon::Old{..} => panic!("TrRelIndCommon: unwrap_new called on Old"),
      }
   }

   pub fn unwrap_old(&self) -> &BinaryRel<T> {
      match self {
         TrRelIndCommon::Old{rel, ..} => rel,
         TrRelIndCommon::New{..} => panic!("TrRelIndCommon: unwrap_old called on New"),
      }
   }

   #[inline]
   pub fn insert(&mut self, x: T, y: T) -> bool {
      let rel = self.unwrap_new_mut();
      rel.insert(x, y)
   }

   #[inline]
   pub fn insert_by_ref(&mut self, x: &T, y: &T) -> bool {
      let rel = self.unwrap_new_mut();
      rel.insert_by_ref(x, y)
   }

   pub fn is_empty(&self) -> bool {
      match self {
        TrRelIndCommon::New{rel, ..} => rel.map.is_empty(),
        TrRelIndCommon::Old{rel, ..} => rel.map.is_empty(),
      }
   }
   
}

pub static mut MERGE_TIME: Duration = Duration::ZERO;
pub static mut MERGE_COUNT: usize = 0;

pub trait ToTrRelIndCommon<T: Clone + Hash + Eq> {
   fn to_tr_rel_ind(&self) -> &TrRelIndCommon<T>;
   fn to_tr_rel_ind_mut(&mut self) -> &mut TrRelIndCommon<T>;
}

impl<T: Clone + Hash + Eq> ToTrRelIndCommon<T> for TrRelIndCommon<T> {
   fn to_tr_rel_ind(&self) -> &TrRelIndCommon<T> { self }
   fn to_tr_rel_ind_mut(&mut self) -> &mut TrRelIndCommon<T> { self }
}

impl<T: Clone + Hash + Eq> Default for TrRelIndCommon<T> {
   #[inline]
   fn default() -> Self {Self::Old { rel: Default::default(), anti_reflexive: true }}
}

impl<T: Clone + Hash + Eq> RelIndexMerge for TrRelIndCommon<T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) {
      panic!("merge_delta_to_total_new_to_delta must be called instead.")
   }

   fn init(new: &mut Self, delta: &mut Self, total: &mut Self) {
      assert!(matches!(delta, Self::Old { .. }));
      assert!(matches!(total, Self::Old { .. }));
      *new = Self::New { rel: Default::default(), anti_reflexive: delta.anti_reflexive() };
   }

   fn merge_delta_to_total_new_to_delta(new: &mut Self, delta: &mut Self, total: &mut Self) {
      let before = Instant::now();
      let anti_reflexive = total.anti_reflexive(); 

      let mut total_rel = match total {
         TrRelIndCommon::New{..} => Default::default(),
         TrRelIndCommon::Old{rel, ..} => std::mem::take(rel),
      };
      let mut delta_rel = match delta {
         TrRelIndCommon::New{..} => Default::default(),
         TrRelIndCommon::Old{rel, ..} => std::mem::take(rel),
      };

      move_hash_map_of_hash_set_contents_disjoint(&mut delta_rel.map, &mut total_rel.map);
      move_hash_map_of_vec_contents(&mut delta_rel.reverse_map, &mut total_rel.reverse_map);
      
      let mut new_delta = BinaryRel::<T>::default();

      // let new_delta_prog = ascent::ascent_run! {
      //    struct AscentProg<T: Clone + Hash + Eq>;
      //    relation delta(T, T);
      //    relation new(T, T);
      //    relation total_map(T);
      //    relation total_reverse_map(T);
      //    total_map(x) <-- for x in total_rel.map.keys();
      //    total_reverse_map(x) <-- for x in total_rel.reverse_map.keys();
         
      //    new(x, y), delta(x, y) <-- for (x, y) in new.unwrap_new().iter();
      //    delta(x, z) <-- delta(x, y), new(y, z);
      //    delta(x, z) <-- delta(x, y), total_map(y), for z in total_rel.map.get(y).unwrap();
      //    delta(x, z) <-- delta(y, z), total_reverse_map(y), for x in total_rel.reverse_map.get(y).unwrap();
      // };

      // new_delta.reverse_map = new_delta_prog.delta_indices_1.0.into_iter()
      //    .map(|(k, v)| (k.0, v.into_iter().map(|x| x.0).collect())).collect();

      // new_delta.map = new_delta_prog.delta_indices_0.0.into_iter()
      //    .map(|(k, v)| (k.0, v.into_iter().map(|x| x.0).collect())).collect();


      type RelMap<T> = HashMap::<T, MyHashSet<T, BuildHasherDefault<FxHasher>>, BuildHasherDefault<FxHasher>>;
      type RelRevMap<T> = HashMap::<T, Vec<T>, BuildHasherDefault<FxHasher>>;

      let new_rel = std::mem::take(new.unwrap_new_mut());
      let new_map = new_rel.map;
      let mut delta_delta_map = new_map.clone();
      let mut delta_delta_rev_map = new_rel.reverse_map;

      let mut delta_total_map = RelMap::<T>::default();
      let mut delta_total_rev_map = RelRevMap::<T>::default();

      let mut delta_new_map = RelMap::<T>::default();
      let mut delta_new_rev_map = RelRevMap::<T>::default();

      fn join<T: Clone + Hash + Eq>(
         target: &mut RelMap<T>, target_rev: &mut RelRevMap<T>, rel1: &RelMap<T>, rel2_rev: &RelRevMap<T>, 
         mut can_add: impl FnMut(&T, &T) -> bool, name: &str
      ) -> bool {
         let mut changed = false;
         if rel1.len() < rel2_rev.len() {
            for (x, x_set) in rel1.iter() {
               assert!(!x_set.is_empty(), "bad join {name}. rel1 has non-empty sets");
               if let Some(x_rev_set) = rel2_rev.get(x) {
                  assert!(!x_set.is_empty(), "bad join {name}. rel2_rev has non-empty sets");
                  for w in x_rev_set {
                     let entry = target.entry(w.clone()).or_default();
                     for y in x_set.iter() {
                        if !can_add(w, y) {continue}
                        if entry.insert(y.clone()) {
                           target_rev.entry(y.clone()).or_default().push(w.clone());
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
            for (x, x_rev_set) in rel2_rev.iter() {
               if let Some(x_set) = rel1.get(x) {
                  for w in x_rev_set {
                     let entry = target.entry(w.clone()).or_default();
                     for y in x_set.iter() {
                        if !can_add(w, y) {continue}
                        if entry.insert(y.clone()) {
                           target_rev.entry(y.clone()).or_default().push(w.clone());
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
      loop {

         let mut cached_delta_delta_map_entry_for_can_add = None;
         let mut cached_delta_delta_map_x_for_can_add = None;
         let mut cached_delta_total_map_entry_for_can_add = None;
         let mut cached_delta_total_map_x_for_can_add = None;
         let mut cached_total_map_entry_for_can_add = None;
         let mut cached_total_map_x_for_can_add = None;
         let mut can_add = |x: &T, y: &T| {
            if anti_reflexive && x == y { return false }
            {
               if cached_delta_delta_map_x_for_can_add.as_ref() != Some(x) {
                  cached_delta_delta_map_entry_for_can_add = delta_delta_map.get(x);
                  cached_delta_delta_map_x_for_can_add = Some(x.clone());
               };
            }
            !cached_delta_delta_map_entry_for_can_add.map_or(false, |s| s.contains(y)) &&
            {
               if cached_delta_total_map_x_for_can_add.as_ref() != Some(x) {
                  cached_delta_total_map_entry_for_can_add = delta_total_map.get(x);
                  cached_delta_total_map_x_for_can_add = Some(x.clone());
               };
               !cached_delta_total_map_entry_for_can_add.map_or(false, |s| s.contains(y))
            } &&
            {
               if cached_total_map_x_for_can_add.as_ref() != Some(x) {
                  cached_total_map_entry_for_can_add = total_rel.map.get(x);
                  cached_total_map_x_for_can_add = Some(x.clone());
               }
               !cached_total_map_entry_for_can_add.map_or(false, |s| s.contains(y))
            }
         };
         
         let join1 = join(&mut delta_new_map, &mut delta_new_rev_map, &delta_delta_map, &total_rel.reverse_map, &mut can_add, "join1");
         let join2 = join(&mut delta_new_map, &mut delta_new_rev_map, &total_rel.map, &delta_delta_rev_map, &mut can_add, "join2");
         let join3 = join(&mut delta_new_map, &mut delta_new_rev_map, &new_map, &delta_delta_rev_map, &mut can_add, "join3");
         
         let changed = join1 | join2 | join3;

         move_hash_map_of_hash_set_contents_disjoint(&mut delta_delta_map, &mut delta_total_map);
         move_hash_map_of_vec_contents(&mut delta_delta_rev_map, &mut delta_total_rev_map);

         assert!(delta_delta_map.is_empty());
         assert!(delta_delta_rev_map.is_empty());

         std::mem::swap(&mut delta_delta_map, &mut delta_new_map);
         std::mem::swap(&mut delta_delta_rev_map, &mut delta_new_rev_map);

         if !changed { break }
      }
      new_delta.map = delta_total_map;
      new_delta.reverse_map = delta_total_rev_map;

      *total = TrRelIndCommon::Old{ rel: total_rel, anti_reflexive };
      *delta = TrRelIndCommon::Old{ rel: new_delta, anti_reflexive };
      *new = TrRelIndCommon::New{ rel: Default::default(), anti_reflexive };

      unsafe {
         MERGE_TIME += before.elapsed();
         MERGE_COUNT += 1;
      }
   }
}

pub struct TrRelInd0<'a, T: Clone + Hash + Eq>(pub(crate) &'a TrRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexReadAll<'a> for TrRelInd0<'a, T> {
   type Key = (&'a T, );
   type Value = (&'a T, );

   type ValueIteratorType = Map<MyHashSetIter<'a, T>, fn(&T) -> (&T,)>;

   type AllIteratorType = Map<hashbrown::hash_map::Iter<'a, T, MyHashSet<T, BuildHasherDefault<FxHasher>>>, for<'aa> fn((&'aa T, &'aa MyHashSet<T, BuildHasherDefault<FxHasher>>)) -> ((&'aa T,), Map<MyHashSetIter<'aa, T>, for <'bb> fn(&'bb T) -> (&'bb T,)>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res : Self::AllIteratorType = self.0.unwrap_old().map.iter().map(|(k, v)| ((k, ), v.iter().map(|x| (x, ))));
      res
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for TrRelInd0<'a, T> {
   type Key = (T, );
   type Value = (&'a T, );

   type IteratorType = Map<MyHashSetIter<'a, T>, fn(&T) -> (&T,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let set = self.0.unwrap_old().map.get(&key.0)?;
      let res: Self::IteratorType = set.iter().map(|x| (x, ));
      Some(res)
   }

   fn len(&self) -> usize {
      self.0.unwrap_old().map.len()
   }
}

pub struct TrRelInd1<'a, T: Clone + Hash + Eq>(pub(crate) &'a TrRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq + 'a> RelIndexReadAll<'a> for TrRelInd1<'a, T> {
   type Key = (&'a T, );
   type Value = (&'a T, );

   type ValueIteratorType = Map<std::slice::Iter<'a, T>, fn(&T) -> (&T,)>;

   type AllIteratorType = Map<hashbrown::hash_map::Iter<'a, T, Vec<T>>, for<'aa> fn((&'aa T, &'aa Vec<T>)) -> ((&'aa T,), Map<std::slice::Iter<'aa, T>, for <'bb> fn(&'bb T) -> (&'bb T,)>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res : Self::AllIteratorType = self.0.rel().reverse_map.iter().map(|(k, v)| ((k, ), v.iter().map(|x| (x, ))));
      res
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for TrRelInd1<'a, T> {
   type Key = (T, );
   type Value = (&'a T, );

   type IteratorType = Map<std::slice::Iter<'a, T>, fn(&T) -> (&T,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let set = self.0.rel().reverse_map.get(&key.0)?;
      let res: Self::IteratorType = set.iter().map(|x| (x, ));
      Some(res)
   }

   fn len(&self) -> usize {
      self.0.rel().reverse_map.len()
   }
}

pub struct TrRelIndNone<'a, T: Clone + Hash + Eq>(&'a TrRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRelIndNone<'a, T> {
   type Key = ();
   type Value = (&'a T, &'a T);

   type ValueIteratorType = <Self as RelIndexRead<'a>>::IteratorType;

   type AllIteratorType = std::iter::Once<((), Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      std::iter::once(((), self.index_get(&()).unwrap()))
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for TrRelIndNone<'a, T> {
   type Key = ();
   type Value = (&'a T, &'a T);

   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (): &Self::Key) -> Option<Self::IteratorType> {
      println!("iterating TrRelIndNone. {} tuples", self.0.rel().map.values().map(|x| x.len()).sum::<usize>());
      let res = || self.0.rel().map.iter().flat_map(|(x, x_set)| x_set.iter().map(move |y| (x, y)));
      Some(IteratorFromDyn::new(res))
   }

   fn len(&self) -> usize {
      1
   }
}

pub struct TrRelIndFullWrite<'a, T: Clone + Hash + Eq>(&'a mut TrRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq> RelIndexMerge for TrRelIndFullWrite<'a, T> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { } //noop
}

impl<'a, T: Clone + Hash + Eq> RelFullIndexWrite for TrRelIndFullWrite<'a, T> {
   type Key = (T, T);
   type Value = ();

   fn insert_if_not_present(&mut self, (x, y): &Self::Key, (): Self::Value) -> bool {
      self.0.insert_by_ref(x, y)
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexWrite for TrRelIndFullWrite<'a, T> {
   type Key = (T, T);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, (): Self::Value) {
      self.0.insert(key.0, key.1);
   }
}

pub struct TrRelIndFull<'a, T: Clone + Hash + Eq>(pub(crate) &'a TrRelIndCommon<T>);

impl<'a, T: Clone + Hash + Eq> RelFullIndexRead<'a> for TrRelIndFull<'a, T> {
   type Key = (T, T);

   fn contains_key(&'a self, key: &Self::Key) -> bool {
      self.0.rel().map.get(&key.0).map_or(false, |s| s.contains(&key.1))
   }
}


impl<'a, T: Clone + Hash + Eq> RelIndexReadAll<'a> for TrRelIndFull<'a, T> {
   type Key = (&'a T, &'a T);
   type Value = ();

   type ValueIteratorType = std::iter::Once<Self::Value>;
   type AllIteratorType = Box<dyn Iterator<Item = (Self::Key, Self::ValueIteratorType)> + 'a>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = self.0.rel().map.iter().flat_map(|(x, x_set)| x_set.iter().map(move |y| (x, y)))
         .map(|key| (key, std::iter::once(())));
      Box::new(res)
   }
}

impl<'a, T: Clone + Hash + Eq> RelIndexRead<'a> for TrRelIndFull<'a, T> {
   type Key = (T, T);
   type Value = ();

   type IteratorType = std::iter::Once<()>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      if self.0.rel().map.get(&key.0)?.contains(&key.1) {
         Some(std::iter::once(()))
      } else {
         None
      }
   }

   fn len(&self) -> usize {
      let sample_size = 3;
      let sum: usize = self.0.rel().map.values().take(sample_size).map(|x| x.len()).sum();
      let map_len = self.0.rel().map.len();
      sum * map_len / sample_size.min(map_len).max(1)
   }
}

macro_rules! to_rel_ind {
   ($name: ident, $key: ty, $val: ty) => {paste::paste!{
      pub struct [<To $name>]<T: Clone + Hash + Eq>(PhantomData<T>);

      impl<T: Clone + Hash + Eq> Default for [<To $name>]<T> {
         fn default() -> Self { Self(PhantomData) }
      }

      impl<T: Clone + Hash + Eq, Rel> ToRelIndex<Rel> for [<To $name>]<T> 
      where Rel: ToTrRelIndCommon<T>
      {
         type RelIndex<'a> = $name<'a, T> where Self: 'a, Rel: 'a;
         fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { $name(rel.to_tr_rel_ind()) }

         type RelIndexWrite<'a> = NoopRelIndexWrite<$key, $val> where Self: 'a, Rel: 'a;
         fn to_rel_index_write<'a>(&'a mut self, _rel: &'a mut Rel) -> Self::RelIndexWrite<'a> { 
            NoopRelIndexWrite::default() 
         }
      }
   }};
}

to_rel_ind!(TrRelIndNone, (), (T, T));
to_rel_ind!(TrRelInd0, (T, ), (T, ));
to_rel_ind!(TrRelInd1, (T, ), (T, ));
// to_rel_ind!(TrRelIndFull, (T, T), ());

pub struct ToTrRelIndFull<T: Clone + Hash + Eq>(PhantomData<T>);

impl<T: Clone + Hash + Eq> Default for ToTrRelIndFull<T> {
   fn default() -> Self { Self(PhantomData) }
}
impl<T: Clone + Hash + Eq, Rel> ToRelIndex<Rel> for ToTrRelIndFull<T>
where
   Rel: ToTrRelIndCommon<T>,
{
   type RelIndex<'a>  = TrRelIndFull<'a,T>where Self:'a, Rel:'a;
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { TrRelIndFull(rel.to_tr_rel_ind()) }

   type RelIndexWrite<'a>  = TrRelIndFullWrite<'a, T> where Self:'a, Rel:'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a> {
      TrRelIndFullWrite(rel.to_tr_rel_ind_mut())
   }
}
