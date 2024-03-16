use std::iter::{Map, once, Once};
use std::marker::PhantomData;

use ascent::internal::{RelIndexMerge, RelIndexWrite, RelFullIndexWrite, RelFullIndexRead};
use ascent::rel_index_read::{RelIndexRead, RelIndexReadAll};

use crate::iterator_from_dyn::IteratorFromDyn;


pub trait ByodsBinRel: RelIndexMerge + Default {
   type T0;
   type T1;

   fn contains(&self, x0: &Self::T0, x1: &Self::T1) -> bool;

   type AllIter<'a>: Iterator<Item = (&'a Self::T0, &'a Self::T1)> where Self: 'a;
   fn iter_all<'a>(&'a self) -> Self::AllIter<'a>;
   fn len_estimate(&self) -> usize;

   type Ind0AllIterValsIter<'a>: Iterator<Item = &'a Self::T1> where Self: 'a;
   type Ind0AllIter<'a>: Iterator<Item = (&'a Self::T0, Self::Ind0AllIterValsIter<'a>)> where Self: 'a;
   fn ind0_iter_all<'a>(&'a self) -> Self::Ind0AllIter<'a>;
   fn ind0_len_estimate(&self) -> usize;

   type Ind0ValsIter<'a>: Iterator<Item = &'a Self::T1> + Clone where Self: 'a;
   fn ind0_index_get<'a>(&'a self, key: &Self::T0) -> Option<Self::Ind0ValsIter<'a>>;


   type Ind1AllIterValsIter<'a>: Iterator<Item = &'a Self::T0> where Self: 'a;
   type Ind1AllIter<'a>: Iterator<Item = (&'a Self::T1, Self::Ind1AllIterValsIter<'a>)> where Self: 'a;
   fn ind1_iter_all<'a>(&'a self) -> Self::Ind1AllIter<'a>;
   fn ind1_len_estimate(&self) -> usize;

   type Ind1ValsIter<'a>: Iterator<Item = &'a Self::T0> + Clone where Self: 'a;
   fn ind1_index_get<'a>(&'a self, key: &Self::T1) -> Option<Self::Ind1ValsIter<'a>>;

   fn insert(&mut self, x0: Self::T0, x1: Self::T1) -> bool;

   fn is_empty(&self) -> bool {
      self.iter_all().next().is_none()
   }
}

pub struct ByodsBinRelInd0<'a, TBinRel>(&'a TBinRel);

impl<'a, TBinRel: ByodsBinRel> RelIndexRead<'a> for ByodsBinRelInd0<'a, TBinRel> {
   type Key = (TBinRel::T0,);
   type Value = (&'a TBinRel::T1, );

   type IteratorType = std::iter::Map<TBinRel::Ind0ValsIter<'a>, fn(&TBinRel::T1) -> (&TBinRel::T1,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let res = self.0.ind0_index_get(&key.0)?;
      let res: Self::IteratorType = res.map(|v| (v, ));
      Some(res)
   }

   fn len(&'a self) -> usize {
      self.0.ind0_len_estimate()
   }
}

impl<'a, TBinRel: ByodsBinRel> RelIndexReadAll<'a> for ByodsBinRelInd0<'a, TBinRel> {
   type Key = (&'a TBinRel::T0, );
   type Value = (&'a TBinRel::T1, );

   type ValueIteratorType = std::iter::Map<TBinRel::Ind0AllIterValsIter<'a>, fn(&TBinRel::T1) -> (&TBinRel::T1,)>;
   type AllIteratorType = Map<TBinRel::Ind0AllIter<'a>, for<'aa> fn((&'aa TBinRel::T0, TBinRel::Ind0AllIterValsIter<'a>)) -> ((&'aa TBinRel::T0,), Map<TBinRel::Ind0AllIterValsIter<'a>, fn(&TBinRel::T1) -> (&TBinRel::T1,)>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.0.ind0_iter_all().map(|(k, vals_iter)| {
         let new_vals_iter: Self::ValueIteratorType = vals_iter.map(|v| (v, ));
         ((k, ), new_vals_iter)
      });
      res
   }
}

pub struct ByodsBinRelInd1<'a, TBinRel>(&'a TBinRel);

impl<'a, TBinRel: ByodsBinRel> RelIndexRead<'a> for ByodsBinRelInd1<'a, TBinRel> {
   type Key = (TBinRel::T1,);
   type Value = (&'a TBinRel::T0, );

   type IteratorType = std::iter::Map<TBinRel::Ind1ValsIter<'a>, fn(&TBinRel::T0) -> (&TBinRel::T0,)>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      let res = self.0.ind1_index_get(&key.0)?;
      let res: Self::IteratorType = res.map(|v| (v, ));
      Some(res)
   }

   fn len(&'a self) -> usize {
      self.0.ind1_len_estimate()
   }
}

impl<'a, TBinRel: ByodsBinRel> RelIndexReadAll<'a> for ByodsBinRelInd1<'a, TBinRel> {
   type Key = (&'a TBinRel::T1, );
   type Value = (&'a TBinRel::T0, );

   type ValueIteratorType = std::iter::Map<TBinRel::Ind1AllIterValsIter<'a>, fn(&TBinRel::T0) -> (&TBinRel::T0,)>;
   type AllIteratorType = Map<TBinRel::Ind1AllIter<'a>, for<'aa> fn((&'aa TBinRel::T1, TBinRel::Ind1AllIterValsIter<'a>)) -> ((&'aa TBinRel::T1,), Map<TBinRel::Ind1AllIterValsIter<'a>, fn(&TBinRel::T0) -> (&TBinRel::T0,)>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.0.ind1_iter_all().map(|(k, vals_iter)| {
         let new_vals_iter: Self::ValueIteratorType = vals_iter.map(|v| (v, ));
         ((k, ), new_vals_iter)
      });
      res
   }
}

pub struct ByodsBinRelInd0_1<'a, TBinRel>(&'a TBinRel);

impl<'a, TBinRel: ByodsBinRel> RelIndexRead<'a> for ByodsBinRelInd0_1<'a, TBinRel> {
   type Key = (TBinRel::T0, TBinRel::T1);
   type Value = ();

   type IteratorType = Once<()>;

   fn index_get(&'a self, key: &Self::Key) -> Option<Self::IteratorType> {
      if self.0.contains(&key.0, &key.1) {
         Some(once(()))
      } else {
         None
      }
   }

   fn len(&'a self) -> usize {
      self.0.len_estimate()
   }
}

impl<'a, TBinRel: ByodsBinRel> RelIndexReadAll<'a> for ByodsBinRelInd0_1<'a, TBinRel> {
   type Key = (&'a TBinRel::T0, &'a TBinRel::T1);
   type Value = ();

   type ValueIteratorType = Once<()>;
   type AllIteratorType = Map<TBinRel::AllIter<'a>, for<'aa> fn((&'aa TBinRel::T0, &'aa TBinRel::T1)) -> ((&'aa TBinRel::T0, &'aa TBinRel::T1), Once<()>)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res: Self::AllIteratorType = self.0.iter_all().map(|t| (t, once(())));
      res
   }
}

impl<'a, TBinRel: ByodsBinRel> RelFullIndexRead<'a> for ByodsBinRelInd0_1<'a, TBinRel> {
   type Key = (TBinRel::T0, TBinRel::T1);

   fn contains_key(&'a self, key: &Self::Key) -> bool {
      self.0.contains(&key.0, &key.1)
   }
}

pub struct ByodsBinRelInd0_1Write<'a, TBinRel>(&'a mut TBinRel);

impl<'a, TBinRel> RelIndexMerge for ByodsBinRelInd0_1Write<'a, TBinRel> {
   fn move_index_contents(_from: &mut Self, _to: &mut Self) { } //noop
}

impl<'a, TBinRel: ByodsBinRel> RelIndexWrite for ByodsBinRelInd0_1Write<'a, TBinRel> {
   type Key = (TBinRel::T0, TBinRel::T1);
   type Value = ();

   fn index_insert(&mut self, key: Self::Key, (): Self::Value) {
      self.0.insert(key.0, key.1);
   }
}

impl<'a, TBinRel: ByodsBinRel> RelFullIndexWrite for ByodsBinRelInd0_1Write<'a, TBinRel> 
where TBinRel::T0: Clone, TBinRel::T1: Clone
{
   type Key = (TBinRel::T0, TBinRel::T1);
   type Value = ();

   fn insert_if_not_present(&mut self, key: &Self::Key, (): Self::Value) -> bool {
      self.0.insert(key.0.clone(), key.1.clone())
   }
}


pub struct ByodsBinRelIndNone<'a, TBinRel>(&'a TBinRel);

impl<'a, TBinRel: ByodsBinRel> RelIndexRead<'a> for ByodsBinRelIndNone<'a, TBinRel> {
   type Key = ();

   type Value = (&'a TBinRel::T0, &'a TBinRel::T1);
   type IteratorType = IteratorFromDyn<'a, Self::Value>;

   fn index_get(&'a self, (): &Self::Key) -> Option<Self::IteratorType> {
      let res = || self.0.iter_all();
      Some(IteratorFromDyn::new(res))
   }

   fn len(&'a self) -> usize {
      1
   }
}

impl<'a, TBinRel: ByodsBinRel> RelIndexReadAll<'a> for ByodsBinRelIndNone<'a, TBinRel> {
   type Key = ();
   type Value = (&'a TBinRel::T0, &'a TBinRel::T1);

   type ValueIteratorType = TBinRel::AllIter<'a>;
   type AllIteratorType = Once<((), Self::ValueIteratorType)>;

   fn iter_all(&'a self) -> Self::AllIteratorType {
      let res = once(((), self.0.iter_all()));
      res
   }
}


use ascent::to_rel_index::ToRelIndex;
use crate::rel_boilerplate::NoopRelIndexWrite;
macro_rules! to_rel_ind {
   ($name: ident, $key: ty, $val: ty) => {paste::paste!{
      pub struct [<To $name>]<T0, T1>(PhantomData<(T0, T1)>);

      impl<T0, T1> Default for [<To $name>]<T0, T1> {
         fn default() -> Self { Self(PhantomData) }
      }

      impl<T0, T1, Rel> ToRelIndex<Rel> for [<To $name>]<T0, T1> 
      where Rel: ByodsBinRel<T0 = T0, T1 = T1>,
      {
         type RelIndex<'a> = $name<'a, Rel> where Self: 'a, Rel: 'a;
         fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { $name(rel) }

         type RelIndexWrite<'a> = NoopRelIndexWrite<$key, $val> where Self: 'a, Rel: 'a;
         fn to_rel_index_write<'a>(&'a mut self, _rel: &'a mut Rel) -> Self::RelIndexWrite<'a> { 
            NoopRelIndexWrite::default() 
         }
      }
   }};
}

to_rel_ind!(ByodsBinRelIndNone, (), (T0, T1));
to_rel_ind!(ByodsBinRelInd0, (T0, ), (T1, ));
to_rel_ind!(ByodsBinRelInd1, (T1, ), (T0, ));

pub struct ToByodsBinRelInd0_1<T0, T1>(PhantomData<(T0, T1)>);

impl<T0, T1> Default for ToByodsBinRelInd0_1<T0, T1> {
   fn default() -> Self { Self(PhantomData) }
}
impl<T0, T1, Rel> ToRelIndex<Rel> for ToByodsBinRelInd0_1<T0, T1>
where
   Rel: ByodsBinRel<T0 = T0, T1 = T1>,
{
   type RelIndex<'a>  = ByodsBinRelInd0_1<'a, Rel> where Self:'a, Rel:'a;
   fn to_rel_index<'a>(&'a self, rel: &'a Rel) -> Self::RelIndex<'a> { ByodsBinRelInd0_1(rel) }

   type RelIndexWrite<'a>  = ByodsBinRelInd0_1Write<'a, Rel> where Self:'a, Rel:'a;
   fn to_rel_index_write<'a>(&'a mut self, rel: &'a mut Rel) -> Self::RelIndexWrite<'a> {
      ByodsBinRelInd0_1Write(rel)
   }
}

