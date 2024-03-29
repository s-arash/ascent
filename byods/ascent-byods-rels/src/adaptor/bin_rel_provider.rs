//! Re-export macros in this module for your binary relation data structure provider
//! implementd via [`ByodsBinRel`](crate::adaptor::bin_rel::ByodsBinRel)
#[doc(hidden)]
#[macro_export]
macro_rules! bin_rel_provider_rel {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, ()) => {
      $crate::fake_vec::FakeVec<($col0, $col1)>
   };
}
pub use bin_rel_provider_rel as rel;

#[doc(hidden)]
#[macro_export]
macro_rules! bin_rel_provider_full_ind {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelInd0_1<$col0, $col1>
   };
}
pub use bin_rel_provider_full_ind as rel_full_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! bin_rel_provider_ind {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelInd0<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelInd1<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelIndNone<$col0, $col1>
   };
}
pub use bin_rel_provider_ind as rel_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! bin_rel_provider_rel_codegen {
   ( $($tt: tt)* ) => { };
}
pub use bin_rel_provider_rel_codegen as rel_codegen;

pub(crate) mod test {
   use std::iter::Once;
   use std::marker::PhantomData;

   use ascent::internal::RelIndexMerge;

   use crate::adaptor::bin_rel::ByodsBinRel;

   #[doc(hidden)]
   #[macro_export]
   macro_rules! bin_rel_provider_ind_common {
      ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, ()) => {
         $crate::adaptor::bin_rel_provider::test::DummyRel<$col0, $col1>
      };
   }
   pub use bin_rel_provider_ind_common as rel_ind_common;

   pub use super::{rel, rel_codegen, rel_full_ind, rel_ind};

   pub struct DummyRel<T0, T1>(PhantomData<(T0, T1)>);

   impl<T0, T1> Default for DummyRel<T0, T1> {
      fn default() -> Self { Self(Default::default()) }
   }

   impl<T0, T1> RelIndexMerge for DummyRel<T0, T1> {
      fn move_index_contents(_from: &mut Self, _to: &mut Self) { todo!() }
   }

   impl<T0, T1> ByodsBinRel for DummyRel<T0, T1> {
      type T0 = T0;
      type T1 = T1;

      fn contains(&self, _x0: &Self::T0, _x1: &Self::T1) -> bool { todo!() }

      type AllIter<'a> = Once<(&'a T0, &'a T1)> where Self: 'a;
      fn iter_all<'a>(&'a self) -> Self::AllIter<'a> { todo!() }

      fn len_estimate(&self) -> usize { todo!() }

      type Ind0AllIterValsIter<'a> = Once<&'a Self::T1> where Self: 'a;
      type Ind0AllIter<'a> =  Once<(&'a Self::T0, Self::Ind0AllIterValsIter<'a>)> where Self: 'a;
      fn ind0_iter_all<'a>(&'a self) -> Self::Ind0AllIter<'a> { todo!() }
      fn ind0_len_estimate(&self) -> usize { todo!() }

      type Ind0ValsIter<'a> = Once<&'a Self::T1> where Self: 'a;
      fn ind0_index_get<'a>(&'a self, _key: &Self::T0) -> Option<Self::Ind0ValsIter<'a>> { todo!() }

      type Ind1AllIterValsIter<'a> = Once<&'a Self::T0> where Self: 'a;
      type Ind1AllIter<'a> =  Once<(&'a Self::T1, Self::Ind1AllIterValsIter<'a>)> where Self: 'a;
      fn ind1_iter_all<'a>(&'a self) -> Self::Ind1AllIter<'a> { todo!() }
      fn ind1_len_estimate(&self) -> usize { todo!() }

      type Ind1ValsIter<'a> = Once<&'a Self::T0> where Self: 'a;
      fn ind1_index_get<'a>(&'a self, _key: &Self::T1) -> Option<Self::Ind1ValsIter<'a>> { todo!() }
      fn insert(&mut self, _x0: Self::T0, _x1: Self::T1) -> bool { todo!() }
   }

   ascent::ascent! {
      #[ds(super::test)]
      relation foo(u32, usize);

      foo(*x as u32, *y as usize) <-- foo(y, x);
      foo(x, y) <-- foo(x, y), foo(& (*y as u32), &(*x as usize));

   }

}