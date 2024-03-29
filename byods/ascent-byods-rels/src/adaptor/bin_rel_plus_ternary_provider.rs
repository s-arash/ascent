/// Re-export macros in this module for your binary relation data structure provider
/// that you wish to be a ternary relation as well

#[macro_export]
macro_rules! bin_rel_plus_ternary_provider_rel {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, ()) => {
      $crate::fake_vec::FakeVec<($col0, $col1)>
   };

   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, ()) => {
      $crate::fake_vec::FakeVec<($col0, $col1, $col2)>
   };
}
pub use bin_rel_plus_ternary_provider_rel as rel;

#[macro_export]
macro_rules! bin_rel_plus_ternary_provider_full_ind {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelInd0_1<$col0, $col1>
   };

   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd0_1_2<$col0, $col1, $col2>
   };
}
pub use bin_rel_plus_ternary_provider_full_ind as rel_full_ind;

#[macro_export]
macro_rules! bin_rel_plus_ternary_provider_ind {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelInd0<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelInd1<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel::ToByodsBinRelIndNone<$col0, $col1>
   };

   // ternary
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryIndNone<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd0<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd1<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [2], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd2<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 1], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd0_1<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 2], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd0_2<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [1, 2], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd1_2<$col0, $col1, $col2>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 1, 2], $key: ty, $val: ty) => {
      $crate::adaptor::bin_rel_to_ternary::ToBinRelToTernaryInd0_1_2<$col0, $col1, $col2>
   };
}
pub use bin_rel_plus_ternary_provider_ind as rel_ind;

#[macro_export]
macro_rules! bin_rel_plus_ternary_provider_rel_codegen {
   ( $($tt: tt)* ) => { };
}
pub use bin_rel_plus_ternary_provider_rel_codegen as rel_codegen;

mod test {
   #[macro_export]
   macro_rules! bin_rel_plus_ternary_provider_ind_common {
      ($name: ident, ($col0: ty, $col1: ty), $indices: tt, ser, ()) => {
         $crate::adaptor::bin_rel_provider::test::DummyRel<$col0, $col1>
      };

      ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: tt, ser, ()) => {
         $crate::adaptor::bin_rel_to_ternary::BinRelToTernaryWrapper<
            // reverse_map_1 required:
            {$crate::inds_contain!($indices, [1]) || $crate::inds_contain!($indices, [1, 2])},
            // reverse_map_2 required:
            {$crate::inds_contain!($indices, [2]) || $crate::inds_contain!($indices, [1, 2])}, 
            $col0, $col1, $col2, 
            $crate::adaptor::bin_rel_provider::test::DummyRel<$col1, $col2>
         >
      };
   }
   pub use bin_rel_plus_ternary_provider_ind_common as rel_ind_common;

   pub use super::{rel, rel_codegen, rel_full_ind, rel_ind};

   ascent::ascent! {
      #[ds(self)]
      relation foo(u32, u64, u128);

      relation bar(u32, u64, u128);

      foo(*x as u32, *y as u64, *z as u128) <-- foo(y, x, z);

      bar(x, y, z) <-- foo(x, y, z), bar(x, _, z);

      bar(x, y, z) <-- foo(_, y, z), bar(x, y, z), foo(x, _, _);

   }
}