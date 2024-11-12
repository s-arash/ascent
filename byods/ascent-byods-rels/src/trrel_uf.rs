//! reflexive transitive relations for Ascent, supported by [`TrRelUnionFind`](crate::trrel_union_find::TrRelUnionFind)

#[doc(hidden)]
#[macro_export]
macro_rules! trrel_uf_ind_common {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, ()) => {
      $crate::trrel_union_find_binary_ind::TrRelIndCommon<$col0>
   };

   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: tt, ser, ()) => {
      $crate::adaptor::bin_rel_to_ternary::BinRelToTernaryWrapper<
         // reverse_map_1 required:
         {$crate::inds_contain!($indices, [1]) || $crate::inds_contain!($indices, [1, 2])},
         // reverse_map_2 required:
         {$crate::inds_contain!($indices, [2]) || $crate::inds_contain!($indices, [1, 2])},
         $col0, $col1, $col2,
         $crate::trrel_union_find_binary_ind::TrRelIndCommon<$col1>
      >
   };
}
pub use trrel_uf_ind_common as rel_ind_common;

pub use crate::adaptor::bin_rel_plus_ternary_provider::{rel, rel_codegen, rel_full_ind, rel_ind};
