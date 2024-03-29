//! transitive relations for Ascent

pub use crate::eqrel::rel_codegen as rel_codegen;

#[doc(hidden)]
#[macro_export]
macro_rules! trrel_rel {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, ()) => {
      $crate::fake_vec::FakeVec<($col0, $col1)>
   };

   // ternary
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, ()) => {
      $crate::fake_vec::FakeVec<($col0, $col1, $col2)>
   };
}
pub use trrel_rel as rel;

#[doc(hidden)]
#[macro_export]
macro_rules! trrel_rel_full_ind {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::trrel_binary_ind::ToTrRelIndFull<$col0>
   };

   // ternary
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2IndFull<$col0, $col1>
   };
}
pub use trrel_rel_full_ind as rel_full_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! trrel_rel_ind {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::trrel_binary_ind::ToTrRelInd0<$col0>
   };
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::trrel_binary_ind::ToTrRelInd1<$col0>
   };
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::trrel_binary_ind::ToTrRelIndNone<$col0>
   };

   // ternary
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2IndNone<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2Ind0<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2Ind1<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [2], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2Ind2<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 1], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2Ind0_1<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 2], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2Ind0_2<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [1, 2], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2Ind1_2<$col0, $col1>
   };
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: expr, ser, (), [0, 1, 2], $key: ty, $val: ty) => {
      $crate::trrel_ternary_ind::ToTrRel2IndFull<$col0, $col1>
   };
}
pub use trrel_rel_ind as rel_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! trrel_rel_ind_common {
   ($name: ident, ($col0: ty, $col1: ty), $indices: expr, ser, ()) => {
      $crate::trrel_binary_ind::TrRelIndCommon<$col0>
   };

   // ternary
   ($name: ident, ($col0: ty, $col1: ty, $col2: ty), $indices: tt, ser, ()) => {
      $crate::trrel_ternary_ind::TrRel2IndCommonWrapper<
         // reverse_map_1 required:
         {$crate::inds_contain!($indices, [1]) || $crate::inds_contain!($indices, [1, 2])},
         // reverse_map_2 required:
         {$crate::inds_contain!($indices, [2]) || $crate::inds_contain!($indices, [1, 2])}, 
         $col0, $col1>
   };
}
pub use trrel_rel_ind_common as rel_ind_common;

#[doc(hidden)]
#[macro_export]
macro_rules! inds_contain {
   ([], $ind: tt) => {
      false
   };
   ([$head: tt], $ind: tt) => {
      ($crate::arrs_eq!($head, $ind))
   };
   ([$head: tt, $($tail: tt),*], $ind: tt) => {
      ($crate::arrs_eq!($head, $ind)) || $crate::inds_contain!([$($tail),*], $ind)
   };
}

#[doc(hidden)]
#[macro_export]
macro_rules! arrs_eq {
   ([], []) => { true };
   ([$x: expr], [$y: expr]) => { $x == $y };
   ([$x: expr, $($xs: expr),*], [$y: expr, $($ys: expr),*]) => {
      $x == $y && $crate::arrs_eq!([$($xs),*], [$($ys),*])
   };
   ([$($xs: expr),*], [$($ys: expr),*]) => { false };
}

#[test]
fn test_arrs_eq() {
   let test1 = arrs_eq!([1, 2], [1, 2]); 
   assert!(test1);
   assert!(!arrs_eq!([1], [1, 2]));
   assert!(arrs_eq!([1], [1]));
   assert!(arrs_eq!([], []));
   assert!(!arrs_eq!([1, 2], [1]));
}

#[cfg(test)]
#[allow(dead_code)]
fn _test_trrel_rel_ind_common() {
   let _ind_common1: crate::trrel::rel_ind_common!(rel, (u64, u32, u32), [[], [0, 1], [0], [0, 1, 2]], ser, ());
   let _ind_common2: crate::trrel::rel_ind_common!(
      rel,
      (u32, u64, u64),
      [[0, 1, 2], [0], [1], [0, 1]],
      ser, 
      ()
   );
}