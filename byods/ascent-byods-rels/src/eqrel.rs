//! equivalence relations for Ascent

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_rel_codegen {
   ( $($tt: tt)* ) => { };
}
pub use eqrel_rel_codegen as rel_codegen;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_rel {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use eqrel_rel as rel;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_rel_full_ind {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel_full_ind!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel_full_ind!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use eqrel_rel_full_ind as rel_full_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_rel_ind {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel_ind!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel_ind!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use eqrel_rel_ind as rel_ind;

#[doc(hidden)]
#[macro_export]
macro_rules! eqrel_rel_ind_common {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel_ind_common!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel_ind_common!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use eqrel_rel_ind_common as rel_ind_common;
