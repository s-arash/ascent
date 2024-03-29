//! equivalence relations for Ascent

#[macro_export]
macro_rules! rel_codegen {
   ( $($tt: tt)* ) => { };
}
pub use rel_codegen;

#[macro_export]
macro_rules! rel {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use rel;

#[macro_export]
macro_rules! rel_full_ind {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel_full_ind!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel_full_ind!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use rel_full_ind;

#[macro_export]
macro_rules! rel_ind {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel_ind!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel_ind!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use rel_ind;

#[macro_export]
macro_rules! rel_ind_common {
   ($name: ident, ($col1: ty, $col2: ty), $($rest: tt)*) => {
      $crate::eqrel_binary::rel_ind_common!(($col1, $col2), $($rest)*)
   };
   ($name: ident, ($col1: ty, $col2: ty, $col3: ty), $($rest: tt)*) => {
      $crate::eqrel_ternary::rel_ind_common!(($col1, $col2, $col3), $($rest)*)
   };
}
pub use rel_ind_common;
