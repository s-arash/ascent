#[macro_export]
macro_rules! eqrel_binary_rel {
   (($col1: ty, $col2: ty), $indices: expr, ser, ()) => {
      // ::std::vec::Vec<($col1, $col2)>
      $crate::fake_vec::FakeVec<($col1, $col2)>
   };

   // par: 
   (($col1: ty, $col2: ty), $indices: expr, par, ()) => {
      // ::std::vec::Vec<($col1, $col2)>
      $crate::fake_vec::FakeVec<($col1, $col2)>
   };
}
pub use eqrel_binary_rel as rel;

#[macro_export]
macro_rules! eqrel_binary_rel_full_ind {
   (($col1: ty, $col2: ty), $indices: expr, ser, (), $key: ty, $val: ty) => {
      $crate::eqrel_ind::ToEqRelInd0_1<$col1>
   };
   
   // par:
   (($col1: ty, $col2: ty), $indices: expr, par, (), $key: ty, $val: ty) => {
      $crate::ceqrel_ind::ToEqRelInd0_1<$col1>
   };
}
pub use eqrel_binary_rel_full_ind as rel_full_ind;

#[macro_export]
macro_rules! eqrel_binary_rel_ind {
   (($col1: ty, $col2: ty), $indices: expr, ser, (), [0], $key: ty, $val: ty) => {
      $crate::eqrel_ind::ToEqRelInd0<$col1>
   };
   (($col1: ty, $col2: ty), $indices: expr, ser, (), [1], $key: ty, $val: ty) => {
      $crate::eqrel_ind::ToEqRelInd0<$col1>
   };
   (($col1: ty, $col2: ty), $indices: expr, ser, (), [], $key: ty, $val: ty) => {
      $crate::eqrel_ind::ToEqRelIndNone<$col1>
   };

   // par:
   (($col1: ty, $col2: ty), $indices: expr, par, (), [0], $key: ty, $val: ty) => {
      $crate::ceqrel_ind::ToEqRelInd0<$col1>
   };
   (($col1: ty, $col2: ty), $indices: expr, par, (), [1], $key: ty, $val: ty) => {
      $crate::ceqrel_ind::ToEqRelInd0<$col1>
   };
   (($col1: ty, $col2: ty), $indices: expr, par, (), [], $key: ty, $val: ty) => {
      $crate::ceqrel_ind::ToEqRelIndNone<$col1>
   };
}
pub use eqrel_binary_rel_ind as rel_ind;

#[macro_export]
macro_rules! eqrel_binary_rel_ind_common {
   (($col1: ty, $col2: ty), $indices: expr, ser, ()) => {
      $crate::eqrel_ind::EqRelIndCommon<$col1>
   };

   // par:
   (($col1: ty, $col2: ty), $indices: expr, par, ()) => {
      $crate::ceqrel_ind::CEqRelIndCommon<$col1>
   };
}
pub use eqrel_binary_rel_ind_common as rel_ind_common;


fn _test_macros() {
   let _x: rel!((u32, u32), [[0,1], [0]], ser, ());
   let _full_ind: rel_full_ind!((u32, u32), [[0, 1], [0]], ser, (), (u32, u32), ());
   let _ind_0: rel_ind!((u32, u32), [[0, 1], [0]], ser, (), [0], (u32,) , (u32,));
}
