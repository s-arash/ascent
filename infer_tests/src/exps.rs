#![allow(warnings)]
use infer::infer;
fn exp_type_eq(){
   
}

fn types_eq<T1, T2>() -> bool{
   todo!()
}

// trait TypesEq<TOther> {
//    const ARE_EQUAL : bool;
// }

// impl<T> TypesEq<T> for T {
//    const ARE_EQUAL : bool = true;
// }

// impl<T, U> TypesEq<U> for T {
//     const ARE_EQUAL : bool = false;
// }

fn exp_type_param<T>(inp: T) {
   struct Inner<T> {
      one: T,
   }
   let instance = Inner {one: inp};
   
}

fn exp_rel_traits(){
   trait HasRel<const name: u64> {
      type Fields;
   }

   infer!{
      struct InferProg1;
      relation foo(i32, i32);
   }

   infer!{
      struct InferProg2;
      relation foo(i32, i32);
   }

   impl HasRel<{mangle("foo")}> for InferProg1 {
      type Fields = (i32, i32);
   }

   impl HasRel<{mangle("foo")}> for InferProg2 {
      type Fields = (i32, i32);
   }

   run_combined!(InferProg1::default(), InferProg2::default() on foo);

}
const fn mangle(input: &str) -> u64{
   const_fnv1a_hash::fnv1a_hash_str_64(input)
}

macro run_combined($($b:tt)*) {

}