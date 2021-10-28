

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