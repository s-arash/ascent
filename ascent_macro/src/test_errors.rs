#![cfg(test)]
use std::{clone, cmp::max, rc::Rc};

use proc_macro2::TokenStream;

use crate::{ascent_impl};


#[test]
fn test_agg_not_stratifiable() {
   let inp = quote!{
      relation foo(i32, i32, i32);
      relation bar(i32, i32);
      relation baz(i32);

      baz(x) <--
         foo(x, _, _),
         !bar(_, x);
         
      bar(x, x + 1) <-- baz(x); 
   };
   let res = ascent_impl(inp, false);
   println!("res: {:?}", res);
   assert!(res.is_err());
   assert!(res.unwrap_err().to_string().contains("bar"));
}
