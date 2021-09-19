use std::{cmp::max, collections::HashMap, hash, rc::Rc};

use std::collections;
use infer_macro::dl;


// #[derive(Clone, PartialEq, Eq, Debug)]
// enum LambdaCalcExpr{
//    Ref(String),
//    Lam(String, Rc<LambdaCalcExpr>),
//    App(Rc<LambdaCalcExpr>, Rc<LambdaCalcExpr>)
// }

// impl LambdaCalcExpr {
//    fn depth(&self) -> usize {
//       match self{
//          LambdaCalcExpr::Ref(_) => 0,
//          LambdaCalcExpr::Lam(x,b) => 1 + b.depth(),
//          LambdaCalcExpr::App(f,e) => 1 + max(f.depth(), e.depth())
//       }
//    }
// }

// #[test]
// fn test_dl(){
//    dl!{
//       relation bar(i32, i32);
//       relation foo1(i32, i32, i32);
//       relation foo2(i32, Option<i32>);
   
//       relation baz(i32, LambdaCalcExpr);
//       relation baz2(i32, LambdaCalcExpr, i32);
   
   
//       // bar(x, y + z) <-- foo1(x, y, z), foo2(z) when x < y;
//       bar(x, y + z + w.unwrap_or(0)) <-- foo1(x, y, z), foo2(x, w) when z != 4;
   
//       baz2(x, exp.clone(), exp.depth() as i32) <-- baz(x, exp) when x < 100;
//    }   
// }

#[test]
fn test_dl(){
   dl!{
      relation bar(i32, i32);
      relation foo1(i32, i32);
      relation foo2(i32, i32);

   
      bar(*x, y + z) <-- foo1(x, y), foo2(y, z) when *x < 100;
   }   
}

