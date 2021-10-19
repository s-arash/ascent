#![allow(warnings)]
#![feature(bindings_after_at)]
#![feature(decl_macro)]

mod tests;
mod utils;
mod se;

use std::{any::Any, cmp::max, rc::Rc};
use stopwatch::Stopwatch;
use infer::infer;


fn lambda_calc_example() {
   println!("Hello, world!");

   use LambdaCalcExpr::*;
   let id = Lam("x".to_string(), Rc::new(Ref("x".to_string())));
   let foo1 = vec![(1,2,3), (1,2,4), (10, 20, 30)];
   let foo2 = vec![(1, Some(3)), (10, Some(30)), (1, None)];
   let baz = vec![(1, Ref("x".to_string())), (2, id)];
   let mut db = DLProgram::default();
   db.foo1 = foo1;
   db.foo2 = foo2;
   db.baz = baz;
   db.update_indices();


   db.run();

   println!("bar: {:?}", db.bar);
   println!("baz2: {:?}", db.baz2);

}

type Var = String;
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum LambdaCalcExpr{
   Ref(Var),
   Lam(Var, Rc<LambdaCalcExpr>),
   App(Rc<LambdaCalcExpr>, Rc<LambdaCalcExpr>)
}

impl LambdaCalcExpr {
   fn depth(&self) -> usize {
      match self{
         LambdaCalcExpr::Ref(_) => 0,
         LambdaCalcExpr::Lam(x,b) => 1 + b.depth(),
         LambdaCalcExpr::App(f,e) => 1 + max(f.depth(), e.depth())
      }
   }
}

infer!{
   relation bar(i32, i32);
   relation foo1(i32, i32, i32);
   relation foo2(i32, Option<i32>);

   relation baz(i32, LambdaCalcExpr);
   relation baz2(i32, LambdaCalcExpr, i32);


   // bar(x, y + z) <-- foo1(x, y, z), foo2(z) when x < y;
   bar(*x, y + z + w.unwrap_or(0)) <-- foo1(x, y, z), foo2(x, w) if *z != 4;

   baz2(*x, exp.clone(), exp.depth() as i32) <-- baz(x, exp) if *x < 100;
   // bar(1, 2, 3) <--;
}

mod tc{
    use infer::infer;

   infer!{
      relation edge(i32, i32);
      relation path(i32, i32);

      path(*x, *y) <-- edge(x,y);
      path(*x, *z) <-- edge(x,y), path(y, z);
   }
}

fn main(){
   let mut tc = tc::DLProgram::default();

   for i in 0..1000 {
      tc.edge.push((i, i + 1));
   }
   tc.update_indices();

   let mut stopwatch = Stopwatch::start_new();
   tc.run();
   stopwatch.stop();

   println!("tc took {:?}", stopwatch.elapsed());
   println!("path size: {}", tc.path.len());

}