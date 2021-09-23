use std::{cmp::max, collections::HashMap, hash, rc::Rc};


use std::collections;
use infer_macro::dl;

use derive_more::*;
use LambdaCalcExpr::*;

#[derive(Clone, PartialEq, Eq, Debug, IsVariant, Hash)]
enum LambdaCalcExpr{
   Ref(String),
   Lam(String, Rc<LambdaCalcExpr>),
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

fn get_app(exp: &LambdaCalcExpr) -> (&LambdaCalcExpr, &LambdaCalcExpr) {
   match exp {
      App(ref f, ref e) => (f,e),
      _ => panic!("not an App")
   }
}

fn get_lam(exp: &LambdaCalcExpr) -> (&String, &LambdaCalcExpr) {
   match exp {
      Lam(ref x, ref e) => (x,e),
      _ => panic!("not a Lam")
   }
}

fn sub(exp: &LambdaCalcExpr, var: &str, e: &LambdaCalcExpr) -> LambdaCalcExpr {
   todo!()
}
#[test]
fn test_dl1(){
   dl!{
      relation output(LambdaCalcExpr);
      relation input(LambdaCalcExpr);
      relation eval(LambdaCalcExpr, LambdaCalcExpr);
      relation do_eval(LambdaCalcExpr);


      eval(exp.clone(), exp.clone()) <-- do_eval(exp) if exp.is_ref();
      eval(exp.clone(), exp.clone()) <-- do_eval(exp) if exp.is_lam();
      do_eval(get_app(exp).0.clone()) <-- do_eval(exp) if exp.is_app();
      eval(exp.clone(), sub(get_lam(f_res).1, get_lam(f_res).0, get_app(exp).1)) <-- 
         do_eval(exp) if exp.is_app(), 
         eval(f, f_res) if f_res.is_lam() && f == get_app(exp).0 ;
   }   
}

#[test]
fn test_dl2(){
   dl!{
      relation bar(i32, i32);
      relation foo1(i32, i32);
      relation foo2(i32, i32);

      foo1(1, 2);
      foo1(10, 20);
      foo1(0, 2);

      bar(*x, y + z) <-- foo1(x, y) if *x != 0, foo2(y, z);
   }

   let mut prog = DLProgram::default();
   
   let foo2 = vec![
      (2,4),
      (2, 1),
      (20, 40),
      (20, 0),
   ];
   prog.foo2 = foo2;
   prog.update_indices();

   prog.run();

   assert!(prog.bar.contains(&(1, 6)));
   assert!(prog.bar.contains(&(10, 60)));
   assert!(!prog.bar.contains(&(0, 6)));
}

