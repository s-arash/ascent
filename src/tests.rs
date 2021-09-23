use std::{cmp::max, collections::HashMap, hash, rc::Rc};


use std::collections;
use infer_macro::dl;

use derive_more::*;
use LambdaCalcExpr::*;

#[derive(Clone, PartialEq, Eq, Debug, IsVariant, Hash)]
enum LambdaCalcExpr{
   Ref(&'static str),
   Lam(&'static str, Rc<LambdaCalcExpr>),
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
fn app(f: LambdaCalcExpr, a: LambdaCalcExpr) -> LambdaCalcExpr {
   App(Rc::new(f), Rc::new(a))
}
fn lam(x: &'static str, e: LambdaCalcExpr) -> LambdaCalcExpr {
   Lam(x, Rc::new(e))
}

fn sub(exp: &LambdaCalcExpr, var: &str, e: &LambdaCalcExpr) -> LambdaCalcExpr {
   match exp {
      Ref(x) if *x == var => e.clone(),
      Ref(x) => exp.clone(),
      App(ef,ea) => app(sub(ef, var, e), sub(ea, var, e)),
      Lam(x, eb) if *x == var => exp.clone(),
      Lam(x, eb) => lam(x, sub(eb, var, e))
   }
}

fn U() -> LambdaCalcExpr {lam("x", app(Ref("x"), Ref("x")))}
fn I() -> LambdaCalcExpr {lam("x", Ref("x"))}

#[test]
fn test_dl_lambda(){
   dl!{
      relation output(LambdaCalcExpr);
      relation input(LambdaCalcExpr);
      relation eval(LambdaCalcExpr, LambdaCalcExpr);
      relation do_eval(LambdaCalcExpr);

      input(app(U(), I()));
      do_eval(exp.clone()) <-- input(exp);
      output(res.clone()) <-- input(exp), eval(exp, res);

      eval(exp.clone(), exp.clone()) <-- do_eval(exp) if let Ref(_) = exp;

      eval(exp.clone(), exp.clone()) <-- do_eval(exp) if let Lam(_,_) = exp;

      do_eval(ef.as_ref().clone()) <-- do_eval(exp) if let App(ef,ea) = exp;

      do_eval(sub(fb, fx, ea)) <-- 
         do_eval(exp) if let App(ef, ea) = exp, 
         eval(ef2, f_res) if let Lam(fx, fb) = f_res if ef.as_ref() == ef2;
      
      eval(exp.clone(), final_res.clone()) <-- 
         do_eval(exp) if let App(ef, ea) = exp, 
         eval(ef2, f_res) if let Lam(fx, fb) = f_res if ef.as_ref() == ef2
         if let sub_res = sub(fb, fx, ea),
         eval(sub_res2, final_res) if sub_res2 == &sub_res;
   };
   let mut prog = DLProgram::default();
   prog.run();   
   println!("output: {:?}", prog.output);
   assert!(prog.output.contains(&(I(),)));
   assert!(prog.output.len() == 1);
}

#[test]
fn test_dl_patterns(){
   dl!{
      relation foo(i32, Option<i32>);
      relation bar(i32, i32);
      foo(1, None);
      foo(2, Some(2));
      foo(3, Some(30));
      bar(*x, *y) <-- foo(x, y_opt) if let Some(y) = y_opt if y != x;
   };
   let mut prog = DLProgram::default();
   prog.run();
   println!("bar: {:?}", prog.bar);
   assert!(prog.bar.contains(&(3,30)));
   assert!(prog.bar.len() == 1);
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

