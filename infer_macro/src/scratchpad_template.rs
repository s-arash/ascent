
use std::{clone, cmp::max, rc::Rc};
use std::ops::Deref;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum LambdaCalcExpr{
   Ref(&'static str),
   Lam(&'static str, Rc<LambdaCalcExpr>),
   App(Rc<LambdaCalcExpr>, Rc<LambdaCalcExpr>)
}

use LambdaCalcExpr::*;

impl LambdaCalcExpr {
   #[allow(dead_code)]
   fn depth(&self) -> usize {
      match self{
         LambdaCalcExpr::Ref(_) => 0,
         LambdaCalcExpr::Lam(_x,b) => 1 + b.depth(),
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
      Ref(_x) => exp.clone(),
      App(ef,ea) => app(sub(ef, var, e), sub(ea, var, e)),
      Lam(x, _eb) if *x == var => exp.clone(),
      Lam(x, eb) => lam(x, sub(eb, var, e))
   }
}

#[allow(non_snake_case)]
fn U() -> LambdaCalcExpr {lam("x", app(Ref("x"), Ref("x")))}
#[allow(non_snake_case)]
fn I() -> LambdaCalcExpr {lam("x", Ref("x"))}

fn min<'a>(inp: impl Iterator<Item = (&'a i32,)>) -> impl Iterator<Item = i32> {
   inp.map(|tuple| tuple.0).min().cloned().into_iter()
}

#[warn(warnings)]
#[allow(unused_imports)]
#[cfg(test)]
fn _test() {
   use infer::aggregators::*;
   use infer::lattice::set::Set;

   todo!("here");
   ;
}