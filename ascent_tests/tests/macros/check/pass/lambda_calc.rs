use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum LambdaCalcExpr {
   Ref(&'static str),
   Lam(&'static str, Rc<LambdaCalcExpr>),
   App(Rc<LambdaCalcExpr>, Rc<LambdaCalcExpr>),
}

use LambdaCalcExpr::*;

impl LambdaCalcExpr {
   #[allow(dead_code)]
   fn depth(&self) -> usize {
      match self {
         LambdaCalcExpr::Ref(_) => 0,
         LambdaCalcExpr::Lam(_x, b) => 1 + b.depth(),
         LambdaCalcExpr::App(f, e) => 1 + max(f.depth(), e.depth()),
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
      App(ef, ea) => app(sub(ef, var, e), sub(ea, var, e)),
      Lam(x, _eb) if *x == var => exp.clone(),
      Lam(x, eb) => lam(x, sub(eb, var, e)),
   }
}

#[allow(non_snake_case)]
fn U() -> LambdaCalcExpr {
   lam("x", app(Ref("x"), Ref("x")))
}
#[allow(non_snake_case)]
fn I() -> LambdaCalcExpr {
   lam("x", Ref("x"))
}

fn min<'a>(inp: impl Iterator<Item = (&'a i32,)>) -> impl Iterator<Item = i32> {
   inp.map(|tuple| tuple.0).min().cloned().into_iter()
}

ascent::ascent! {
   relation output(LambdaCalcExpr);
   relation input(LambdaCalcExpr);
   relation do_eval(LambdaCalcExpr);
   relation eval(LambdaCalcExpr, LambdaCalcExpr);

   input(app(U(), I()));
   do_eval(exp.clone()) <-- input(exp);
   output(res.clone()) <-- input(exp), eval(exp, res);

   eval(exp.clone(), exp.clone()) <-- do_eval(?exp @Ref(_));

   eval(exp.clone(), exp.clone()) <-- do_eval(exp), if let Lam(_,_) = exp;

   do_eval(ef.as_ref().clone()) <-- do_eval(?App(ef,_ea));

   do_eval(sub(fb, fx, ea)) <--
      do_eval(?App(ef, ea)),
      eval(ef.deref(), ?Lam(fx, fb));

   eval(exp.clone(), final_res.clone()) <--
      do_eval(?exp @ App(ef, ea)), // this requires nightly
      eval(ef.deref(), ?Lam(fx, fb)),
      eval(sub(fb, fx, ea), final_res);
}

fn main() {}
