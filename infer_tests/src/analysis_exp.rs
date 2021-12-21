#![allow(dead_code)]
///! k-cfa on lambda calculus + numbers

use std::collections::BTreeMap;
use std::ops::Deref;
use std::rc::Rc;

use arrayvec::ArrayVec;
use infer::infer;
use infer::infer_run;

use Expr::*;
use infer::lattice::constant_propagation::ConstPropagation;
use crate::utils::*;
use itertools::Itertools;
type Var = &'static str;
type NumConcrete = isize;
type Num = ConstPropagation<NumConcrete>;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Op {
   Add, Mul, Sub, Div
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Expr{
   Ref(Var),
   Lam(Var, Rc<Expr>),
   App(Rc<Expr>, Rc<Expr>),
   Lit(Num),
   Binop(Op, Rc<Expr>, Rc<Expr>)
}
fn app(f: Expr, a: Expr) -> Expr {
   App(Rc::new(f), Rc::new(a))
}
fn lam(x: Var, e: Expr) -> Expr {
   Lam(x, Rc::new(e))
}
fn binop(op: Op, e1: Expr, e2: Expr) -> Expr {
   Binop(op, Rc::new(e1), Rc::new(e2))
}

fn sub(exp: &Expr, var: &str, e: &Expr) -> Expr {
   match exp {
      Ref(x) if *x == var => e.clone(),
      Ref(_x) => exp.clone(),
      App(ef,ea) => app(sub(ef, var, e), sub(ea, var, e)),
      Lam(x, _eb) if *x == var => exp.clone(),
      Lam(x, eb) => lam(x, sub(eb, var, e)),
      Lit(_) => exp.clone(),
      Binop(op, e1, e2) => Binop(*op, Rc::new(sub(e1, var, e)), Rc::new(sub(e2, var, e))),
   }
}

#[allow(non_snake_case)]
fn U() -> Expr {lam("ux", app(Ref("ux"), Ref("ux")))}
#[allow(non_snake_case)]
fn I() -> Expr {lam("ix", Ref("ix"))}

const K: usize = 1;
type Contour = ArrayVec<Expr, K>;
type Lab = Expr;
type Time = (Option<Lab>, Contour);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Either<L,R> {Left(L), Right(R)}
type Addr = (Either<Lab, Var>, Contour);
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
enum Storable {
   Value(Expr, Env),
   Kont(Continuation)
}
use Storable::*;
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
enum Continuation {
   Fn(Expr, Env, Addr),
   Ar(Expr, Env, Addr),
   BinopAr2(Op, Expr, Env, Addr),
   BinopAr1(Op, Num, Addr),
   Mt
}
use Continuation::*;
type Env = Rc<BTreeMap<Var, Addr>>;

fn array_vec_cons<T: Clone, const N: usize>(x: T, array_vec: &ArrayVec<T, N>) -> ArrayVec<T, N> {
   if N == 0 {return ArrayVec::new()}
   let mut res = ArrayVec::new();
   res.insert(0, x);
   let to_take = array_vec.len();
   let to_take = if to_take == N {N - 1} else {to_take};
   res.extend(array_vec[0..to_take].iter().cloned());
   res
}
fn tick(e: &Expr, _ρ: &Env, _a: &Addr, t: &Time, k: &Continuation) -> Time {
   let (lt, δ) = t;
   match e {
      Ref(_) => t.clone(),
      App(..) => (Some(e.clone()), t.1.clone()),
      Lam(..) => match k {
         BinopAr2(_, _, _, _) |
         BinopAr1(_, _, _) | // TODO this line is a judgment call
         Ar(_, _, _) => t.clone(),
         Fn(_, _, _) => (None, match lt {Some(lt) => array_vec_cons(lt.clone(), δ), None => δ.clone()}),
         Mt => panic!("tick on Mt continuation")
      },
      Lit(_) => t.clone(),
      Binop(_, _, _) => t.clone(), // TODO this line is a judgment call
   }
}
fn alloc(e: &Expr, _ρ: &Env, _a: &Addr, t: &Time, k: &Continuation) -> Addr { 
   let (_lt, δ) = t;
   match e {
      Binop(..) |
      App(..) => (Either::Left(e.clone()), δ.clone()),
      Lit(_) |
      Lam(_, _) => match k {
         BinopAr2(_, ek, _, _) | // TODO judgment call
         Ar(ek, _, _) => (Either::Left(ek.clone()), δ.clone()),
         Fn(Lam(x, _e), _, _) => (Either::Right(x), δ.clone()),
         Fn(..) => panic!("alloc() call with Fn continuation with non-lambda expression"),
         BinopAr1(_, _, _) => (Either::Right("IT"), δ.clone()),
         Mt => panic!("alloc() called with Mt continuation"),
      },
      Ref(_) => panic!("alloc with Ref(_) as expression"),
   }
}
fn upd(ρ: &Env, var: Var, addr: Addr) -> Env{
   let mut ρ = ρ.deref().clone();
   ρ.insert(var, addr);
   Rc::new(ρ)
}

fn atom(e: &Expr) -> bool {
   match e {
      Lit(_) => true,
      Lam(_, _) => true,
      _ => false
   }  
}

fn apply_op_concrete(op: Op, x: NumConcrete, y: NumConcrete) -> NumConcrete {
   match op {
      Op::Add => x + y,
      Op::Mul => x * y,
      Op::Sub => x - y,
      Op::Div => x / y,
   }
}

fn apply_op(op: Op, x: &Num, y: &Num) -> Num {
   use infer::lattice::constant_propagation::ConstPropagation::*;
   match (x, y) {
      (Bottom, _) => Bottom,
      (_, Bottom) => Bottom,
      (Constant(x), Constant(y)) => Constant(apply_op_concrete(op, *x, *y)),
      (Constant(0), Top) if op == Op::Mul => Constant(0),
      (Top, Constant(0)) if op == Op::Mul => Constant(0),
      (_, Top) => Top,
      (Top, _) => Top,
   }
}

infer!{
   struct CESK;
   relation σ(Addr, Storable);
   lattice σnum(Addr, Num);
   relation ς(Expr, Env, Addr, Time);

   ς(v.clone(), ρ2.clone(), a.clone(), tick(e, ρ, a, t, k)) <--
      ς(?e@Ref(x), ρ, a, t),
      (σ(ρ[x], ?Value(v, ρ2)) || 
      σnum(ρ[x], lit), let v = Lit(*lit), let ρ2 = ρ),
      σ(a, ?Kont(k));

   σ(b.clone(), Kont(Ar(e1.deref().clone(), ρ.clone(), a.clone()))),
   ς(e0.deref().clone(), ρ.clone(), b, tick(e, ρ, a, t, k)) <--
      ς(?e@App(e0, e1), ρ, a, t),
      σ(a, ?Kont(k)),
      let b = alloc(e, ρ, a, t, k);
   
   σ(b.clone(), Kont(BinopAr2(*op, e2.deref().clone(), ρ.clone(), a.clone()))),
   ς(e1.deref().clone(), ρ.clone(), b, tick(e, ρ, a, t, k)) <--
      ς(?e@Binop(op, e1, e2), ρ, a, t),
      σ(a, ?Kont(k)),
      let b = alloc(e, ρ, a, t, k);

   σ(b.clone(), Kont(Fn(v.clone(), ρ.clone(), c.clone()))),
   ς(e.clone(), ρ2.clone(), b, tick(e, ρ, a, t, k)) <--
      ς(?v@Lam(..), ρ, a, t),
      σ(a, ?Kont(k)),
      if let Ar(e, ρ2, c) = k,
      let b = alloc(v, ρ, a, t, k);
   
   σ(b.clone(), Kont(BinopAr1(*op, *l, c.clone()))),
   ς(e.clone(), ρ2.clone(), b, tick(v, ρ, a, t, k)) <--
      ς(?v@Lit(l), ρ, a, t), 
      σ(a, ?Kont(k)),
      if let BinopAr2(op, e, ρ2, c) = k,
      let b = alloc(v, ρ, a, t, k);

   // TODO bug: lattice requires op_addr.clone()
   σnum(op_addr.clone(), apply_op(*op, l1, l2)), 
   ς(Ref("IT"), upd(ρ, "IT", op_addr) , c.clone(), tick(v2, ρ, a, t, k)) <--
      ς(?v2@Lit(l2), ρ, a, t), 
      σ(a, ?Kont(k)),
      if let BinopAr1(op, l1, c) = k,
      let op_addr = alloc(v2, ρ, a, t, k);

   σ(b.clone(), Value(v.clone(), ρ.clone())),
   ς(e.deref().clone(), upd(&ρ2, x, b.clone()), c.clone(), tick(v, ρ, a, t, k)) <--
      ς(?v@Lam(..), ρ, a, t),
      σ(a, ?Kont(k)),
      if let Fn(Lam(x, e), ρ2, c) = k,
      let b = alloc(v, ρ, a, t, k);
   
   σnum(b.clone(), *lit),
   ς(e.deref().clone(), upd(&ρ2, x, b.clone()), c.clone(), tick(v, ρ, a, t, k)) <--
      ς(?v@Lit(lit), ρ, a, t),
      σ(a, ?Kont(k)),
      if let Fn(Lam(x, e), ρ2, c) = k,
      let b = alloc(v, ρ, a, t, k);

   relation output(Expr, Env);
   relation input(Expr);
   σ((Either::Right("__a0"), Contour::default()), Storable::Kont(Continuation::Mt)),
   ς(e.clone(), Env::default(), (Either::Right("__a0"), Contour::default()), (None, Contour::default())) <--
      input(e);

   output(e.clone(), ρ.clone()) <--
      ς(e, ρ, a, t) if atom(e),
      σ(a, Kont(Continuation::Mt));
}

fn let_(x: &'static str, e0: Expr, e1: Expr) -> Expr {
   app(lam(x, e1), e0)
}
#[allow(non_snake_case)]
fn Y() -> Expr{
   // (λ (f) (let ([u′ (λ (x) (f (λ (v) (let ([xx (x x)]) (xx v)))))])
   //          (u′ u′)))

   // u' = (λ (x) (f (λ (v) (let ([xx (x x)]) (xx v)))))
   let uprime = lam("x", app(Ref("f"), lam("v", let_("xx", app(Ref("x"), Ref("x")), app(Ref("xx"), Ref("v"))))));
   lam("f", let_("u'", uprime, app(Ref("u'"), Ref("u'"))))
}

// #[test]
pub fn analysis_exp(){
   use infer::lattice::constant_propagation::ConstPropagation::*;
   // println!("CESK summary:\n{}", CESK::summary());
   
   // term = (λx. 42 + x) 58
   // let term = app(lam("x", binop(Op::Add, Lit(Constant(42)), Ref("x"))),
   //                Lit(Constant(58)));

   //equal to f, where f = λ x. f(x + 1)
   let f = app(Y(), lam("self", lam("x", app(Ref("self"), binop(Op::Add, Ref("x"), Lit(Constant(1)))))));
   let term = app(f, Lit(Constant(0)));
   let mut cesk = CESK::default();
   cesk.input = vec![(term,)];
   cesk.update_indices();
   cesk.run();
   // println!("ς: \n{}", cesk.ς.iter().map(|x| format!("{:?}", x)).join("\n"));
   // println!("σ: \n{}", cesk.σ.iter().map(|x| format!("{:?}", x)).join("\n"));
   println!("σnum: \n{}", cesk.σnum.iter().map(|x| format!("{:?}", x)).join("\n"));
   
   println!("summary: \n{}", cesk.relation_sizes_summary());
   println!("output: \n{}", cesk.output.iter().map(|x| format!("{:?}", x.0)).join("\n\n"));
}
