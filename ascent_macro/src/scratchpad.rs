use std::ops::Deref;
use std::{clone, cmp::max, rc::Rc};

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

#[warn(warnings)]
#[allow(unused_imports)]
#[allow(redundant_semicolons)]
#[cfg(test)]
fn _test() {
   use ascent::aggregators::*;
   use ascent::lattice::set::Set;

   pub struct AscentProgram {
      pub bar3: Vec<(i32, i32, i32)>,
      #[allow(non_snake_case)]
      pub bar3_indices_0: ascent::internal::RelIndexType<(i32,)>,
      #[allow(non_snake_case)]
      pub bar3_indices_0_1_2: ascent::internal::RelFullIndexType<(i32, i32, i32)>,
      pub bar_refl: Vec<(i32,)>,
      #[allow(non_snake_case)]
      pub bar_refl_indices_0: ascent::internal::RelFullIndexType<(i32,)>,
      pub foo: Vec<(i32,)>,
      #[allow(non_snake_case)]
      pub foo_indices_0: ascent::internal::RelFullIndexType<(i32,)>,
      #[allow(non_snake_case)]
      pub foo_indices_: ascent::internal::RelIndexType<()>,
      pub bar: Vec<(i32, i32)>,
      #[allow(non_snake_case)]
      pub bar_indices_: ascent::internal::RelIndexType<()>,
      #[allow(non_snake_case)]
      pub bar_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32)>,
      pub res: Vec<(i32,)>,
      #[allow(non_snake_case)]
      pub res_indices_0: ascent::internal::RelFullIndexType<(i32,)>,
      pub bar3_res: Vec<(i32,)>,
      #[allow(non_snake_case)]
      pub bar3_res_indices_: ascent::internal::RelIndexType<()>,
      #[allow(non_snake_case)]
      pub bar3_res_indices_0: ascent::internal::RelFullIndexType<(i32,)>,
      pub scc0_duration: std::time::Duration,
      pub scc1_duration: std::time::Duration,
      pub scc2_duration: std::time::Duration,
      pub scc3_duration: std::time::Duration,
      pub scc4_duration: std::time::Duration,
      pub scc5_duration: std::time::Duration,
      pub scc6_duration: std::time::Duration,
      pub scc7_duration: std::time::Duration,
      pub scc8_duration: std::time::Duration,
   }
   impl AscentProgram {
      #[allow(unused_imports)]
      #[doc = "Runs the Ascent program to a fixed point."]
      pub fn run(&mut self) {
         macro_rules! __check_return_conditions {
            () => {};
         }
         use ascent::internal::RelIndexRead;
         use ascent::internal::RelIndexReadAll;
         use core::cmp::PartialEq;
         self.update_indices_priv();
         let _self = self;
         ascent::internal::comment("scc 0");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar3_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<(i32, i32, i32)> =
               &mut _self.bar3_indices_0_1_2;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_total: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_new: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_indices_0_delta: &mut ascent::internal::RelIndexType<(i32,)> = &mut _self.bar3_indices_0;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_total: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_new: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar3 <-- ");
               let __new_row: (i32, i32, i32) = (1, 1, 1);
               let __new_row_ind = _self.bar3.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar3_indices_0_1_2_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar3_indices_0_1_2_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar3_indices_0_1_2_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(
                     &mut bar3_indices_0_new,
                     (__new_row.0.clone(),),
                     __new_row_ind,
                  );
                  _self.bar3.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               __check_return_conditions!();
            }
            _self.bar3_indices_0_1_2 = bar3_indices_0_1_2_total;
            _self.bar3_indices_0 = bar3_indices_0_total;
            _self.scc0_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 1");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar3_indices_0_delta: &mut ascent::internal::RelIndexType<(i32,)> = &mut _self.bar3_indices_0;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_total: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_new: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<(i32, i32, i32)> =
               &mut _self.bar3_indices_0_1_2;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_total: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_new: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar3 <-- ");
               let __new_row: (i32, i32, i32) = (2, 1, 3);
               let __new_row_ind = _self.bar3.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar3_indices_0_1_2_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar3_indices_0_1_2_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar3_indices_0_1_2_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(
                     &mut bar3_indices_0_new,
                     (__new_row.0.clone(),),
                     __new_row_ind,
                  );
                  _self.bar3.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               __check_return_conditions!();
            }
            _self.bar3_indices_0 = bar3_indices_0_total;
            _self.bar3_indices_0_1_2 = bar3_indices_0_1_2_total;
            _self.scc1_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 2");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar3_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<(i32, i32, i32)> =
               &mut _self.bar3_indices_0_1_2;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_total: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_new: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_indices_0_delta: &mut ascent::internal::RelIndexType<(i32,)> = &mut _self.bar3_indices_0;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_total: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_new: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar3 <-- ");
               let __new_row: (i32, i32, i32) = (1, 2, 3);
               let __new_row_ind = _self.bar3.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar3_indices_0_1_2_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar3_indices_0_1_2_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar3_indices_0_1_2_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(
                     &mut bar3_indices_0_new,
                     (__new_row.0.clone(),),
                     __new_row_ind,
                  );
                  _self.bar3.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               __check_return_conditions!();
            }
            _self.bar3_indices_0_1_2 = bar3_indices_0_1_2_total;
            _self.bar3_indices_0 = bar3_indices_0_total;
            _self.scc2_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 3");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar3_indices_0_delta: &mut ascent::internal::RelIndexType<(i32,)> = &mut _self.bar3_indices_0;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_total: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_new: ascent::internal::RelIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<(i32, i32, i32)> =
               &mut _self.bar3_indices_0_1_2;
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_total: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_indices_0_1_2_new: ascent::internal::RelFullIndexType<(i32, i32, i32)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar3 <-- ");
               let __new_row: (i32, i32, i32) = (10, 10, 11);
               let __new_row_ind = _self.bar3.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar3_indices_0_1_2_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar3_indices_0_1_2_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar3_indices_0_1_2_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(
                     &mut bar3_indices_0_new,
                     (__new_row.0.clone(),),
                     __new_row_ind,
                  );
                  _self.bar3.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar3_indices_0_delta, &mut bar3_indices_0_total);
               std::mem::swap(&mut bar3_indices_0_new, bar3_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_indices_0_1_2_delta,
                  &mut bar3_indices_0_1_2_total,
               );
               std::mem::swap(&mut bar3_indices_0_1_2_new, bar3_indices_0_1_2_delta);
               __check_return_conditions!();
            }
            _self.bar3_indices_0 = bar3_indices_0_total;
            _self.bar3_indices_0_1_2 = bar3_indices_0_1_2_total;
            _self.scc3_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 4");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let foo_indices_0_delta: &mut ascent::internal::RelFullIndexType<(i32,)> = &mut _self.foo_indices_0;
            #[allow(non_snake_case)]
            let mut foo_indices_0_total: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut foo_indices_0_new: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let foo_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.foo_indices_;
            #[allow(non_snake_case)]
            let mut foo_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut foo_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("foo <-- ");
               let __new_row: (i32,) = (3,);
               let __new_row_ind = _self.foo.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&foo_indices_0_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(foo_indices_0_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut foo_indices_0_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(&mut foo_indices__new, (), __new_row_ind);
                  _self.foo.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(foo_indices_0_delta, &mut foo_indices_0_total);
               std::mem::swap(&mut foo_indices_0_new, foo_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(foo_indices__delta, &mut foo_indices__total);
               std::mem::swap(&mut foo_indices__new, foo_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(foo_indices_0_delta, &mut foo_indices_0_total);
               std::mem::swap(&mut foo_indices_0_new, foo_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(foo_indices__delta, &mut foo_indices__total);
               std::mem::swap(&mut foo_indices__new, foo_indices__delta);
               __check_return_conditions!();
            }
            _self.foo_indices_0 = foo_indices_0_total;
            _self.foo_indices_ = foo_indices__total;
            _self.scc4_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 5");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<(i32, i32)> = &mut _self.bar_indices_0_1;
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let bar_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.bar_indices_;
            #[allow(non_snake_case)]
            let mut bar_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar <-- ");
               let __new_row: (i32, i32) = (1, 1);
               let __new_row_ind = _self.bar.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar_indices_0_1_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar_indices_0_1_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar_indices_0_1_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(&mut bar_indices__new, (), __new_row_ind);
                  _self.bar.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               __check_return_conditions!();
            }
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.bar_indices_ = bar_indices__total;
            _self.scc5_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 6");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.bar_indices_;
            #[allow(non_snake_case)]
            let mut bar_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<(i32, i32)> = &mut _self.bar_indices_0_1;
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar <-- ");
               let __new_row: (i32, i32) = (3, 3);
               let __new_row_ind = _self.bar.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar_indices_0_1_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar_indices_0_1_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar_indices_0_1_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(&mut bar_indices__new, (), __new_row_ind);
                  _self.bar.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               __check_return_conditions!();
            }
            _self.bar_indices_ = bar_indices__total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.scc6_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 7");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<(i32, i32)> = &mut _self.bar_indices_0_1;
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let bar_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.bar_indices_;
            #[allow(non_snake_case)]
            let mut bar_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar <-- ");
               let __new_row: (i32, i32) = (2, 1);
               let __new_row_ind = _self.bar.len();
               if !::ascent::internal::RelFullIndexRead::contains_key(&bar_indices_0_1_total, &__new_row)
                  && !::ascent::internal::RelFullIndexRead::contains_key(bar_indices_0_1_delta, &__new_row)
                  && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                     &mut bar_indices_0_1_new,
                     &__new_row,
                     __new_row_ind,
                  )
               {
                  ::ascent::internal::RelIndexWrite::index_insert(&mut bar_indices__new, (), __new_row_ind);
                  _self.bar.push(__new_row);
                  __changed = true;
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               __check_return_conditions!();
            }
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.bar_indices_ = bar_indices__total;
            _self.scc7_duration += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 8");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #[allow(non_snake_case)]
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<(i32, i32)> = &mut _self.bar_indices_0_1;
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<(i32, i32)> = Default::default();
            #[allow(non_snake_case)]
            let bar_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.bar_indices_;
            #[allow(non_snake_case)]
            let mut bar_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let foo_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.foo_indices_;
            #[allow(non_snake_case)]
            let mut foo_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut foo_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let foo_indices_0_delta: &mut ascent::internal::RelFullIndexType<(i32,)> = &mut _self.foo_indices_0;
            #[allow(non_snake_case)]
            let mut foo_indices_0_total: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut foo_indices_0_new: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_res_indices_0_delta: &mut ascent::internal::RelFullIndexType<(i32,)> =
               &mut _self.bar3_res_indices_0;
            #[allow(non_snake_case)]
            let mut bar3_res_indices_0_total: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_res_indices_0_new: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_res_indices__delta: &mut ascent::internal::RelIndexType<()> = &mut _self.bar3_res_indices_;
            #[allow(non_snake_case)]
            let mut bar3_res_indices__total: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let mut bar3_res_indices__new: ascent::internal::RelIndexType<()> = Default::default();
            #[allow(non_snake_case)]
            let bar_refl_indices_0_delta: &mut ascent::internal::RelFullIndexType<(i32,)> =
               &mut _self.bar_refl_indices_0;
            #[allow(non_snake_case)]
            let mut bar_refl_indices_0_total: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut bar_refl_indices_0_new: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let res_indices_0_delta: &mut ascent::internal::RelFullIndexType<(i32,)> = &mut _self.res_indices_0;
            #[allow(non_snake_case)]
            let mut res_indices_0_total: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let mut res_indices_0_new: ascent::internal::RelFullIndexType<(i32,)> = Default::default();
            #[allow(non_snake_case)]
            let bar3_indices_0_total: &mut ascent::internal::RelIndexType<(i32,)> = &mut _self.bar3_indices_0;
            #[allow(unused_assignments, unused_variables)]
            loop {
               let mut __changed = false;
               ascent::internal::comment("foo, bar <-- bar3_res_indices__delta, let ⋯, bar_refl_indices_0_total+delta");
               if let Some(__matching) = bar3_res_indices__delta.index_get(&()) {
                  for __ind in __matching {
                     let __row = &_self.bar3_res[__ind].clone();
                     let x = &__row.0;
                     let y = x - 2;
                     if let Some(__matching) =
                        ascent::internal::RelIndexCombined::new(&bar_refl_indices_0_total, bar_refl_indices_0_delta)
                           .index_get(&(x.clone(),))
                     {
                        for __ind in __matching {
                           let __row = &_self.bar_refl[__ind].clone();
                           let __new_row: (i32,) = (ascent::internal::Convert::convert(x),);
                           let __new_row_ind = _self.foo.len();
                           if !::ascent::internal::RelFullIndexRead::contains_key(&foo_indices_0_total, &__new_row)
                              && !::ascent::internal::RelFullIndexRead::contains_key(foo_indices_0_delta, &__new_row)
                              && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                 &mut foo_indices_0_new,
                                 &__new_row,
                                 __new_row_ind,
                              )
                           {
                              ::ascent::internal::RelIndexWrite::index_insert(&mut foo_indices__new, (), __new_row_ind);
                              _self.foo.push(__new_row);
                              __changed = true;
                           }
                           let __new_row: (i32, i32) = (ascent::internal::Convert::convert(x), x + 1);
                           let __new_row_ind = _self.bar.len();
                           if !::ascent::internal::RelFullIndexRead::contains_key(&bar_indices_0_1_total, &__new_row)
                              && !::ascent::internal::RelFullIndexRead::contains_key(bar_indices_0_1_delta, &__new_row)
                              && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                 &mut bar_indices_0_1_new,
                                 &__new_row,
                                 __new_row_ind,
                              )
                           {
                              ::ascent::internal::RelIndexWrite::index_insert(&mut bar_indices__new, (), __new_row_ind);
                              _self.bar.push(__new_row);
                              __changed = true;
                           }
                        }
                     }
                  }
               }
               ascent::internal::comment("foo, bar <-- bar3_res_indices__total, let ⋯, bar_refl_indices_0_delta");
               if let Some(__matching) = bar3_res_indices__total.index_get(&()) {
                  for __ind in __matching {
                     let __row = &_self.bar3_res[__ind].clone();
                     let x = &__row.0;
                     let y = x - 2;
                     if let Some(__matching) = bar_refl_indices_0_delta.index_get(&(x.clone(),)) {
                        for __ind in __matching {
                           let __row = &_self.bar_refl[__ind].clone();
                           let __new_row: (i32,) = (ascent::internal::Convert::convert(x),);
                           let __new_row_ind = _self.foo.len();
                           if !::ascent::internal::RelFullIndexRead::contains_key(&foo_indices_0_total, &__new_row)
                              && !::ascent::internal::RelFullIndexRead::contains_key(foo_indices_0_delta, &__new_row)
                              && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                 &mut foo_indices_0_new,
                                 &__new_row,
                                 __new_row_ind,
                              )
                           {
                              ::ascent::internal::RelIndexWrite::index_insert(&mut foo_indices__new, (), __new_row_ind);
                              _self.foo.push(__new_row);
                              __changed = true;
                           }
                           let __new_row: (i32, i32) = (ascent::internal::Convert::convert(x), x + 1);
                           let __new_row_ind = _self.bar.len();
                           if !::ascent::internal::RelFullIndexRead::contains_key(&bar_indices_0_1_total, &__new_row)
                              && !::ascent::internal::RelFullIndexRead::contains_key(bar_indices_0_1_delta, &__new_row)
                              && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                 &mut bar_indices_0_1_new,
                                 &__new_row,
                                 __new_row_ind,
                              )
                           {
                              ::ascent::internal::RelIndexWrite::index_insert(&mut bar_indices__new, (), __new_row_ind);
                              _self.bar.push(__new_row);
                              __changed = true;
                           }
                        }
                     }
                  }
               }
               ascent::internal::comment("bar_refl <-- bar_indices__delta");
               if let Some(__matching) = bar_indices__delta.index_get(&()) {
                  for __ind in __matching {
                     let __row = &_self.bar[__ind].clone();
                     let x = &__row.0;
                     let x_ = &__row.1;
                     if x_.eq(&(x)) {
                        let __new_row: (i32,) = (*x,);
                        let __new_row_ind = _self.bar_refl.len();
                        if !::ascent::internal::RelFullIndexRead::contains_key(&bar_refl_indices_0_total, &__new_row)
                           && !::ascent::internal::RelFullIndexRead::contains_key(bar_refl_indices_0_delta, &__new_row)
                           && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                              &mut bar_refl_indices_0_new,
                              &__new_row,
                              __new_row_ind,
                           )
                        {
                           _self.bar_refl.push(__new_row);
                           __changed = true;
                        }
                     }
                  }
               }
               ascent::internal::comment("res <-- foo_indices__delta, bar_indices_0_1_total+delta");
               if let Some(__matching) = foo_indices__delta.index_get(&()) {
                  for __ind in __matching {
                     let __row = &_self.foo[__ind].clone();
                     let x = &__row.0;
                     if let Some(__matching) =
                        ascent::internal::RelIndexCombined::new(&bar_indices_0_1_total, bar_indices_0_1_delta)
                           .index_get(&(x.clone(), x.clone()))
                     {
                        for __ind in __matching {
                           let __row = &_self.bar[__ind].clone();
                           let __new_row: (i32,) = (*x,);
                           let __new_row_ind = _self.res.len();
                           if !::ascent::internal::RelFullIndexRead::contains_key(&res_indices_0_total, &__new_row)
                              && !::ascent::internal::RelFullIndexRead::contains_key(res_indices_0_delta, &__new_row)
                              && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                 &mut res_indices_0_new,
                                 &__new_row,
                                 __new_row_ind,
                              )
                           {
                              _self.res.push(__new_row);
                              __changed = true;
                           }
                        }
                     }
                  }
               }
               ascent::internal::comment("res <-- foo_indices__total, bar_indices_0_1_delta");
               if let Some(__matching) = foo_indices__total.index_get(&()) {
                  for __ind in __matching {
                     let __row = &_self.foo[__ind].clone();
                     let x = &__row.0;
                     if let Some(__matching) = bar_indices_0_1_delta.index_get(&(x.clone(), x.clone())) {
                        for __ind in __matching {
                           let __row = &_self.bar[__ind].clone();
                           let __new_row: (i32,) = (*x,);
                           let __new_row_ind = _self.res.len();
                           if !::ascent::internal::RelFullIndexRead::contains_key(&res_indices_0_total, &__new_row)
                              && !::ascent::internal::RelFullIndexRead::contains_key(res_indices_0_delta, &__new_row)
                              && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                 &mut res_indices_0_new,
                                 &__new_row,
                                 __new_row_ind,
                              )
                           {
                              _self.res.push(__new_row);
                              __changed = true;
                           }
                        }
                     }
                  }
               }
               ascent :: internal :: comment ("bar3_res <-- bar3_indices_0_total, res_indices_0_delta, if ⋯, bar3_res_indices_0_total+delta [SIMPLE JOIN]") ;
               if bar3_indices_0_total.len() <= res_indices_0_delta.len() {
                  for (__cl1_joined_columns, __cl1_tuple_indices) in bar3_indices_0_total.iter_all() {
                     let x = &__cl1_joined_columns.0;
                     if let Some(__matching) = res_indices_0_delta.index_get(&(x.clone(),)) {
                        for cl1_ind in __cl1_tuple_indices {
                           let __row = &_self.bar3[cl1_ind].clone();
                           let y = &__row.1;
                           let z = &__row.2;
                           for __ind in __matching.clone() {
                              let __row = &_self.res[__ind].clone();
                              if x > y {
                                 if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                    &bar3_res_indices_0_total,
                                    bar3_res_indices_0_delta,
                                 )
                                 .index_get(&(y.clone(),))
                                 {
                                    for __ind in __matching {
                                       let __row = &_self.bar3_res[__ind].clone();
                                       let __new_row: (i32,) = (*x,);
                                       let __new_row_ind = _self.bar3_res.len();
                                       if !::ascent::internal::RelFullIndexRead::contains_key(
                                          &bar3_res_indices_0_total,
                                          &__new_row,
                                       ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                          bar3_res_indices_0_delta,
                                          &__new_row,
                                       ) && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut bar3_res_indices_0_new,
                                          &__new_row,
                                          __new_row_ind,
                                       ) {
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut bar3_res_indices__new,
                                             (),
                                             __new_row_ind,
                                          );
                                          _self.bar3_res.push(__new_row);
                                          __changed = true;
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               } else {
                  for (__cl1_joined_columns, __cl1_tuple_indices) in res_indices_0_delta.iter_all() {
                     let x = &__cl1_joined_columns.0;
                     if let Some(__matching) = bar3_indices_0_total.index_get(&(x.clone(),)) {
                        for cl1_ind in __cl1_tuple_indices {
                           let __row = &_self.res[cl1_ind].clone();
                           for __ind in __matching.clone() {
                              let __row = &_self.bar3[__ind].clone();
                              let y = &__row.1;
                              let z = &__row.2;
                              if x > y {
                                 if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                    &bar3_res_indices_0_total,
                                    bar3_res_indices_0_delta,
                                 )
                                 .index_get(&(y.clone(),))
                                 {
                                    for __ind in __matching {
                                       let __row = &_self.bar3_res[__ind].clone();
                                       let __new_row: (i32,) = (*x,);
                                       let __new_row_ind = _self.bar3_res.len();
                                       if !::ascent::internal::RelFullIndexRead::contains_key(
                                          &bar3_res_indices_0_total,
                                          &__new_row,
                                       ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                          bar3_res_indices_0_delta,
                                          &__new_row,
                                       ) && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut bar3_res_indices_0_new,
                                          &__new_row,
                                          __new_row_ind,
                                       ) {
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut bar3_res_indices__new,
                                             (),
                                             __new_row_ind,
                                          );
                                          _self.bar3_res.push(__new_row);
                                          __changed = true;
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
               ascent :: internal :: comment ("bar3_res <-- bar3_indices_0_total, res_indices_0_total, if ⋯, bar3_res_indices_0_delta [SIMPLE JOIN]") ;
               if bar3_indices_0_total.len() <= res_indices_0_total.len() {
                  for (__cl1_joined_columns, __cl1_tuple_indices) in bar3_indices_0_total.iter_all() {
                     let x = &__cl1_joined_columns.0;
                     if let Some(__matching) = res_indices_0_total.index_get(&(x.clone(),)) {
                        for cl1_ind in __cl1_tuple_indices {
                           let __row = &_self.bar3[cl1_ind].clone();
                           let y = &__row.1;
                           let z = &__row.2;
                           for __ind in __matching.clone() {
                              let __row = &_self.res[__ind].clone();
                              if x > y {
                                 if let Some(__matching) = bar3_res_indices_0_delta.index_get(&(y.clone(),)) {
                                    for __ind in __matching {
                                       let __row = &_self.bar3_res[__ind].clone();
                                       let __new_row: (i32,) = (*x,);
                                       let __new_row_ind = _self.bar3_res.len();
                                       if !::ascent::internal::RelFullIndexRead::contains_key(
                                          &bar3_res_indices_0_total,
                                          &__new_row,
                                       ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                          bar3_res_indices_0_delta,
                                          &__new_row,
                                       ) && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut bar3_res_indices_0_new,
                                          &__new_row,
                                          __new_row_ind,
                                       ) {
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut bar3_res_indices__new,
                                             (),
                                             __new_row_ind,
                                          );
                                          _self.bar3_res.push(__new_row);
                                          __changed = true;
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               } else {
                  for (__cl1_joined_columns, __cl1_tuple_indices) in res_indices_0_total.iter_all() {
                     let x = &__cl1_joined_columns.0;
                     if let Some(__matching) = bar3_indices_0_total.index_get(&(x.clone(),)) {
                        for cl1_ind in __cl1_tuple_indices {
                           let __row = &_self.res[cl1_ind].clone();
                           for __ind in __matching.clone() {
                              let __row = &_self.bar3[__ind].clone();
                              let y = &__row.1;
                              let z = &__row.2;
                              if x > y {
                                 if let Some(__matching) = bar3_res_indices_0_delta.index_get(&(y.clone(),)) {
                                    for __ind in __matching {
                                       let __row = &_self.bar3_res[__ind].clone();
                                       let __new_row: (i32,) = (*x,);
                                       let __new_row_ind = _self.bar3_res.len();
                                       if !::ascent::internal::RelFullIndexRead::contains_key(
                                          &bar3_res_indices_0_total,
                                          &__new_row,
                                       ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                          bar3_res_indices_0_delta,
                                          &__new_row,
                                       ) && ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut bar3_res_indices_0_new,
                                          &__new_row,
                                          __new_row_ind,
                                       ) {
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut bar3_res_indices__new,
                                             (),
                                             __new_row_ind,
                                          );
                                          _self.bar3_res.push(__new_row);
                                          __changed = true;
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_indices_0_1_delta,
                  &mut bar_indices_0_1_total,
               );
               std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(bar_indices__delta, &mut bar_indices__total);
               std::mem::swap(&mut bar_indices__new, bar_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(foo_indices__delta, &mut foo_indices__total);
               std::mem::swap(&mut foo_indices__new, foo_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(foo_indices_0_delta, &mut foo_indices_0_total);
               std::mem::swap(&mut foo_indices_0_new, foo_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_res_indices_0_delta,
                  &mut bar3_res_indices_0_total,
               );
               std::mem::swap(&mut bar3_res_indices_0_new, bar3_res_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar3_res_indices__delta,
                  &mut bar3_res_indices__total,
               );
               std::mem::swap(&mut bar3_res_indices__new, bar3_res_indices__delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(
                  bar_refl_indices_0_delta,
                  &mut bar_refl_indices_0_total,
               );
               std::mem::swap(&mut bar_refl_indices_0_new, bar_refl_indices_0_delta);
               ::ascent::internal::RelIndexWrite::move_index_contents(res_indices_0_delta, &mut res_indices_0_total);
               std::mem::swap(&mut res_indices_0_new, res_indices_0_delta);
               if !__changed {
                  break;
               }
               __check_return_conditions!();
            }
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.bar_indices_ = bar_indices__total;
            _self.foo_indices_ = foo_indices__total;
            _self.foo_indices_0 = foo_indices_0_total;
            _self.bar3_res_indices_0 = bar3_res_indices_0_total;
            _self.bar3_res_indices_ = bar3_res_indices__total;
            _self.bar_refl_indices_0 = bar_refl_indices_0_total;
            _self.res_indices_0 = res_indices_0_total;
            _self.scc8_duration += _scc_start_time.elapsed();
         }
      }
      fn update_indices_priv(&mut self) {
         for (i, tuple) in self.bar3.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar3_indices_0, selection_tuple, i);
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar3_indices_0_1_2, selection_tuple, i);
         }
         for (i, tuple) in self.bar_refl.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar_refl_indices_0, selection_tuple, i);
         }
         for (i, tuple) in self.foo.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            ascent::internal::RelIndexWrite::index_insert(&mut self.foo_indices_0, selection_tuple, i);
            let selection_tuple = ();
            ascent::internal::RelIndexWrite::index_insert(&mut self.foo_indices_, selection_tuple, i);
         }
         for (i, tuple) in self.bar.iter().enumerate() {
            let selection_tuple = ();
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar_indices_, selection_tuple, i);
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar_indices_0_1, selection_tuple, i);
         }
         for (i, tuple) in self.res.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            ascent::internal::RelIndexWrite::index_insert(&mut self.res_indices_0, selection_tuple, i);
         }
         for (i, tuple) in self.bar3_res.iter().enumerate() {
            let selection_tuple = ();
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar3_res_indices_, selection_tuple, i);
            let selection_tuple = (tuple.0.clone(),);
            ascent::internal::RelIndexWrite::index_insert(&mut self.bar3_res_indices_0, selection_tuple, i);
         }
      }
      #[deprecated = "Explicit call to update_indices not required anymore."]
      pub fn update_indices(&mut self) {
         self.update_indices_priv();
      }
      #[allow(unused_imports)]
      fn type_constaints() {
         let _type_constraints: ascent::internal::TypeConstraints<i32>;
      }
      pub fn summary() -> &'static str {
         "scc 0, is_looping: false:\n  bar3 <-- \n  dynamic relations: bar3\nscc 1, is_looping: false:\n  bar3 <-- \n  dynamic relations: bar3\nscc 2, is_looping: false:\n  bar3 <-- \n  dynamic relations: bar3\nscc 3, is_looping: false:\n  bar3 <-- \n  dynamic relations: bar3\nscc 4, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 5, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 6, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 7, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 8, is_looping: true:\n  foo, bar <-- bar3_res_indices__delta, let ⋯, bar_refl_indices_0_total+delta\n  foo, bar <-- bar3_res_indices__total, let ⋯, bar_refl_indices_0_delta\n  bar_refl <-- bar_indices__delta\n  res <-- foo_indices__delta, bar_indices_0_1_total+delta\n  res <-- foo_indices__total, bar_indices_0_1_delta\n  bar3_res <-- bar3_indices_0_total, res_indices_0_delta, if ⋯, bar3_res_indices_0_total+delta [SIMPLE JOIN]\n  bar3_res <-- bar3_indices_0_total, res_indices_0_total, if ⋯, bar3_res_indices_0_delta [SIMPLE JOIN]\n  dynamic relations: bar, foo, bar3_res, bar_refl, res\n"
      }
      pub fn relation_sizes_summary(&self) -> String {
         use std::fmt::Write;
         let mut res = String::new();
         writeln!(&mut res, "{} size: {}", "bar3", self.bar3.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "bar_refl", self.bar_refl.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "foo", self.foo.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "bar", self.bar.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "res", self.res.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "bar3_res", self.bar3_res.len()).unwrap();
         res
      }
      pub fn scc_times_summary(&self) -> String {
         use std::fmt::Write;
         let mut res = String::new();
         writeln!(&mut res, "scc {} time: {:?}", "0", self.scc0_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "1", self.scc1_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "2", self.scc2_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "3", self.scc3_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "4", self.scc4_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "5", self.scc5_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "6", self.scc6_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "7", self.scc7_duration).unwrap();
         writeln!(&mut res, "scc {} time: {:?}", "8", self.scc8_duration).unwrap();
         res
      }
   }
   impl Default for AscentProgram {
      fn default() -> Self {
         let mut _self = AscentProgram {
            bar3: Default::default(),
            bar3_indices_0: Default::default(),
            bar3_indices_0_1_2: Default::default(),
            bar_refl: Default::default(),
            bar_refl_indices_0: Default::default(),
            foo: Default::default(),
            foo_indices_0: Default::default(),
            foo_indices_: Default::default(),
            bar: Default::default(),
            bar_indices_: Default::default(),
            bar_indices_0_1: Default::default(),
            res: Default::default(),
            res_indices_0: Default::default(),
            bar3_res: Default::default(),
            bar3_res_indices_: Default::default(),
            bar3_res_indices_0: Default::default(),
            scc0_duration: std::time::Duration::ZERO,
            scc1_duration: std::time::Duration::ZERO,
            scc2_duration: std::time::Duration::ZERO,
            scc3_duration: std::time::Duration::ZERO,
            scc4_duration: std::time::Duration::ZERO,
            scc5_duration: std::time::Duration::ZERO,
            scc6_duration: std::time::Duration::ZERO,
            scc7_duration: std::time::Duration::ZERO,
            scc8_duration: std::time::Duration::ZERO,
         };
         _self
      }
   };
}
