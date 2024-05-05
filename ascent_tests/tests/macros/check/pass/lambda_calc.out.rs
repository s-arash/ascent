use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub enum LambdaCalcExpr {
    Ref(&'static str),
    Lam(&'static str, Rc<LambdaCalcExpr>),
    App(Rc<LambdaCalcExpr>, Rc<LambdaCalcExpr>),
}
#[automatically_derived]
impl ::core::clone::Clone for LambdaCalcExpr {
    #[inline]
    fn clone(&self) -> LambdaCalcExpr {
        match self {
            LambdaCalcExpr::Ref(__self_0) => {
                LambdaCalcExpr::Ref(::core::clone::Clone::clone(__self_0))
            }
            LambdaCalcExpr::Lam(__self_0, __self_1) => {
                LambdaCalcExpr::Lam(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                )
            }
            LambdaCalcExpr::App(__self_0, __self_1) => {
                LambdaCalcExpr::App(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                )
            }
        }
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for LambdaCalcExpr {}
#[automatically_derived]
impl ::core::cmp::PartialEq for LambdaCalcExpr {
    #[inline]
    fn eq(&self, other: &LambdaCalcExpr) -> bool {
        let __self_tag = ::core::intrinsics::discriminant_value(self);
        let __arg1_tag = ::core::intrinsics::discriminant_value(other);
        __self_tag == __arg1_tag
            && match (self, other) {
                (LambdaCalcExpr::Ref(__self_0), LambdaCalcExpr::Ref(__arg1_0)) => {
                    *__self_0 == *__arg1_0
                }
                (
                    LambdaCalcExpr::Lam(__self_0, __self_1),
                    LambdaCalcExpr::Lam(__arg1_0, __arg1_1),
                ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                (
                    LambdaCalcExpr::App(__self_0, __self_1),
                    LambdaCalcExpr::App(__arg1_0, __arg1_1),
                ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                _ => unsafe { ::core::intrinsics::unreachable() }
            }
    }
}
#[automatically_derived]
impl ::core::marker::StructuralEq for LambdaCalcExpr {}
#[automatically_derived]
impl ::core::cmp::Eq for LambdaCalcExpr {
    #[inline]
    #[doc(hidden)]
    #[coverage(off)]
    fn assert_receiver_is_total_eq(&self) -> () {
        let _: ::core::cmp::AssertParamIsEq<&'static str>;
        let _: ::core::cmp::AssertParamIsEq<&'static str>;
        let _: ::core::cmp::AssertParamIsEq<Rc<LambdaCalcExpr>>;
        let _: ::core::cmp::AssertParamIsEq<Rc<LambdaCalcExpr>>;
        let _: ::core::cmp::AssertParamIsEq<Rc<LambdaCalcExpr>>;
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for LambdaCalcExpr {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            LambdaCalcExpr::Ref(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Ref", &__self_0)
            }
            LambdaCalcExpr::Lam(__self_0, __self_1) => {
                ::core::fmt::Formatter::debug_tuple_field2_finish(
                    f,
                    "Lam",
                    __self_0,
                    &__self_1,
                )
            }
            LambdaCalcExpr::App(__self_0, __self_1) => {
                ::core::fmt::Formatter::debug_tuple_field2_finish(
                    f,
                    "App",
                    __self_0,
                    &__self_1,
                )
            }
        }
    }
}
#[automatically_derived]
impl ::core::hash::Hash for LambdaCalcExpr {
    #[inline]
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        let __self_tag = ::core::intrinsics::discriminant_value(self);
        ::core::hash::Hash::hash(&__self_tag, state);
        match self {
            LambdaCalcExpr::Ref(__self_0) => ::core::hash::Hash::hash(__self_0, state),
            LambdaCalcExpr::Lam(__self_0, __self_1) => {
                ::core::hash::Hash::hash(__self_0, state);
                ::core::hash::Hash::hash(__self_1, state)
            }
            LambdaCalcExpr::App(__self_0, __self_1) => {
                ::core::hash::Hash::hash(__self_0, state);
                ::core::hash::Hash::hash(__self_1, state)
            }
        }
    }
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
pub struct AscentProgram {
    /**
logical indices: do_eval_indices_0; do_eval_indices_none*/
    pub do_eval: ::std::vec::Vec<(LambdaCalcExpr,)>,
    pub __do_eval_ind_common: (),
    pub do_eval_indices_0: ascent::internal::RelFullIndexType<(LambdaCalcExpr,), ()>,
    pub do_eval_indices_none: ascent::rel::ToRelIndexType<(), (LambdaCalcExpr,)>,
    /**
logical indices: eval_indices_0; eval_indices_0_1*/
    pub eval: ::std::vec::Vec<(LambdaCalcExpr, LambdaCalcExpr)>,
    pub __eval_ind_common: (),
    pub eval_indices_0: ascent::rel::ToRelIndexType<
        (LambdaCalcExpr,),
        (LambdaCalcExpr,),
    >,
    pub eval_indices_0_1: ascent::internal::RelFullIndexType<
        (LambdaCalcExpr, LambdaCalcExpr),
        (),
    >,
    /**
logical indices: input_indices_0; input_indices_none*/
    pub input: ::std::vec::Vec<(LambdaCalcExpr,)>,
    pub __input_ind_common: (),
    pub input_indices_0: ascent::internal::RelFullIndexType<(LambdaCalcExpr,), ()>,
    pub input_indices_none: ascent::rel::ToRelIndexType<(), (LambdaCalcExpr,)>,
    /**
logical indices: output_indices_0*/
    pub output: ::std::vec::Vec<(LambdaCalcExpr,)>,
    pub __output_ind_common: (),
    pub output_indices_0: ascent::internal::RelFullIndexType<(LambdaCalcExpr,), ()>,
    scc_times: [std::time::Duration; 4usize],
    scc_iters: [usize; 4usize],
    pub update_time_nanos: std::sync::atomic::AtomicU64,
    pub update_indices_duration: std::time::Duration,
}
impl AscentProgram {
    #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
    ///Runs the Ascent program to a fixed point.
    pub fn run(&mut self) {
        use core::cmp::PartialEq;
        use ascent::internal::{
            RelIndexRead, RelIndexReadAll, ToRelIndex0, TupleOfBorrowed,
        };
        use ascent::internal::RelIndexWrite;
        self.update_indices_priv();
        let _self = self;
        ascent::internal::comment("scc 0");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __input_ind_common_delta: () = ::std::mem::take(
                &mut _self.__input_ind_common,
            );
            let mut __input_ind_common_total: () = Default::default();
            let mut __input_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __input_ind_common_new,
                &mut __input_ind_common_delta,
                &mut __input_ind_common_total,
            );
            let mut input_indices_0_delta: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = ::std::mem::take(&mut _self.input_indices_0);
            let mut input_indices_0_total: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            let mut input_indices_0_new: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut input_indices_0_new.to_rel_index_write(&mut __input_ind_common_new),
                &mut input_indices_0_delta
                    .to_rel_index_write(&mut __input_ind_common_delta),
                &mut input_indices_0_total
                    .to_rel_index_write(&mut __input_ind_common_total),
            );
            let mut input_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = ::std::mem::take(&mut _self.input_indices_none);
            let mut input_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = Default::default();
            let mut input_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut input_indices_none_new
                    .to_rel_index_write(&mut __input_ind_common_new),
                &mut input_indices_none_delta
                    .to_rel_index_write(&mut __input_ind_common_delta),
                &mut input_indices_none_total
                    .to_rel_index_write(&mut __input_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("input <-- ");
                {
                    let __new_row: (LambdaCalcExpr,) = (app(U(), I()),);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &input_indices_0_total.to_rel_index(&__input_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &input_indices_0_delta
                                .to_rel_index(&__input_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut input_indices_0_new
                                .to_rel_index_write(&mut __input_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.input.len();
                            _self.input.push((__new_row.0.clone(),));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut input_indices_none_new
                                    .to_rel_index_write(&mut __input_ind_common_new),
                                (),
                                (__new_row.0.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __input_ind_common_new,
                    &mut __input_ind_common_delta,
                    &mut __input_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut input_indices_0_new
                        .to_rel_index_write(&mut __input_ind_common_new),
                    &mut input_indices_0_delta
                        .to_rel_index_write(&mut __input_ind_common_delta),
                    &mut input_indices_0_total
                        .to_rel_index_write(&mut __input_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut input_indices_none_new
                        .to_rel_index_write(&mut __input_ind_common_new),
                    &mut input_indices_none_delta
                        .to_rel_index_write(&mut __input_ind_common_delta),
                    &mut input_indices_none_total
                        .to_rel_index_write(&mut __input_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __input_ind_common_new,
                    &mut __input_ind_common_delta,
                    &mut __input_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut input_indices_0_new
                        .to_rel_index_write(&mut __input_ind_common_new),
                    &mut input_indices_0_delta
                        .to_rel_index_write(&mut __input_ind_common_delta),
                    &mut input_indices_0_total
                        .to_rel_index_write(&mut __input_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut input_indices_none_new
                        .to_rel_index_write(&mut __input_ind_common_new),
                    &mut input_indices_none_delta
                        .to_rel_index_write(&mut __input_ind_common_delta),
                    &mut input_indices_none_total
                        .to_rel_index_write(&mut __input_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__input_ind_common = __input_ind_common_total;
            _self.input_indices_0 = input_indices_0_total;
            _self.input_indices_none = input_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __do_eval_ind_common_delta: () = ::std::mem::take(
                &mut _self.__do_eval_ind_common,
            );
            let mut __do_eval_ind_common_total: () = Default::default();
            let mut __do_eval_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __do_eval_ind_common_new,
                &mut __do_eval_ind_common_delta,
                &mut __do_eval_ind_common_total,
            );
            let mut do_eval_indices_0_delta: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = ::std::mem::take(&mut _self.do_eval_indices_0);
            let mut do_eval_indices_0_total: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            let mut do_eval_indices_0_new: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut do_eval_indices_0_new
                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                &mut do_eval_indices_0_delta
                    .to_rel_index_write(&mut __do_eval_ind_common_delta),
                &mut do_eval_indices_0_total
                    .to_rel_index_write(&mut __do_eval_ind_common_total),
            );
            let mut do_eval_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = ::std::mem::take(&mut _self.do_eval_indices_none);
            let mut do_eval_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = Default::default();
            let mut do_eval_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut do_eval_indices_none_new
                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                &mut do_eval_indices_none_delta
                    .to_rel_index_write(&mut __do_eval_ind_common_delta),
                &mut do_eval_indices_none_total
                    .to_rel_index_write(&mut __do_eval_ind_common_total),
            );
            let __input_ind_common_total: () = std::mem::take(
                &mut _self.__input_ind_common,
            );
            let input_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = std::mem::take(&mut _self.input_indices_none);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("do_eval <-- input_indices_none_total");
                {
                    if let Some(__matching) = input_indices_none_total
                        .to_rel_index(&__input_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let exp: &LambdaCalcExpr = __val.0;
                                let __new_row: (LambdaCalcExpr,) = (exp.clone(),);
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &do_eval_indices_0_total
                                        .to_rel_index(&__do_eval_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &do_eval_indices_0_delta
                                            .to_rel_index(&__do_eval_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut do_eval_indices_0_new
                                            .to_rel_index_write(&mut __do_eval_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.do_eval.len();
                                        _self.do_eval.push((__new_row.0.clone(),));
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut do_eval_indices_none_new
                                                .to_rel_index_write(&mut __do_eval_ind_common_new),
                                            (),
                                            (__new_row.0.clone(),),
                                        );
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __do_eval_ind_common_new,
                    &mut __do_eval_ind_common_delta,
                    &mut __do_eval_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut do_eval_indices_0_new
                        .to_rel_index_write(&mut __do_eval_ind_common_new),
                    &mut do_eval_indices_0_delta
                        .to_rel_index_write(&mut __do_eval_ind_common_delta),
                    &mut do_eval_indices_0_total
                        .to_rel_index_write(&mut __do_eval_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut do_eval_indices_none_new
                        .to_rel_index_write(&mut __do_eval_ind_common_new),
                    &mut do_eval_indices_none_delta
                        .to_rel_index_write(&mut __do_eval_ind_common_delta),
                    &mut do_eval_indices_none_total
                        .to_rel_index_write(&mut __do_eval_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __do_eval_ind_common_new,
                    &mut __do_eval_ind_common_delta,
                    &mut __do_eval_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut do_eval_indices_0_new
                        .to_rel_index_write(&mut __do_eval_ind_common_new),
                    &mut do_eval_indices_0_delta
                        .to_rel_index_write(&mut __do_eval_ind_common_delta),
                    &mut do_eval_indices_0_total
                        .to_rel_index_write(&mut __do_eval_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut do_eval_indices_none_new
                        .to_rel_index_write(&mut __do_eval_ind_common_new),
                    &mut do_eval_indices_none_delta
                        .to_rel_index_write(&mut __do_eval_ind_common_delta),
                    &mut do_eval_indices_none_total
                        .to_rel_index_write(&mut __do_eval_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
            }
            _self.__do_eval_ind_common = __do_eval_ind_common_total;
            _self.do_eval_indices_0 = do_eval_indices_0_total;
            _self.do_eval_indices_none = do_eval_indices_none_total;
            _self.__input_ind_common = __input_ind_common_total;
            _self.input_indices_none = input_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __do_eval_ind_common_delta: () = ::std::mem::take(
                &mut _self.__do_eval_ind_common,
            );
            let mut __do_eval_ind_common_total: () = Default::default();
            let mut __do_eval_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __do_eval_ind_common_new,
                &mut __do_eval_ind_common_delta,
                &mut __do_eval_ind_common_total,
            );
            let mut do_eval_indices_0_delta: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = ::std::mem::take(&mut _self.do_eval_indices_0);
            let mut do_eval_indices_0_total: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            let mut do_eval_indices_0_new: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut do_eval_indices_0_new
                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                &mut do_eval_indices_0_delta
                    .to_rel_index_write(&mut __do_eval_ind_common_delta),
                &mut do_eval_indices_0_total
                    .to_rel_index_write(&mut __do_eval_ind_common_total),
            );
            let mut do_eval_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = ::std::mem::take(&mut _self.do_eval_indices_none);
            let mut do_eval_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = Default::default();
            let mut do_eval_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (LambdaCalcExpr,),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut do_eval_indices_none_new
                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                &mut do_eval_indices_none_delta
                    .to_rel_index_write(&mut __do_eval_ind_common_delta),
                &mut do_eval_indices_none_total
                    .to_rel_index_write(&mut __do_eval_ind_common_total),
            );
            let mut __eval_ind_common_delta: () = ::std::mem::take(
                &mut _self.__eval_ind_common,
            );
            let mut __eval_ind_common_total: () = Default::default();
            let mut __eval_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __eval_ind_common_new,
                &mut __eval_ind_common_delta,
                &mut __eval_ind_common_total,
            );
            let mut eval_indices_0_delta: ascent::rel::ToRelIndexType<
                (LambdaCalcExpr,),
                (LambdaCalcExpr,),
            > = ::std::mem::take(&mut _self.eval_indices_0);
            let mut eval_indices_0_total: ascent::rel::ToRelIndexType<
                (LambdaCalcExpr,),
                (LambdaCalcExpr,),
            > = Default::default();
            let mut eval_indices_0_new: ascent::rel::ToRelIndexType<
                (LambdaCalcExpr,),
                (LambdaCalcExpr,),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut eval_indices_0_new.to_rel_index_write(&mut __eval_ind_common_new),
                &mut eval_indices_0_delta
                    .to_rel_index_write(&mut __eval_ind_common_delta),
                &mut eval_indices_0_total
                    .to_rel_index_write(&mut __eval_ind_common_total),
            );
            let mut eval_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr, LambdaCalcExpr),
                (),
            > = ::std::mem::take(&mut _self.eval_indices_0_1);
            let mut eval_indices_0_1_total: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr, LambdaCalcExpr),
                (),
            > = Default::default();
            let mut eval_indices_0_1_new: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr, LambdaCalcExpr),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut eval_indices_0_1_new.to_rel_index_write(&mut __eval_ind_common_new),
                &mut eval_indices_0_1_delta
                    .to_rel_index_write(&mut __eval_ind_common_delta),
                &mut eval_indices_0_1_total
                    .to_rel_index_write(&mut __eval_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment("eval <-- do_eval_indices_none_delta");
                {
                    if let Some(__matching) = do_eval_indices_none_delta
                        .to_rel_index(&__do_eval_ind_common_delta)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let exp @ Ref(_) = __arg_pattern_ {
                                    let __new_row: (LambdaCalcExpr, LambdaCalcExpr) = (
                                        exp.clone(),
                                        exp.clone(),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &eval_indices_0_1_total
                                            .to_rel_index(&__eval_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &eval_indices_0_1_delta
                                                .to_rel_index(&__eval_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut eval_indices_0_1_new
                                                .to_rel_index_write(&mut __eval_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.eval.len();
                                            _self.eval.push((__new_row.0.clone(), __new_row.1.clone()));
                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                &mut eval_indices_0_new
                                                    .to_rel_index_write(&mut __eval_ind_common_new),
                                                (__new_row.0.clone(),),
                                                (__new_row.1.clone(),),
                                            );
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment(
                    "eval <-- do_eval_indices_none_delta, if let ⋯",
                );
                {
                    if let Some(__matching) = do_eval_indices_none_delta
                        .to_rel_index(&__do_eval_ind_common_delta)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let exp: &LambdaCalcExpr = __val.0;
                                if let Lam(_, _) = exp {
                                    let __new_row: (LambdaCalcExpr, LambdaCalcExpr) = (
                                        exp.clone(),
                                        exp.clone(),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &eval_indices_0_1_total
                                            .to_rel_index(&__eval_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &eval_indices_0_1_delta
                                                .to_rel_index(&__eval_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut eval_indices_0_1_new
                                                .to_rel_index_write(&mut __eval_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.eval.len();
                                            _self.eval.push((__new_row.0.clone(), __new_row.1.clone()));
                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                &mut eval_indices_0_new
                                                    .to_rel_index_write(&mut __eval_ind_common_new),
                                                (__new_row.0.clone(),),
                                                (__new_row.1.clone(),),
                                            );
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment(
                    "eval <-- do_eval_indices_none_delta, eval_indices_0_total+delta, eval_indices_0_total+delta",
                );
                {
                    if let Some(__matching) = do_eval_indices_none_delta
                        .to_rel_index(&__do_eval_ind_common_delta)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let exp @ App(ef, ea) = __arg_pattern_ {
                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                            &eval_indices_0_total
                                                .to_rel_index(&__eval_ind_common_total),
                                            &eval_indices_0_delta.to_rel_index(&__eval_ind_common_delta),
                                        )
                                        .index_get(&((ef.deref()).clone(),))
                                    {
                                        __matching
                                            .for_each(|__val| {
                                                let __val = __val.tuple_of_borrowed();
                                                let __arg_pattern_1: &LambdaCalcExpr = __val.0;
                                                if let Lam(fx, fb) = __arg_pattern_1 {
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &eval_indices_0_total
                                                                .to_rel_index(&__eval_ind_common_total),
                                                            &eval_indices_0_delta.to_rel_index(&__eval_ind_common_delta),
                                                        )
                                                        .index_get(&((sub(fb, fx, ea)).clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let final_res: &LambdaCalcExpr = __val.0;
                                                                let __new_row: (LambdaCalcExpr, LambdaCalcExpr) = (
                                                                    exp.clone(),
                                                                    final_res.clone(),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &eval_indices_0_1_total
                                                                        .to_rel_index(&__eval_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &eval_indices_0_1_delta
                                                                            .to_rel_index(&__eval_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut eval_indices_0_1_new
                                                                            .to_rel_index_write(&mut __eval_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.eval.len();
                                                                        _self.eval.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut eval_indices_0_new
                                                                                .to_rel_index_write(&mut __eval_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                            });
                                                    }
                                                }
                                            });
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment(
                    "eval <-- do_eval_indices_none_total, eval_indices_0_delta, eval_indices_0_total+delta",
                );
                {
                    if let Some(__matching) = do_eval_indices_none_total
                        .to_rel_index(&__do_eval_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let exp @ App(ef, ea) = __arg_pattern_ {
                                    if let Some(__matching) = eval_indices_0_delta
                                        .to_rel_index(&__eval_ind_common_delta)
                                        .index_get(&((ef.deref()).clone(),))
                                    {
                                        __matching
                                            .for_each(|__val| {
                                                let __val = __val.tuple_of_borrowed();
                                                let __arg_pattern_1: &LambdaCalcExpr = __val.0;
                                                if let Lam(fx, fb) = __arg_pattern_1 {
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &eval_indices_0_total
                                                                .to_rel_index(&__eval_ind_common_total),
                                                            &eval_indices_0_delta.to_rel_index(&__eval_ind_common_delta),
                                                        )
                                                        .index_get(&((sub(fb, fx, ea)).clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let final_res: &LambdaCalcExpr = __val.0;
                                                                let __new_row: (LambdaCalcExpr, LambdaCalcExpr) = (
                                                                    exp.clone(),
                                                                    final_res.clone(),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &eval_indices_0_1_total
                                                                        .to_rel_index(&__eval_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &eval_indices_0_1_delta
                                                                            .to_rel_index(&__eval_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut eval_indices_0_1_new
                                                                            .to_rel_index_write(&mut __eval_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.eval.len();
                                                                        _self.eval.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut eval_indices_0_new
                                                                                .to_rel_index_write(&mut __eval_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                            });
                                                    }
                                                }
                                            });
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment(
                    "eval <-- do_eval_indices_none_total, eval_indices_0_total, eval_indices_0_delta",
                );
                {
                    if let Some(__matching) = do_eval_indices_none_total
                        .to_rel_index(&__do_eval_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let exp @ App(ef, ea) = __arg_pattern_ {
                                    if let Some(__matching) = eval_indices_0_total
                                        .to_rel_index(&__eval_ind_common_total)
                                        .index_get(&((ef.deref()).clone(),))
                                    {
                                        __matching
                                            .for_each(|__val| {
                                                let __val = __val.tuple_of_borrowed();
                                                let __arg_pattern_1: &LambdaCalcExpr = __val.0;
                                                if let Lam(fx, fb) = __arg_pattern_1 {
                                                    if let Some(__matching) = eval_indices_0_delta
                                                        .to_rel_index(&__eval_ind_common_delta)
                                                        .index_get(&((sub(fb, fx, ea)).clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let final_res: &LambdaCalcExpr = __val.0;
                                                                let __new_row: (LambdaCalcExpr, LambdaCalcExpr) = (
                                                                    exp.clone(),
                                                                    final_res.clone(),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &eval_indices_0_1_total
                                                                        .to_rel_index(&__eval_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &eval_indices_0_1_delta
                                                                            .to_rel_index(&__eval_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut eval_indices_0_1_new
                                                                            .to_rel_index_write(&mut __eval_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.eval.len();
                                                                        _self.eval.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut eval_indices_0_new
                                                                                .to_rel_index_write(&mut __eval_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                            });
                                                    }
                                                }
                                            });
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment("do_eval <-- do_eval_indices_none_delta");
                {
                    if let Some(__matching) = do_eval_indices_none_delta
                        .to_rel_index(&__do_eval_ind_common_delta)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let App(ef, _ea) = __arg_pattern_ {
                                    let __new_row: (LambdaCalcExpr,) = (ef.as_ref().clone(),);
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &do_eval_indices_0_total
                                            .to_rel_index(&__do_eval_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &do_eval_indices_0_delta
                                                .to_rel_index(&__do_eval_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut do_eval_indices_0_new
                                                .to_rel_index_write(&mut __do_eval_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.do_eval.len();
                                            _self.do_eval.push((__new_row.0.clone(),));
                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                &mut do_eval_indices_none_new
                                                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                                                (),
                                                (__new_row.0.clone(),),
                                            );
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment(
                    "do_eval <-- do_eval_indices_none_delta, eval_indices_0_total+delta",
                );
                {
                    if let Some(__matching) = do_eval_indices_none_delta
                        .to_rel_index(&__do_eval_ind_common_delta)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let App(ef, ea) = __arg_pattern_ {
                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                            &eval_indices_0_total
                                                .to_rel_index(&__eval_ind_common_total),
                                            &eval_indices_0_delta.to_rel_index(&__eval_ind_common_delta),
                                        )
                                        .index_get(&((ef.deref()).clone(),))
                                    {
                                        __matching
                                            .for_each(|__val| {
                                                let __val = __val.tuple_of_borrowed();
                                                let __arg_pattern_1: &LambdaCalcExpr = __val.0;
                                                if let Lam(fx, fb) = __arg_pattern_1 {
                                                    let __new_row: (LambdaCalcExpr,) = (sub(fb, fx, ea),);
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &do_eval_indices_0_total
                                                            .to_rel_index(&__do_eval_ind_common_total),
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &do_eval_indices_0_delta
                                                                .to_rel_index(&__do_eval_ind_common_delta),
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut do_eval_indices_0_new
                                                                .to_rel_index_write(&mut __do_eval_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.do_eval.len();
                                                            _self.do_eval.push((__new_row.0.clone(),));
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut do_eval_indices_none_new
                                                                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                                                                (),
                                                                (__new_row.0.clone(),),
                                                            );
                                                            __changed = true;
                                                        }
                                                    }
                                                }
                                            });
                                    }
                                }
                            });
                    }
                }
                ascent::internal::comment(
                    "do_eval <-- do_eval_indices_none_total, eval_indices_0_delta",
                );
                {
                    if let Some(__matching) = do_eval_indices_none_total
                        .to_rel_index(&__do_eval_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let __arg_pattern_: &LambdaCalcExpr = __val.0;
                                if let App(ef, ea) = __arg_pattern_ {
                                    if let Some(__matching) = eval_indices_0_delta
                                        .to_rel_index(&__eval_ind_common_delta)
                                        .index_get(&((ef.deref()).clone(),))
                                    {
                                        __matching
                                            .for_each(|__val| {
                                                let __val = __val.tuple_of_borrowed();
                                                let __arg_pattern_1: &LambdaCalcExpr = __val.0;
                                                if let Lam(fx, fb) = __arg_pattern_1 {
                                                    let __new_row: (LambdaCalcExpr,) = (sub(fb, fx, ea),);
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &do_eval_indices_0_total
                                                            .to_rel_index(&__do_eval_ind_common_total),
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &do_eval_indices_0_delta
                                                                .to_rel_index(&__do_eval_ind_common_delta),
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut do_eval_indices_0_new
                                                                .to_rel_index_write(&mut __do_eval_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.do_eval.len();
                                                            _self.do_eval.push((__new_row.0.clone(),));
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut do_eval_indices_none_new
                                                                    .to_rel_index_write(&mut __do_eval_ind_common_new),
                                                                (),
                                                                (__new_row.0.clone(),),
                                                            );
                                                            __changed = true;
                                                        }
                                                    }
                                                }
                                            });
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __do_eval_ind_common_new,
                    &mut __do_eval_ind_common_delta,
                    &mut __do_eval_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut do_eval_indices_0_new
                        .to_rel_index_write(&mut __do_eval_ind_common_new),
                    &mut do_eval_indices_0_delta
                        .to_rel_index_write(&mut __do_eval_ind_common_delta),
                    &mut do_eval_indices_0_total
                        .to_rel_index_write(&mut __do_eval_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut do_eval_indices_none_new
                        .to_rel_index_write(&mut __do_eval_ind_common_new),
                    &mut do_eval_indices_none_delta
                        .to_rel_index_write(&mut __do_eval_ind_common_delta),
                    &mut do_eval_indices_none_total
                        .to_rel_index_write(&mut __do_eval_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __eval_ind_common_new,
                    &mut __eval_ind_common_delta,
                    &mut __eval_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut eval_indices_0_new
                        .to_rel_index_write(&mut __eval_ind_common_new),
                    &mut eval_indices_0_delta
                        .to_rel_index_write(&mut __eval_ind_common_delta),
                    &mut eval_indices_0_total
                        .to_rel_index_write(&mut __eval_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut eval_indices_0_1_new
                        .to_rel_index_write(&mut __eval_ind_common_new),
                    &mut eval_indices_0_1_delta
                        .to_rel_index_write(&mut __eval_ind_common_delta),
                    &mut eval_indices_0_1_total
                        .to_rel_index_write(&mut __eval_ind_common_total),
                );
                _self.scc_iters[2usize] += 1;
                if !__changed {
                    break;
                }
            }
            _self.__do_eval_ind_common = __do_eval_ind_common_total;
            _self.do_eval_indices_0 = do_eval_indices_0_total;
            _self.do_eval_indices_none = do_eval_indices_none_total;
            _self.__eval_ind_common = __eval_ind_common_total;
            _self.eval_indices_0 = eval_indices_0_total;
            _self.eval_indices_0_1 = eval_indices_0_1_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 3");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __output_ind_common_delta: () = ::std::mem::take(
                &mut _self.__output_ind_common,
            );
            let mut __output_ind_common_total: () = Default::default();
            let mut __output_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __output_ind_common_new,
                &mut __output_ind_common_delta,
                &mut __output_ind_common_total,
            );
            let mut output_indices_0_delta: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = ::std::mem::take(&mut _self.output_indices_0);
            let mut output_indices_0_total: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            let mut output_indices_0_new: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut output_indices_0_new
                    .to_rel_index_write(&mut __output_ind_common_new),
                &mut output_indices_0_delta
                    .to_rel_index_write(&mut __output_ind_common_delta),
                &mut output_indices_0_total
                    .to_rel_index_write(&mut __output_ind_common_total),
            );
            let __eval_ind_common_total: () = std::mem::take(
                &mut _self.__eval_ind_common,
            );
            let eval_indices_0_total: ascent::rel::ToRelIndexType<
                (LambdaCalcExpr,),
                (LambdaCalcExpr,),
            > = std::mem::take(&mut _self.eval_indices_0);
            let __input_ind_common_total: () = std::mem::take(
                &mut _self.__input_ind_common,
            );
            let input_indices_0_total: ascent::internal::RelFullIndexType<
                (LambdaCalcExpr,),
                (),
            > = std::mem::take(&mut _self.input_indices_0);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment(
                    "output <-- input_indices_0_total, eval_indices_0_total [SIMPLE JOIN]",
                );
                {
                    if input_indices_0_total
                        .to_rel_index(&__input_ind_common_total)
                        .len()
                        <= eval_indices_0_total
                            .to_rel_index(&__eval_ind_common_total)
                            .len()
                    {
                        input_indices_0_total
                            .to_rel_index(&__input_ind_common_total)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let exp = __cl1_joined_columns.0;
                                if let Some(__matching) = eval_indices_0_total
                                    .to_rel_index(&__eval_ind_common_total)
                                    .index_get(&(exp.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let __val = __val.tuple_of_borrowed();
                                                    let res: &LambdaCalcExpr = __val.0;
                                                    let __new_row: (LambdaCalcExpr,) = (res.clone(),);
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &output_indices_0_total
                                                            .to_rel_index(&__output_ind_common_total),
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &output_indices_0_delta
                                                                .to_rel_index(&__output_ind_common_delta),
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut output_indices_0_new
                                                                .to_rel_index_write(&mut __output_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.output.len();
                                                            _self.output.push((__new_row.0,));
                                                            __changed = true;
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        eval_indices_0_total
                            .to_rel_index(&__eval_ind_common_total)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let exp = __cl1_joined_columns.0;
                                if let Some(__matching) = input_indices_0_total
                                    .to_rel_index(&__input_ind_common_total)
                                    .index_get(&(exp.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let cl1_val = cl1_val.tuple_of_borrowed();
                                            let res: &LambdaCalcExpr = cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let __new_row: (LambdaCalcExpr,) = (res.clone(),);
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &output_indices_0_total
                                                            .to_rel_index(&__output_ind_common_total),
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &output_indices_0_delta
                                                                .to_rel_index(&__output_ind_common_delta),
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut output_indices_0_new
                                                                .to_rel_index_write(&mut __output_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.output.len();
                                                            _self.output.push((__new_row.0,));
                                                            __changed = true;
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __output_ind_common_new,
                    &mut __output_ind_common_delta,
                    &mut __output_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut output_indices_0_new
                        .to_rel_index_write(&mut __output_ind_common_new),
                    &mut output_indices_0_delta
                        .to_rel_index_write(&mut __output_ind_common_delta),
                    &mut output_indices_0_total
                        .to_rel_index_write(&mut __output_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __output_ind_common_new,
                    &mut __output_ind_common_delta,
                    &mut __output_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut output_indices_0_new
                        .to_rel_index_write(&mut __output_ind_common_new),
                    &mut output_indices_0_delta
                        .to_rel_index_write(&mut __output_ind_common_delta),
                    &mut output_indices_0_total
                        .to_rel_index_write(&mut __output_ind_common_total),
                );
                _self.scc_iters[3usize] += 1;
            }
            _self.__output_ind_common = __output_ind_common_total;
            _self.output_indices_0 = output_indices_0_total;
            _self.__eval_ind_common = __eval_ind_common_total;
            _self.eval_indices_0 = eval_indices_0_total;
            _self.__input_ind_common = __input_ind_common_total;
            _self.input_indices_0 = input_indices_0_total;
            _self.scc_times[3usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.do_eval.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.do_eval_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__do_eval_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.do_eval_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__do_eval_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.eval.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.eval_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__eval_ind_common),
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.eval_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__eval_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.input.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.input_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__input_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.input_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__input_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.output.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.output_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__output_ind_common),
                selection_tuple,
                (),
            );
        }
        self.update_indices_duration += before.elapsed();
    }
    #[deprecated = "Explicit call to update_indices not required anymore."]
    pub fn update_indices(&mut self) {
        self.update_indices_priv();
    }
    fn type_constraints() {
        let _type_constraints: ascent::internal::TypeConstraints<LambdaCalcExpr>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  input <-- \n  dynamic relations: input\nscc 1, is_looping: false:\n  do_eval <-- input_indices_none_total\n  dynamic relations: do_eval\nscc 2, is_looping: true:\n  eval <-- do_eval_indices_none_delta\n  eval <-- do_eval_indices_none_delta, if let ⋯\n  eval <-- do_eval_indices_none_delta, eval_indices_0_total+delta, eval_indices_0_total+delta\n  eval <-- do_eval_indices_none_total, eval_indices_0_delta, eval_indices_0_total+delta\n  eval <-- do_eval_indices_none_total, eval_indices_0_total, eval_indices_0_delta\n  do_eval <-- do_eval_indices_none_delta\n  do_eval <-- do_eval_indices_none_delta, eval_indices_0_total+delta\n  do_eval <-- do_eval_indices_none_total, eval_indices_0_delta\n  dynamic relations: do_eval, eval\nscc 3, is_looping: false:\n  output <-- input_indices_0_total, eval_indices_0_total [SIMPLE JOIN]\n  dynamic relations: output\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "do_eval", self.do_eval.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "eval", self.eval.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "input", self.input.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "output", self.output.len()))
            .unwrap();
        res
    }
    pub fn scc_times_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(
                format_args!(
                    "update_indices time: {0:?}\n", self.update_indices_duration,
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "0", self
                    .scc_iters[0usize], self.scc_times[0usize],
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "1", self
                    .scc_iters[1usize], self.scc_times[1usize],
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "2", self
                    .scc_iters[2usize], self.scc_times[2usize],
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "3", self
                    .scc_iters[3usize], self.scc_times[3usize],
                ),
            )
            .unwrap();
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            do_eval: Default::default(),
            __do_eval_ind_common: Default::default(),
            do_eval_indices_0: Default::default(),
            do_eval_indices_none: Default::default(),
            eval: Default::default(),
            __eval_ind_common: Default::default(),
            eval_indices_0: Default::default(),
            eval_indices_0_1: Default::default(),
            input: Default::default(),
            __input_ind_common: Default::default(),
            input_indices_0: Default::default(),
            input_indices_none: Default::default(),
            output: Default::default(),
            __output_ind_common: Default::default(),
            output_indices_0: Default::default(),
            scc_times: [std::time::Duration::ZERO; 4usize],
            scc_iters: [0; 4usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
