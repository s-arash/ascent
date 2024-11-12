//! Simple rule relation
use ascent::ascent;
pub struct AscentProgram {
    /**
logical indices: fact_one_indices_0; fact_one_indices_none*/
    pub fact_one: ::std::vec::Vec<(isize,)>,
    pub __fact_one_ind_common: (),
    pub fact_one_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    pub fact_one_indices_none: ascent::rel::ToRelIndexType<(), (isize,)>,
    /**
logical indices: fact_two_indices_0_1; fact_two_indices_none*/
    pub fact_two: ::std::vec::Vec<(isize, isize)>,
    pub __fact_two_ind_common: (),
    pub fact_two_indices_0_1: ascent::internal::RelFullIndexType<(isize, isize), ()>,
    pub fact_two_indices_none: ascent::rel::ToRelIndexType<(), (isize, isize)>,
    /**
logical indices: fact_zero_indices_none*/
    pub fact_zero: ::std::vec::Vec<()>,
    pub __fact_zero_ind_common: (),
    pub fact_zero_indices_none: ascent::internal::RelFullIndexType<(), ()>,
    /**
logical indices: rule_one_indices_0*/
    pub rule_one: ::std::vec::Vec<(isize,)>,
    pub __rule_one_ind_common: (),
    pub rule_one_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    /**
logical indices: rule_two_indices_0_1*/
    pub rule_two: ::std::vec::Vec<(isize, isize)>,
    pub __rule_two_ind_common: (),
    pub rule_two_indices_0_1: ascent::internal::RelFullIndexType<(isize, isize), ()>,
    /**
logical indices: rule_zero_indices_none*/
    pub rule_zero: ::std::vec::Vec<()>,
    pub __rule_zero_ind_common: (),
    pub rule_zero_indices_none: ascent::internal::RelFullIndexType<(), ()>,
    scc_times: [std::time::Duration; 3usize],
    scc_iters: [usize; 3usize],
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
            let mut __rule_zero_ind_common_delta: () = ::std::mem::take(
                &mut _self.__rule_zero_ind_common,
            );
            let mut __rule_zero_ind_common_total: () = Default::default();
            let mut __rule_zero_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __rule_zero_ind_common_new,
                &mut __rule_zero_ind_common_delta,
                &mut __rule_zero_ind_common_total,
            );
            let mut rule_zero_indices_none_delta: ascent::internal::RelFullIndexType<
                (),
                (),
            > = ::std::mem::take(&mut _self.rule_zero_indices_none);
            let mut rule_zero_indices_none_total: ascent::internal::RelFullIndexType<
                (),
                (),
            > = Default::default();
            let mut rule_zero_indices_none_new: ascent::internal::RelFullIndexType<
                (),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut rule_zero_indices_none_new
                    .to_rel_index_write(&mut __rule_zero_ind_common_new),
                &mut rule_zero_indices_none_delta
                    .to_rel_index_write(&mut __rule_zero_ind_common_delta),
                &mut rule_zero_indices_none_total
                    .to_rel_index_write(&mut __rule_zero_ind_common_total),
            );
            let __fact_zero_ind_common_total: () = std::mem::take(
                &mut _self.__fact_zero_ind_common,
            );
            let fact_zero_indices_none_total: ascent::internal::RelFullIndexType<
                (),
                (),
            > = std::mem::take(&mut _self.fact_zero_indices_none);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("rule_zero <-- fact_zero_indices_none_total");
                {
                    if let Some(__matching) = fact_zero_indices_none_total
                        .to_rel_index(&__fact_zero_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __new_row: () = ();
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &rule_zero_indices_none_total
                                        .to_rel_index(&__rule_zero_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &rule_zero_indices_none_delta
                                            .to_rel_index(&__rule_zero_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut rule_zero_indices_none_new
                                            .to_rel_index_write(&mut __rule_zero_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.rule_zero.len();
                                        _self.rule_zero.push(());
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __rule_zero_ind_common_new,
                    &mut __rule_zero_ind_common_delta,
                    &mut __rule_zero_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut rule_zero_indices_none_new
                        .to_rel_index_write(&mut __rule_zero_ind_common_new),
                    &mut rule_zero_indices_none_delta
                        .to_rel_index_write(&mut __rule_zero_ind_common_delta),
                    &mut rule_zero_indices_none_total
                        .to_rel_index_write(&mut __rule_zero_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __rule_zero_ind_common_new,
                    &mut __rule_zero_ind_common_delta,
                    &mut __rule_zero_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut rule_zero_indices_none_new
                        .to_rel_index_write(&mut __rule_zero_ind_common_new),
                    &mut rule_zero_indices_none_delta
                        .to_rel_index_write(&mut __rule_zero_ind_common_delta),
                    &mut rule_zero_indices_none_total
                        .to_rel_index_write(&mut __rule_zero_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__rule_zero_ind_common = __rule_zero_ind_common_total;
            _self.rule_zero_indices_none = rule_zero_indices_none_total;
            _self.__fact_zero_ind_common = __fact_zero_ind_common_total;
            _self.fact_zero_indices_none = fact_zero_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __rule_one_ind_common_delta: () = ::std::mem::take(
                &mut _self.__rule_one_ind_common,
            );
            let mut __rule_one_ind_common_total: () = Default::default();
            let mut __rule_one_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __rule_one_ind_common_new,
                &mut __rule_one_ind_common_delta,
                &mut __rule_one_ind_common_total,
            );
            let mut rule_one_indices_0_delta: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = ::std::mem::take(&mut _self.rule_one_indices_0);
            let mut rule_one_indices_0_total: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            let mut rule_one_indices_0_new: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut rule_one_indices_0_new
                    .to_rel_index_write(&mut __rule_one_ind_common_new),
                &mut rule_one_indices_0_delta
                    .to_rel_index_write(&mut __rule_one_ind_common_delta),
                &mut rule_one_indices_0_total
                    .to_rel_index_write(&mut __rule_one_ind_common_total),
            );
            let __fact_one_ind_common_total: () = std::mem::take(
                &mut _self.__fact_one_ind_common,
            );
            let fact_one_indices_none_total: ascent::rel::ToRelIndexType<(), (isize,)> = std::mem::take(
                &mut _self.fact_one_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("rule_one <-- fact_one_indices_none_total");
                {
                    if let Some(__matching) = fact_one_indices_none_total
                        .to_rel_index(&__fact_one_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let n: &isize = __val.0;
                                let __new_row: (isize,) = (
                                    ascent::internal::Convert::convert(n),
                                );
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &rule_one_indices_0_total
                                        .to_rel_index(&__rule_one_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &rule_one_indices_0_delta
                                            .to_rel_index(&__rule_one_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut rule_one_indices_0_new
                                            .to_rel_index_write(&mut __rule_one_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.rule_one.len();
                                        _self.rule_one.push((__new_row.0,));
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __rule_one_ind_common_new,
                    &mut __rule_one_ind_common_delta,
                    &mut __rule_one_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut rule_one_indices_0_new
                        .to_rel_index_write(&mut __rule_one_ind_common_new),
                    &mut rule_one_indices_0_delta
                        .to_rel_index_write(&mut __rule_one_ind_common_delta),
                    &mut rule_one_indices_0_total
                        .to_rel_index_write(&mut __rule_one_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __rule_one_ind_common_new,
                    &mut __rule_one_ind_common_delta,
                    &mut __rule_one_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut rule_one_indices_0_new
                        .to_rel_index_write(&mut __rule_one_ind_common_new),
                    &mut rule_one_indices_0_delta
                        .to_rel_index_write(&mut __rule_one_ind_common_delta),
                    &mut rule_one_indices_0_total
                        .to_rel_index_write(&mut __rule_one_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
            }
            _self.__rule_one_ind_common = __rule_one_ind_common_total;
            _self.rule_one_indices_0 = rule_one_indices_0_total;
            _self.__fact_one_ind_common = __fact_one_ind_common_total;
            _self.fact_one_indices_none = fact_one_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __rule_two_ind_common_delta: () = ::std::mem::take(
                &mut _self.__rule_two_ind_common,
            );
            let mut __rule_two_ind_common_total: () = Default::default();
            let mut __rule_two_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __rule_two_ind_common_new,
                &mut __rule_two_ind_common_delta,
                &mut __rule_two_ind_common_total,
            );
            let mut rule_two_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (isize, isize),
                (),
            > = ::std::mem::take(&mut _self.rule_two_indices_0_1);
            let mut rule_two_indices_0_1_total: ascent::internal::RelFullIndexType<
                (isize, isize),
                (),
            > = Default::default();
            let mut rule_two_indices_0_1_new: ascent::internal::RelFullIndexType<
                (isize, isize),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut rule_two_indices_0_1_new
                    .to_rel_index_write(&mut __rule_two_ind_common_new),
                &mut rule_two_indices_0_1_delta
                    .to_rel_index_write(&mut __rule_two_ind_common_delta),
                &mut rule_two_indices_0_1_total
                    .to_rel_index_write(&mut __rule_two_ind_common_total),
            );
            let __fact_two_ind_common_total: () = std::mem::take(
                &mut _self.__fact_two_ind_common,
            );
            let fact_two_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (isize, isize),
            > = std::mem::take(&mut _self.fact_two_indices_none);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("rule_two <-- fact_two_indices_none_total");
                {
                    if let Some(__matching) = fact_two_indices_none_total
                        .to_rel_index(&__fact_two_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let a: &isize = __val.0;
                                let b: &isize = __val.1;
                                let __new_row: (isize, isize) = (
                                    ascent::internal::Convert::convert(a),
                                    ascent::internal::Convert::convert(b),
                                );
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &rule_two_indices_0_1_total
                                        .to_rel_index(&__rule_two_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &rule_two_indices_0_1_delta
                                            .to_rel_index(&__rule_two_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut rule_two_indices_0_1_new
                                            .to_rel_index_write(&mut __rule_two_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.rule_two.len();
                                        _self.rule_two.push((__new_row.0, __new_row.1));
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __rule_two_ind_common_new,
                    &mut __rule_two_ind_common_delta,
                    &mut __rule_two_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut rule_two_indices_0_1_new
                        .to_rel_index_write(&mut __rule_two_ind_common_new),
                    &mut rule_two_indices_0_1_delta
                        .to_rel_index_write(&mut __rule_two_ind_common_delta),
                    &mut rule_two_indices_0_1_total
                        .to_rel_index_write(&mut __rule_two_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __rule_two_ind_common_new,
                    &mut __rule_two_ind_common_delta,
                    &mut __rule_two_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut rule_two_indices_0_1_new
                        .to_rel_index_write(&mut __rule_two_ind_common_new),
                    &mut rule_two_indices_0_1_delta
                        .to_rel_index_write(&mut __rule_two_ind_common_delta),
                    &mut rule_two_indices_0_1_total
                        .to_rel_index_write(&mut __rule_two_ind_common_total),
                );
                _self.scc_iters[2usize] += 1;
            }
            _self.__rule_two_ind_common = __rule_two_ind_common_total;
            _self.rule_two_indices_0_1 = rule_two_indices_0_1_total;
            _self.__fact_two_ind_common = __fact_two_ind_common_total;
            _self.fact_two_indices_none = fact_two_indices_none_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.fact_one.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.fact_one_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_one_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.fact_one_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_one_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.fact_two.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.fact_two_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_two_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.fact_two_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_two_ind_common),
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone()),
            );
        }
        for (_i, tuple) in self.fact_zero.iter().enumerate() {
            let selection_tuple = ();
            let rel_ind = &mut self.fact_zero_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_zero_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.rule_one.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.rule_one_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__rule_one_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.rule_two.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.rule_two_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__rule_two_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.rule_zero.iter().enumerate() {
            let selection_tuple = ();
            let rel_ind = &mut self.rule_zero_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__rule_zero_ind_common),
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
        let _type_constraints: ascent::internal::TypeConstraints<isize>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  rule_zero <-- fact_zero_indices_none_total\n  dynamic relations: rule_zero\nscc 1, is_looping: false:\n  rule_one <-- fact_one_indices_none_total\n  dynamic relations: rule_one\nscc 2, is_looping: false:\n  rule_two <-- fact_two_indices_none_total\n  dynamic relations: rule_two\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "fact_one", self.fact_one.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "fact_two", self.fact_two.len()))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!("{0} size: {1}\n", "fact_zero", self.fact_zero.len()),
            )
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "rule_one", self.rule_one.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "rule_two", self.rule_two.len()))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!("{0} size: {1}\n", "rule_zero", self.rule_zero.len()),
            )
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            fact_one: Default::default(),
            __fact_one_ind_common: Default::default(),
            fact_one_indices_0: Default::default(),
            fact_one_indices_none: Default::default(),
            fact_two: Default::default(),
            __fact_two_ind_common: Default::default(),
            fact_two_indices_0_1: Default::default(),
            fact_two_indices_none: Default::default(),
            fact_zero: Default::default(),
            __fact_zero_ind_common: Default::default(),
            fact_zero_indices_none: Default::default(),
            rule_one: Default::default(),
            __rule_one_ind_common: Default::default(),
            rule_one_indices_0: Default::default(),
            rule_two: Default::default(),
            __rule_two_ind_common: Default::default(),
            rule_two_indices_0_1: Default::default(),
            rule_zero: Default::default(),
            __rule_zero_ind_common: Default::default(),
            rule_zero_indices_none: Default::default(),
            scc_times: [std::time::Duration::ZERO; 3usize],
            scc_iters: [0; 3usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
