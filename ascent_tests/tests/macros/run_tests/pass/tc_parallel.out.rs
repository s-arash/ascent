use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
struct TC {
    /**
logical indices: edge_indices_0_1; edge_indices_1; edge_indices_none*/
    pub edge: ::ascent::boxcar::Vec<(i32, i32)>,
    pub __edge_ind_common: (),
    pub edge_indices_0_1: ascent::internal::CRelFullIndex<(i32, i32), ()>,
    pub edge_indices_1: ascent::internal::CRelIndex<(i32,), (i32,)>,
    pub edge_indices_none: ascent::internal::CRelNoIndex<(i32, i32)>,
    /**
logical indices: path_indices_0; path_indices_0_1*/
    pub path: ::ascent::boxcar::Vec<(i32, i32)>,
    pub __path_ind_common: (),
    pub path_indices_0: ascent::internal::CRelIndex<(i32,), (i32,)>,
    pub path_indices_0_1: ascent::internal::CRelFullIndex<(i32, i32), ()>,
    scc_times: [std::time::Duration; 2usize],
    scc_iters: [usize; 2usize],
    pub rule0_0_duration: std::time::Duration,
    pub rule1_0_duration: std::time::Duration,
    pub update_time_nanos: std::sync::atomic::AtomicU64,
    pub update_indices_duration: std::time::Duration,
}
impl TC {
    #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
    ///Runs the Ascent program to a fixed point.
    pub fn run(&mut self) {
        use core::cmp::PartialEq;
        use ascent::internal::{
            RelIndexRead, RelIndexReadAll, ToRelIndex0, TupleOfBorrowed,
        };
        use ascent::internal::CRelIndexWrite;
        use ascent::rayon::iter::ParallelBridge;
        use ascent::rayon::iter::ParallelIterator;
        use ascent::internal::CRelIndexRead;
        use ascent::internal::CRelIndexReadAll;
        use ascent::internal::Freezable;
        self.update_indices_priv();
        let _self = self;
        ascent::internal::comment("scc 0");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __path_ind_common_delta: () = ::std::mem::take(
                &mut _self.__path_ind_common,
            );
            let mut __path_ind_common_total: () = Default::default();
            let mut __path_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __path_ind_common_new,
                &mut __path_ind_common_delta,
                &mut __path_ind_common_total,
            );
            let mut path_indices_0_delta: ascent::internal::CRelIndex<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.path_indices_0,
            );
            let mut path_indices_0_total: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let mut path_indices_0_new: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut path_indices_0_new.to_rel_index_write(&mut __path_ind_common_new),
                &mut path_indices_0_delta
                    .to_rel_index_write(&mut __path_ind_common_delta),
                &mut path_indices_0_total
                    .to_rel_index_write(&mut __path_ind_common_total),
            );
            let mut path_indices_0_1_delta: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.path_indices_0_1);
            let mut path_indices_0_1_total: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut path_indices_0_1_new.to_rel_index_write(&mut __path_ind_common_new),
                &mut path_indices_0_1_delta
                    .to_rel_index_write(&mut __path_ind_common_delta),
                &mut path_indices_0_1_total
                    .to_rel_index_write(&mut __path_ind_common_total),
            );
            _self.__edge_ind_common.freeze();
            let __edge_ind_common_total: () = std::mem::take(
                &mut _self.__edge_ind_common,
            );
            _self.edge_indices_none.freeze();
            let edge_indices_none_total: ascent::internal::CRelNoIndex<(i32, i32)> = std::mem::take(
                &mut _self.edge_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let __changed = std::sync::atomic::AtomicBool::new(false);
                __path_ind_common_total.freeze();
                __path_ind_common_delta.freeze();
                path_indices_0_total.freeze();
                path_indices_0_delta.freeze();
                path_indices_0_1_total.freeze();
                path_indices_0_1_delta.freeze();
                let before_rule = ::ascent::internal::Instant::now();
                ascent::internal::comment("path <-- edge_indices_none_total");
                {
                    if let Some(__matching) = edge_indices_none_total
                        .to_rel_index(&__edge_ind_common_total)
                        .c_index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                let y: &i32 = __val.1;
                                let __new_row: (i32, i32) = (
                                    ascent::internal::Convert::convert(x),
                                    ascent::internal::Convert::convert(y),
                                );
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &path_indices_0_1_total
                                        .to_rel_index(&__path_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &path_indices_0_1_delta
                                            .to_rel_index(&__path_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                        &path_indices_0_1_new
                                            .to_c_rel_index_write(&__path_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self
                                            .path
                                            .push((__new_row.0.clone(), __new_row.1.clone()));
                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                            &path_indices_0_new
                                                .to_c_rel_index_write(&__path_ind_common_new),
                                            (__new_row.0.clone(),),
                                            (__new_row.1.clone(),),
                                        );
                                        __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                    }
                                }
                            });
                    }
                }
                _self.rule0_0_duration += before_rule.elapsed();
                __path_ind_common_total.unfreeze();
                __path_ind_common_delta.unfreeze();
                path_indices_0_total.unfreeze();
                path_indices_0_delta.unfreeze();
                path_indices_0_1_total.unfreeze();
                path_indices_0_1_delta.unfreeze();
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __path_ind_common_new,
                    &mut __path_ind_common_delta,
                    &mut __path_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut path_indices_0_new
                        .to_rel_index_write(&mut __path_ind_common_new),
                    &mut path_indices_0_delta
                        .to_rel_index_write(&mut __path_ind_common_delta),
                    &mut path_indices_0_total
                        .to_rel_index_write(&mut __path_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut path_indices_0_1_new
                        .to_rel_index_write(&mut __path_ind_common_new),
                    &mut path_indices_0_1_delta
                        .to_rel_index_write(&mut __path_ind_common_delta),
                    &mut path_indices_0_1_total
                        .to_rel_index_write(&mut __path_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __path_ind_common_new,
                    &mut __path_ind_common_delta,
                    &mut __path_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut path_indices_0_new
                        .to_rel_index_write(&mut __path_ind_common_new),
                    &mut path_indices_0_delta
                        .to_rel_index_write(&mut __path_ind_common_delta),
                    &mut path_indices_0_total
                        .to_rel_index_write(&mut __path_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut path_indices_0_1_new
                        .to_rel_index_write(&mut __path_ind_common_new),
                    &mut path_indices_0_1_delta
                        .to_rel_index_write(&mut __path_ind_common_delta),
                    &mut path_indices_0_1_total
                        .to_rel_index_write(&mut __path_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__path_ind_common = __path_ind_common_total;
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_none = edge_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __path_ind_common_delta: () = ::std::mem::take(
                &mut _self.__path_ind_common,
            );
            let mut __path_ind_common_total: () = Default::default();
            let mut __path_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __path_ind_common_new,
                &mut __path_ind_common_delta,
                &mut __path_ind_common_total,
            );
            let mut path_indices_0_delta: ascent::internal::CRelIndex<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.path_indices_0,
            );
            let mut path_indices_0_total: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let mut path_indices_0_new: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut path_indices_0_new.to_rel_index_write(&mut __path_ind_common_new),
                &mut path_indices_0_delta
                    .to_rel_index_write(&mut __path_ind_common_delta),
                &mut path_indices_0_total
                    .to_rel_index_write(&mut __path_ind_common_total),
            );
            let mut path_indices_0_1_delta: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.path_indices_0_1);
            let mut path_indices_0_1_total: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut path_indices_0_1_new.to_rel_index_write(&mut __path_ind_common_new),
                &mut path_indices_0_1_delta
                    .to_rel_index_write(&mut __path_ind_common_delta),
                &mut path_indices_0_1_total
                    .to_rel_index_write(&mut __path_ind_common_total),
            );
            _self.__edge_ind_common.freeze();
            let __edge_ind_common_total: () = std::mem::take(
                &mut _self.__edge_ind_common,
            );
            _self.edge_indices_1.freeze();
            let edge_indices_1_total: ascent::internal::CRelIndex<(i32,), (i32,)> = std::mem::take(
                &mut _self.edge_indices_1,
            );
            #[allow(unused_assignments, unused_variables)]
            loop {
                let __changed = std::sync::atomic::AtomicBool::new(false);
                __path_ind_common_total.freeze();
                __path_ind_common_delta.freeze();
                path_indices_0_total.freeze();
                path_indices_0_delta.freeze();
                path_indices_0_1_total.freeze();
                path_indices_0_1_delta.freeze();
                let before_rule = ::ascent::internal::Instant::now();
                ascent::internal::comment(
                    "path <-- edge_indices_1_total, path_indices_0_delta [SIMPLE JOIN]",
                );
                {
                    if edge_indices_1_total.to_rel_index(&__edge_ind_common_total).len()
                        <= path_indices_0_delta
                            .to_rel_index(&__path_ind_common_delta)
                            .len()
                    {
                        edge_indices_1_total
                            .to_rel_index(&__edge_ind_common_total)
                            .c_iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = path_indices_0_delta
                                    .to_rel_index(&__path_ind_common_delta)
                                    .c_index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let cl1_val = cl1_val.tuple_of_borrowed();
                                            let x: &i32 = cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let __val = __val.tuple_of_borrowed();
                                                    let z: &i32 = __val.0;
                                                    let __new_row: (i32, i32) = (
                                                        ascent::internal::Convert::convert(x),
                                                        ascent::internal::Convert::convert(z),
                                                    );
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total
                                                            .to_rel_index(&__path_ind_common_total),
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &path_indices_0_1_delta
                                                                .to_rel_index(&__path_ind_common_delta),
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                            &path_indices_0_1_new
                                                                .to_c_rel_index_write(&__path_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self
                                                                .path
                                                                .push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_0_new
                                                                    .to_c_rel_index_write(&__path_ind_common_new),
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        path_indices_0_delta
                            .to_rel_index(&__path_ind_common_delta)
                            .c_iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = edge_indices_1_total
                                    .to_rel_index(&__edge_ind_common_total)
                                    .c_index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let cl1_val = cl1_val.tuple_of_borrowed();
                                            let z: &i32 = cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let __val = __val.tuple_of_borrowed();
                                                    let x: &i32 = __val.0;
                                                    let __new_row: (i32, i32) = (
                                                        ascent::internal::Convert::convert(x),
                                                        ascent::internal::Convert::convert(z),
                                                    );
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total
                                                            .to_rel_index(&__path_ind_common_total),
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &path_indices_0_1_delta
                                                                .to_rel_index(&__path_ind_common_delta),
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                            &path_indices_0_1_new
                                                                .to_c_rel_index_write(&__path_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self
                                                                .path
                                                                .push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_0_new
                                                                    .to_c_rel_index_write(&__path_ind_common_new),
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    }
                }
                _self.rule1_0_duration += before_rule.elapsed();
                __path_ind_common_total.unfreeze();
                __path_ind_common_delta.unfreeze();
                path_indices_0_total.unfreeze();
                path_indices_0_delta.unfreeze();
                path_indices_0_1_total.unfreeze();
                path_indices_0_1_delta.unfreeze();
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __path_ind_common_new,
                    &mut __path_ind_common_delta,
                    &mut __path_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut path_indices_0_new
                        .to_rel_index_write(&mut __path_ind_common_new),
                    &mut path_indices_0_delta
                        .to_rel_index_write(&mut __path_ind_common_delta),
                    &mut path_indices_0_total
                        .to_rel_index_write(&mut __path_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut path_indices_0_1_new
                        .to_rel_index_write(&mut __path_ind_common_new),
                    &mut path_indices_0_1_delta
                        .to_rel_index_write(&mut __path_ind_common_delta),
                    &mut path_indices_0_1_total
                        .to_rel_index_write(&mut __path_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
                if !__changed.load(std::sync::atomic::Ordering::Relaxed) {
                    break;
                }
            }
            _self.__path_ind_common = __path_ind_common_total;
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_1 = edge_indices_1_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::CRelIndexWrite;
        use ascent::rayon::iter::{IntoParallelIterator, ParallelIterator};
        (0..self.edge.len())
            .into_par_iter()
            .for_each(|_i| {
                let tuple = &self.edge[_i];
                let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                let rel_ind = &self.edge_indices_0_1;
                ascent::internal::CRelIndexWrite::index_insert(
                    &rel_ind.to_c_rel_index_write(&self.__edge_ind_common),
                    selection_tuple,
                    (),
                );
                let selection_tuple = (tuple.1.clone(),);
                let rel_ind = &self.edge_indices_1;
                ascent::internal::CRelIndexWrite::index_insert(
                    &rel_ind.to_c_rel_index_write(&self.__edge_ind_common),
                    selection_tuple,
                    (tuple.0.clone(),),
                );
                let selection_tuple = ();
                let rel_ind = &self.edge_indices_none;
                ascent::internal::CRelIndexWrite::index_insert(
                    &rel_ind.to_c_rel_index_write(&self.__edge_ind_common),
                    selection_tuple,
                    (tuple.0.clone(), tuple.1.clone()),
                );
            });
        (0..self.path.len())
            .into_par_iter()
            .for_each(|_i| {
                let tuple = &self.path[_i];
                let selection_tuple = (tuple.0.clone(),);
                let rel_ind = &self.path_indices_0;
                ascent::internal::CRelIndexWrite::index_insert(
                    &rel_ind.to_c_rel_index_write(&self.__path_ind_common),
                    selection_tuple,
                    (tuple.1.clone(),),
                );
                let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                let rel_ind = &self.path_indices_0_1;
                ascent::internal::CRelIndexWrite::index_insert(
                    &rel_ind.to_c_rel_index_write(&self.__path_ind_common),
                    selection_tuple,
                    (),
                );
            });
        self.update_indices_duration += before.elapsed();
    }
    #[deprecated = "Explicit call to update_indices not required anymore."]
    pub fn update_indices(&mut self) {
        self.update_indices_priv();
    }
    fn type_constraints() {
        let _type_constraints: ascent::internal::TypeConstraints<i32>;
        let _par_constraints: ascent::internal::ParTypeConstraints<i32>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  path <-- edge_indices_none_total\n  dynamic relations: path\nscc 1, is_looping: true:\n  path <-- edge_indices_1_total, path_indices_0_delta [SIMPLE JOIN]\n  dynamic relations: path\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "edge", self.edge.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "path", self.path.len()))
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
        let sum_of_rule_times = std::time::Duration::ZERO + self.rule0_0_duration;
        (&mut res)
            .write_fmt(format_args!("  sum of rule times: {0:?}\n", sum_of_rule_times))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "  rule {0}\n    time: {1:?}\n", "path <-- edge_indices_none_total",
                    self.rule0_0_duration,
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("-----------------------------------------\n"))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "1", self
                    .scc_iters[1usize], self.scc_times[1usize],
                ),
            )
            .unwrap();
        let sum_of_rule_times = std::time::Duration::ZERO + self.rule1_0_duration;
        (&mut res)
            .write_fmt(format_args!("  sum of rule times: {0:?}\n", sum_of_rule_times))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "  rule {0}\n    time: {1:?}\n",
                    "path <-- edge_indices_1_total, path_indices_0_delta [SIMPLE JOIN]",
                    self.rule1_0_duration,
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("-----------------------------------------\n"))
            .unwrap();
        res
    }
}
impl Default for TC {
    fn default() -> Self {
        let mut _self = TC {
            edge: Default::default(),
            __edge_ind_common: Default::default(),
            edge_indices_0_1: Default::default(),
            edge_indices_1: Default::default(),
            edge_indices_none: Default::default(),
            path: Default::default(),
            __path_ind_common: Default::default(),
            path_indices_0: Default::default(),
            path_indices_0_1: Default::default(),
            scc_times: [std::time::Duration::ZERO; 2usize],
            scc_iters: [0; 2usize],
            rule0_0_duration: std::time::Duration::ZERO,
            rule1_0_duration: std::time::Duration::ZERO,
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
