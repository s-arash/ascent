use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
struct TC {
    /**
physical indices:
 edge_indices_0_1; edge_indices_none*/
    pub edge: ::ascent::boxcar::Vec<(i32, i32)>,
    pub edge_indices_0_1: ascent::internal::CRelFullIndex<(i32, i32), ()>,
    pub edge_indices_none: ascent::internal::CRelNoIndex<(i32, i32)>,
    /**
physical indices:
 path_indices_0; path_indices_0_1; path_indices_1*/
    pub path: ::ascent::boxcar::Vec<(i32, i32)>,
    pub path_indices_0: ascent::internal::CRelIndex<(i32,), (i32,)>,
    pub path_indices_0_1: ascent::internal::CRelFullIndex<(i32, i32), ()>,
    pub path_indices_1: ascent::internal::CRelIndex<(i32,), (i32,)>,
    pub scc0_duration: std::time::Duration,
    pub scc1_duration: std::time::Duration,
    pub rule0_0_duration: std::time::Duration,
    pub rule1_0_duration: std::time::Duration,
    pub rule1_1_duration: std::time::Duration,
    pub update_time_nanos: std::sync::atomic::AtomicU64,
}
impl TC {
    #[allow(unused_imports)]
    ///Runs the Ascent program to a fixed point.
    pub fn run(&mut self) {
        use core::cmp::PartialEq;
        use ascent::internal::RelIndexRead;
        use ascent::internal::RelIndexReadAll;
        use ascent::rayon::iter::ParallelBridge;
        use ascent::rayon::iter::ParallelIterator;
        use ascent::internal::CRelIndexRead;
        use ascent::internal::CRelIndexReadAll;
        self.update_indices_priv();
        let _self = self;
        ascent::internal::comment("scc 0");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let path_indices_0_delta: &mut ascent::internal::CRelIndex<(i32,), (i32,)> = &mut _self
                .path_indices_0;
            let mut path_indices_0_total: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let mut path_indices_0_new: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let path_indices_0_1_delta: &mut ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = &mut _self.path_indices_0_1;
            let mut path_indices_0_1_total: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            let path_indices_1_delta: &mut ascent::internal::CRelIndex<(i32,), (i32,)> = &mut _self
                .path_indices_1;
            let mut path_indices_1_total: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let mut path_indices_1_new: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let edge_indices_none_total: &mut ascent::internal::CRelNoIndex<
                (i32, i32),
            > = &mut _self.edge_indices_none;
            #[allow(unused_assignments, unused_variables)]
            {
                let __changed = std::sync::atomic::AtomicBool::new(false);
                path_indices_0_total.freeze();
                path_indices_0_delta.freeze();
                path_indices_0_1_total.freeze();
                path_indices_0_1_delta.freeze();
                path_indices_1_total.freeze();
                path_indices_1_delta.freeze();
                edge_indices_none_total.freeze();
                let before_rule = ::ascent::internal::Instant::now();
                ascent::internal::comment("path <-- edge_indices_none_total");
                {
                    if let Some(__matching) = edge_indices_none_total.c_index_get(&()) {
                        __matching
                            .for_each(|__val| {
                                let x = &__val.0;
                                let y = &__val.1;
                                let __new_row: (i32, i32) = (
                                    ascent::internal::Convert::convert(x),
                                    ascent::internal::Convert::convert(y),
                                );
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &path_indices_0_1_total,
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        path_indices_0_1_delta,
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                        &path_indices_0_1_new,
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self
                                            .path
                                            .push((__new_row.0.clone(), __new_row.1.clone()));
                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                            &path_indices_0_new,
                                            (__new_row.0.clone(),),
                                            (__new_row.1.clone(),),
                                        );
                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                            &path_indices_1_new,
                                            (__new_row.1.clone(),),
                                            (__new_row.0.clone(),),
                                        );
                                        __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                    }
                                }
                            });
                    }
                }
                _self.rule0_0_duration += before_rule.elapsed();
                path_indices_0_total.unfreeze();
                path_indices_0_delta.unfreeze();
                path_indices_0_1_total.unfreeze();
                path_indices_0_1_delta.unfreeze();
                path_indices_1_total.unfreeze();
                path_indices_1_delta.unfreeze();
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_delta,
                    &mut path_indices_0_total,
                );
                std::mem::swap(&mut path_indices_0_new, path_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_1_delta,
                    &mut path_indices_0_1_total,
                );
                std::mem::swap(&mut path_indices_0_1_new, path_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_1_delta,
                    &mut path_indices_1_total,
                );
                std::mem::swap(&mut path_indices_1_new, path_indices_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_delta,
                    &mut path_indices_0_total,
                );
                std::mem::swap(&mut path_indices_0_new, path_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_1_delta,
                    &mut path_indices_0_1_total,
                );
                std::mem::swap(&mut path_indices_0_1_new, path_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_1_delta,
                    &mut path_indices_1_total,
                );
                std::mem::swap(&mut path_indices_1_new, path_indices_1_delta);
            }
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.path_indices_1 = path_indices_1_total;
            _self.scc0_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let path_indices_0_delta: &mut ascent::internal::CRelIndex<(i32,), (i32,)> = &mut _self
                .path_indices_0;
            let mut path_indices_0_total: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let mut path_indices_0_new: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let path_indices_0_1_delta: &mut ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = &mut _self.path_indices_0_1;
            let mut path_indices_0_1_total: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::CRelFullIndex<
                (i32, i32),
                (),
            > = Default::default();
            let path_indices_1_delta: &mut ascent::internal::CRelIndex<(i32,), (i32,)> = &mut _self
                .path_indices_1;
            let mut path_indices_1_total: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            let mut path_indices_1_new: ascent::internal::CRelIndex<(i32,), (i32,)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            loop {
                let __changed = std::sync::atomic::AtomicBool::new(false);
                path_indices_0_total.freeze();
                path_indices_0_delta.freeze();
                path_indices_0_1_total.freeze();
                path_indices_0_1_delta.freeze();
                path_indices_1_total.freeze();
                path_indices_1_delta.freeze();
                let before_rule = ::ascent::internal::Instant::now();
                ascent::internal::comment(
                    "path <-- path_indices_1_delta, path_indices_0_total+delta [SIMPLE JOIN]",
                );
                {
                    if path_indices_1_delta.len()
                        <= ascent::internal::RelIndexCombined::new(
                                &path_indices_0_total,
                                path_indices_0_delta,
                            )
                            .len()
                    {
                        path_indices_1_delta
                            .c_iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                        &path_indices_0_total,
                                        path_indices_0_delta,
                                    )
                                    .c_index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let z = &__val.0;
                                                    let __new_row: (i32, i32) = (
                                                        ascent::internal::Convert::convert(x),
                                                        ascent::internal::Convert::convert(z),
                                                    );
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total,
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            path_indices_0_1_delta,
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                            &path_indices_0_1_new,
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self
                                                                .path
                                                                .push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_0_new,
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_1_new,
                                                                (__new_row.1.clone(),),
                                                                (__new_row.0.clone(),),
                                                            );
                                                            __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        ascent::internal::RelIndexCombined::new(
                                &path_indices_0_total,
                                path_indices_0_delta,
                            )
                            .c_iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = path_indices_1_delta
                                    .c_index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    let __new_row: (i32, i32) = (
                                                        ascent::internal::Convert::convert(x),
                                                        ascent::internal::Convert::convert(z),
                                                    );
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total,
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            path_indices_0_1_delta,
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                            &path_indices_0_1_new,
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self
                                                                .path
                                                                .push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_0_new,
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_1_new,
                                                                (__new_row.1.clone(),),
                                                                (__new_row.0.clone(),),
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
                let before_rule = ::ascent::internal::Instant::now();
                ascent::internal::comment(
                    "path <-- path_indices_1_total, path_indices_0_delta [SIMPLE JOIN]",
                );
                {
                    if path_indices_1_total.len() <= path_indices_0_delta.len() {
                        path_indices_1_total
                            .c_iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = path_indices_0_delta
                                    .c_index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let z = &__val.0;
                                                    let __new_row: (i32, i32) = (
                                                        ascent::internal::Convert::convert(x),
                                                        ascent::internal::Convert::convert(z),
                                                    );
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total,
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            path_indices_0_1_delta,
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                            &path_indices_0_1_new,
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self
                                                                .path
                                                                .push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_0_new,
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_1_new,
                                                                (__new_row.1.clone(),),
                                                                (__new_row.0.clone(),),
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
                            .c_iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = path_indices_1_total
                                    .c_index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    let __new_row: (i32, i32) = (
                                                        ascent::internal::Convert::convert(x),
                                                        ascent::internal::Convert::convert(z),
                                                    );
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total,
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            path_indices_0_1_delta,
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                            &path_indices_0_1_new,
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self
                                                                .path
                                                                .push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_0_new,
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            ::ascent::internal::CRelIndexWrite::index_insert(
                                                                &path_indices_1_new,
                                                                (__new_row.1.clone(),),
                                                                (__new_row.0.clone(),),
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
                _self.rule1_1_duration += before_rule.elapsed();
                path_indices_0_total.unfreeze();
                path_indices_0_delta.unfreeze();
                path_indices_0_1_total.unfreeze();
                path_indices_0_1_delta.unfreeze();
                path_indices_1_total.unfreeze();
                path_indices_1_delta.unfreeze();
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_delta,
                    &mut path_indices_0_total,
                );
                std::mem::swap(&mut path_indices_0_new, path_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_1_delta,
                    &mut path_indices_0_1_total,
                );
                std::mem::swap(&mut path_indices_0_1_new, path_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_1_delta,
                    &mut path_indices_1_total,
                );
                std::mem::swap(&mut path_indices_1_new, path_indices_1_delta);
                if !__changed.load(std::sync::atomic::Ordering::Relaxed) {
                    break;
                }
            }
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.path_indices_1 = path_indices_1_total;
            _self.scc1_duration += _scc_start_time.elapsed();
        }
    }
    fn update_indices_priv(&mut self) {
        use ascent::rayon::iter::{IntoParallelIterator, ParallelIterator};
        (0..self.edge.len())
            .into_par_iter()
            .for_each(|_i| {
                let tuple = &self.edge[_i];
                let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                let rel_ind = &self.edge_indices_0_1;
                ascent::internal::CRelIndexWrite::index_insert(
                    rel_ind,
                    selection_tuple,
                    (),
                );
                let selection_tuple = ();
                let rel_ind = &self.edge_indices_none;
                ascent::internal::CRelIndexWrite::index_insert(
                    rel_ind,
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
                    rel_ind,
                    selection_tuple,
                    (tuple.1.clone(),),
                );
                let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                let rel_ind = &self.path_indices_0_1;
                ascent::internal::CRelIndexWrite::index_insert(
                    rel_ind,
                    selection_tuple,
                    (),
                );
                let selection_tuple = (tuple.1.clone(),);
                let rel_ind = &self.path_indices_1;
                ascent::internal::CRelIndexWrite::index_insert(
                    rel_ind,
                    selection_tuple,
                    (tuple.0.clone(),),
                );
            });
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
        "scc 0, is_looping: false:\n  path <-- edge_indices_none_total\n  dynamic relations: path\nscc 1, is_looping: true:\n  path <-- path_indices_1_delta, path_indices_0_total+delta [SIMPLE JOIN]\n  path <-- path_indices_1_total, path_indices_0_delta [SIMPLE JOIN]\n  dynamic relations: path\n"
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
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "0", self.scc0_duration))
            .unwrap();
        let sum_of_rule_times = std::time::Duration::ZERO + self.rule0_0_duration;
        (&mut res)
            .write_fmt(format_args!("  some of rule times: {0:?}\n", sum_of_rule_times))
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
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "1", self.scc1_duration))
            .unwrap();
        let sum_of_rule_times = std::time::Duration::ZERO + self.rule1_0_duration
            + self.rule1_1_duration;
        (&mut res)
            .write_fmt(format_args!("  some of rule times: {0:?}\n", sum_of_rule_times))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "  rule {0}\n    time: {1:?}\n",
                    "path <-- path_indices_1_delta, path_indices_0_total+delta [SIMPLE JOIN]",
                    self.rule1_0_duration,
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "  rule {0}\n    time: {1:?}\n",
                    "path <-- path_indices_1_total, path_indices_0_delta [SIMPLE JOIN]",
                    self.rule1_1_duration,
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
            edge_indices_0_1: Default::default(),
            edge_indices_none: Default::default(),
            path: Default::default(),
            path_indices_0: Default::default(),
            path_indices_0_1: Default::default(),
            path_indices_1: Default::default(),
            scc0_duration: std::time::Duration::ZERO,
            scc1_duration: std::time::Duration::ZERO,
            rule0_0_duration: std::time::Duration::ZERO,
            rule1_0_duration: std::time::Duration::ZERO,
            rule1_1_duration: std::time::Duration::ZERO,
            update_time_nanos: Default::default(),
        };
        _self
    }
}
fn main() {}
