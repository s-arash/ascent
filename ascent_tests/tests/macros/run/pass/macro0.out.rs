use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
#[warn(warnings)]
#[allow(unused_imports)]
#[allow(dead_code)]
#[allow(redundant_semicolons)]
fn test<T: FactTypes>() {
    use ascent::{aggregators::*, lattice::set::Set, Dual};
    let _ = {
        struct Polonius<T: FactTypes> {
            /**
logical indices: cfg_edge_indices_0; cfg_edge_indices_0_1*/
            pub cfg_edge: ::ascent::boxcar::Vec<(T::Point, T::Point)>,
            pub __cfg_edge_ind_common: (),
            pub cfg_edge_indices_0: ascent::internal::CRelIndex<
                (T::Point,),
                (T::Point,),
            >,
            pub cfg_edge_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Point, T::Point),
                (),
            >,
            /**
logical indices: errors_indices_0_1*/
            pub errors: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub __errors_ind_common: (),
            pub errors_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
logical indices: known_placeholder_subset_indices_0_1*/
            pub known_placeholder_subset: ::ascent::boxcar::Vec<(T::Origin, T::Origin)>,
            pub __known_placeholder_subset_ind_common: (),
            pub known_placeholder_subset_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Origin, T::Origin),
                (),
            >,
            /**
logical indices: loan_invalidated_at_indices_0_1*/
            pub loan_invalidated_at: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub __loan_invalidated_at_ind_common: (),
            pub loan_invalidated_at_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
logical indices: loan_killed_at_indices_0_1*/
            pub loan_killed_at: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub __loan_killed_at_ind_common: (),
            pub loan_killed_at_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
logical indices: loan_live_at_indices_0_1*/
            pub loan_live_at: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub __loan_live_at_ind_common: (),
            pub loan_live_at_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
logical indices: origin_contains_loan_on_entry_indices_0_1_2; origin_contains_loan_on_entry_indices_0_2; origin_contains_loan_on_entry_indices_2*/
            pub origin_contains_loan_on_entry: ::ascent::boxcar::Vec<
                (T::Origin, T::Loan, T::Point),
            >,
            pub __origin_contains_loan_on_entry_ind_common: (),
            pub origin_contains_loan_on_entry_indices_0_1_2: ascent::internal::CRelFullIndex<
                (T::Origin, T::Loan, T::Point),
                (),
            >,
            pub origin_contains_loan_on_entry_indices_0_2: ascent::internal::CRelIndex<
                (T::Origin, T::Point),
                (T::Loan,),
            >,
            pub origin_contains_loan_on_entry_indices_2: ascent::internal::CRelIndex<
                (T::Point,),
                (T::Origin, T::Loan),
            >,
            /**
logical indices: origin_live_on_entry_indices_0_1*/
            pub origin_live_on_entry: ::ascent::boxcar::Vec<(T::Origin, T::Point)>,
            pub __origin_live_on_entry_ind_common: (),
            pub origin_live_on_entry_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Origin, T::Point),
                (),
            >,
            /**
logical indices: placeholder_origin_indices_0*/
            pub placeholder_origin: ::ascent::boxcar::Vec<(T::Origin,)>,
            pub __placeholder_origin_ind_common: (),
            pub placeholder_origin_indices_0: ascent::internal::CRelFullIndex<
                (T::Origin,),
                (),
            >,
            /**
logical indices: subset_indices_0; subset_indices_0_1_2; subset_indices_0_2; subset_indices_1_2; subset_indices_2*/
            pub subset: ::ascent::boxcar::Vec<(T::Origin, T::Origin, T::Point)>,
            pub __subset_ind_common: (),
            pub subset_indices_0: ascent::internal::CRelIndex<
                (T::Origin,),
                (T::Origin, T::Point),
            >,
            pub subset_indices_0_1_2: ascent::internal::CRelFullIndex<
                (T::Origin, T::Origin, T::Point),
                (),
            >,
            pub subset_indices_0_2: ascent::internal::CRelIndex<
                (T::Origin, T::Point),
                (T::Origin,),
            >,
            pub subset_indices_1_2: ascent::internal::CRelIndex<
                (T::Origin, T::Point),
                (T::Origin,),
            >,
            pub subset_indices_2: ascent::internal::CRelIndex<
                (T::Point,),
                (T::Origin, T::Origin),
            >,
            /**
logical indices: subset_error_indices_0_1_2*/
            pub subset_error: ::ascent::boxcar::Vec<(T::Origin, T::Origin, T::Point)>,
            pub __subset_error_ind_common: (),
            pub subset_error_indices_0_1_2: ascent::internal::CRelFullIndex<
                (T::Origin, T::Origin, T::Point),
                (),
            >,
            scc_times: [std::time::Duration; 5usize],
            scc_iters: [usize; 5usize],
            pub update_time_nanos: std::sync::atomic::AtomicU64,
            pub update_indices_duration: std::time::Duration,
        }
        impl<T: FactTypes> Polonius<T> {
            #[allow(noop_method_call, suspicious_double_ref_op)]
            fn update_indices_priv(&mut self) {
                let before = ::ascent::internal::Instant::now();
                use ascent::internal::ToRelIndex0;
                use ascent::internal::CRelIndexWrite;
                use ascent::rayon::iter::{IntoParallelIterator, ParallelIterator};
                (0..self.cfg_edge.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.cfg_edge[_i];
                        let selection_tuple = (tuple.0.clone(),);
                        let rel_ind = &self.cfg_edge_indices_0;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__cfg_edge_ind_common),
                            selection_tuple,
                            (tuple.1.clone(),),
                        );
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.cfg_edge_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__cfg_edge_ind_common),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.errors.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.errors[_i];
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.errors_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__errors_ind_common),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.known_placeholder_subset.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.known_placeholder_subset[_i];
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.known_placeholder_subset_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__known_placeholder_subset_ind_common,
                                ),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.loan_invalidated_at.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.loan_invalidated_at[_i];
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.loan_invalidated_at_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__loan_invalidated_at_ind_common,
                                ),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.loan_killed_at.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.loan_killed_at[_i];
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.loan_killed_at_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(&self.__loan_killed_at_ind_common),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.loan_live_at.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.loan_live_at[_i];
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.loan_live_at_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(&self.__loan_live_at_ind_common),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.origin_contains_loan_on_entry.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.origin_contains_loan_on_entry[_i];
                        let selection_tuple = (
                            tuple.0.clone(),
                            tuple.1.clone(),
                            tuple.2.clone(),
                        );
                        let rel_ind = &self.origin_contains_loan_on_entry_indices_0_1_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__origin_contains_loan_on_entry_ind_common,
                                ),
                            selection_tuple,
                            (),
                        );
                        let selection_tuple = (tuple.0.clone(), tuple.2.clone());
                        let rel_ind = &self.origin_contains_loan_on_entry_indices_0_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__origin_contains_loan_on_entry_ind_common,
                                ),
                            selection_tuple,
                            (tuple.1.clone(),),
                        );
                        let selection_tuple = (tuple.2.clone(),);
                        let rel_ind = &self.origin_contains_loan_on_entry_indices_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__origin_contains_loan_on_entry_ind_common,
                                ),
                            selection_tuple,
                            (tuple.0.clone(), tuple.1.clone()),
                        );
                    });
                (0..self.origin_live_on_entry.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.origin_live_on_entry[_i];
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.origin_live_on_entry_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__origin_live_on_entry_ind_common,
                                ),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.placeholder_origin.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.placeholder_origin[_i];
                        let selection_tuple = (tuple.0.clone(),);
                        let rel_ind = &self.placeholder_origin_indices_0;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(
                                    &self.__placeholder_origin_ind_common,
                                ),
                            selection_tuple,
                            (),
                        );
                    });
                (0..self.subset.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.subset[_i];
                        let selection_tuple = (tuple.0.clone(),);
                        let rel_ind = &self.subset_indices_0;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__subset_ind_common),
                            selection_tuple,
                            (tuple.1.clone(), tuple.2.clone()),
                        );
                        let selection_tuple = (
                            tuple.0.clone(),
                            tuple.1.clone(),
                            tuple.2.clone(),
                        );
                        let rel_ind = &self.subset_indices_0_1_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__subset_ind_common),
                            selection_tuple,
                            (),
                        );
                        let selection_tuple = (tuple.0.clone(), tuple.2.clone());
                        let rel_ind = &self.subset_indices_0_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__subset_ind_common),
                            selection_tuple,
                            (tuple.1.clone(),),
                        );
                        let selection_tuple = (tuple.1.clone(), tuple.2.clone());
                        let rel_ind = &self.subset_indices_1_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__subset_ind_common),
                            selection_tuple,
                            (tuple.0.clone(),),
                        );
                        let selection_tuple = (tuple.2.clone(),);
                        let rel_ind = &self.subset_indices_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind.to_c_rel_index_write(&self.__subset_ind_common),
                            selection_tuple,
                            (tuple.0.clone(), tuple.1.clone()),
                        );
                    });
                (0..self.subset_error.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.subset_error[_i];
                        let selection_tuple = (
                            tuple.0.clone(),
                            tuple.1.clone(),
                            tuple.2.clone(),
                        );
                        let rel_ind = &self.subset_error_indices_0_1_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            &rel_ind
                                .to_c_rel_index_write(&self.__subset_error_ind_common),
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
                let _type_constraints: ascent::internal::TypeConstraints<T::Point>;
                let _par_constraints: ascent::internal::ParTypeConstraints<T::Point>;
                let _type_constraints: ascent::internal::TypeConstraints<T::Loan>;
                let _par_constraints: ascent::internal::ParTypeConstraints<T::Loan>;
                let _type_constraints: ascent::internal::TypeConstraints<T::Origin>;
                let _par_constraints: ascent::internal::ParTypeConstraints<T::Origin>;
            }
            pub fn summary(&self) -> &'static str {
                "scc 0, is_looping: true:\n  subset <-- subset_indices_1_2_delta, subset_indices_0_2_total+delta, if ⋯ [SIMPLE JOIN]\n  subset <-- subset_indices_1_2_total, subset_indices_0_2_delta, if ⋯ [SIMPLE JOIN]\n  subset <-- subset_indices_2_delta, cfg_edge_indices_0_total, origin_live_on_entry_indices_0_1_total, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]\n  dynamic relations: subset\nscc 1, is_looping: true:\n  origin_contains_loan_on_entry <-- origin_contains_loan_on_entry_indices_0_2_delta, subset_indices_0_2_total [SIMPLE JOIN]\n  origin_contains_loan_on_entry <-- origin_contains_loan_on_entry_indices_2_delta, cfg_edge_indices_0_total, agg loan_killed_at_indices_0_1, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]\n  dynamic relations: origin_contains_loan_on_entry\nscc 2, is_looping: false:\n  loan_live_at <-- origin_contains_loan_on_entry_indices_0_2_total, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]\n  dynamic relations: loan_live_at\nscc 3, is_looping: false:\n  errors <-- loan_invalidated_at_indices_0_1_total, loan_live_at_indices_0_1_total [SIMPLE JOIN]\n  dynamic relations: errors\nscc 4, is_looping: false:\n  subset_error <-- subset_indices_0_total, placeholder_origin_indices_0_total, placeholder_origin_indices_0_total, agg known_placeholder_subset_indices_0_1, if ⋯ [SIMPLE JOIN]\n  dynamic relations: subset_error\n"
            }
            pub fn relation_sizes_summary(&self) -> String {
                use std::fmt::Write;
                let mut res = String::new();
                (&mut res)
                    .write_fmt(
                        format_args!("{0} size: {1}\n", "cfg_edge", self.cfg_edge.len()),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!("{0} size: {1}\n", "errors", self.errors.len()),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "known_placeholder_subset", self
                            .known_placeholder_subset.len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "loan_invalidated_at", self
                            .loan_invalidated_at.len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "loan_killed_at", self.loan_killed_at
                            .len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "loan_live_at", self.loan_live_at.len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "origin_contains_loan_on_entry", self
                            .origin_contains_loan_on_entry.len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "origin_live_on_entry", self
                            .origin_live_on_entry.len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "placeholder_origin", self
                            .placeholder_origin.len(),
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!("{0} size: {1}\n", "subset", self.subset.len()),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "{0} size: {1}\n", "subset_error", self.subset_error.len(),
                        ),
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
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "scc {0}: iterations: {1}, time: {2:?}\n", "3", self
                            .scc_iters[3usize], self.scc_times[3usize],
                        ),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "scc {0}: iterations: {1}, time: {2:?}\n", "4", self
                            .scc_iters[4usize], self.scc_times[4usize],
                        ),
                    )
                    .unwrap();
                res
            }
        }
        impl<T: FactTypes> Default for Polonius<T> {
            fn default() -> Self {
                let mut _self = Polonius {
                    cfg_edge: Default::default(),
                    __cfg_edge_ind_common: Default::default(),
                    cfg_edge_indices_0: Default::default(),
                    cfg_edge_indices_0_1: Default::default(),
                    errors: Default::default(),
                    __errors_ind_common: Default::default(),
                    errors_indices_0_1: Default::default(),
                    known_placeholder_subset: Default::default(),
                    __known_placeholder_subset_ind_common: Default::default(),
                    known_placeholder_subset_indices_0_1: Default::default(),
                    loan_invalidated_at: Default::default(),
                    __loan_invalidated_at_ind_common: Default::default(),
                    loan_invalidated_at_indices_0_1: Default::default(),
                    loan_killed_at: Default::default(),
                    __loan_killed_at_ind_common: Default::default(),
                    loan_killed_at_indices_0_1: Default::default(),
                    loan_live_at: Default::default(),
                    __loan_live_at_ind_common: Default::default(),
                    loan_live_at_indices_0_1: Default::default(),
                    origin_contains_loan_on_entry: Default::default(),
                    __origin_contains_loan_on_entry_ind_common: Default::default(),
                    origin_contains_loan_on_entry_indices_0_1_2: Default::default(),
                    origin_contains_loan_on_entry_indices_0_2: Default::default(),
                    origin_contains_loan_on_entry_indices_2: Default::default(),
                    origin_live_on_entry: Default::default(),
                    __origin_live_on_entry_ind_common: Default::default(),
                    origin_live_on_entry_indices_0_1: Default::default(),
                    placeholder_origin: Default::default(),
                    __placeholder_origin_ind_common: Default::default(),
                    placeholder_origin_indices_0: Default::default(),
                    subset: Default::default(),
                    __subset_ind_common: Default::default(),
                    subset_indices_0: Default::default(),
                    subset_indices_0_1_2: Default::default(),
                    subset_indices_0_2: Default::default(),
                    subset_indices_1_2: Default::default(),
                    subset_indices_2: Default::default(),
                    subset_error: Default::default(),
                    __subset_error_ind_common: Default::default(),
                    subset_error_indices_0_1_2: Default::default(),
                    scc_times: [std::time::Duration::ZERO; 5usize],
                    scc_iters: [0; 5usize],
                    update_time_nanos: Default::default(),
                    update_indices_duration: std::time::Duration::default(),
                };
                _self
            }
        }
        let mut __run_res: Polonius<T> = Polonius::default();
        #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
        {
            ascent::internal::comment("running...");
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
            let _self = &mut __run_res;
            ascent::internal::comment("scc 0");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let mut __subset_ind_common_delta: () = ::std::mem::take(
                    &mut _self.__subset_ind_common,
                );
                let mut __subset_ind_common_total: () = Default::default();
                let mut __subset_ind_common_new: () = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut __subset_ind_common_new,
                    &mut __subset_ind_common_delta,
                    &mut __subset_ind_common_total,
                );
                let mut subset_indices_0_delta: ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = ::std::mem::take(&mut _self.subset_indices_0);
                let mut subset_indices_0_total: ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = Default::default();
                let mut subset_indices_0_new: ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut subset_indices_0_new
                        .to_rel_index_write(&mut __subset_ind_common_new),
                    &mut subset_indices_0_delta
                        .to_rel_index_write(&mut __subset_ind_common_delta),
                    &mut subset_indices_0_total
                        .to_rel_index_write(&mut __subset_ind_common_total),
                );
                let mut subset_indices_0_1_2_delta: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = ::std::mem::take(&mut _self.subset_indices_0_1_2);
                let mut subset_indices_0_1_2_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                let mut subset_indices_0_1_2_new: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut subset_indices_0_1_2_new
                        .to_rel_index_write(&mut __subset_ind_common_new),
                    &mut subset_indices_0_1_2_delta
                        .to_rel_index_write(&mut __subset_ind_common_delta),
                    &mut subset_indices_0_1_2_total
                        .to_rel_index_write(&mut __subset_ind_common_total),
                );
                let mut subset_indices_0_2_delta: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = ::std::mem::take(&mut _self.subset_indices_0_2);
                let mut subset_indices_0_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                let mut subset_indices_0_2_new: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut subset_indices_0_2_new
                        .to_rel_index_write(&mut __subset_ind_common_new),
                    &mut subset_indices_0_2_delta
                        .to_rel_index_write(&mut __subset_ind_common_delta),
                    &mut subset_indices_0_2_total
                        .to_rel_index_write(&mut __subset_ind_common_total),
                );
                let mut subset_indices_1_2_delta: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = ::std::mem::take(&mut _self.subset_indices_1_2);
                let mut subset_indices_1_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                let mut subset_indices_1_2_new: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut subset_indices_1_2_new
                        .to_rel_index_write(&mut __subset_ind_common_new),
                    &mut subset_indices_1_2_delta
                        .to_rel_index_write(&mut __subset_ind_common_delta),
                    &mut subset_indices_1_2_total
                        .to_rel_index_write(&mut __subset_ind_common_total),
                );
                let mut subset_indices_2_delta: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Origin),
                > = ::std::mem::take(&mut _self.subset_indices_2);
                let mut subset_indices_2_total: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Origin),
                > = Default::default();
                let mut subset_indices_2_new: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Origin),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut subset_indices_2_new
                        .to_rel_index_write(&mut __subset_ind_common_new),
                    &mut subset_indices_2_delta
                        .to_rel_index_write(&mut __subset_ind_common_delta),
                    &mut subset_indices_2_total
                        .to_rel_index_write(&mut __subset_ind_common_total),
                );
                _self.__cfg_edge_ind_common.freeze();
                let __cfg_edge_ind_common_total: () = std::mem::take(
                    &mut _self.__cfg_edge_ind_common,
                );
                _self.cfg_edge_indices_0.freeze();
                let cfg_edge_indices_0_total: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Point,),
                > = std::mem::take(&mut _self.cfg_edge_indices_0);
                _self.__origin_live_on_entry_ind_common.freeze();
                let __origin_live_on_entry_ind_common_total: () = std::mem::take(
                    &mut _self.__origin_live_on_entry_ind_common,
                );
                _self.origin_live_on_entry_indices_0_1.freeze();
                let origin_live_on_entry_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Point),
                    (),
                > = std::mem::take(&mut _self.origin_live_on_entry_indices_0_1);
                #[allow(unused_assignments, unused_variables)]
                loop {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    __subset_ind_common_total.freeze();
                    __subset_ind_common_delta.freeze();
                    subset_indices_0_total.freeze();
                    subset_indices_0_delta.freeze();
                    subset_indices_0_1_2_total.freeze();
                    subset_indices_0_1_2_delta.freeze();
                    subset_indices_0_2_total.freeze();
                    subset_indices_0_2_delta.freeze();
                    subset_indices_1_2_total.freeze();
                    subset_indices_1_2_delta.freeze();
                    subset_indices_2_total.freeze();
                    subset_indices_2_delta.freeze();
                    ascent::internal::comment(
                        "subset <-- subset_indices_1_2_delta, subset_indices_0_2_total+delta, if ⋯ [SIMPLE JOIN]",
                    );
                    {
                        if subset_indices_1_2_delta
                            .to_rel_index(&__subset_ind_common_delta)
                            .len()
                            <= ascent::internal::RelIndexCombined::new(
                                    &subset_indices_0_2_total
                                        .to_rel_index(&__subset_ind_common_total),
                                    &subset_indices_0_2_delta
                                        .to_rel_index(&__subset_ind_common_delta),
                                )
                                .len()
                        {
                            subset_indices_1_2_delta
                                .to_rel_index(&__subset_ind_common_delta)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin2 = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                            &subset_indices_0_2_total
                                                .to_rel_index(&__subset_ind_common_total),
                                            &subset_indices_0_2_delta
                                                .to_rel_index(&__subset_ind_common_delta),
                                        )
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin1: &T::Origin = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin3: &T::Origin = __val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total
                                                                    .to_rel_index(&__subset_ind_common_total),
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &subset_indices_0_1_2_delta
                                                                        .to_rel_index(&__subset_ind_common_delta),
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new
                                                                        .to_c_rel_index_write(&__subset_ind_common_new),
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self
                                                                        .subset
                                                                        .push((
                                                                            __new_row.0.clone(),
                                                                            __new_row.1.clone(),
                                                                            __new_row.2.clone(),
                                                                        ));
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.2.clone(),),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                }
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            ascent::internal::RelIndexCombined::new(
                                    &subset_indices_0_2_total
                                        .to_rel_index(&__subset_ind_common_total),
                                    &subset_indices_0_2_delta
                                        .to_rel_index(&__subset_ind_common_delta),
                                )
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin2 = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_1_2_delta
                                        .to_rel_index(&__subset_ind_common_delta)
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin3: &T::Origin = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin1: &T::Origin = __val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total
                                                                    .to_rel_index(&__subset_ind_common_total),
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &subset_indices_0_1_2_delta
                                                                        .to_rel_index(&__subset_ind_common_delta),
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new
                                                                        .to_c_rel_index_write(&__subset_ind_common_new),
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self
                                                                        .subset
                                                                        .push((
                                                                            __new_row.0.clone(),
                                                                            __new_row.1.clone(),
                                                                            __new_row.2.clone(),
                                                                        ));
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.2.clone(),),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                }
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    ascent::internal::comment(
                        "subset <-- subset_indices_1_2_total, subset_indices_0_2_delta, if ⋯ [SIMPLE JOIN]",
                    );
                    {
                        if subset_indices_1_2_total
                            .to_rel_index(&__subset_ind_common_total)
                            .len()
                            <= subset_indices_0_2_delta
                                .to_rel_index(&__subset_ind_common_delta)
                                .len()
                        {
                            subset_indices_1_2_total
                                .to_rel_index(&__subset_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin2 = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_0_2_delta
                                        .to_rel_index(&__subset_ind_common_delta)
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin1: &T::Origin = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin3: &T::Origin = __val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total
                                                                    .to_rel_index(&__subset_ind_common_total),
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &subset_indices_0_1_2_delta
                                                                        .to_rel_index(&__subset_ind_common_delta),
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new
                                                                        .to_c_rel_index_write(&__subset_ind_common_new),
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self
                                                                        .subset
                                                                        .push((
                                                                            __new_row.0.clone(),
                                                                            __new_row.1.clone(),
                                                                            __new_row.2.clone(),
                                                                        ));
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.2.clone(),),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                }
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            subset_indices_0_2_delta
                                .to_rel_index(&__subset_ind_common_delta)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin2 = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_1_2_total
                                        .to_rel_index(&__subset_ind_common_total)
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin3: &T::Origin = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin1: &T::Origin = __val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total
                                                                    .to_rel_index(&__subset_ind_common_total),
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &subset_indices_0_1_2_delta
                                                                        .to_rel_index(&__subset_ind_common_delta),
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new
                                                                        .to_c_rel_index_write(&__subset_ind_common_new),
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self
                                                                        .subset
                                                                        .push((
                                                                            __new_row.0.clone(),
                                                                            __new_row.1.clone(),
                                                                            __new_row.2.clone(),
                                                                        ));
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new
                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                        (__new_row.2.clone(),),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                }
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    ascent::internal::comment(
                        "subset <-- subset_indices_2_delta, cfg_edge_indices_0_total, origin_live_on_entry_indices_0_1_total, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]",
                    );
                    {
                        if subset_indices_2_delta
                            .to_rel_index(&__subset_ind_common_delta)
                            .len()
                            <= cfg_edge_indices_0_total
                                .to_rel_index(&__cfg_edge_ind_common_total)
                                .len()
                        {
                            subset_indices_2_delta
                                .to_rel_index(&__subset_ind_common_delta)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let point1 = __cl1_joined_columns.0;
                                    if let Some(__matching) = cfg_edge_indices_0_total
                                        .to_rel_index(&__cfg_edge_ind_common_total)
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin1: &T::Origin = cl1_val.0;
                                                let origin2: &T::Origin = cl1_val.1;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let point2: &T::Point = __val.0;
                                                        if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                            .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                                            .index_get(&(origin1.clone(), point2.clone()))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                                        .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                                                        .index_get(&(origin2.clone(), point2.clone()))
                                                                    {
                                                                        __matching
                                                                            .for_each(|__val| {
                                                                                let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                                    ascent::internal::Convert::convert(origin1),
                                                                                    ascent::internal::Convert::convert(origin2),
                                                                                    ascent::internal::Convert::convert(point2),
                                                                                );
                                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                    &subset_indices_0_1_2_total
                                                                                        .to_rel_index(&__subset_ind_common_total),
                                                                                    &__new_row,
                                                                                )
                                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                        &subset_indices_0_1_2_delta
                                                                                            .to_rel_index(&__subset_ind_common_delta),
                                                                                        &__new_row,
                                                                                    )
                                                                                {
                                                                                    if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                        &subset_indices_0_1_2_new
                                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                        &__new_row,
                                                                                        (),
                                                                                    ) {
                                                                                        let __new_row_ind = _self
                                                                                            .subset
                                                                                            .push((
                                                                                                __new_row.0.clone(),
                                                                                                __new_row.1.clone(),
                                                                                                __new_row.2.clone(),
                                                                                            ));
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_0_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.0.clone(),),
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_0_2_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.0.clone(), __new_row.2.clone()),
                                                                                            (__new_row.1.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_1_2_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                            (__new_row.0.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_2_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.2.clone(),),
                                                                                            (__new_row.0.clone(), __new_row.1.clone()),
                                                                                        );
                                                                                        __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                                    }
                                                                                }
                                                                            });
                                                                    }
                                                                });
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            cfg_edge_indices_0_total
                                .to_rel_index(&__cfg_edge_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let point1 = __cl1_joined_columns.0;
                                    if let Some(__matching) = subset_indices_2_delta
                                        .to_rel_index(&__subset_ind_common_delta)
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let point2: &T::Point = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin1: &T::Origin = __val.0;
                                                        let origin2: &T::Origin = __val.1;
                                                        if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                            .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                                            .index_get(&(origin1.clone(), point2.clone()))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                                        .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                                                        .index_get(&(origin2.clone(), point2.clone()))
                                                                    {
                                                                        __matching
                                                                            .for_each(|__val| {
                                                                                let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                                    ascent::internal::Convert::convert(origin1),
                                                                                    ascent::internal::Convert::convert(origin2),
                                                                                    ascent::internal::Convert::convert(point2),
                                                                                );
                                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                    &subset_indices_0_1_2_total
                                                                                        .to_rel_index(&__subset_ind_common_total),
                                                                                    &__new_row,
                                                                                )
                                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                        &subset_indices_0_1_2_delta
                                                                                            .to_rel_index(&__subset_ind_common_delta),
                                                                                        &__new_row,
                                                                                    )
                                                                                {
                                                                                    if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                        &subset_indices_0_1_2_new
                                                                                            .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                        &__new_row,
                                                                                        (),
                                                                                    ) {
                                                                                        let __new_row_ind = _self
                                                                                            .subset
                                                                                            .push((
                                                                                                __new_row.0.clone(),
                                                                                                __new_row.1.clone(),
                                                                                                __new_row.2.clone(),
                                                                                            ));
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_0_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.0.clone(),),
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_0_2_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.0.clone(), __new_row.2.clone()),
                                                                                            (__new_row.1.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_1_2_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                            (__new_row.0.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_2_new
                                                                                                .to_c_rel_index_write(&__subset_ind_common_new),
                                                                                            (__new_row.2.clone(),),
                                                                                            (__new_row.0.clone(), __new_row.1.clone()),
                                                                                        );
                                                                                        __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                                    }
                                                                                }
                                                                            });
                                                                    }
                                                                });
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    __subset_ind_common_total.unfreeze();
                    __subset_ind_common_delta.unfreeze();
                    subset_indices_0_total.unfreeze();
                    subset_indices_0_delta.unfreeze();
                    subset_indices_0_1_2_total.unfreeze();
                    subset_indices_0_1_2_delta.unfreeze();
                    subset_indices_0_2_total.unfreeze();
                    subset_indices_0_2_delta.unfreeze();
                    subset_indices_1_2_total.unfreeze();
                    subset_indices_1_2_delta.unfreeze();
                    subset_indices_2_total.unfreeze();
                    subset_indices_2_delta.unfreeze();
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __subset_ind_common_new,
                        &mut __subset_ind_common_delta,
                        &mut __subset_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_indices_0_new
                            .to_rel_index_write(&mut __subset_ind_common_new),
                        &mut subset_indices_0_delta
                            .to_rel_index_write(&mut __subset_ind_common_delta),
                        &mut subset_indices_0_total
                            .to_rel_index_write(&mut __subset_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_indices_0_1_2_new
                            .to_rel_index_write(&mut __subset_ind_common_new),
                        &mut subset_indices_0_1_2_delta
                            .to_rel_index_write(&mut __subset_ind_common_delta),
                        &mut subset_indices_0_1_2_total
                            .to_rel_index_write(&mut __subset_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_indices_0_2_new
                            .to_rel_index_write(&mut __subset_ind_common_new),
                        &mut subset_indices_0_2_delta
                            .to_rel_index_write(&mut __subset_ind_common_delta),
                        &mut subset_indices_0_2_total
                            .to_rel_index_write(&mut __subset_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_indices_1_2_new
                            .to_rel_index_write(&mut __subset_ind_common_new),
                        &mut subset_indices_1_2_delta
                            .to_rel_index_write(&mut __subset_ind_common_delta),
                        &mut subset_indices_1_2_total
                            .to_rel_index_write(&mut __subset_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_indices_2_new
                            .to_rel_index_write(&mut __subset_ind_common_new),
                        &mut subset_indices_2_delta
                            .to_rel_index_write(&mut __subset_ind_common_delta),
                        &mut subset_indices_2_total
                            .to_rel_index_write(&mut __subset_ind_common_total),
                    );
                    _self.scc_iters[0usize] += 1;
                    if !__changed.load(std::sync::atomic::Ordering::Relaxed) {
                        break;
                    }
                }
                _self.__subset_ind_common = __subset_ind_common_total;
                _self.subset_indices_0 = subset_indices_0_total;
                _self.subset_indices_0_1_2 = subset_indices_0_1_2_total;
                _self.subset_indices_0_2 = subset_indices_0_2_total;
                _self.subset_indices_1_2 = subset_indices_1_2_total;
                _self.subset_indices_2 = subset_indices_2_total;
                _self.__cfg_edge_ind_common = __cfg_edge_ind_common_total;
                _self.cfg_edge_indices_0 = cfg_edge_indices_0_total;
                _self
                    .__origin_live_on_entry_ind_common = __origin_live_on_entry_ind_common_total;
                _self
                    .origin_live_on_entry_indices_0_1 = origin_live_on_entry_indices_0_1_total;
                _self.scc_times[0usize] += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 1");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let mut __origin_contains_loan_on_entry_ind_common_delta: () = ::std::mem::take(
                    &mut _self.__origin_contains_loan_on_entry_ind_common,
                );
                let mut __origin_contains_loan_on_entry_ind_common_total: () = Default::default();
                let mut __origin_contains_loan_on_entry_ind_common_new: () = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut __origin_contains_loan_on_entry_ind_common_new,
                    &mut __origin_contains_loan_on_entry_ind_common_delta,
                    &mut __origin_contains_loan_on_entry_ind_common_total,
                );
                let mut origin_contains_loan_on_entry_indices_0_1_2_delta: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Loan, T::Point),
                    (),
                > = ::std::mem::take(
                    &mut _self.origin_contains_loan_on_entry_indices_0_1_2,
                );
                let mut origin_contains_loan_on_entry_indices_0_1_2_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Loan, T::Point),
                    (),
                > = Default::default();
                let mut origin_contains_loan_on_entry_indices_0_1_2_new: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Loan, T::Point),
                    (),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut origin_contains_loan_on_entry_indices_0_1_2_new
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_new,
                        ),
                    &mut origin_contains_loan_on_entry_indices_0_1_2_delta
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_delta,
                        ),
                    &mut origin_contains_loan_on_entry_indices_0_1_2_total
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_total,
                        ),
                );
                let mut origin_contains_loan_on_entry_indices_0_2_delta: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = ::std::mem::take(
                    &mut _self.origin_contains_loan_on_entry_indices_0_2,
                );
                let mut origin_contains_loan_on_entry_indices_0_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = Default::default();
                let mut origin_contains_loan_on_entry_indices_0_2_new: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut origin_contains_loan_on_entry_indices_0_2_new
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_new,
                        ),
                    &mut origin_contains_loan_on_entry_indices_0_2_delta
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_delta,
                        ),
                    &mut origin_contains_loan_on_entry_indices_0_2_total
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_total,
                        ),
                );
                let mut origin_contains_loan_on_entry_indices_2_delta: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Loan),
                > = ::std::mem::take(&mut _self.origin_contains_loan_on_entry_indices_2);
                let mut origin_contains_loan_on_entry_indices_2_total: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Loan),
                > = Default::default();
                let mut origin_contains_loan_on_entry_indices_2_new: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Loan),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut origin_contains_loan_on_entry_indices_2_new
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_new,
                        ),
                    &mut origin_contains_loan_on_entry_indices_2_delta
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_delta,
                        ),
                    &mut origin_contains_loan_on_entry_indices_2_total
                        .to_rel_index_write(
                            &mut __origin_contains_loan_on_entry_ind_common_total,
                        ),
                );
                _self.__cfg_edge_ind_common.freeze();
                let __cfg_edge_ind_common_total: () = std::mem::take(
                    &mut _self.__cfg_edge_ind_common,
                );
                _self.cfg_edge_indices_0.freeze();
                let cfg_edge_indices_0_total: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Point,),
                > = std::mem::take(&mut _self.cfg_edge_indices_0);
                _self.__loan_killed_at_ind_common.freeze();
                let __loan_killed_at_ind_common_total: () = std::mem::take(
                    &mut _self.__loan_killed_at_ind_common,
                );
                _self.loan_killed_at_indices_0_1.freeze();
                let loan_killed_at_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = std::mem::take(&mut _self.loan_killed_at_indices_0_1);
                _self.__origin_live_on_entry_ind_common.freeze();
                let __origin_live_on_entry_ind_common_total: () = std::mem::take(
                    &mut _self.__origin_live_on_entry_ind_common,
                );
                _self.origin_live_on_entry_indices_0_1.freeze();
                let origin_live_on_entry_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Point),
                    (),
                > = std::mem::take(&mut _self.origin_live_on_entry_indices_0_1);
                _self.__subset_ind_common.freeze();
                let __subset_ind_common_total: () = std::mem::take(
                    &mut _self.__subset_ind_common,
                );
                _self.subset_indices_0_2.freeze();
                let subset_indices_0_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = std::mem::take(&mut _self.subset_indices_0_2);
                #[allow(unused_assignments, unused_variables)]
                loop {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    __origin_contains_loan_on_entry_ind_common_total.freeze();
                    __origin_contains_loan_on_entry_ind_common_delta.freeze();
                    origin_contains_loan_on_entry_indices_0_1_2_total.freeze();
                    origin_contains_loan_on_entry_indices_0_1_2_delta.freeze();
                    origin_contains_loan_on_entry_indices_0_2_total.freeze();
                    origin_contains_loan_on_entry_indices_0_2_delta.freeze();
                    origin_contains_loan_on_entry_indices_2_total.freeze();
                    origin_contains_loan_on_entry_indices_2_delta.freeze();
                    ascent::internal::comment(
                        "origin_contains_loan_on_entry <-- origin_contains_loan_on_entry_indices_0_2_delta, subset_indices_0_2_total [SIMPLE JOIN]",
                    );
                    {
                        if origin_contains_loan_on_entry_indices_0_2_delta
                            .to_rel_index(
                                &__origin_contains_loan_on_entry_ind_common_delta,
                            )
                            .len()
                            <= subset_indices_0_2_total
                                .to_rel_index(&__subset_ind_common_total)
                                .len()
                        {
                            origin_contains_loan_on_entry_indices_0_2_delta
                                .to_rel_index(
                                    &__origin_contains_loan_on_entry_ind_common_delta,
                                )
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin1 = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_0_2_total
                                        .to_rel_index(&__subset_ind_common_total)
                                        .c_index_get(&(origin1.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let loan: &T::Loan = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin2: &T::Origin = __val.0;
                                                        let __new_row: (T::Origin, T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(origin2),
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &origin_contains_loan_on_entry_indices_0_1_2_total
                                                                .to_rel_index(
                                                                    &__origin_contains_loan_on_entry_ind_common_total,
                                                                ),
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &origin_contains_loan_on_entry_indices_0_1_2_delta
                                                                    .to_rel_index(
                                                                        &__origin_contains_loan_on_entry_ind_common_delta,
                                                                    ),
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &origin_contains_loan_on_entry_indices_0_1_2_new
                                                                    .to_c_rel_index_write(
                                                                        &__origin_contains_loan_on_entry_ind_common_new,
                                                                    ),
                                                                &__new_row,
                                                                (),
                                                            ) {
                                                                let __new_row_ind = _self
                                                                    .origin_contains_loan_on_entry
                                                                    .push((
                                                                        __new_row.0.clone(),
                                                                        __new_row.1.clone(),
                                                                        __new_row.2.clone(),
                                                                    ));
                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                    &origin_contains_loan_on_entry_indices_0_2_new
                                                                        .to_c_rel_index_write(
                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                        ),
                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                    (__new_row.1.clone(),),
                                                                );
                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                    &origin_contains_loan_on_entry_indices_2_new
                                                                        .to_c_rel_index_write(
                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                        ),
                                                                    (__new_row.2.clone(),),
                                                                    (__new_row.0.clone(), __new_row.1.clone()),
                                                                );
                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            subset_indices_0_2_total
                                .to_rel_index(&__subset_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin1 = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = origin_contains_loan_on_entry_indices_0_2_delta
                                        .to_rel_index(
                                            &__origin_contains_loan_on_entry_ind_common_delta,
                                        )
                                        .c_index_get(&(origin1.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin2: &T::Origin = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let loan: &T::Loan = __val.0;
                                                        let __new_row: (T::Origin, T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(origin2),
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &origin_contains_loan_on_entry_indices_0_1_2_total
                                                                .to_rel_index(
                                                                    &__origin_contains_loan_on_entry_ind_common_total,
                                                                ),
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &origin_contains_loan_on_entry_indices_0_1_2_delta
                                                                    .to_rel_index(
                                                                        &__origin_contains_loan_on_entry_ind_common_delta,
                                                                    ),
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &origin_contains_loan_on_entry_indices_0_1_2_new
                                                                    .to_c_rel_index_write(
                                                                        &__origin_contains_loan_on_entry_ind_common_new,
                                                                    ),
                                                                &__new_row,
                                                                (),
                                                            ) {
                                                                let __new_row_ind = _self
                                                                    .origin_contains_loan_on_entry
                                                                    .push((
                                                                        __new_row.0.clone(),
                                                                        __new_row.1.clone(),
                                                                        __new_row.2.clone(),
                                                                    ));
                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                    &origin_contains_loan_on_entry_indices_0_2_new
                                                                        .to_c_rel_index_write(
                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                        ),
                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                    (__new_row.1.clone(),),
                                                                );
                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                    &origin_contains_loan_on_entry_indices_2_new
                                                                        .to_c_rel_index_write(
                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                        ),
                                                                    (__new_row.2.clone(),),
                                                                    (__new_row.0.clone(), __new_row.1.clone()),
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
                    ascent::internal::comment(
                        "origin_contains_loan_on_entry <-- origin_contains_loan_on_entry_indices_2_delta, cfg_edge_indices_0_total, agg loan_killed_at_indices_0_1, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]",
                    );
                    {
                        if origin_contains_loan_on_entry_indices_2_delta
                            .to_rel_index(
                                &__origin_contains_loan_on_entry_ind_common_delta,
                            )
                            .len()
                            <= cfg_edge_indices_0_total
                                .to_rel_index(&__cfg_edge_ind_common_total)
                                .len()
                        {
                            origin_contains_loan_on_entry_indices_2_delta
                                .to_rel_index(
                                    &__origin_contains_loan_on_entry_ind_common_delta,
                                )
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let point1 = __cl1_joined_columns.0;
                                    if let Some(__matching) = cfg_edge_indices_0_total
                                        .to_rel_index(&__cfg_edge_ind_common_total)
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin: &T::Origin = cl1_val.0;
                                                let loan: &T::Loan = cl1_val.1;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let point2: &T::Point = __val.0;
                                                        let __aggregated_rel = loan_killed_at_indices_0_1_total
                                                            .to_rel_index(&__loan_killed_at_ind_common_total);
                                                        let __matching = __aggregated_rel
                                                            .index_get(&(loan.clone(), point1.clone()));
                                                        let __agg_args = __matching
                                                            .into_iter()
                                                            .flatten()
                                                            .map(|__val| { () });
                                                        for () in ::ascent::aggregators::not(__agg_args) {
                                                            if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                                .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                                                .index_get(&(origin.clone(), point2.clone()))
                                                            {
                                                                __matching
                                                                    .for_each(|__val| {
                                                                        let __new_row: (T::Origin, T::Loan, T::Point) = (
                                                                            ascent::internal::Convert::convert(origin),
                                                                            ascent::internal::Convert::convert(loan),
                                                                            ascent::internal::Convert::convert(point2),
                                                                        );
                                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                            &origin_contains_loan_on_entry_indices_0_1_2_total
                                                                                .to_rel_index(
                                                                                    &__origin_contains_loan_on_entry_ind_common_total,
                                                                                ),
                                                                            &__new_row,
                                                                        )
                                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                &origin_contains_loan_on_entry_indices_0_1_2_delta
                                                                                    .to_rel_index(
                                                                                        &__origin_contains_loan_on_entry_ind_common_delta,
                                                                                    ),
                                                                                &__new_row,
                                                                            )
                                                                        {
                                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                &origin_contains_loan_on_entry_indices_0_1_2_new
                                                                                    .to_c_rel_index_write(
                                                                                        &__origin_contains_loan_on_entry_ind_common_new,
                                                                                    ),
                                                                                &__new_row,
                                                                                (),
                                                                            ) {
                                                                                let __new_row_ind = _self
                                                                                    .origin_contains_loan_on_entry
                                                                                    .push((
                                                                                        __new_row.0.clone(),
                                                                                        __new_row.1.clone(),
                                                                                        __new_row.2.clone(),
                                                                                    ));
                                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                    &origin_contains_loan_on_entry_indices_0_2_new
                                                                                        .to_c_rel_index_write(
                                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                                        ),
                                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                                    (__new_row.1.clone(),),
                                                                                );
                                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                    &origin_contains_loan_on_entry_indices_2_new
                                                                                        .to_c_rel_index_write(
                                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                                        ),
                                                                                    (__new_row.2.clone(),),
                                                                                    (__new_row.0.clone(), __new_row.1.clone()),
                                                                                );
                                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                            }
                                                                        }
                                                                    });
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            cfg_edge_indices_0_total
                                .to_rel_index(&__cfg_edge_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let point1 = __cl1_joined_columns.0;
                                    if let Some(__matching) = origin_contains_loan_on_entry_indices_2_delta
                                        .to_rel_index(
                                            &__origin_contains_loan_on_entry_ind_common_delta,
                                        )
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let point2: &T::Point = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin: &T::Origin = __val.0;
                                                        let loan: &T::Loan = __val.1;
                                                        let __aggregated_rel = loan_killed_at_indices_0_1_total
                                                            .to_rel_index(&__loan_killed_at_ind_common_total);
                                                        let __matching = __aggregated_rel
                                                            .index_get(&(loan.clone(), point1.clone()));
                                                        let __agg_args = __matching
                                                            .into_iter()
                                                            .flatten()
                                                            .map(|__val| { () });
                                                        for () in ::ascent::aggregators::not(__agg_args) {
                                                            if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                                .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                                                .index_get(&(origin.clone(), point2.clone()))
                                                            {
                                                                __matching
                                                                    .for_each(|__val| {
                                                                        let __new_row: (T::Origin, T::Loan, T::Point) = (
                                                                            ascent::internal::Convert::convert(origin),
                                                                            ascent::internal::Convert::convert(loan),
                                                                            ascent::internal::Convert::convert(point2),
                                                                        );
                                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                            &origin_contains_loan_on_entry_indices_0_1_2_total
                                                                                .to_rel_index(
                                                                                    &__origin_contains_loan_on_entry_ind_common_total,
                                                                                ),
                                                                            &__new_row,
                                                                        )
                                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                &origin_contains_loan_on_entry_indices_0_1_2_delta
                                                                                    .to_rel_index(
                                                                                        &__origin_contains_loan_on_entry_ind_common_delta,
                                                                                    ),
                                                                                &__new_row,
                                                                            )
                                                                        {
                                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                &origin_contains_loan_on_entry_indices_0_1_2_new
                                                                                    .to_c_rel_index_write(
                                                                                        &__origin_contains_loan_on_entry_ind_common_new,
                                                                                    ),
                                                                                &__new_row,
                                                                                (),
                                                                            ) {
                                                                                let __new_row_ind = _self
                                                                                    .origin_contains_loan_on_entry
                                                                                    .push((
                                                                                        __new_row.0.clone(),
                                                                                        __new_row.1.clone(),
                                                                                        __new_row.2.clone(),
                                                                                    ));
                                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                    &origin_contains_loan_on_entry_indices_0_2_new
                                                                                        .to_c_rel_index_write(
                                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                                        ),
                                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                                    (__new_row.1.clone(),),
                                                                                );
                                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                    &origin_contains_loan_on_entry_indices_2_new
                                                                                        .to_c_rel_index_write(
                                                                                            &__origin_contains_loan_on_entry_ind_common_new,
                                                                                        ),
                                                                                    (__new_row.2.clone(),),
                                                                                    (__new_row.0.clone(), __new_row.1.clone()),
                                                                                );
                                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                            }
                                                                        }
                                                                    });
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    __origin_contains_loan_on_entry_ind_common_total.unfreeze();
                    __origin_contains_loan_on_entry_ind_common_delta.unfreeze();
                    origin_contains_loan_on_entry_indices_0_1_2_total.unfreeze();
                    origin_contains_loan_on_entry_indices_0_1_2_delta.unfreeze();
                    origin_contains_loan_on_entry_indices_0_2_total.unfreeze();
                    origin_contains_loan_on_entry_indices_0_2_delta.unfreeze();
                    origin_contains_loan_on_entry_indices_2_total.unfreeze();
                    origin_contains_loan_on_entry_indices_2_delta.unfreeze();
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __origin_contains_loan_on_entry_ind_common_new,
                        &mut __origin_contains_loan_on_entry_ind_common_delta,
                        &mut __origin_contains_loan_on_entry_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut origin_contains_loan_on_entry_indices_0_1_2_new
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_new,
                            ),
                        &mut origin_contains_loan_on_entry_indices_0_1_2_delta
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_delta,
                            ),
                        &mut origin_contains_loan_on_entry_indices_0_1_2_total
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_total,
                            ),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut origin_contains_loan_on_entry_indices_0_2_new
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_new,
                            ),
                        &mut origin_contains_loan_on_entry_indices_0_2_delta
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_delta,
                            ),
                        &mut origin_contains_loan_on_entry_indices_0_2_total
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_total,
                            ),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut origin_contains_loan_on_entry_indices_2_new
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_new,
                            ),
                        &mut origin_contains_loan_on_entry_indices_2_delta
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_delta,
                            ),
                        &mut origin_contains_loan_on_entry_indices_2_total
                            .to_rel_index_write(
                                &mut __origin_contains_loan_on_entry_ind_common_total,
                            ),
                    );
                    _self.scc_iters[1usize] += 1;
                    if !__changed.load(std::sync::atomic::Ordering::Relaxed) {
                        break;
                    }
                }
                _self
                    .__origin_contains_loan_on_entry_ind_common = __origin_contains_loan_on_entry_ind_common_total;
                _self
                    .origin_contains_loan_on_entry_indices_0_1_2 = origin_contains_loan_on_entry_indices_0_1_2_total;
                _self
                    .origin_contains_loan_on_entry_indices_0_2 = origin_contains_loan_on_entry_indices_0_2_total;
                _self
                    .origin_contains_loan_on_entry_indices_2 = origin_contains_loan_on_entry_indices_2_total;
                _self.__cfg_edge_ind_common = __cfg_edge_ind_common_total;
                _self.cfg_edge_indices_0 = cfg_edge_indices_0_total;
                _self.__loan_killed_at_ind_common = __loan_killed_at_ind_common_total;
                _self.loan_killed_at_indices_0_1 = loan_killed_at_indices_0_1_total;
                _self
                    .__origin_live_on_entry_ind_common = __origin_live_on_entry_ind_common_total;
                _self
                    .origin_live_on_entry_indices_0_1 = origin_live_on_entry_indices_0_1_total;
                _self.__subset_ind_common = __subset_ind_common_total;
                _self.subset_indices_0_2 = subset_indices_0_2_total;
                _self.scc_times[1usize] += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 2");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let mut __loan_live_at_ind_common_delta: () = ::std::mem::take(
                    &mut _self.__loan_live_at_ind_common,
                );
                let mut __loan_live_at_ind_common_total: () = Default::default();
                let mut __loan_live_at_ind_common_new: () = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut __loan_live_at_ind_common_new,
                    &mut __loan_live_at_ind_common_delta,
                    &mut __loan_live_at_ind_common_total,
                );
                let mut loan_live_at_indices_0_1_delta: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = ::std::mem::take(&mut _self.loan_live_at_indices_0_1);
                let mut loan_live_at_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                let mut loan_live_at_indices_0_1_new: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut loan_live_at_indices_0_1_new
                        .to_rel_index_write(&mut __loan_live_at_ind_common_new),
                    &mut loan_live_at_indices_0_1_delta
                        .to_rel_index_write(&mut __loan_live_at_ind_common_delta),
                    &mut loan_live_at_indices_0_1_total
                        .to_rel_index_write(&mut __loan_live_at_ind_common_total),
                );
                _self.__origin_contains_loan_on_entry_ind_common.freeze();
                let __origin_contains_loan_on_entry_ind_common_total: () = std::mem::take(
                    &mut _self.__origin_contains_loan_on_entry_ind_common,
                );
                _self.origin_contains_loan_on_entry_indices_0_2.freeze();
                let origin_contains_loan_on_entry_indices_0_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = std::mem::take(&mut _self.origin_contains_loan_on_entry_indices_0_2);
                _self.__origin_live_on_entry_ind_common.freeze();
                let __origin_live_on_entry_ind_common_total: () = std::mem::take(
                    &mut _self.__origin_live_on_entry_ind_common,
                );
                _self.origin_live_on_entry_indices_0_1.freeze();
                let origin_live_on_entry_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Point),
                    (),
                > = std::mem::take(&mut _self.origin_live_on_entry_indices_0_1);
                #[allow(unused_assignments, unused_variables)]
                {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    __loan_live_at_ind_common_total.freeze();
                    __loan_live_at_ind_common_delta.freeze();
                    loan_live_at_indices_0_1_total.freeze();
                    loan_live_at_indices_0_1_delta.freeze();
                    ascent::internal::comment(
                        "loan_live_at <-- origin_contains_loan_on_entry_indices_0_2_total, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]",
                    );
                    {
                        if origin_contains_loan_on_entry_indices_0_2_total
                            .to_rel_index(
                                &__origin_contains_loan_on_entry_ind_common_total,
                            )
                            .len()
                            <= origin_live_on_entry_indices_0_1_total
                                .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                .len()
                        {
                            origin_contains_loan_on_entry_indices_0_2_total
                                .to_rel_index(
                                    &__origin_contains_loan_on_entry_ind_common_total,
                                )
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                        .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                        .c_index_get(&(origin.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let loan: &T::Loan = cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __new_row: (T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &loan_live_at_indices_0_1_total
                                                                .to_rel_index(&__loan_live_at_ind_common_total),
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &loan_live_at_indices_0_1_delta
                                                                    .to_rel_index(&__loan_live_at_ind_common_delta),
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &loan_live_at_indices_0_1_new
                                                                    .to_c_rel_index_write(&__loan_live_at_ind_common_new),
                                                                &__new_row,
                                                                (),
                                                            ) {
                                                                let __new_row_ind = _self
                                                                    .loan_live_at
                                                                    .push((__new_row.0, __new_row.1));
                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            origin_live_on_entry_indices_0_1_total
                                .to_rel_index(&__origin_live_on_entry_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = origin_contains_loan_on_entry_indices_0_2_total
                                        .to_rel_index(
                                            &__origin_contains_loan_on_entry_ind_common_total,
                                        )
                                        .c_index_get(&(origin.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let loan: &T::Loan = __val.0;
                                                        let __new_row: (T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &loan_live_at_indices_0_1_total
                                                                .to_rel_index(&__loan_live_at_ind_common_total),
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &loan_live_at_indices_0_1_delta
                                                                    .to_rel_index(&__loan_live_at_ind_common_delta),
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &loan_live_at_indices_0_1_new
                                                                    .to_c_rel_index_write(&__loan_live_at_ind_common_new),
                                                                &__new_row,
                                                                (),
                                                            ) {
                                                                let __new_row_ind = _self
                                                                    .loan_live_at
                                                                    .push((__new_row.0, __new_row.1));
                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    __loan_live_at_ind_common_total.unfreeze();
                    __loan_live_at_ind_common_delta.unfreeze();
                    loan_live_at_indices_0_1_total.unfreeze();
                    loan_live_at_indices_0_1_delta.unfreeze();
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __loan_live_at_ind_common_new,
                        &mut __loan_live_at_ind_common_delta,
                        &mut __loan_live_at_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut loan_live_at_indices_0_1_new
                            .to_rel_index_write(&mut __loan_live_at_ind_common_new),
                        &mut loan_live_at_indices_0_1_delta
                            .to_rel_index_write(&mut __loan_live_at_ind_common_delta),
                        &mut loan_live_at_indices_0_1_total
                            .to_rel_index_write(&mut __loan_live_at_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __loan_live_at_ind_common_new,
                        &mut __loan_live_at_ind_common_delta,
                        &mut __loan_live_at_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut loan_live_at_indices_0_1_new
                            .to_rel_index_write(&mut __loan_live_at_ind_common_new),
                        &mut loan_live_at_indices_0_1_delta
                            .to_rel_index_write(&mut __loan_live_at_ind_common_delta),
                        &mut loan_live_at_indices_0_1_total
                            .to_rel_index_write(&mut __loan_live_at_ind_common_total),
                    );
                    _self.scc_iters[2usize] += 1;
                }
                _self.__loan_live_at_ind_common = __loan_live_at_ind_common_total;
                _self.loan_live_at_indices_0_1 = loan_live_at_indices_0_1_total;
                _self
                    .__origin_contains_loan_on_entry_ind_common = __origin_contains_loan_on_entry_ind_common_total;
                _self
                    .origin_contains_loan_on_entry_indices_0_2 = origin_contains_loan_on_entry_indices_0_2_total;
                _self
                    .__origin_live_on_entry_ind_common = __origin_live_on_entry_ind_common_total;
                _self
                    .origin_live_on_entry_indices_0_1 = origin_live_on_entry_indices_0_1_total;
                _self.scc_times[2usize] += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 3");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let mut __errors_ind_common_delta: () = ::std::mem::take(
                    &mut _self.__errors_ind_common,
                );
                let mut __errors_ind_common_total: () = Default::default();
                let mut __errors_ind_common_new: () = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut __errors_ind_common_new,
                    &mut __errors_ind_common_delta,
                    &mut __errors_ind_common_total,
                );
                let mut errors_indices_0_1_delta: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = ::std::mem::take(&mut _self.errors_indices_0_1);
                let mut errors_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                let mut errors_indices_0_1_new: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut errors_indices_0_1_new
                        .to_rel_index_write(&mut __errors_ind_common_new),
                    &mut errors_indices_0_1_delta
                        .to_rel_index_write(&mut __errors_ind_common_delta),
                    &mut errors_indices_0_1_total
                        .to_rel_index_write(&mut __errors_ind_common_total),
                );
                _self.__loan_invalidated_at_ind_common.freeze();
                let __loan_invalidated_at_ind_common_total: () = std::mem::take(
                    &mut _self.__loan_invalidated_at_ind_common,
                );
                _self.loan_invalidated_at_indices_0_1.freeze();
                let loan_invalidated_at_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = std::mem::take(&mut _self.loan_invalidated_at_indices_0_1);
                _self.__loan_live_at_ind_common.freeze();
                let __loan_live_at_ind_common_total: () = std::mem::take(
                    &mut _self.__loan_live_at_ind_common,
                );
                _self.loan_live_at_indices_0_1.freeze();
                let loan_live_at_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = std::mem::take(&mut _self.loan_live_at_indices_0_1);
                #[allow(unused_assignments, unused_variables)]
                {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    __errors_ind_common_total.freeze();
                    __errors_ind_common_delta.freeze();
                    errors_indices_0_1_total.freeze();
                    errors_indices_0_1_delta.freeze();
                    ascent::internal::comment(
                        "errors <-- loan_invalidated_at_indices_0_1_total, loan_live_at_indices_0_1_total [SIMPLE JOIN]",
                    );
                    {
                        if loan_invalidated_at_indices_0_1_total
                            .to_rel_index(&__loan_invalidated_at_ind_common_total)
                            .len()
                            <= loan_live_at_indices_0_1_total
                                .to_rel_index(&__loan_live_at_ind_common_total)
                                .len()
                        {
                            loan_invalidated_at_indices_0_1_total
                                .to_rel_index(&__loan_invalidated_at_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let loan = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = loan_live_at_indices_0_1_total
                                        .to_rel_index(&__loan_live_at_ind_common_total)
                                        .c_index_get(&(loan.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __new_row: (T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &errors_indices_0_1_total
                                                                .to_rel_index(&__errors_ind_common_total),
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &errors_indices_0_1_delta
                                                                    .to_rel_index(&__errors_ind_common_delta),
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &errors_indices_0_1_new
                                                                    .to_c_rel_index_write(&__errors_ind_common_new),
                                                                &__new_row,
                                                                (),
                                                            ) {
                                                                let __new_row_ind = _self
                                                                    .errors
                                                                    .push((__new_row.0, __new_row.1));
                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            loan_live_at_indices_0_1_total
                                .to_rel_index(&__loan_live_at_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let loan = __cl1_joined_columns.0;
                                    let point = __cl1_joined_columns.1;
                                    if let Some(__matching) = loan_invalidated_at_indices_0_1_total
                                        .to_rel_index(&__loan_invalidated_at_ind_common_total)
                                        .c_index_get(&(loan.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __new_row: (T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &errors_indices_0_1_total
                                                                .to_rel_index(&__errors_ind_common_total),
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &errors_indices_0_1_delta
                                                                    .to_rel_index(&__errors_ind_common_delta),
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &errors_indices_0_1_new
                                                                    .to_c_rel_index_write(&__errors_ind_common_new),
                                                                &__new_row,
                                                                (),
                                                            ) {
                                                                let __new_row_ind = _self
                                                                    .errors
                                                                    .push((__new_row.0, __new_row.1));
                                                                __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                            }
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    __errors_ind_common_total.unfreeze();
                    __errors_ind_common_delta.unfreeze();
                    errors_indices_0_1_total.unfreeze();
                    errors_indices_0_1_delta.unfreeze();
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __errors_ind_common_new,
                        &mut __errors_ind_common_delta,
                        &mut __errors_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut errors_indices_0_1_new
                            .to_rel_index_write(&mut __errors_ind_common_new),
                        &mut errors_indices_0_1_delta
                            .to_rel_index_write(&mut __errors_ind_common_delta),
                        &mut errors_indices_0_1_total
                            .to_rel_index_write(&mut __errors_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __errors_ind_common_new,
                        &mut __errors_ind_common_delta,
                        &mut __errors_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut errors_indices_0_1_new
                            .to_rel_index_write(&mut __errors_ind_common_new),
                        &mut errors_indices_0_1_delta
                            .to_rel_index_write(&mut __errors_ind_common_delta),
                        &mut errors_indices_0_1_total
                            .to_rel_index_write(&mut __errors_ind_common_total),
                    );
                    _self.scc_iters[3usize] += 1;
                }
                _self.__errors_ind_common = __errors_ind_common_total;
                _self.errors_indices_0_1 = errors_indices_0_1_total;
                _self
                    .__loan_invalidated_at_ind_common = __loan_invalidated_at_ind_common_total;
                _self
                    .loan_invalidated_at_indices_0_1 = loan_invalidated_at_indices_0_1_total;
                _self.__loan_live_at_ind_common = __loan_live_at_ind_common_total;
                _self.loan_live_at_indices_0_1 = loan_live_at_indices_0_1_total;
                _self.scc_times[3usize] += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 4");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let mut __subset_error_ind_common_delta: () = ::std::mem::take(
                    &mut _self.__subset_error_ind_common,
                );
                let mut __subset_error_ind_common_total: () = Default::default();
                let mut __subset_error_ind_common_new: () = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut __subset_error_ind_common_new,
                    &mut __subset_error_ind_common_delta,
                    &mut __subset_error_ind_common_total,
                );
                let mut subset_error_indices_0_1_2_delta: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = ::std::mem::take(&mut _self.subset_error_indices_0_1_2);
                let mut subset_error_indices_0_1_2_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                let mut subset_error_indices_0_1_2_new: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                ::ascent::internal::RelIndexMerge::init(
                    &mut subset_error_indices_0_1_2_new
                        .to_rel_index_write(&mut __subset_error_ind_common_new),
                    &mut subset_error_indices_0_1_2_delta
                        .to_rel_index_write(&mut __subset_error_ind_common_delta),
                    &mut subset_error_indices_0_1_2_total
                        .to_rel_index_write(&mut __subset_error_ind_common_total),
                );
                _self.__known_placeholder_subset_ind_common.freeze();
                let __known_placeholder_subset_ind_common_total: () = std::mem::take(
                    &mut _self.__known_placeholder_subset_ind_common,
                );
                _self.known_placeholder_subset_indices_0_1.freeze();
                let known_placeholder_subset_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin),
                    (),
                > = std::mem::take(&mut _self.known_placeholder_subset_indices_0_1);
                _self.__placeholder_origin_ind_common.freeze();
                let __placeholder_origin_ind_common_total: () = std::mem::take(
                    &mut _self.__placeholder_origin_ind_common,
                );
                _self.placeholder_origin_indices_0.freeze();
                let placeholder_origin_indices_0_total: ascent::internal::CRelFullIndex<
                    (T::Origin,),
                    (),
                > = std::mem::take(&mut _self.placeholder_origin_indices_0);
                _self.__subset_ind_common.freeze();
                let __subset_ind_common_total: () = std::mem::take(
                    &mut _self.__subset_ind_common,
                );
                _self.subset_indices_0.freeze();
                let subset_indices_0_total: ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = std::mem::take(&mut _self.subset_indices_0);
                #[allow(unused_assignments, unused_variables)]
                {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    __subset_error_ind_common_total.freeze();
                    __subset_error_ind_common_delta.freeze();
                    subset_error_indices_0_1_2_total.freeze();
                    subset_error_indices_0_1_2_delta.freeze();
                    ascent::internal::comment(
                        "subset_error <-- subset_indices_0_total, placeholder_origin_indices_0_total, placeholder_origin_indices_0_total, agg known_placeholder_subset_indices_0_1, if ⋯ [SIMPLE JOIN]",
                    );
                    {
                        if subset_indices_0_total
                            .to_rel_index(&__subset_ind_common_total)
                            .len()
                            <= placeholder_origin_indices_0_total
                                .to_rel_index(&__placeholder_origin_ind_common_total)
                                .len()
                        {
                            subset_indices_0_total
                                .to_rel_index(&__subset_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin1 = __cl1_joined_columns.0;
                                    if let Some(__matching) = placeholder_origin_indices_0_total
                                        .to_rel_index(&__placeholder_origin_ind_common_total)
                                        .c_index_get(&(origin1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let cl1_val = cl1_val.tuple_of_borrowed();
                                                let origin2: &T::Origin = cl1_val.0;
                                                let point: &T::Point = cl1_val.1;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        if let Some(__matching) = placeholder_origin_indices_0_total
                                                            .to_rel_index(&__placeholder_origin_ind_common_total)
                                                            .index_get(&(origin2.clone(),))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    let __aggregated_rel = known_placeholder_subset_indices_0_1_total
                                                                        .to_rel_index(&__known_placeholder_subset_ind_common_total);
                                                                    let __matching = __aggregated_rel
                                                                        .index_get(&(origin1.clone(), origin2.clone()));
                                                                    let __agg_args = __matching
                                                                        .into_iter()
                                                                        .flatten()
                                                                        .map(|__val| { () });
                                                                    for () in ::ascent::aggregators::not(__agg_args) {
                                                                        if origin1 != origin2 {
                                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                                ascent::internal::Convert::convert(origin1),
                                                                                ascent::internal::Convert::convert(origin2),
                                                                                ascent::internal::Convert::convert(point),
                                                                            );
                                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                &subset_error_indices_0_1_2_total
                                                                                    .to_rel_index(&__subset_error_ind_common_total),
                                                                                &__new_row,
                                                                            )
                                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                    &subset_error_indices_0_1_2_delta
                                                                                        .to_rel_index(&__subset_error_ind_common_delta),
                                                                                    &__new_row,
                                                                                )
                                                                            {
                                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                    &subset_error_indices_0_1_2_new
                                                                                        .to_c_rel_index_write(&__subset_error_ind_common_new),
                                                                                    &__new_row,
                                                                                    (),
                                                                                ) {
                                                                                    let __new_row_ind = _self
                                                                                        .subset_error
                                                                                        .push((__new_row.0, __new_row.1, __new_row.2));
                                                                                    __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                });
                                                        }
                                                    });
                                            });
                                    }
                                });
                        } else {
                            placeholder_origin_indices_0_total
                                .to_rel_index(&__placeholder_origin_ind_common_total)
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let __cl1_joined_columns = __cl1_joined_columns
                                        .tuple_of_borrowed();
                                    let origin1 = __cl1_joined_columns.0;
                                    if let Some(__matching) = subset_indices_0_total
                                        .to_rel_index(&__subset_ind_common_total)
                                        .c_index_get(&(origin1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let origin2: &T::Origin = __val.0;
                                                        let point: &T::Point = __val.1;
                                                        if let Some(__matching) = placeholder_origin_indices_0_total
                                                            .to_rel_index(&__placeholder_origin_ind_common_total)
                                                            .index_get(&(origin2.clone(),))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    let __aggregated_rel = known_placeholder_subset_indices_0_1_total
                                                                        .to_rel_index(&__known_placeholder_subset_ind_common_total);
                                                                    let __matching = __aggregated_rel
                                                                        .index_get(&(origin1.clone(), origin2.clone()));
                                                                    let __agg_args = __matching
                                                                        .into_iter()
                                                                        .flatten()
                                                                        .map(|__val| { () });
                                                                    for () in ::ascent::aggregators::not(__agg_args) {
                                                                        if origin1 != origin2 {
                                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                                ascent::internal::Convert::convert(origin1),
                                                                                ascent::internal::Convert::convert(origin2),
                                                                                ascent::internal::Convert::convert(point),
                                                                            );
                                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                &subset_error_indices_0_1_2_total
                                                                                    .to_rel_index(&__subset_error_ind_common_total),
                                                                                &__new_row,
                                                                            )
                                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                    &subset_error_indices_0_1_2_delta
                                                                                        .to_rel_index(&__subset_error_ind_common_delta),
                                                                                    &__new_row,
                                                                                )
                                                                            {
                                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                    &subset_error_indices_0_1_2_new
                                                                                        .to_c_rel_index_write(&__subset_error_ind_common_new),
                                                                                    &__new_row,
                                                                                    (),
                                                                                ) {
                                                                                    let __new_row_ind = _self
                                                                                        .subset_error
                                                                                        .push((__new_row.0, __new_row.1, __new_row.2));
                                                                                    __changed.store(true, std::sync::atomic::Ordering::Relaxed);
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                });
                                                        }
                                                    });
                                            });
                                    }
                                });
                        }
                    }
                    __subset_error_ind_common_total.unfreeze();
                    __subset_error_ind_common_delta.unfreeze();
                    subset_error_indices_0_1_2_total.unfreeze();
                    subset_error_indices_0_1_2_delta.unfreeze();
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __subset_error_ind_common_new,
                        &mut __subset_error_ind_common_delta,
                        &mut __subset_error_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_error_indices_0_1_2_new
                            .to_rel_index_write(&mut __subset_error_ind_common_new),
                        &mut subset_error_indices_0_1_2_delta
                            .to_rel_index_write(&mut __subset_error_ind_common_delta),
                        &mut subset_error_indices_0_1_2_total
                            .to_rel_index_write(&mut __subset_error_ind_common_total),
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut __subset_error_ind_common_new,
                        &mut __subset_error_ind_common_delta,
                        &mut __subset_error_ind_common_total,
                    );
                    ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                        &mut subset_error_indices_0_1_2_new
                            .to_rel_index_write(&mut __subset_error_ind_common_new),
                        &mut subset_error_indices_0_1_2_delta
                            .to_rel_index_write(&mut __subset_error_ind_common_delta),
                        &mut subset_error_indices_0_1_2_total
                            .to_rel_index_write(&mut __subset_error_ind_common_total),
                    );
                    _self.scc_iters[4usize] += 1;
                }
                _self.__subset_error_ind_common = __subset_error_ind_common_total;
                _self.subset_error_indices_0_1_2 = subset_error_indices_0_1_2_total;
                _self
                    .__known_placeholder_subset_ind_common = __known_placeholder_subset_ind_common_total;
                _self
                    .known_placeholder_subset_indices_0_1 = known_placeholder_subset_indices_0_1_total;
                _self
                    .__placeholder_origin_ind_common = __placeholder_origin_ind_common_total;
                _self.placeholder_origin_indices_0 = placeholder_origin_indices_0_total;
                _self.__subset_ind_common = __subset_ind_common_total;
                _self.subset_indices_0 = subset_indices_0_total;
                _self.scc_times[4usize] += _scc_start_time.elapsed();
            }
        }
        __run_res
    };
}
fn main() {}
