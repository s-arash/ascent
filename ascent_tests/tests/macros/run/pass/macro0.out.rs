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
physical indices:
 cfg_edge_indices_0; cfg_edge_indices_0_1*/
            pub cfg_edge: ::ascent::boxcar::Vec<(T::Point, T::Point)>,
            pub cfg_edge_indices_0: ascent::internal::CRelIndex<
                (T::Point,),
                (T::Point,),
            >,
            pub cfg_edge_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Point, T::Point),
                (),
            >,
            /**
physical indices:
 errors_indices_0_1*/
            pub errors: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub errors_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
physical indices:
 known_placeholder_subset_indices_0_1*/
            pub known_placeholder_subset: ::ascent::boxcar::Vec<(T::Origin, T::Origin)>,
            pub known_placeholder_subset_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Origin, T::Origin),
                (),
            >,
            /**
physical indices:
 loan_invalidated_at_indices_0_1*/
            pub loan_invalidated_at: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub loan_invalidated_at_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
physical indices:
 loan_killed_at_indices_0_1*/
            pub loan_killed_at: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub loan_killed_at_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
physical indices:
 loan_live_at_indices_0_1*/
            pub loan_live_at: ::ascent::boxcar::Vec<(T::Loan, T::Point)>,
            pub loan_live_at_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Loan, T::Point),
                (),
            >,
            /**
physical indices:
 origin_contains_loan_on_entry_indices_0_1_2; origin_contains_loan_on_entry_indices_0_2; origin_contains_loan_on_entry_indices_2*/
            pub origin_contains_loan_on_entry: ::ascent::boxcar::Vec<
                (T::Origin, T::Loan, T::Point),
            >,
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
physical indices:
 origin_live_on_entry_indices_0_1*/
            pub origin_live_on_entry: ::ascent::boxcar::Vec<(T::Origin, T::Point)>,
            pub origin_live_on_entry_indices_0_1: ascent::internal::CRelFullIndex<
                (T::Origin, T::Point),
                (),
            >,
            /**
physical indices:
 placeholder_origin_indices_0*/
            pub placeholder_origin: ::ascent::boxcar::Vec<(T::Origin,)>,
            pub placeholder_origin_indices_0: ascent::internal::CRelFullIndex<
                (T::Origin,),
                (),
            >,
            /**
physical indices:
 subset_indices_0; subset_indices_0_1_2; subset_indices_0_2; subset_indices_1_2; subset_indices_2*/
            pub subset: ::ascent::boxcar::Vec<(T::Origin, T::Origin, T::Point)>,
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
physical indices:
 subset_error_indices_0_1_2*/
            pub subset_error: ::ascent::boxcar::Vec<(T::Origin, T::Origin, T::Point)>,
            pub subset_error_indices_0_1_2: ascent::internal::CRelFullIndex<
                (T::Origin, T::Origin, T::Point),
                (),
            >,
            pub scc0_duration: std::time::Duration,
            pub scc1_duration: std::time::Duration,
            pub scc2_duration: std::time::Duration,
            pub scc3_duration: std::time::Duration,
            pub scc4_duration: std::time::Duration,
            pub update_time_nanos: std::sync::atomic::AtomicU64,
        }
        impl<T: FactTypes> Polonius<T> {
            fn update_indices_priv(&mut self) {
                use ascent::rayon::iter::{IntoParallelIterator, ParallelIterator};
                (0..self.cfg_edge.len())
                    .into_par_iter()
                    .for_each(|_i| {
                        let tuple = &self.cfg_edge[_i];
                        let selection_tuple = (tuple.0.clone(),);
                        let rel_ind = &self.cfg_edge_indices_0;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
                            selection_tuple,
                            (tuple.1.clone(),),
                        );
                        let selection_tuple = (tuple.0.clone(), tuple.1.clone());
                        let rel_ind = &self.cfg_edge_indices_0_1;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
                            selection_tuple,
                            (),
                        );
                        let selection_tuple = (tuple.0.clone(), tuple.2.clone());
                        let rel_ind = &self.origin_contains_loan_on_entry_indices_0_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
                            selection_tuple,
                            (tuple.1.clone(),),
                        );
                        let selection_tuple = (tuple.2.clone(),);
                        let rel_ind = &self.origin_contains_loan_on_entry_indices_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
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
                            rel_ind,
                            selection_tuple,
                            (),
                        );
                        let selection_tuple = (tuple.0.clone(), tuple.2.clone());
                        let rel_ind = &self.subset_indices_0_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
                            selection_tuple,
                            (tuple.1.clone(),),
                        );
                        let selection_tuple = (tuple.1.clone(), tuple.2.clone());
                        let rel_ind = &self.subset_indices_1_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
                            selection_tuple,
                            (tuple.0.clone(),),
                        );
                        let selection_tuple = (tuple.2.clone(),);
                        let rel_ind = &self.subset_indices_2;
                        ascent::internal::CRelIndexWrite::index_insert(
                            rel_ind,
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
                            rel_ind,
                            selection_tuple,
                            (),
                        );
                    });
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
                        format_args!("scc {0} time: {1:?}\n", "0", self.scc0_duration),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!("scc {0} time: {1:?}\n", "1", self.scc1_duration),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!("scc {0} time: {1:?}\n", "2", self.scc2_duration),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!("scc {0} time: {1:?}\n", "3", self.scc3_duration),
                    )
                    .unwrap();
                (&mut res)
                    .write_fmt(
                        format_args!("scc {0} time: {1:?}\n", "4", self.scc4_duration),
                    )
                    .unwrap();
                res
            }
        }
        impl<T: FactTypes> Default for Polonius<T> {
            fn default() -> Self {
                let mut _self = Polonius {
                    cfg_edge: Default::default(),
                    cfg_edge_indices_0: Default::default(),
                    cfg_edge_indices_0_1: Default::default(),
                    errors: Default::default(),
                    errors_indices_0_1: Default::default(),
                    known_placeholder_subset: Default::default(),
                    known_placeholder_subset_indices_0_1: Default::default(),
                    loan_invalidated_at: Default::default(),
                    loan_invalidated_at_indices_0_1: Default::default(),
                    loan_killed_at: Default::default(),
                    loan_killed_at_indices_0_1: Default::default(),
                    loan_live_at: Default::default(),
                    loan_live_at_indices_0_1: Default::default(),
                    origin_contains_loan_on_entry: Default::default(),
                    origin_contains_loan_on_entry_indices_0_1_2: Default::default(),
                    origin_contains_loan_on_entry_indices_0_2: Default::default(),
                    origin_contains_loan_on_entry_indices_2: Default::default(),
                    origin_live_on_entry: Default::default(),
                    origin_live_on_entry_indices_0_1: Default::default(),
                    placeholder_origin: Default::default(),
                    placeholder_origin_indices_0: Default::default(),
                    subset: Default::default(),
                    subset_indices_0: Default::default(),
                    subset_indices_0_1_2: Default::default(),
                    subset_indices_0_2: Default::default(),
                    subset_indices_1_2: Default::default(),
                    subset_indices_2: Default::default(),
                    subset_error: Default::default(),
                    subset_error_indices_0_1_2: Default::default(),
                    scc0_duration: std::time::Duration::ZERO,
                    scc1_duration: std::time::Duration::ZERO,
                    scc2_duration: std::time::Duration::ZERO,
                    scc3_duration: std::time::Duration::ZERO,
                    scc4_duration: std::time::Duration::ZERO,
                    update_time_nanos: Default::default(),
                };
                _self
            }
        }
        let mut __run_res: Polonius<T> = Polonius::default();
        #[allow(unused_imports)]
        {
            ascent::internal::comment("running...");
            use core::cmp::PartialEq;
            use ascent::internal::RelIndexRead;
            use ascent::internal::RelIndexReadAll;
            use ascent::rayon::iter::ParallelBridge;
            use ascent::rayon::iter::ParallelIterator;
            use ascent::internal::CRelIndexRead;
            use ascent::internal::CRelIndexReadAll;
            let _self = &mut __run_res;
            ascent::internal::comment("scc 0");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let subset_indices_0_delta: &mut ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = &mut _self.subset_indices_0;
                let mut subset_indices_0_total: ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = Default::default();
                let mut subset_indices_0_new: ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = Default::default();
                let subset_indices_0_1_2_delta: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = &mut _self.subset_indices_0_1_2;
                let mut subset_indices_0_1_2_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                let mut subset_indices_0_1_2_new: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                let subset_indices_0_2_delta: &mut ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = &mut _self.subset_indices_0_2;
                let mut subset_indices_0_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                let mut subset_indices_0_2_new: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                let subset_indices_1_2_delta: &mut ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = &mut _self.subset_indices_1_2;
                let mut subset_indices_1_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                let mut subset_indices_1_2_new: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = Default::default();
                let subset_indices_2_delta: &mut ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Origin),
                > = &mut _self.subset_indices_2;
                let mut subset_indices_2_total: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Origin),
                > = Default::default();
                let mut subset_indices_2_new: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Origin),
                > = Default::default();
                let cfg_edge_indices_0_total: &mut ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Point,),
                > = &mut _self.cfg_edge_indices_0;
                let origin_live_on_entry_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Point),
                    (),
                > = &mut _self.origin_live_on_entry_indices_0_1;
                #[allow(unused_assignments, unused_variables)]
                loop {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
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
                    cfg_edge_indices_0_total.freeze();
                    origin_live_on_entry_indices_0_1_total.freeze();
                    ascent::internal::comment(
                        "subset <-- subset_indices_1_2_delta, subset_indices_0_2_total+delta, if ⋯ [SIMPLE JOIN]",
                    );
                    {
                        if subset_indices_1_2_delta.len()
                            <= ascent::internal::RelIndexCombined::new(
                                    &subset_indices_0_2_total,
                                    subset_indices_0_2_delta,
                                )
                                .len()
                        {
                            subset_indices_1_2_delta
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin2 = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                            &subset_indices_0_2_total,
                                            subset_indices_0_2_delta,
                                        )
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin1 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin3 = &__val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    subset_indices_0_1_2_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new,
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
                                                                        &subset_indices_0_new,
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new,
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new,
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new,
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
                                    &subset_indices_0_2_total,
                                    subset_indices_0_2_delta,
                                )
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin2 = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_1_2_delta
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin3 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin1 = &__val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    subset_indices_0_1_2_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new,
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
                                                                        &subset_indices_0_new,
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new,
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new,
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new,
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
                        if subset_indices_1_2_total.len()
                            <= subset_indices_0_2_delta.len()
                        {
                            subset_indices_1_2_total
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin2 = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_0_2_delta
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin1 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin3 = &__val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    subset_indices_0_1_2_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new,
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
                                                                        &subset_indices_0_new,
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new,
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new,
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin2 = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_1_2_total
                                        .c_index_get(&(origin2.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin3 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin1 = &__val.0;
                                                        if origin1 != origin3 {
                                                            let __new_row: (T::Origin, T::Origin, T::Point) = (
                                                                ascent::internal::Convert::convert(origin1),
                                                                ascent::internal::Convert::convert(origin3),
                                                                ascent::internal::Convert::convert(point),
                                                            );
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &subset_indices_0_1_2_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    subset_indices_0_1_2_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                    &subset_indices_0_1_2_new,
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
                                                                        &subset_indices_0_new,
                                                                        (__new_row.0.clone(),),
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_0_2_new,
                                                                        (__new_row.0.clone(), __new_row.2.clone()),
                                                                        (__new_row.1.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_1_2_new,
                                                                        (__new_row.1.clone(), __new_row.2.clone()),
                                                                        (__new_row.0.clone(),),
                                                                    );
                                                                    ::ascent::internal::CRelIndexWrite::index_insert(
                                                                        &subset_indices_2_new,
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
                        if subset_indices_2_delta.len() <= cfg_edge_indices_0_total.len()
                        {
                            subset_indices_2_delta
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let point1 = &__cl1_joined_columns.0;
                                    if let Some(__matching) = cfg_edge_indices_0_total
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin1 = &cl1_val.0;
                                                let origin2 = &cl1_val.1;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let point2 = &__val.0;
                                                        if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                            .index_get(&(origin1.clone(), point2.clone()))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    if let Some(__matching) = origin_live_on_entry_indices_0_1_total
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
                                                                                    &subset_indices_0_1_2_total,
                                                                                    &__new_row,
                                                                                )
                                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                        subset_indices_0_1_2_delta,
                                                                                        &__new_row,
                                                                                    )
                                                                                {
                                                                                    if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                        &subset_indices_0_1_2_new,
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
                                                                                            &subset_indices_0_new,
                                                                                            (__new_row.0.clone(),),
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_0_2_new,
                                                                                            (__new_row.0.clone(), __new_row.2.clone()),
                                                                                            (__new_row.1.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_1_2_new,
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                            (__new_row.0.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_2_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let point1 = &__cl1_joined_columns.0;
                                    if let Some(__matching) = subset_indices_2_delta
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let point2 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin1 = &__val.0;
                                                        let origin2 = &__val.1;
                                                        if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                                            .index_get(&(origin1.clone(), point2.clone()))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    if let Some(__matching) = origin_live_on_entry_indices_0_1_total
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
                                                                                    &subset_indices_0_1_2_total,
                                                                                    &__new_row,
                                                                                )
                                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                        subset_indices_0_1_2_delta,
                                                                                        &__new_row,
                                                                                    )
                                                                                {
                                                                                    if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                        &subset_indices_0_1_2_new,
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
                                                                                            &subset_indices_0_new,
                                                                                            (__new_row.0.clone(),),
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_0_2_new,
                                                                                            (__new_row.0.clone(), __new_row.2.clone()),
                                                                                            (__new_row.1.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_1_2_new,
                                                                                            (__new_row.1.clone(), __new_row.2.clone()),
                                                                                            (__new_row.0.clone(),),
                                                                                        );
                                                                                        ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                            &subset_indices_2_new,
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
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_indices_0_delta,
                        &mut subset_indices_0_total,
                    );
                    std::mem::swap(&mut subset_indices_0_new, subset_indices_0_delta);
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_indices_0_1_2_delta,
                        &mut subset_indices_0_1_2_total,
                    );
                    std::mem::swap(
                        &mut subset_indices_0_1_2_new,
                        subset_indices_0_1_2_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_indices_0_2_delta,
                        &mut subset_indices_0_2_total,
                    );
                    std::mem::swap(
                        &mut subset_indices_0_2_new,
                        subset_indices_0_2_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_indices_1_2_delta,
                        &mut subset_indices_1_2_total,
                    );
                    std::mem::swap(
                        &mut subset_indices_1_2_new,
                        subset_indices_1_2_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_indices_2_delta,
                        &mut subset_indices_2_total,
                    );
                    std::mem::swap(&mut subset_indices_2_new, subset_indices_2_delta);
                    if !__changed.load(std::sync::atomic::Ordering::Relaxed) {
                        break;
                    }
                }
                _self.subset_indices_0 = subset_indices_0_total;
                _self.subset_indices_0_1_2 = subset_indices_0_1_2_total;
                _self.subset_indices_0_2 = subset_indices_0_2_total;
                _self.subset_indices_1_2 = subset_indices_1_2_total;
                _self.subset_indices_2 = subset_indices_2_total;
                _self.scc0_duration += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 1");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let origin_contains_loan_on_entry_indices_0_1_2_delta: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Loan, T::Point),
                    (),
                > = &mut _self.origin_contains_loan_on_entry_indices_0_1_2;
                let mut origin_contains_loan_on_entry_indices_0_1_2_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Loan, T::Point),
                    (),
                > = Default::default();
                let mut origin_contains_loan_on_entry_indices_0_1_2_new: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Loan, T::Point),
                    (),
                > = Default::default();
                let origin_contains_loan_on_entry_indices_0_2_delta: &mut ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = &mut _self.origin_contains_loan_on_entry_indices_0_2;
                let mut origin_contains_loan_on_entry_indices_0_2_total: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = Default::default();
                let mut origin_contains_loan_on_entry_indices_0_2_new: ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = Default::default();
                let origin_contains_loan_on_entry_indices_2_delta: &mut ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Loan),
                > = &mut _self.origin_contains_loan_on_entry_indices_2;
                let mut origin_contains_loan_on_entry_indices_2_total: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Loan),
                > = Default::default();
                let mut origin_contains_loan_on_entry_indices_2_new: ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Origin, T::Loan),
                > = Default::default();
                let cfg_edge_indices_0_total: &mut ascent::internal::CRelIndex<
                    (T::Point,),
                    (T::Point,),
                > = &mut _self.cfg_edge_indices_0;
                let loan_killed_at_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = &mut _self.loan_killed_at_indices_0_1;
                let origin_live_on_entry_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Point),
                    (),
                > = &mut _self.origin_live_on_entry_indices_0_1;
                let subset_indices_0_2_total: &mut ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Origin,),
                > = &mut _self.subset_indices_0_2;
                #[allow(unused_assignments, unused_variables)]
                loop {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    origin_contains_loan_on_entry_indices_0_1_2_total.freeze();
                    origin_contains_loan_on_entry_indices_0_1_2_delta.freeze();
                    origin_contains_loan_on_entry_indices_0_2_total.freeze();
                    origin_contains_loan_on_entry_indices_0_2_delta.freeze();
                    origin_contains_loan_on_entry_indices_2_total.freeze();
                    origin_contains_loan_on_entry_indices_2_delta.freeze();
                    cfg_edge_indices_0_total.freeze();
                    loan_killed_at_indices_0_1_total.freeze();
                    origin_live_on_entry_indices_0_1_total.freeze();
                    subset_indices_0_2_total.freeze();
                    ascent::internal::comment(
                        "origin_contains_loan_on_entry <-- origin_contains_loan_on_entry_indices_0_2_delta, subset_indices_0_2_total [SIMPLE JOIN]",
                    );
                    {
                        if origin_contains_loan_on_entry_indices_0_2_delta.len()
                            <= subset_indices_0_2_total.len()
                        {
                            origin_contains_loan_on_entry_indices_0_2_delta
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin1 = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = subset_indices_0_2_total
                                        .c_index_get(&(origin1.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let loan = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin2 = &__val.0;
                                                        let __new_row: (T::Origin, T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(origin2),
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &origin_contains_loan_on_entry_indices_0_1_2_total,
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                origin_contains_loan_on_entry_indices_0_1_2_delta,
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &origin_contains_loan_on_entry_indices_0_1_2_new,
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
                                                                    &origin_contains_loan_on_entry_indices_0_2_new,
                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                    (__new_row.1.clone(),),
                                                                );
                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                    &origin_contains_loan_on_entry_indices_2_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin1 = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = origin_contains_loan_on_entry_indices_0_2_delta
                                        .c_index_get(&(origin1.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin2 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let loan = &__val.0;
                                                        let __new_row: (T::Origin, T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(origin2),
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &origin_contains_loan_on_entry_indices_0_1_2_total,
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                origin_contains_loan_on_entry_indices_0_1_2_delta,
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &origin_contains_loan_on_entry_indices_0_1_2_new,
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
                                                                    &origin_contains_loan_on_entry_indices_0_2_new,
                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                    (__new_row.1.clone(),),
                                                                );
                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                    &origin_contains_loan_on_entry_indices_2_new,
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
                        if origin_contains_loan_on_entry_indices_2_delta.len()
                            <= cfg_edge_indices_0_total.len()
                        {
                            origin_contains_loan_on_entry_indices_2_delta
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let point1 = &__cl1_joined_columns.0;
                                    if let Some(__matching) = cfg_edge_indices_0_total
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin = &cl1_val.0;
                                                let loan = &cl1_val.1;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let point2 = &__val.0;
                                                        let __matching = loan_killed_at_indices_0_1_total
                                                            .index_get(&(loan.clone(), point1.clone()));
                                                        let __aggregated_rel = &_self.loan_killed_at;
                                                        let __agg_args = __matching
                                                            .into_iter()
                                                            .flatten()
                                                            .map(|__val| { () });
                                                        for () in ::ascent::aggregators::not(__agg_args) {
                                                            if let Some(__matching) = origin_live_on_entry_indices_0_1_total
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
                                                                            &origin_contains_loan_on_entry_indices_0_1_2_total,
                                                                            &__new_row,
                                                                        )
                                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                origin_contains_loan_on_entry_indices_0_1_2_delta,
                                                                                &__new_row,
                                                                            )
                                                                        {
                                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                &origin_contains_loan_on_entry_indices_0_1_2_new,
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
                                                                                    &origin_contains_loan_on_entry_indices_0_2_new,
                                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                                    (__new_row.1.clone(),),
                                                                                );
                                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                    &origin_contains_loan_on_entry_indices_2_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let point1 = &__cl1_joined_columns.0;
                                    if let Some(__matching) = origin_contains_loan_on_entry_indices_2_delta
                                        .c_index_get(&(point1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let point2 = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin = &__val.0;
                                                        let loan = &__val.1;
                                                        let __matching = loan_killed_at_indices_0_1_total
                                                            .index_get(&(loan.clone(), point1.clone()));
                                                        let __aggregated_rel = &_self.loan_killed_at;
                                                        let __agg_args = __matching
                                                            .into_iter()
                                                            .flatten()
                                                            .map(|__val| { () });
                                                        for () in ::ascent::aggregators::not(__agg_args) {
                                                            if let Some(__matching) = origin_live_on_entry_indices_0_1_total
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
                                                                            &origin_contains_loan_on_entry_indices_0_1_2_total,
                                                                            &__new_row,
                                                                        )
                                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                origin_contains_loan_on_entry_indices_0_1_2_delta,
                                                                                &__new_row,
                                                                            )
                                                                        {
                                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                &origin_contains_loan_on_entry_indices_0_1_2_new,
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
                                                                                    &origin_contains_loan_on_entry_indices_0_2_new,
                                                                                    (__new_row.0.clone(), __new_row.2.clone()),
                                                                                    (__new_row.1.clone(),),
                                                                                );
                                                                                ::ascent::internal::CRelIndexWrite::index_insert(
                                                                                    &origin_contains_loan_on_entry_indices_2_new,
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
                    origin_contains_loan_on_entry_indices_0_1_2_total.unfreeze();
                    origin_contains_loan_on_entry_indices_0_1_2_delta.unfreeze();
                    origin_contains_loan_on_entry_indices_0_2_total.unfreeze();
                    origin_contains_loan_on_entry_indices_0_2_delta.unfreeze();
                    origin_contains_loan_on_entry_indices_2_total.unfreeze();
                    origin_contains_loan_on_entry_indices_2_delta.unfreeze();
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        origin_contains_loan_on_entry_indices_0_1_2_delta,
                        &mut origin_contains_loan_on_entry_indices_0_1_2_total,
                    );
                    std::mem::swap(
                        &mut origin_contains_loan_on_entry_indices_0_1_2_new,
                        origin_contains_loan_on_entry_indices_0_1_2_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        origin_contains_loan_on_entry_indices_0_2_delta,
                        &mut origin_contains_loan_on_entry_indices_0_2_total,
                    );
                    std::mem::swap(
                        &mut origin_contains_loan_on_entry_indices_0_2_new,
                        origin_contains_loan_on_entry_indices_0_2_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        origin_contains_loan_on_entry_indices_2_delta,
                        &mut origin_contains_loan_on_entry_indices_2_total,
                    );
                    std::mem::swap(
                        &mut origin_contains_loan_on_entry_indices_2_new,
                        origin_contains_loan_on_entry_indices_2_delta,
                    );
                    if !__changed.load(std::sync::atomic::Ordering::Relaxed) {
                        break;
                    }
                }
                _self
                    .origin_contains_loan_on_entry_indices_0_1_2 = origin_contains_loan_on_entry_indices_0_1_2_total;
                _self
                    .origin_contains_loan_on_entry_indices_0_2 = origin_contains_loan_on_entry_indices_0_2_total;
                _self
                    .origin_contains_loan_on_entry_indices_2 = origin_contains_loan_on_entry_indices_2_total;
                _self.scc1_duration += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 2");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let loan_live_at_indices_0_1_delta: &mut ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = &mut _self.loan_live_at_indices_0_1;
                let mut loan_live_at_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                let mut loan_live_at_indices_0_1_new: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                let origin_contains_loan_on_entry_indices_0_2_total: &mut ascent::internal::CRelIndex<
                    (T::Origin, T::Point),
                    (T::Loan,),
                > = &mut _self.origin_contains_loan_on_entry_indices_0_2;
                let origin_live_on_entry_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Point),
                    (),
                > = &mut _self.origin_live_on_entry_indices_0_1;
                #[allow(unused_assignments, unused_variables)]
                {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    loan_live_at_indices_0_1_total.freeze();
                    loan_live_at_indices_0_1_delta.freeze();
                    origin_contains_loan_on_entry_indices_0_2_total.freeze();
                    origin_live_on_entry_indices_0_1_total.freeze();
                    ascent::internal::comment(
                        "loan_live_at <-- origin_contains_loan_on_entry_indices_0_2_total, origin_live_on_entry_indices_0_1_total [SIMPLE JOIN]",
                    );
                    {
                        if origin_contains_loan_on_entry_indices_0_2_total.len()
                            <= origin_live_on_entry_indices_0_1_total.len()
                        {
                            origin_contains_loan_on_entry_indices_0_2_total
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = origin_live_on_entry_indices_0_1_total
                                        .c_index_get(&(origin.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let loan = &cl1_val.0;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __new_row: (T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &loan_live_at_indices_0_1_total,
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                loan_live_at_indices_0_1_delta,
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &loan_live_at_indices_0_1_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = origin_contains_loan_on_entry_indices_0_2_total
                                        .c_index_get(&(origin.clone(), point.clone()))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let loan = &__val.0;
                                                        let __new_row: (T::Loan, T::Point) = (
                                                            ascent::internal::Convert::convert(loan),
                                                            ascent::internal::Convert::convert(point),
                                                        );
                                                        if !::ascent::internal::RelFullIndexRead::contains_key(
                                                            &loan_live_at_indices_0_1_total,
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                loan_live_at_indices_0_1_delta,
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &loan_live_at_indices_0_1_new,
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
                    loan_live_at_indices_0_1_total.unfreeze();
                    loan_live_at_indices_0_1_delta.unfreeze();
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        loan_live_at_indices_0_1_delta,
                        &mut loan_live_at_indices_0_1_total,
                    );
                    std::mem::swap(
                        &mut loan_live_at_indices_0_1_new,
                        loan_live_at_indices_0_1_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        loan_live_at_indices_0_1_delta,
                        &mut loan_live_at_indices_0_1_total,
                    );
                    std::mem::swap(
                        &mut loan_live_at_indices_0_1_new,
                        loan_live_at_indices_0_1_delta,
                    );
                }
                _self.loan_live_at_indices_0_1 = loan_live_at_indices_0_1_total;
                _self.scc2_duration += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 3");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let errors_indices_0_1_delta: &mut ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = &mut _self.errors_indices_0_1;
                let mut errors_indices_0_1_total: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                let mut errors_indices_0_1_new: ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = Default::default();
                let loan_invalidated_at_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = &mut _self.loan_invalidated_at_indices_0_1;
                let loan_live_at_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Loan, T::Point),
                    (),
                > = &mut _self.loan_live_at_indices_0_1;
                #[allow(unused_assignments, unused_variables)]
                {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    errors_indices_0_1_total.freeze();
                    errors_indices_0_1_delta.freeze();
                    loan_invalidated_at_indices_0_1_total.freeze();
                    loan_live_at_indices_0_1_total.freeze();
                    ascent::internal::comment(
                        "errors <-- loan_invalidated_at_indices_0_1_total, loan_live_at_indices_0_1_total [SIMPLE JOIN]",
                    );
                    {
                        if loan_invalidated_at_indices_0_1_total.len()
                            <= loan_live_at_indices_0_1_total.len()
                        {
                            loan_invalidated_at_indices_0_1_total
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let loan = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = loan_live_at_indices_0_1_total
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
                                                            &errors_indices_0_1_total,
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                errors_indices_0_1_delta,
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &errors_indices_0_1_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let loan = &__cl1_joined_columns.0;
                                    let point = &__cl1_joined_columns.1;
                                    if let Some(__matching) = loan_invalidated_at_indices_0_1_total
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
                                                            &errors_indices_0_1_total,
                                                            &__new_row,
                                                        )
                                                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                errors_indices_0_1_delta,
                                                                &__new_row,
                                                            )
                                                        {
                                                            if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                &errors_indices_0_1_new,
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
                    errors_indices_0_1_total.unfreeze();
                    errors_indices_0_1_delta.unfreeze();
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        errors_indices_0_1_delta,
                        &mut errors_indices_0_1_total,
                    );
                    std::mem::swap(
                        &mut errors_indices_0_1_new,
                        errors_indices_0_1_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        errors_indices_0_1_delta,
                        &mut errors_indices_0_1_total,
                    );
                    std::mem::swap(
                        &mut errors_indices_0_1_new,
                        errors_indices_0_1_delta,
                    );
                }
                _self.errors_indices_0_1 = errors_indices_0_1_total;
                _self.scc3_duration += _scc_start_time.elapsed();
            }
            ascent::internal::comment("scc 4");
            {
                let _scc_start_time = ::ascent::internal::Instant::now();
                let subset_error_indices_0_1_2_delta: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = &mut _self.subset_error_indices_0_1_2;
                let mut subset_error_indices_0_1_2_total: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                let mut subset_error_indices_0_1_2_new: ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin, T::Point),
                    (),
                > = Default::default();
                let known_placeholder_subset_indices_0_1_total: &mut ascent::internal::CRelFullIndex<
                    (T::Origin, T::Origin),
                    (),
                > = &mut _self.known_placeholder_subset_indices_0_1;
                let placeholder_origin_indices_0_total: &mut ascent::internal::CRelFullIndex<
                    (T::Origin,),
                    (),
                > = &mut _self.placeholder_origin_indices_0;
                let subset_indices_0_total: &mut ascent::internal::CRelIndex<
                    (T::Origin,),
                    (T::Origin, T::Point),
                > = &mut _self.subset_indices_0;
                #[allow(unused_assignments, unused_variables)]
                {
                    let __changed = std::sync::atomic::AtomicBool::new(false);
                    subset_error_indices_0_1_2_total.freeze();
                    subset_error_indices_0_1_2_delta.freeze();
                    known_placeholder_subset_indices_0_1_total.freeze();
                    placeholder_origin_indices_0_total.freeze();
                    subset_indices_0_total.freeze();
                    ascent::internal::comment(
                        "subset_error <-- subset_indices_0_total, placeholder_origin_indices_0_total, placeholder_origin_indices_0_total, agg known_placeholder_subset_indices_0_1, if ⋯ [SIMPLE JOIN]",
                    );
                    {
                        if subset_indices_0_total.len()
                            <= placeholder_origin_indices_0_total.len()
                        {
                            subset_indices_0_total
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin1 = &__cl1_joined_columns.0;
                                    if let Some(__matching) = placeholder_origin_indices_0_total
                                        .c_index_get(&(origin1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                let origin2 = &cl1_val.0;
                                                let point = &cl1_val.1;
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        if let Some(__matching) = placeholder_origin_indices_0_total
                                                            .index_get(&(origin2.clone(),))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    let __matching = known_placeholder_subset_indices_0_1_total
                                                                        .index_get(&(origin1.clone(), origin2.clone()));
                                                                    let __aggregated_rel = &_self.known_placeholder_subset;
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
                                                                                &subset_error_indices_0_1_2_total,
                                                                                &__new_row,
                                                                            )
                                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                    subset_error_indices_0_1_2_delta,
                                                                                    &__new_row,
                                                                                )
                                                                            {
                                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                    &subset_error_indices_0_1_2_new,
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
                                .c_iter_all()
                                .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                    let origin1 = &__cl1_joined_columns.0;
                                    if let Some(__matching) = subset_indices_0_total
                                        .c_index_get(&(origin1.clone(),))
                                    {
                                        __cl1_tuple_indices
                                            .for_each(|cl1_val| {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let origin2 = &__val.0;
                                                        let point = &__val.1;
                                                        if let Some(__matching) = placeholder_origin_indices_0_total
                                                            .index_get(&(origin2.clone(),))
                                                        {
                                                            __matching
                                                                .for_each(|__val| {
                                                                    let __matching = known_placeholder_subset_indices_0_1_total
                                                                        .index_get(&(origin1.clone(), origin2.clone()));
                                                                    let __aggregated_rel = &_self.known_placeholder_subset;
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
                                                                                &subset_error_indices_0_1_2_total,
                                                                                &__new_row,
                                                                            )
                                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                                    subset_error_indices_0_1_2_delta,
                                                                                    &__new_row,
                                                                                )
                                                                            {
                                                                                if ::ascent::internal::CRelFullIndexWrite::insert_if_not_present(
                                                                                    &subset_error_indices_0_1_2_new,
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
                    subset_error_indices_0_1_2_total.unfreeze();
                    subset_error_indices_0_1_2_delta.unfreeze();
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_error_indices_0_1_2_delta,
                        &mut subset_error_indices_0_1_2_total,
                    );
                    std::mem::swap(
                        &mut subset_error_indices_0_1_2_new,
                        subset_error_indices_0_1_2_delta,
                    );
                    ::ascent::internal::RelIndexWrite::move_index_contents(
                        subset_error_indices_0_1_2_delta,
                        &mut subset_error_indices_0_1_2_total,
                    );
                    std::mem::swap(
                        &mut subset_error_indices_0_1_2_new,
                        subset_error_indices_0_1_2_delta,
                    );
                }
                _self.subset_error_indices_0_1_2 = subset_error_indices_0_1_2_total;
                _self.scc4_duration += _scc_start_time.elapsed();
            }
        }
        __run_res
    };
}
fn main() {}
