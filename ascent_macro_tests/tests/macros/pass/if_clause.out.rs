//! Conditional `if` clause
use ascent::ascent;
pub struct AscentProgram {
    /**
logical indices: even_indices_0*/
    pub even: ::std::vec::Vec<(isize,)>,
    pub __even_ind_common: (),
    pub even_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    /**
logical indices: number_indices_0; number_indices_none*/
    pub number: ::std::vec::Vec<(isize,)>,
    pub __number_ind_common: (),
    pub number_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    pub number_indices_none: ascent::rel::ToRelIndexType<(), (isize,)>,
    /**
logical indices: odd_indices_0*/
    pub odd: ::std::vec::Vec<(isize,)>,
    pub __odd_ind_common: (),
    pub odd_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    scc_times: [std::time::Duration; 2usize],
    scc_iters: [usize; 2usize],
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
            let mut __even_ind_common_delta: () = ::std::mem::take(
                &mut _self.__even_ind_common,
            );
            let mut __even_ind_common_total: () = Default::default();
            let mut __even_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __even_ind_common_new,
                &mut __even_ind_common_delta,
                &mut __even_ind_common_total,
            );
            let mut even_indices_0_delta: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = ::std::mem::take(&mut _self.even_indices_0);
            let mut even_indices_0_total: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            let mut even_indices_0_new: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut even_indices_0_new.to_rel_index_write(&mut __even_ind_common_new),
                &mut even_indices_0_delta
                    .to_rel_index_write(&mut __even_ind_common_delta),
                &mut even_indices_0_total
                    .to_rel_index_write(&mut __even_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (isize,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("even <-- number_indices_none_total, if ⋯");
                {
                    if let Some(__matching) = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &isize = __val.0;
                                if x % 2 == 0 {
                                    let __new_row: (isize,) = (
                                        ascent::internal::Convert::convert(x),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &even_indices_0_total
                                            .to_rel_index(&__even_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &even_indices_0_delta
                                                .to_rel_index(&__even_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut even_indices_0_new
                                                .to_rel_index_write(&mut __even_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.even.len();
                                            _self.even.push((__new_row.0,));
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __even_ind_common_new,
                    &mut __even_ind_common_delta,
                    &mut __even_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_indices_0_new
                        .to_rel_index_write(&mut __even_ind_common_new),
                    &mut even_indices_0_delta
                        .to_rel_index_write(&mut __even_ind_common_delta),
                    &mut even_indices_0_total
                        .to_rel_index_write(&mut __even_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __even_ind_common_new,
                    &mut __even_ind_common_delta,
                    &mut __even_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_indices_0_new
                        .to_rel_index_write(&mut __even_ind_common_new),
                    &mut even_indices_0_delta
                        .to_rel_index_write(&mut __even_ind_common_delta),
                    &mut even_indices_0_total
                        .to_rel_index_write(&mut __even_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__even_ind_common = __even_ind_common_total;
            _self.even_indices_0 = even_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __odd_ind_common_delta: () = ::std::mem::take(
                &mut _self.__odd_ind_common,
            );
            let mut __odd_ind_common_total: () = Default::default();
            let mut __odd_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __odd_ind_common_new,
                &mut __odd_ind_common_delta,
                &mut __odd_ind_common_total,
            );
            let mut odd_indices_0_delta: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = ::std::mem::take(&mut _self.odd_indices_0);
            let mut odd_indices_0_total: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            let mut odd_indices_0_new: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut odd_indices_0_new.to_rel_index_write(&mut __odd_ind_common_new),
                &mut odd_indices_0_delta.to_rel_index_write(&mut __odd_ind_common_delta),
                &mut odd_indices_0_total.to_rel_index_write(&mut __odd_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (isize,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("odd <-- number_indices_none_total, if ⋯");
                {
                    if let Some(__matching) = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &isize = __val.0;
                                if x % 2 != 0 {
                                    let __new_row: (isize,) = (
                                        ascent::internal::Convert::convert(x),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &odd_indices_0_total.to_rel_index(&__odd_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &odd_indices_0_delta.to_rel_index(&__odd_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut odd_indices_0_new
                                                .to_rel_index_write(&mut __odd_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.odd.len();
                                            _self.odd.push((__new_row.0,));
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __odd_ind_common_new,
                    &mut __odd_ind_common_delta,
                    &mut __odd_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut odd_indices_0_new.to_rel_index_write(&mut __odd_ind_common_new),
                    &mut odd_indices_0_delta
                        .to_rel_index_write(&mut __odd_ind_common_delta),
                    &mut odd_indices_0_total
                        .to_rel_index_write(&mut __odd_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __odd_ind_common_new,
                    &mut __odd_ind_common_delta,
                    &mut __odd_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut odd_indices_0_new.to_rel_index_write(&mut __odd_ind_common_new),
                    &mut odd_indices_0_delta
                        .to_rel_index_write(&mut __odd_ind_common_delta),
                    &mut odd_indices_0_total
                        .to_rel_index_write(&mut __odd_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
            }
            _self.__odd_ind_common = __odd_ind_common_total;
            _self.odd_indices_0 = odd_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.even.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.even_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__even_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.number.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.number_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__number_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.number_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__number_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.odd.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.odd_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__odd_ind_common),
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
        "scc 0, is_looping: false:\n  even <-- number_indices_none_total, if ⋯\n  dynamic relations: even\nscc 1, is_looping: false:\n  odd <-- number_indices_none_total, if ⋯\n  dynamic relations: odd\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "even", self.even.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "number", self.number.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "odd", self.odd.len()))
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            even: Default::default(),
            __even_ind_common: Default::default(),
            even_indices_0: Default::default(),
            number: Default::default(),
            __number_ind_common: Default::default(),
            number_indices_0: Default::default(),
            number_indices_none: Default::default(),
            odd: Default::default(),
            __odd_ind_common: Default::default(),
            odd_indices_0: Default::default(),
            scc_times: [std::time::Duration::ZERO; 2usize],
            scc_iters: [0; 2usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
