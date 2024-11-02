//! Conditional `if let` clause
use ascent::ascent;
pub struct AscentProgram {
    /**
logical indices: option_indices_0; option_indices_none*/
    pub option: ::std::vec::Vec<(Option<isize>,)>,
    pub __option_ind_common: (),
    pub option_indices_0: ascent::internal::RelFullIndexType<(Option<isize>,), ()>,
    pub option_indices_none: ascent::rel::ToRelIndexType<(), (Option<isize>,)>,
    /**
logical indices: some_indices_0*/
    pub some: ::std::vec::Vec<(isize,)>,
    pub __some_ind_common: (),
    pub some_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    scc_times: [std::time::Duration; 1usize],
    scc_iters: [usize; 1usize],
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
            let mut __some_ind_common_delta: () = ::std::mem::take(
                &mut _self.__some_ind_common,
            );
            let mut __some_ind_common_total: () = Default::default();
            let mut __some_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __some_ind_common_new,
                &mut __some_ind_common_delta,
                &mut __some_ind_common_total,
            );
            let mut some_indices_0_delta: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = ::std::mem::take(&mut _self.some_indices_0);
            let mut some_indices_0_total: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            let mut some_indices_0_new: ascent::internal::RelFullIndexType<
                (isize,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut some_indices_0_new.to_rel_index_write(&mut __some_ind_common_new),
                &mut some_indices_0_delta
                    .to_rel_index_write(&mut __some_ind_common_delta),
                &mut some_indices_0_total
                    .to_rel_index_write(&mut __some_ind_common_total),
            );
            let __option_ind_common_total: () = std::mem::take(
                &mut _self.__option_ind_common,
            );
            let option_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (Option<isize>,),
            > = std::mem::take(&mut _self.option_indices_none);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment(
                    "some <-- option_indices_none_total, if let ⋯",
                );
                {
                    if let Some(__matching) = option_indices_none_total
                        .to_rel_index(&__option_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &Option<isize> = __val.0;
                                if let Some(y) = x {
                                    let __new_row: (isize,) = (
                                        ascent::internal::Convert::convert(y),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &some_indices_0_total
                                            .to_rel_index(&__some_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &some_indices_0_delta
                                                .to_rel_index(&__some_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut some_indices_0_new
                                                .to_rel_index_write(&mut __some_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.some.len();
                                            _self.some.push((__new_row.0,));
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __some_ind_common_new,
                    &mut __some_ind_common_delta,
                    &mut __some_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut some_indices_0_new
                        .to_rel_index_write(&mut __some_ind_common_new),
                    &mut some_indices_0_delta
                        .to_rel_index_write(&mut __some_ind_common_delta),
                    &mut some_indices_0_total
                        .to_rel_index_write(&mut __some_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __some_ind_common_new,
                    &mut __some_ind_common_delta,
                    &mut __some_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut some_indices_0_new
                        .to_rel_index_write(&mut __some_ind_common_new),
                    &mut some_indices_0_delta
                        .to_rel_index_write(&mut __some_ind_common_delta),
                    &mut some_indices_0_total
                        .to_rel_index_write(&mut __some_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__some_ind_common = __some_ind_common_total;
            _self.some_indices_0 = some_indices_0_total;
            _self.__option_ind_common = __option_ind_common_total;
            _self.option_indices_none = option_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.option.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.option_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__option_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.option_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__option_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.some.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.some_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__some_ind_common),
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
        let _type_constraints: ascent::internal::TypeConstraints<Option<isize>>;
        let _type_constraints: ascent::internal::TypeConstraints<isize>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  some <-- option_indices_none_total, if let ⋯\n  dynamic relations: some\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "option", self.option.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "some", self.some.len()))
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            option: Default::default(),
            __option_ind_common: Default::default(),
            option_indices_0: Default::default(),
            option_indices_none: Default::default(),
            some: Default::default(),
            __some_ind_common: Default::default(),
            some_indices_0: Default::default(),
            scc_times: [std::time::Duration::ZERO; 1usize],
            scc_iters: [0; 1usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
