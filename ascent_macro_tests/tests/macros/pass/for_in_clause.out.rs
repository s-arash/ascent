//! Generative `for … in` clause
use ascent::ascent;
pub struct AscentProgram {
    /**
logical indices: number_indices_0*/
    pub number: ::std::vec::Vec<(i32,)>,
    pub __number_ind_common: (),
    pub number_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    /**
logical indices: seed_indices_0; seed_indices_none*/
    pub seed: ::std::vec::Vec<(i32,)>,
    pub __seed_ind_common: (),
    pub seed_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    pub seed_indices_none: ascent::rel::ToRelIndexType<(), (i32,)>,
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
            let mut __number_ind_common_delta: () = ::std::mem::take(
                &mut _self.__number_ind_common,
            );
            let mut __number_ind_common_total: () = Default::default();
            let mut __number_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __number_ind_common_new,
                &mut __number_ind_common_delta,
                &mut __number_ind_common_total,
            );
            let mut number_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.number_indices_0);
            let mut number_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut number_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut number_indices_0_new
                    .to_rel_index_write(&mut __number_ind_common_new),
                &mut number_indices_0_delta
                    .to_rel_index_write(&mut __number_ind_common_delta),
                &mut number_indices_0_total
                    .to_rel_index_write(&mut __number_ind_common_total),
            );
            let __seed_ind_common_total: () = std::mem::take(
                &mut _self.__seed_ind_common,
            );
            let seed_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.seed_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("number <-- seed_indices_none_total, for_y");
                {
                    if let Some(__matching) = seed_indices_none_total
                        .to_rel_index(&__seed_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                for y in 0..3 {
                                    let __new_row: (i32,) = (x + y,);
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &number_indices_0_total
                                            .to_rel_index(&__number_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &number_indices_0_delta
                                                .to_rel_index(&__number_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut number_indices_0_new
                                                .to_rel_index_write(&mut __number_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.number.len();
                                            _self.number.push((__new_row.0,));
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __number_ind_common_new,
                    &mut __number_ind_common_delta,
                    &mut __number_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut number_indices_0_new
                        .to_rel_index_write(&mut __number_ind_common_new),
                    &mut number_indices_0_delta
                        .to_rel_index_write(&mut __number_ind_common_delta),
                    &mut number_indices_0_total
                        .to_rel_index_write(&mut __number_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __number_ind_common_new,
                    &mut __number_ind_common_delta,
                    &mut __number_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut number_indices_0_new
                        .to_rel_index_write(&mut __number_ind_common_new),
                    &mut number_indices_0_delta
                        .to_rel_index_write(&mut __number_ind_common_delta),
                    &mut number_indices_0_total
                        .to_rel_index_write(&mut __number_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_0 = number_indices_0_total;
            _self.__seed_ind_common = __seed_ind_common_total;
            _self.seed_indices_none = seed_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.number.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.number_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__number_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.seed.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.seed_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__seed_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.seed_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__seed_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        self.update_indices_duration += before.elapsed();
    }
    #[deprecated = "Explicit call to update_indices not required anymore."]
    pub fn update_indices(&mut self) {
        self.update_indices_priv();
    }
    fn type_constraints() {
        let _type_constraints: ascent::internal::TypeConstraints<i32>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  number <-- seed_indices_none_total, for_y\n  dynamic relations: number\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "number", self.number.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "seed", self.seed.len()))
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
            number: Default::default(),
            __number_ind_common: Default::default(),
            number_indices_0: Default::default(),
            seed: Default::default(),
            __seed_ind_common: Default::default(),
            seed_indices_0: Default::default(),
            seed_indices_none: Default::default(),
            scc_times: [std::time::Duration::ZERO; 1usize],
            scc_iters: [0; 1usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
