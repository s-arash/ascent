//! Aggregate `agg` clause
use ascent::{
    aggregators::{count, max, mean, min, sum},
    ascent,
};
pub struct AscentProgram {
    /**
logical indices: average_indices_0*/
    pub average: ::std::vec::Vec<(i32,)>,
    pub __average_ind_common: (),
    pub average_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    /**
logical indices: cardinality_indices_0*/
    pub cardinality: ::std::vec::Vec<(usize,)>,
    pub __cardinality_ind_common: (),
    pub cardinality_indices_0: ascent::internal::RelFullIndexType<(usize,), ()>,
    /**
logical indices: greatest_indices_0*/
    pub greatest: ::std::vec::Vec<(i32,)>,
    pub __greatest_ind_common: (),
    pub greatest_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    /**
logical indices: lowest_indices_0*/
    pub lowest: ::std::vec::Vec<(i32,)>,
    pub __lowest_ind_common: (),
    pub lowest_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    /**
logical indices: number_indices_0; number_indices_none*/
    pub number: ::std::vec::Vec<(i32,)>,
    pub __number_ind_common: (),
    pub number_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    pub number_indices_none: ascent::rel::ToRelIndexType<(), (i32,)>,
    /**
logical indices: total_indices_0*/
    pub total: ::std::vec::Vec<(i32,)>,
    pub __total_ind_common: (),
    pub total_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    scc_times: [std::time::Duration; 5usize],
    scc_iters: [usize; 5usize],
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
            let mut __lowest_ind_common_delta: () = ::std::mem::take(
                &mut _self.__lowest_ind_common,
            );
            let mut __lowest_ind_common_total: () = Default::default();
            let mut __lowest_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __lowest_ind_common_new,
                &mut __lowest_ind_common_delta,
                &mut __lowest_ind_common_total,
            );
            let mut lowest_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.lowest_indices_0);
            let mut lowest_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut lowest_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut lowest_indices_0_new
                    .to_rel_index_write(&mut __lowest_ind_common_new),
                &mut lowest_indices_0_delta
                    .to_rel_index_write(&mut __lowest_ind_common_delta),
                &mut lowest_indices_0_total
                    .to_rel_index_write(&mut __lowest_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("lowest <-- agg number_indices_none");
                {
                    let __aggregated_rel = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total);
                    let __matching = __aggregated_rel.index_get(&());
                    let __agg_args = __matching
                        .into_iter()
                        .flatten()
                        .map(|__val| {
                            let __val = __val.tuple_of_borrowed();
                            let x: &i32 = __val.0;
                            (x,)
                        });
                    for y in min(__agg_args) {
                        let __new_row: (i32,) = (ascent::internal::Convert::convert(y),);
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                            &lowest_indices_0_total
                                .to_rel_index(&__lowest_ind_common_total),
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                &lowest_indices_0_delta
                                    .to_rel_index(&__lowest_ind_common_delta),
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut lowest_indices_0_new
                                    .to_rel_index_write(&mut __lowest_ind_common_new),
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.lowest.len();
                                _self.lowest.push((__new_row.0,));
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __lowest_ind_common_new,
                    &mut __lowest_ind_common_delta,
                    &mut __lowest_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut lowest_indices_0_new
                        .to_rel_index_write(&mut __lowest_ind_common_new),
                    &mut lowest_indices_0_delta
                        .to_rel_index_write(&mut __lowest_ind_common_delta),
                    &mut lowest_indices_0_total
                        .to_rel_index_write(&mut __lowest_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __lowest_ind_common_new,
                    &mut __lowest_ind_common_delta,
                    &mut __lowest_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut lowest_indices_0_new
                        .to_rel_index_write(&mut __lowest_ind_common_new),
                    &mut lowest_indices_0_delta
                        .to_rel_index_write(&mut __lowest_ind_common_delta),
                    &mut lowest_indices_0_total
                        .to_rel_index_write(&mut __lowest_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__lowest_ind_common = __lowest_ind_common_total;
            _self.lowest_indices_0 = lowest_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __greatest_ind_common_delta: () = ::std::mem::take(
                &mut _self.__greatest_ind_common,
            );
            let mut __greatest_ind_common_total: () = Default::default();
            let mut __greatest_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __greatest_ind_common_new,
                &mut __greatest_ind_common_delta,
                &mut __greatest_ind_common_total,
            );
            let mut greatest_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.greatest_indices_0);
            let mut greatest_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut greatest_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut greatest_indices_0_new
                    .to_rel_index_write(&mut __greatest_ind_common_new),
                &mut greatest_indices_0_delta
                    .to_rel_index_write(&mut __greatest_ind_common_delta),
                &mut greatest_indices_0_total
                    .to_rel_index_write(&mut __greatest_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("greatest <-- agg number_indices_none");
                {
                    let __aggregated_rel = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total);
                    let __matching = __aggregated_rel.index_get(&());
                    let __agg_args = __matching
                        .into_iter()
                        .flatten()
                        .map(|__val| {
                            let __val = __val.tuple_of_borrowed();
                            let x: &i32 = __val.0;
                            (x,)
                        });
                    for y in max(__agg_args) {
                        let __new_row: (i32,) = (ascent::internal::Convert::convert(y),);
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                            &greatest_indices_0_total
                                .to_rel_index(&__greatest_ind_common_total),
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                &greatest_indices_0_delta
                                    .to_rel_index(&__greatest_ind_common_delta),
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut greatest_indices_0_new
                                    .to_rel_index_write(&mut __greatest_ind_common_new),
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.greatest.len();
                                _self.greatest.push((__new_row.0,));
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __greatest_ind_common_new,
                    &mut __greatest_ind_common_delta,
                    &mut __greatest_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut greatest_indices_0_new
                        .to_rel_index_write(&mut __greatest_ind_common_new),
                    &mut greatest_indices_0_delta
                        .to_rel_index_write(&mut __greatest_ind_common_delta),
                    &mut greatest_indices_0_total
                        .to_rel_index_write(&mut __greatest_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __greatest_ind_common_new,
                    &mut __greatest_ind_common_delta,
                    &mut __greatest_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut greatest_indices_0_new
                        .to_rel_index_write(&mut __greatest_ind_common_new),
                    &mut greatest_indices_0_delta
                        .to_rel_index_write(&mut __greatest_ind_common_delta),
                    &mut greatest_indices_0_total
                        .to_rel_index_write(&mut __greatest_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
            }
            _self.__greatest_ind_common = __greatest_ind_common_total;
            _self.greatest_indices_0 = greatest_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __average_ind_common_delta: () = ::std::mem::take(
                &mut _self.__average_ind_common,
            );
            let mut __average_ind_common_total: () = Default::default();
            let mut __average_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __average_ind_common_new,
                &mut __average_ind_common_delta,
                &mut __average_ind_common_total,
            );
            let mut average_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.average_indices_0);
            let mut average_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut average_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut average_indices_0_new
                    .to_rel_index_write(&mut __average_ind_common_new),
                &mut average_indices_0_delta
                    .to_rel_index_write(&mut __average_ind_common_delta),
                &mut average_indices_0_total
                    .to_rel_index_write(&mut __average_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("average <-- agg number_indices_none");
                {
                    let __aggregated_rel = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total);
                    let __matching = __aggregated_rel.index_get(&());
                    let __agg_args = __matching
                        .into_iter()
                        .flatten()
                        .map(|__val| {
                            let __val = __val.tuple_of_borrowed();
                            let x: &i32 = __val.0;
                            (x,)
                        });
                    for y in mean(__agg_args) {
                        let __new_row: (i32,) = (y.round() as i32,);
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                            &average_indices_0_total
                                .to_rel_index(&__average_ind_common_total),
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                &average_indices_0_delta
                                    .to_rel_index(&__average_ind_common_delta),
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut average_indices_0_new
                                    .to_rel_index_write(&mut __average_ind_common_new),
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.average.len();
                                _self.average.push((__new_row.0,));
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __average_ind_common_new,
                    &mut __average_ind_common_delta,
                    &mut __average_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut average_indices_0_new
                        .to_rel_index_write(&mut __average_ind_common_new),
                    &mut average_indices_0_delta
                        .to_rel_index_write(&mut __average_ind_common_delta),
                    &mut average_indices_0_total
                        .to_rel_index_write(&mut __average_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __average_ind_common_new,
                    &mut __average_ind_common_delta,
                    &mut __average_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut average_indices_0_new
                        .to_rel_index_write(&mut __average_ind_common_new),
                    &mut average_indices_0_delta
                        .to_rel_index_write(&mut __average_ind_common_delta),
                    &mut average_indices_0_total
                        .to_rel_index_write(&mut __average_ind_common_total),
                );
                _self.scc_iters[2usize] += 1;
            }
            _self.__average_ind_common = __average_ind_common_total;
            _self.average_indices_0 = average_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 3");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __total_ind_common_delta: () = ::std::mem::take(
                &mut _self.__total_ind_common,
            );
            let mut __total_ind_common_total: () = Default::default();
            let mut __total_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __total_ind_common_new,
                &mut __total_ind_common_delta,
                &mut __total_ind_common_total,
            );
            let mut total_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.total_indices_0);
            let mut total_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut total_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut total_indices_0_new.to_rel_index_write(&mut __total_ind_common_new),
                &mut total_indices_0_delta
                    .to_rel_index_write(&mut __total_ind_common_delta),
                &mut total_indices_0_total
                    .to_rel_index_write(&mut __total_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("total <-- agg number_indices_none");
                {
                    let __aggregated_rel = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total);
                    let __matching = __aggregated_rel.index_get(&());
                    let __agg_args = __matching
                        .into_iter()
                        .flatten()
                        .map(|__val| {
                            let __val = __val.tuple_of_borrowed();
                            let x: &i32 = __val.0;
                            (x,)
                        });
                    for y in sum(__agg_args) {
                        let __new_row: (i32,) = (ascent::internal::Convert::convert(y),);
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                            &total_indices_0_total
                                .to_rel_index(&__total_ind_common_total),
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                &total_indices_0_delta
                                    .to_rel_index(&__total_ind_common_delta),
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut total_indices_0_new
                                    .to_rel_index_write(&mut __total_ind_common_new),
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.total.len();
                                _self.total.push((__new_row.0,));
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __total_ind_common_new,
                    &mut __total_ind_common_delta,
                    &mut __total_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut total_indices_0_new
                        .to_rel_index_write(&mut __total_ind_common_new),
                    &mut total_indices_0_delta
                        .to_rel_index_write(&mut __total_ind_common_delta),
                    &mut total_indices_0_total
                        .to_rel_index_write(&mut __total_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __total_ind_common_new,
                    &mut __total_ind_common_delta,
                    &mut __total_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut total_indices_0_new
                        .to_rel_index_write(&mut __total_ind_common_new),
                    &mut total_indices_0_delta
                        .to_rel_index_write(&mut __total_ind_common_delta),
                    &mut total_indices_0_total
                        .to_rel_index_write(&mut __total_ind_common_total),
                );
                _self.scc_iters[3usize] += 1;
            }
            _self.__total_ind_common = __total_ind_common_total;
            _self.total_indices_0 = total_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[3usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 4");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __cardinality_ind_common_delta: () = ::std::mem::take(
                &mut _self.__cardinality_ind_common,
            );
            let mut __cardinality_ind_common_total: () = Default::default();
            let mut __cardinality_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __cardinality_ind_common_new,
                &mut __cardinality_ind_common_delta,
                &mut __cardinality_ind_common_total,
            );
            let mut cardinality_indices_0_delta: ascent::internal::RelFullIndexType<
                (usize,),
                (),
            > = ::std::mem::take(&mut _self.cardinality_indices_0);
            let mut cardinality_indices_0_total: ascent::internal::RelFullIndexType<
                (usize,),
                (),
            > = Default::default();
            let mut cardinality_indices_0_new: ascent::internal::RelFullIndexType<
                (usize,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut cardinality_indices_0_new
                    .to_rel_index_write(&mut __cardinality_ind_common_new),
                &mut cardinality_indices_0_delta
                    .to_rel_index_write(&mut __cardinality_ind_common_delta),
                &mut cardinality_indices_0_total
                    .to_rel_index_write(&mut __cardinality_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("cardinality <-- agg number_indices_none");
                {
                    let __aggregated_rel = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total);
                    let __matching = __aggregated_rel.index_get(&());
                    let __agg_args = __matching
                        .into_iter()
                        .flatten()
                        .map(|__val| { () });
                    for y in count(__agg_args) {
                        let __new_row: (usize,) = (
                            ascent::internal::Convert::convert(y),
                        );
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                            &cardinality_indices_0_total
                                .to_rel_index(&__cardinality_ind_common_total),
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                &cardinality_indices_0_delta
                                    .to_rel_index(&__cardinality_ind_common_delta),
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut cardinality_indices_0_new
                                    .to_rel_index_write(&mut __cardinality_ind_common_new),
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.cardinality.len();
                                _self.cardinality.push((__new_row.0,));
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __cardinality_ind_common_new,
                    &mut __cardinality_ind_common_delta,
                    &mut __cardinality_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut cardinality_indices_0_new
                        .to_rel_index_write(&mut __cardinality_ind_common_new),
                    &mut cardinality_indices_0_delta
                        .to_rel_index_write(&mut __cardinality_ind_common_delta),
                    &mut cardinality_indices_0_total
                        .to_rel_index_write(&mut __cardinality_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __cardinality_ind_common_new,
                    &mut __cardinality_ind_common_delta,
                    &mut __cardinality_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut cardinality_indices_0_new
                        .to_rel_index_write(&mut __cardinality_ind_common_new),
                    &mut cardinality_indices_0_delta
                        .to_rel_index_write(&mut __cardinality_ind_common_delta),
                    &mut cardinality_indices_0_total
                        .to_rel_index_write(&mut __cardinality_ind_common_total),
                );
                _self.scc_iters[4usize] += 1;
            }
            _self.__cardinality_ind_common = __cardinality_ind_common_total;
            _self.cardinality_indices_0 = cardinality_indices_0_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[4usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.average.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.average_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__average_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.cardinality.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.cardinality_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__cardinality_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.greatest.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.greatest_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__greatest_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.lowest.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.lowest_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__lowest_ind_common),
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
        for (_i, tuple) in self.total.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.total_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__total_ind_common),
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
        let _type_constraints: ascent::internal::TypeConstraints<i32>;
        let _type_constraints: ascent::internal::TypeConstraints<usize>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  lowest <-- agg number_indices_none\n  dynamic relations: lowest\nscc 1, is_looping: false:\n  greatest <-- agg number_indices_none\n  dynamic relations: greatest\nscc 2, is_looping: false:\n  average <-- agg number_indices_none\n  dynamic relations: average\nscc 3, is_looping: false:\n  total <-- agg number_indices_none\n  dynamic relations: total\nscc 4, is_looping: false:\n  cardinality <-- agg number_indices_none\n  dynamic relations: cardinality\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "average", self.average.len()))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!("{0} size: {1}\n", "cardinality", self.cardinality.len()),
            )
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "greatest", self.greatest.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "lowest", self.lowest.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "number", self.number.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "total", self.total.len()))
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
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            average: Default::default(),
            __average_ind_common: Default::default(),
            average_indices_0: Default::default(),
            cardinality: Default::default(),
            __cardinality_ind_common: Default::default(),
            cardinality_indices_0: Default::default(),
            greatest: Default::default(),
            __greatest_ind_common: Default::default(),
            greatest_indices_0: Default::default(),
            lowest: Default::default(),
            __lowest_ind_common: Default::default(),
            lowest_indices_0: Default::default(),
            number: Default::default(),
            __number_ind_common: Default::default(),
            number_indices_0: Default::default(),
            number_indices_none: Default::default(),
            total: Default::default(),
            __total_ind_common: Default::default(),
            total_indices_0: Default::default(),
            scc_times: [std::time::Duration::ZERO; 5usize],
            scc_iters: [0; 5usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
