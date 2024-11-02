//! Disjunction clause
use ascent::ascent;
pub struct AscentProgram {
    /**
logical indices: even_indices_0; even_indices_none*/
    pub even: ::std::vec::Vec<(i32,)>,
    pub __even_ind_common: (),
    pub even_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    pub even_indices_none: ascent::rel::ToRelIndexType<(), (i32,)>,
    /**
logical indices: even_or_square_indices_0*/
    pub even_or_square: ::std::vec::Vec<(i32,)>,
    pub __even_or_square_ind_common: (),
    pub even_or_square_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    /**
logical indices: number_indices_0; number_indices_none*/
    pub number: ::std::vec::Vec<(i32,)>,
    pub __number_ind_common: (),
    pub number_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    pub number_indices_none: ascent::rel::ToRelIndexType<(), (i32,)>,
    /**
logical indices: square_indices_0; square_indices_none*/
    pub square: ::std::vec::Vec<(i32,)>,
    pub __square_ind_common: (),
    pub square_indices_0: ascent::internal::RelFullIndexType<(i32,), ()>,
    pub square_indices_none: ascent::rel::ToRelIndexType<(), (i32,)>,
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
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.even_indices_0);
            let mut even_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut even_indices_0_new: ascent::internal::RelFullIndexType<(i32,), ()> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut even_indices_0_new.to_rel_index_write(&mut __even_ind_common_new),
                &mut even_indices_0_delta
                    .to_rel_index_write(&mut __even_ind_common_delta),
                &mut even_indices_0_total
                    .to_rel_index_write(&mut __even_ind_common_total),
            );
            let mut even_indices_none_delta: ascent::rel::ToRelIndexType<(), (i32,)> = ::std::mem::take(
                &mut _self.even_indices_none,
            );
            let mut even_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = Default::default();
            let mut even_indices_none_new: ascent::rel::ToRelIndexType<(), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut even_indices_none_new
                    .to_rel_index_write(&mut __even_ind_common_new),
                &mut even_indices_none_delta
                    .to_rel_index_write(&mut __even_ind_common_delta),
                &mut even_indices_none_total
                    .to_rel_index_write(&mut __even_ind_common_total),
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
                ascent::internal::comment("even <-- number_indices_none_total");
                {
                    if let Some(__matching) = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                if x % 2 == 0 {
                                    let __new_row: (i32,) = (
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
                                            _self.even.push((__new_row.0.clone(),));
                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                &mut even_indices_none_new
                                                    .to_rel_index_write(&mut __even_ind_common_new),
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
                    &mut even_indices_none_new
                        .to_rel_index_write(&mut __even_ind_common_new),
                    &mut even_indices_none_delta
                        .to_rel_index_write(&mut __even_ind_common_delta),
                    &mut even_indices_none_total
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
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_indices_none_new
                        .to_rel_index_write(&mut __even_ind_common_new),
                    &mut even_indices_none_delta
                        .to_rel_index_write(&mut __even_ind_common_delta),
                    &mut even_indices_none_total
                        .to_rel_index_write(&mut __even_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__even_ind_common = __even_ind_common_total;
            _self.even_indices_0 = even_indices_0_total;
            _self.even_indices_none = even_indices_none_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __even_or_square_ind_common_delta: () = ::std::mem::take(
                &mut _self.__even_or_square_ind_common,
            );
            let mut __even_or_square_ind_common_total: () = Default::default();
            let mut __even_or_square_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __even_or_square_ind_common_new,
                &mut __even_or_square_ind_common_delta,
                &mut __even_or_square_ind_common_total,
            );
            let mut even_or_square_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.even_or_square_indices_0);
            let mut even_or_square_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut even_or_square_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut even_or_square_indices_0_new
                    .to_rel_index_write(&mut __even_or_square_ind_common_new),
                &mut even_or_square_indices_0_delta
                    .to_rel_index_write(&mut __even_or_square_ind_common_delta),
                &mut even_or_square_indices_0_total
                    .to_rel_index_write(&mut __even_or_square_ind_common_total),
            );
            let __even_ind_common_total: () = std::mem::take(
                &mut _self.__even_ind_common,
            );
            let even_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.even_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("even_or_square <-- even_indices_none_total");
                {
                    if let Some(__matching) = even_indices_none_total
                        .to_rel_index(&__even_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                let __new_row: (i32,) = (
                                    ascent::internal::Convert::convert(x),
                                );
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &even_or_square_indices_0_total
                                        .to_rel_index(&__even_or_square_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &even_or_square_indices_0_delta
                                            .to_rel_index(&__even_or_square_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut even_or_square_indices_0_new
                                            .to_rel_index_write(&mut __even_or_square_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.even_or_square.len();
                                        _self.even_or_square.push((__new_row.0,));
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __even_or_square_ind_common_new,
                    &mut __even_or_square_ind_common_delta,
                    &mut __even_or_square_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_or_square_indices_0_new
                        .to_rel_index_write(&mut __even_or_square_ind_common_new),
                    &mut even_or_square_indices_0_delta
                        .to_rel_index_write(&mut __even_or_square_ind_common_delta),
                    &mut even_or_square_indices_0_total
                        .to_rel_index_write(&mut __even_or_square_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __even_or_square_ind_common_new,
                    &mut __even_or_square_ind_common_delta,
                    &mut __even_or_square_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_or_square_indices_0_new
                        .to_rel_index_write(&mut __even_or_square_ind_common_new),
                    &mut even_or_square_indices_0_delta
                        .to_rel_index_write(&mut __even_or_square_ind_common_delta),
                    &mut even_or_square_indices_0_total
                        .to_rel_index_write(&mut __even_or_square_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
            }
            _self.__even_or_square_ind_common = __even_or_square_ind_common_total;
            _self.even_or_square_indices_0 = even_or_square_indices_0_total;
            _self.__even_ind_common = __even_ind_common_total;
            _self.even_indices_none = even_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __square_ind_common_delta: () = ::std::mem::take(
                &mut _self.__square_ind_common,
            );
            let mut __square_ind_common_total: () = Default::default();
            let mut __square_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __square_ind_common_new,
                &mut __square_ind_common_delta,
                &mut __square_ind_common_total,
            );
            let mut square_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.square_indices_0);
            let mut square_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut square_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut square_indices_0_new
                    .to_rel_index_write(&mut __square_ind_common_new),
                &mut square_indices_0_delta
                    .to_rel_index_write(&mut __square_ind_common_delta),
                &mut square_indices_0_total
                    .to_rel_index_write(&mut __square_ind_common_total),
            );
            let mut square_indices_none_delta: ascent::rel::ToRelIndexType<(), (i32,)> = ::std::mem::take(
                &mut _self.square_indices_none,
            );
            let mut square_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = Default::default();
            let mut square_indices_none_new: ascent::rel::ToRelIndexType<(), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut square_indices_none_new
                    .to_rel_index_write(&mut __square_ind_common_new),
                &mut square_indices_none_delta
                    .to_rel_index_write(&mut __square_ind_common_delta),
                &mut square_indices_none_total
                    .to_rel_index_write(&mut __square_ind_common_total),
            );
            let __number_ind_common_total: () = std::mem::take(
                &mut _self.__number_ind_common,
            );
            let number_indices_0_total: ascent::internal::RelFullIndexType<(i32,), ()> = std::mem::take(
                &mut _self.number_indices_0,
            );
            let number_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.number_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment(
                    "square <-- number_indices_none_total, number_indices_0_total",
                );
                {
                    if let Some(__matching) = number_indices_none_total
                        .to_rel_index(&__number_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let y: &i32 = __val.0;
                                if let Some(__matching) = number_indices_0_total
                                    .to_rel_index(&__number_ind_common_total)
                                    .index_get(&((y * y).clone(),))
                                {
                                    __matching
                                        .for_each(|__val| {
                                            let __new_row: (i32,) = (y * y,);
                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                &square_indices_0_total
                                                    .to_rel_index(&__square_ind_common_total),
                                                &__new_row,
                                            )
                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                    &square_indices_0_delta
                                                        .to_rel_index(&__square_ind_common_delta),
                                                    &__new_row,
                                                )
                                            {
                                                if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                    &mut square_indices_0_new
                                                        .to_rel_index_write(&mut __square_ind_common_new),
                                                    &__new_row,
                                                    (),
                                                ) {
                                                    let __new_row_ind = _self.square.len();
                                                    _self.square.push((__new_row.0.clone(),));
                                                    ::ascent::internal::RelIndexWrite::index_insert(
                                                        &mut square_indices_none_new
                                                            .to_rel_index_write(&mut __square_ind_common_new),
                                                        (),
                                                        (__new_row.0.clone(),),
                                                    );
                                                    __changed = true;
                                                }
                                            }
                                        });
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __square_ind_common_new,
                    &mut __square_ind_common_delta,
                    &mut __square_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut square_indices_0_new
                        .to_rel_index_write(&mut __square_ind_common_new),
                    &mut square_indices_0_delta
                        .to_rel_index_write(&mut __square_ind_common_delta),
                    &mut square_indices_0_total
                        .to_rel_index_write(&mut __square_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut square_indices_none_new
                        .to_rel_index_write(&mut __square_ind_common_new),
                    &mut square_indices_none_delta
                        .to_rel_index_write(&mut __square_ind_common_delta),
                    &mut square_indices_none_total
                        .to_rel_index_write(&mut __square_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __square_ind_common_new,
                    &mut __square_ind_common_delta,
                    &mut __square_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut square_indices_0_new
                        .to_rel_index_write(&mut __square_ind_common_new),
                    &mut square_indices_0_delta
                        .to_rel_index_write(&mut __square_ind_common_delta),
                    &mut square_indices_0_total
                        .to_rel_index_write(&mut __square_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut square_indices_none_new
                        .to_rel_index_write(&mut __square_ind_common_new),
                    &mut square_indices_none_delta
                        .to_rel_index_write(&mut __square_ind_common_delta),
                    &mut square_indices_none_total
                        .to_rel_index_write(&mut __square_ind_common_total),
                );
                _self.scc_iters[2usize] += 1;
            }
            _self.__square_ind_common = __square_ind_common_total;
            _self.square_indices_0 = square_indices_0_total;
            _self.square_indices_none = square_indices_none_total;
            _self.__number_ind_common = __number_ind_common_total;
            _self.number_indices_0 = number_indices_0_total;
            _self.number_indices_none = number_indices_none_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 3");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __even_or_square_ind_common_delta: () = ::std::mem::take(
                &mut _self.__even_or_square_ind_common,
            );
            let mut __even_or_square_ind_common_total: () = Default::default();
            let mut __even_or_square_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __even_or_square_ind_common_new,
                &mut __even_or_square_ind_common_delta,
                &mut __even_or_square_ind_common_total,
            );
            let mut even_or_square_indices_0_delta: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = ::std::mem::take(&mut _self.even_or_square_indices_0);
            let mut even_or_square_indices_0_total: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            let mut even_or_square_indices_0_new: ascent::internal::RelFullIndexType<
                (i32,),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut even_or_square_indices_0_new
                    .to_rel_index_write(&mut __even_or_square_ind_common_new),
                &mut even_or_square_indices_0_delta
                    .to_rel_index_write(&mut __even_or_square_ind_common_delta),
                &mut even_or_square_indices_0_total
                    .to_rel_index_write(&mut __even_or_square_ind_common_total),
            );
            let __square_ind_common_total: () = std::mem::take(
                &mut _self.__square_ind_common,
            );
            let square_indices_none_total: ascent::rel::ToRelIndexType<(), (i32,)> = std::mem::take(
                &mut _self.square_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment(
                    "even_or_square <-- square_indices_none_total",
                );
                {
                    if let Some(__matching) = square_indices_none_total
                        .to_rel_index(&__square_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                let __new_row: (i32,) = (
                                    ascent::internal::Convert::convert(x),
                                );
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &even_or_square_indices_0_total
                                        .to_rel_index(&__even_or_square_ind_common_total),
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        &even_or_square_indices_0_delta
                                            .to_rel_index(&__even_or_square_ind_common_delta),
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut even_or_square_indices_0_new
                                            .to_rel_index_write(&mut __even_or_square_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.even_or_square.len();
                                        _self.even_or_square.push((__new_row.0,));
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __even_or_square_ind_common_new,
                    &mut __even_or_square_ind_common_delta,
                    &mut __even_or_square_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_or_square_indices_0_new
                        .to_rel_index_write(&mut __even_or_square_ind_common_new),
                    &mut even_or_square_indices_0_delta
                        .to_rel_index_write(&mut __even_or_square_ind_common_delta),
                    &mut even_or_square_indices_0_total
                        .to_rel_index_write(&mut __even_or_square_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __even_or_square_ind_common_new,
                    &mut __even_or_square_ind_common_delta,
                    &mut __even_or_square_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut even_or_square_indices_0_new
                        .to_rel_index_write(&mut __even_or_square_ind_common_new),
                    &mut even_or_square_indices_0_delta
                        .to_rel_index_write(&mut __even_or_square_ind_common_delta),
                    &mut even_or_square_indices_0_total
                        .to_rel_index_write(&mut __even_or_square_ind_common_total),
                );
                _self.scc_iters[3usize] += 1;
            }
            _self.__even_or_square_ind_common = __even_or_square_ind_common_total;
            _self.even_or_square_indices_0 = even_or_square_indices_0_total;
            _self.__square_ind_common = __square_ind_common_total;
            _self.square_indices_none = square_indices_none_total;
            _self.scc_times[3usize] += _scc_start_time.elapsed();
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
            let selection_tuple = ();
            let rel_ind = &mut self.even_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__even_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.even_or_square.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.even_or_square_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__even_or_square_ind_common),
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
        for (_i, tuple) in self.square.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.square_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__square_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.square_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__square_ind_common),
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
        "scc 0, is_looping: false:\n  even <-- number_indices_none_total\n  dynamic relations: even\nscc 1, is_looping: false:\n  even_or_square <-- even_indices_none_total\n  dynamic relations: even_or_square\nscc 2, is_looping: false:\n  square <-- number_indices_none_total, number_indices_0_total\n  dynamic relations: square\nscc 3, is_looping: false:\n  even_or_square <-- square_indices_none_total\n  dynamic relations: even_or_square\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "even", self.even.len()))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "{0} size: {1}\n", "even_or_square", self.even_or_square.len(),
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "number", self.number.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "square", self.square.len()))
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
            even: Default::default(),
            __even_ind_common: Default::default(),
            even_indices_0: Default::default(),
            even_indices_none: Default::default(),
            even_or_square: Default::default(),
            __even_or_square_ind_common: Default::default(),
            even_or_square_indices_0: Default::default(),
            number: Default::default(),
            __number_ind_common: Default::default(),
            number_indices_0: Default::default(),
            number_indices_none: Default::default(),
            square: Default::default(),
            __square_ind_common: Default::default(),
            square_indices_0: Default::default(),
            square_indices_none: Default::default(),
            scc_times: [std::time::Duration::ZERO; 4usize],
            scc_iters: [0; 4usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
