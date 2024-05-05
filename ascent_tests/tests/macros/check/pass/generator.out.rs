use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
logical indices: edge_indices_0_1; edge_indices_1; edge_indices_none*/
    pub edge: ::std::vec::Vec<(i32, i32)>,
    pub __edge_ind_common: (),
    pub edge_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub edge_indices_1: ascent::rel::ToRelIndexType<(i32,), (i32,)>,
    pub edge_indices_none: ascent::rel::ToRelIndexType<(), (i32, i32)>,
    /**
logical indices: path_indices_0; path_indices_0_1*/
    pub path: ::std::vec::Vec<(i32, i32)>,
    pub __path_ind_common: (),
    pub path_indices_0: ascent::rel::ToRelIndexType<(i32,), (i32,)>,
    pub path_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
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
            let mut __edge_ind_common_delta: () = ::std::mem::take(
                &mut _self.__edge_ind_common,
            );
            let mut __edge_ind_common_total: () = Default::default();
            let mut __edge_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __edge_ind_common_new,
                &mut __edge_ind_common_delta,
                &mut __edge_ind_common_total,
            );
            let mut edge_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.edge_indices_0_1);
            let mut edge_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut edge_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut edge_indices_0_1_new.to_rel_index_write(&mut __edge_ind_common_new),
                &mut edge_indices_0_1_delta
                    .to_rel_index_write(&mut __edge_ind_common_delta),
                &mut edge_indices_0_1_total
                    .to_rel_index_write(&mut __edge_ind_common_total),
            );
            let mut edge_indices_1_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.edge_indices_1,
            );
            let mut edge_indices_1_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut edge_indices_1_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut edge_indices_1_new.to_rel_index_write(&mut __edge_ind_common_new),
                &mut edge_indices_1_delta
                    .to_rel_index_write(&mut __edge_ind_common_delta),
                &mut edge_indices_1_total
                    .to_rel_index_write(&mut __edge_ind_common_total),
            );
            let mut edge_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (i32, i32),
            > = ::std::mem::take(&mut _self.edge_indices_none);
            let mut edge_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (i32, i32),
            > = Default::default();
            let mut edge_indices_none_new: ascent::rel::ToRelIndexType<(), (i32, i32)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut edge_indices_none_new
                    .to_rel_index_write(&mut __edge_ind_common_new),
                &mut edge_indices_none_delta
                    .to_rel_index_write(&mut __edge_ind_common_delta),
                &mut edge_indices_none_total
                    .to_rel_index_write(&mut __edge_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("edge <-- for_x");
                {
                    for x in 0..100 {
                        let __new_row: (i32, i32) = (
                            ascent::internal::Convert::convert(x),
                            x + 1,
                        );
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                            &edge_indices_0_1_total
                                .to_rel_index(&__edge_ind_common_total),
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                &edge_indices_0_1_delta
                                    .to_rel_index(&__edge_ind_common_delta),
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut edge_indices_0_1_new
                                    .to_rel_index_write(&mut __edge_ind_common_new),
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.edge.len();
                                _self.edge.push((__new_row.0.clone(), __new_row.1.clone()));
                                ::ascent::internal::RelIndexWrite::index_insert(
                                    &mut edge_indices_1_new
                                        .to_rel_index_write(&mut __edge_ind_common_new),
                                    (__new_row.1.clone(),),
                                    (__new_row.0.clone(),),
                                );
                                ::ascent::internal::RelIndexWrite::index_insert(
                                    &mut edge_indices_none_new
                                        .to_rel_index_write(&mut __edge_ind_common_new),
                                    (),
                                    (__new_row.0.clone(), __new_row.1.clone()),
                                );
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __edge_ind_common_new,
                    &mut __edge_ind_common_delta,
                    &mut __edge_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut edge_indices_0_1_new
                        .to_rel_index_write(&mut __edge_ind_common_new),
                    &mut edge_indices_0_1_delta
                        .to_rel_index_write(&mut __edge_ind_common_delta),
                    &mut edge_indices_0_1_total
                        .to_rel_index_write(&mut __edge_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut edge_indices_1_new
                        .to_rel_index_write(&mut __edge_ind_common_new),
                    &mut edge_indices_1_delta
                        .to_rel_index_write(&mut __edge_ind_common_delta),
                    &mut edge_indices_1_total
                        .to_rel_index_write(&mut __edge_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut edge_indices_none_new
                        .to_rel_index_write(&mut __edge_ind_common_new),
                    &mut edge_indices_none_delta
                        .to_rel_index_write(&mut __edge_ind_common_delta),
                    &mut edge_indices_none_total
                        .to_rel_index_write(&mut __edge_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __edge_ind_common_new,
                    &mut __edge_ind_common_delta,
                    &mut __edge_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut edge_indices_0_1_new
                        .to_rel_index_write(&mut __edge_ind_common_new),
                    &mut edge_indices_0_1_delta
                        .to_rel_index_write(&mut __edge_ind_common_delta),
                    &mut edge_indices_0_1_total
                        .to_rel_index_write(&mut __edge_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut edge_indices_1_new
                        .to_rel_index_write(&mut __edge_ind_common_new),
                    &mut edge_indices_1_delta
                        .to_rel_index_write(&mut __edge_ind_common_delta),
                    &mut edge_indices_1_total
                        .to_rel_index_write(&mut __edge_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut edge_indices_none_new
                        .to_rel_index_write(&mut __edge_ind_common_new),
                    &mut edge_indices_none_delta
                        .to_rel_index_write(&mut __edge_ind_common_delta),
                    &mut edge_indices_none_total
                        .to_rel_index_write(&mut __edge_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_0_1 = edge_indices_0_1_total;
            _self.edge_indices_1 = edge_indices_1_total;
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
            let mut path_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.path_indices_0,
            );
            let mut path_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut path_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut path_indices_0_new.to_rel_index_write(&mut __path_ind_common_new),
                &mut path_indices_0_delta
                    .to_rel_index_write(&mut __path_ind_common_delta),
                &mut path_indices_0_total
                    .to_rel_index_write(&mut __path_ind_common_total),
            );
            let mut path_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.path_indices_0_1);
            let mut path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::RelFullIndexType<
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
            let __edge_ind_common_total: () = std::mem::take(
                &mut _self.__edge_ind_common,
            );
            let edge_indices_none_total: ascent::rel::ToRelIndexType<(), (i32, i32)> = std::mem::take(
                &mut _self.edge_indices_none,
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("path <-- edge_indices_none_total");
                {
                    if let Some(__matching) = edge_indices_none_total
                        .to_rel_index(&__edge_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                let y: &i32 = __val.1;
                                let __new_row: (i32, i32) = (*x, *y);
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
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut path_indices_0_1_new
                                            .to_rel_index_write(&mut __path_ind_common_new),
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.path.len();
                                        _self.path.push((__new_row.0.clone(), __new_row.1.clone()));
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut path_indices_0_new
                                                .to_rel_index_write(&mut __path_ind_common_new),
                                            (__new_row.0.clone(),),
                                            (__new_row.1.clone(),),
                                        );
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
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
                _self.scc_iters[1usize] += 1;
            }
            _self.__path_ind_common = __path_ind_common_total;
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_none = edge_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
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
            let mut path_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.path_indices_0,
            );
            let mut path_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut path_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut path_indices_0_new.to_rel_index_write(&mut __path_ind_common_new),
                &mut path_indices_0_delta
                    .to_rel_index_write(&mut __path_ind_common_delta),
                &mut path_indices_0_total
                    .to_rel_index_write(&mut __path_ind_common_total),
            );
            let mut path_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.path_indices_0_1);
            let mut path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::RelFullIndexType<
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
            let __edge_ind_common_total: () = std::mem::take(
                &mut _self.__edge_ind_common,
            );
            let edge_indices_1_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = std::mem::take(
                &mut _self.edge_indices_1,
            );
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
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
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = path_indices_0_delta
                                    .to_rel_index(&__path_ind_common_delta)
                                    .index_get(&(y.clone(),))
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
                                                    let __new_row: (i32, i32) = (*x, *z);
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
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut path_indices_0_1_new
                                                                .to_rel_index_write(&mut __path_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.path.len();
                                                            _self.path.push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut path_indices_0_new
                                                                    .to_rel_index_write(&mut __path_ind_common_new),
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            __changed = true;
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        path_indices_0_delta
                            .to_rel_index(&__path_ind_common_delta)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = edge_indices_1_total
                                    .to_rel_index(&__edge_ind_common_total)
                                    .index_get(&(y.clone(),))
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
                                                    let __new_row: (i32, i32) = (*x, *z);
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
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut path_indices_0_1_new
                                                                .to_rel_index_write(&mut __path_ind_common_new),
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.path.len();
                                                            _self.path.push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut path_indices_0_new
                                                                    .to_rel_index_write(&mut __path_ind_common_new),
                                                                (__new_row.0.clone(),),
                                                                (__new_row.1.clone(),),
                                                            );
                                                            __changed = true;
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    }
                }
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
                _self.scc_iters[2usize] += 1;
                if !__changed {
                    break;
                }
            }
            _self.__path_ind_common = __path_ind_common_total;
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_1 = edge_indices_1_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.edge.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.edge_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__edge_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = (tuple.1.clone(),);
            let rel_ind = &mut self.edge_indices_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__edge_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.edge_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__edge_ind_common),
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone()),
            );
        }
        for (_i, tuple) in self.path.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.path_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__path_ind_common),
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.path_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__path_ind_common),
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
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  edge <-- for_x\n  dynamic relations: edge\nscc 1, is_looping: false:\n  path <-- edge_indices_none_total\n  dynamic relations: path\nscc 2, is_looping: true:\n  path <-- edge_indices_1_total, path_indices_0_delta [SIMPLE JOIN]\n  dynamic relations: path\n"
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
            edge: Default::default(),
            __edge_ind_common: Default::default(),
            edge_indices_0_1: Default::default(),
            edge_indices_1: Default::default(),
            edge_indices_none: Default::default(),
            path: Default::default(),
            __path_ind_common: Default::default(),
            path_indices_0: Default::default(),
            path_indices_0_1: Default::default(),
            scc_times: [std::time::Duration::ZERO; 3usize],
            scc_iters: [0; 3usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
