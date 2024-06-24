use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
logical indices: bar_indices_0; bar_indices_0_1*/
    pub bar: ::std::vec::Vec<(i32, i32)>,
    pub __bar_ind_common: (),
    pub bar_indices_0: ascent::rel::ToRelIndexType<(i32,), (i32,)>,
    pub bar_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    /**
logical indices: baz_indices_0_1_2*/
    pub baz: ::std::vec::Vec<(i32, i32, i32)>,
    pub __baz_ind_common: (),
    pub baz_indices_0_1_2: ascent::internal::RelFullIndexType<(i32, i32, i32), ()>,
    /**
logical indices: foo_indices_0_1; foo_indices_none*/
    pub foo: ::std::vec::Vec<(i32, Option<i32>)>,
    pub __foo_ind_common: (),
    pub foo_indices_0_1: ascent::internal::RelFullIndexType<(i32, Option<i32>), ()>,
    pub foo_indices_none: ascent::rel::ToRelIndexType<(), (i32, Option<i32>)>,
    scc_times: [std::time::Duration; 8usize],
    scc_iters: [usize; 8usize],
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
            let mut __foo_ind_common_delta: () = ::std::mem::take(
                &mut _self.__foo_ind_common,
            );
            let mut __foo_ind_common_total: () = Default::default();
            let mut __foo_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __foo_ind_common_new,
                &mut __foo_ind_common_delta,
                &mut __foo_ind_common_total,
            );
            let mut foo_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = ::std::mem::take(&mut _self.foo_indices_0_1);
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_0_1_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_0_1_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_0_1_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            let mut foo_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = ::std::mem::take(&mut _self.foo_indices_none);
            let mut foo_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            let mut foo_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_none_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_none_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_none_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("foo <-- ");
                {
                    let __new_row: (i32, Option<i32>) = (1, Some(2));
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &foo_indices_0_1_total.to_rel_index(&__foo_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &foo_indices_0_1_delta.to_rel_index(&__foo_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut foo_indices_0_1_new
                                .to_rel_index_write(&mut __foo_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.foo.len();
                            _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut foo_indices_none_new
                                    .to_rel_index_write(&mut __foo_ind_common_new),
                                (),
                                (__new_row.0.clone(), __new_row.1.clone()),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_none = foo_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __foo_ind_common_delta: () = ::std::mem::take(
                &mut _self.__foo_ind_common,
            );
            let mut __foo_ind_common_total: () = Default::default();
            let mut __foo_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __foo_ind_common_new,
                &mut __foo_ind_common_delta,
                &mut __foo_ind_common_total,
            );
            let mut foo_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = ::std::mem::take(&mut _self.foo_indices_0_1);
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_0_1_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_0_1_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_0_1_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            let mut foo_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = ::std::mem::take(&mut _self.foo_indices_none);
            let mut foo_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            let mut foo_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_none_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_none_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_none_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("foo <-- ");
                {
                    let __new_row: (i32, Option<i32>) = (
                        2,
                        ascent::internal::Convert::convert(None),
                    );
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &foo_indices_0_1_total.to_rel_index(&__foo_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &foo_indices_0_1_delta.to_rel_index(&__foo_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut foo_indices_0_1_new
                                .to_rel_index_write(&mut __foo_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.foo.len();
                            _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut foo_indices_none_new
                                    .to_rel_index_write(&mut __foo_ind_common_new),
                                (),
                                (__new_row.0.clone(), __new_row.1.clone()),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
            }
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_none = foo_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __foo_ind_common_delta: () = ::std::mem::take(
                &mut _self.__foo_ind_common,
            );
            let mut __foo_ind_common_total: () = Default::default();
            let mut __foo_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __foo_ind_common_new,
                &mut __foo_ind_common_delta,
                &mut __foo_ind_common_total,
            );
            let mut foo_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = ::std::mem::take(&mut _self.foo_indices_0_1);
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_0_1_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_0_1_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_0_1_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            let mut foo_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = ::std::mem::take(&mut _self.foo_indices_none);
            let mut foo_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            let mut foo_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_none_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_none_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_none_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("foo <-- ");
                {
                    let __new_row: (i32, Option<i32>) = (3, Some(5));
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &foo_indices_0_1_total.to_rel_index(&__foo_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &foo_indices_0_1_delta.to_rel_index(&__foo_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut foo_indices_0_1_new
                                .to_rel_index_write(&mut __foo_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.foo.len();
                            _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut foo_indices_none_new
                                    .to_rel_index_write(&mut __foo_ind_common_new),
                                (),
                                (__new_row.0.clone(), __new_row.1.clone()),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                _self.scc_iters[2usize] += 1;
            }
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_none = foo_indices_none_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 3");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __foo_ind_common_delta: () = ::std::mem::take(
                &mut _self.__foo_ind_common,
            );
            let mut __foo_ind_common_total: () = Default::default();
            let mut __foo_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __foo_ind_common_new,
                &mut __foo_ind_common_delta,
                &mut __foo_ind_common_total,
            );
            let mut foo_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = ::std::mem::take(&mut _self.foo_indices_0_1);
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, Option<i32>),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_0_1_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_0_1_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_0_1_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            let mut foo_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = ::std::mem::take(&mut _self.foo_indices_none);
            let mut foo_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            let mut foo_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut foo_indices_none_new.to_rel_index_write(&mut __foo_ind_common_new),
                &mut foo_indices_none_delta
                    .to_rel_index_write(&mut __foo_ind_common_delta),
                &mut foo_indices_none_total
                    .to_rel_index_write(&mut __foo_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("foo <-- ");
                {
                    let __new_row: (i32, Option<i32>) = (4, Some(10));
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &foo_indices_0_1_total.to_rel_index(&__foo_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &foo_indices_0_1_delta.to_rel_index(&__foo_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut foo_indices_0_1_new
                                .to_rel_index_write(&mut __foo_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.foo.len();
                            _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut foo_indices_none_new
                                    .to_rel_index_write(&mut __foo_ind_common_new),
                                (),
                                (__new_row.0.clone(), __new_row.1.clone()),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __foo_ind_common_new,
                    &mut __foo_ind_common_delta,
                    &mut __foo_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_0_1_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_0_1_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_0_1_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut foo_indices_none_new
                        .to_rel_index_write(&mut __foo_ind_common_new),
                    &mut foo_indices_none_delta
                        .to_rel_index_write(&mut __foo_ind_common_delta),
                    &mut foo_indices_none_total
                        .to_rel_index_write(&mut __foo_ind_common_total),
                );
                _self.scc_iters[3usize] += 1;
            }
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_none = foo_indices_none_total;
            _self.scc_times[3usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 4");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __bar_ind_common_delta: () = ::std::mem::take(
                &mut _self.__bar_ind_common,
            );
            let mut __bar_ind_common_total: () = Default::default();
            let mut __bar_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __bar_ind_common_new,
                &mut __bar_ind_common_delta,
                &mut __bar_ind_common_total,
            );
            let mut bar_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.bar_indices_0,
            );
            let mut bar_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut bar_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
                &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
            );
            let mut bar_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.bar_indices_0_1);
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut bar_indices_0_1_new.to_rel_index_write(&mut __bar_ind_common_new),
                &mut bar_indices_0_1_delta
                    .to_rel_index_write(&mut __bar_ind_common_delta),
                &mut bar_indices_0_1_total
                    .to_rel_index_write(&mut __bar_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("bar <-- ");
                {
                    let __new_row: (i32, i32) = (3, 6);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &bar_indices_0_1_total.to_rel_index(&__bar_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &bar_indices_0_1_delta.to_rel_index(&__bar_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut bar_indices_0_1_new
                                .to_rel_index_write(&mut __bar_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.bar.len();
                            _self.bar.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut bar_indices_0_new
                                    .to_rel_index_write(&mut __bar_ind_common_new),
                                (__new_row.0.clone(),),
                                (__new_row.1.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __bar_ind_common_new,
                    &mut __bar_ind_common_delta,
                    &mut __bar_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_1_new
                        .to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_1_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_1_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __bar_ind_common_new,
                    &mut __bar_ind_common_delta,
                    &mut __bar_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_1_new
                        .to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_1_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_1_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                _self.scc_iters[4usize] += 1;
            }
            _self.__bar_ind_common = __bar_ind_common_total;
            _self.bar_indices_0 = bar_indices_0_total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.scc_times[4usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 5");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __bar_ind_common_delta: () = ::std::mem::take(
                &mut _self.__bar_ind_common,
            );
            let mut __bar_ind_common_total: () = Default::default();
            let mut __bar_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __bar_ind_common_new,
                &mut __bar_ind_common_delta,
                &mut __bar_ind_common_total,
            );
            let mut bar_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.bar_indices_0,
            );
            let mut bar_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut bar_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
                &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
            );
            let mut bar_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.bar_indices_0_1);
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut bar_indices_0_1_new.to_rel_index_write(&mut __bar_ind_common_new),
                &mut bar_indices_0_1_delta
                    .to_rel_index_write(&mut __bar_ind_common_delta),
                &mut bar_indices_0_1_total
                    .to_rel_index_write(&mut __bar_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("bar <-- ");
                {
                    let __new_row: (i32, i32) = (5, 10);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &bar_indices_0_1_total.to_rel_index(&__bar_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &bar_indices_0_1_delta.to_rel_index(&__bar_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut bar_indices_0_1_new
                                .to_rel_index_write(&mut __bar_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.bar.len();
                            _self.bar.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut bar_indices_0_new
                                    .to_rel_index_write(&mut __bar_ind_common_new),
                                (__new_row.0.clone(),),
                                (__new_row.1.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __bar_ind_common_new,
                    &mut __bar_ind_common_delta,
                    &mut __bar_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_1_new
                        .to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_1_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_1_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __bar_ind_common_new,
                    &mut __bar_ind_common_delta,
                    &mut __bar_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_1_new
                        .to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_1_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_1_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                _self.scc_iters[5usize] += 1;
            }
            _self.__bar_ind_common = __bar_ind_common_total;
            _self.bar_indices_0 = bar_indices_0_total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.scc_times[5usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 6");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __bar_ind_common_delta: () = ::std::mem::take(
                &mut _self.__bar_ind_common,
            );
            let mut __bar_ind_common_total: () = Default::default();
            let mut __bar_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __bar_ind_common_new,
                &mut __bar_ind_common_delta,
                &mut __bar_ind_common_total,
            );
            let mut bar_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.bar_indices_0,
            );
            let mut bar_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut bar_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
                &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
            );
            let mut bar_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.bar_indices_0_1);
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut bar_indices_0_1_new.to_rel_index_write(&mut __bar_ind_common_new),
                &mut bar_indices_0_1_delta
                    .to_rel_index_write(&mut __bar_ind_common_delta),
                &mut bar_indices_0_1_total
                    .to_rel_index_write(&mut __bar_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("bar <-- ");
                {
                    let __new_row: (i32, i32) = (10, 20);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &bar_indices_0_1_total.to_rel_index(&__bar_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &bar_indices_0_1_delta.to_rel_index(&__bar_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut bar_indices_0_1_new
                                .to_rel_index_write(&mut __bar_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.bar.len();
                            _self.bar.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut bar_indices_0_new
                                    .to_rel_index_write(&mut __bar_ind_common_new),
                                (__new_row.0.clone(),),
                                (__new_row.1.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __bar_ind_common_new,
                    &mut __bar_ind_common_delta,
                    &mut __bar_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_1_new
                        .to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_1_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_1_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __bar_ind_common_new,
                    &mut __bar_ind_common_delta,
                    &mut __bar_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut bar_indices_0_1_new
                        .to_rel_index_write(&mut __bar_ind_common_new),
                    &mut bar_indices_0_1_delta
                        .to_rel_index_write(&mut __bar_ind_common_delta),
                    &mut bar_indices_0_1_total
                        .to_rel_index_write(&mut __bar_ind_common_total),
                );
                _self.scc_iters[6usize] += 1;
            }
            _self.__bar_ind_common = __bar_ind_common_total;
            _self.bar_indices_0 = bar_indices_0_total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.scc_times[6usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 7");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __baz_ind_common_delta: () = ::std::mem::take(
                &mut _self.__baz_ind_common,
            );
            let mut __baz_ind_common_total: () = Default::default();
            let mut __baz_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __baz_ind_common_new,
                &mut __baz_ind_common_delta,
                &mut __baz_ind_common_total,
            );
            let mut baz_indices_0_1_2_delta: ascent::internal::RelFullIndexType<
                (i32, i32, i32),
                (),
            > = ::std::mem::take(&mut _self.baz_indices_0_1_2);
            let mut baz_indices_0_1_2_total: ascent::internal::RelFullIndexType<
                (i32, i32, i32),
                (),
            > = Default::default();
            let mut baz_indices_0_1_2_new: ascent::internal::RelFullIndexType<
                (i32, i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut baz_indices_0_1_2_new.to_rel_index_write(&mut __baz_ind_common_new),
                &mut baz_indices_0_1_2_delta
                    .to_rel_index_write(&mut __baz_ind_common_delta),
                &mut baz_indices_0_1_2_total
                    .to_rel_index_write(&mut __baz_ind_common_total),
            );
            let __bar_ind_common_total: () = std::mem::take(&mut _self.__bar_ind_common);
            let bar_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = std::mem::take(
                &mut _self.bar_indices_0,
            );
            let __foo_ind_common_total: () = std::mem::take(&mut _self.__foo_ind_common);
            let foo_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (i32, Option<i32>),
            > = std::mem::take(&mut _self.foo_indices_none);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment(
                    "baz <-- foo_indices_none_total, bar_indices_0_total",
                );
                {
                    if let Some(__matching) = foo_indices_none_total
                        .to_rel_index(&__foo_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &i32 = __val.0;
                                let __arg_pattern_: &Option<i32> = __val.1;
                                if let Some(y) = __arg_pattern_ {
                                    if let Some(__matching) = bar_indices_0_total
                                        .to_rel_index(&__bar_ind_common_total)
                                        .index_get(&(y.clone(),))
                                    {
                                        __matching
                                            .for_each(|__val| {
                                                let __val = __val.tuple_of_borrowed();
                                                let z: &i32 = __val.0;
                                                let __new_row: (i32, i32, i32) = (*x, *y, *z);
                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                    &baz_indices_0_1_2_total
                                                        .to_rel_index(&__baz_ind_common_total),
                                                    &__new_row,
                                                )
                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &baz_indices_0_1_2_delta
                                                            .to_rel_index(&__baz_ind_common_delta),
                                                        &__new_row,
                                                    )
                                                {
                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                        &mut baz_indices_0_1_2_new
                                                            .to_rel_index_write(&mut __baz_ind_common_new),
                                                        &__new_row,
                                                        (),
                                                    ) {
                                                        let __new_row_ind = _self.baz.len();
                                                        _self.baz.push((__new_row.0, __new_row.1, __new_row.2));
                                                        __changed = true;
                                                    }
                                                }
                                            });
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __baz_ind_common_new,
                    &mut __baz_ind_common_delta,
                    &mut __baz_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut baz_indices_0_1_2_new
                        .to_rel_index_write(&mut __baz_ind_common_new),
                    &mut baz_indices_0_1_2_delta
                        .to_rel_index_write(&mut __baz_ind_common_delta),
                    &mut baz_indices_0_1_2_total
                        .to_rel_index_write(&mut __baz_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __baz_ind_common_new,
                    &mut __baz_ind_common_delta,
                    &mut __baz_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut baz_indices_0_1_2_new
                        .to_rel_index_write(&mut __baz_ind_common_new),
                    &mut baz_indices_0_1_2_delta
                        .to_rel_index_write(&mut __baz_ind_common_delta),
                    &mut baz_indices_0_1_2_total
                        .to_rel_index_write(&mut __baz_ind_common_total),
                );
                _self.scc_iters[7usize] += 1;
            }
            _self.__baz_ind_common = __baz_ind_common_total;
            _self.baz_indices_0_1_2 = baz_indices_0_1_2_total;
            _self.__bar_ind_common = __bar_ind_common_total;
            _self.bar_indices_0 = bar_indices_0_total;
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_none = foo_indices_none_total;
            _self.scc_times[7usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.bar.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.bar_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__bar_ind_common),
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.bar_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__bar_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.baz.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            let rel_ind = &mut self.baz_indices_0_1_2;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__baz_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.foo.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.foo_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__foo_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.foo_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__foo_ind_common),
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone()),
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
        let _type_constraints: ascent::internal::TypeConstraints<Option<i32>>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 1, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 2, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 3, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 4, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 5, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 6, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 7, is_looping: false:\n  baz <-- foo_indices_none_total, bar_indices_0_total\n  dynamic relations: baz\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "bar", self.bar.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "baz", self.baz.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "foo", self.foo.len()))
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
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "5", self
                    .scc_iters[5usize], self.scc_times[5usize],
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "6", self
                    .scc_iters[6usize], self.scc_times[6usize],
                ),
            )
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "scc {0}: iterations: {1}, time: {2:?}\n", "7", self
                    .scc_iters[7usize], self.scc_times[7usize],
                ),
            )
            .unwrap();
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            bar: Default::default(),
            __bar_ind_common: Default::default(),
            bar_indices_0: Default::default(),
            bar_indices_0_1: Default::default(),
            baz: Default::default(),
            __baz_ind_common: Default::default(),
            baz_indices_0_1_2: Default::default(),
            foo: Default::default(),
            __foo_ind_common: Default::default(),
            foo_indices_0_1: Default::default(),
            foo_indices_none: Default::default(),
            scc_times: [std::time::Duration::ZERO; 8usize],
            scc_iters: [0; 8usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
