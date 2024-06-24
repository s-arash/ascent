use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
logical indices: a_indices_0_1; a_indices_1*/
    pub a: ::std::vec::Vec<(i32, i32)>,
    pub __a_ind_common: (),
    pub a_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub a_indices_1: ascent::rel::ToRelIndexType<(i32,), (i32,)>,
    /**
logical indices: b_indices_0; b_indices_0_1*/
    pub b: ::std::vec::Vec<(i32, i32)>,
    pub __b_ind_common: (),
    pub b_indices_0: ascent::rel::ToRelIndexType<(i32,), (i32,)>,
    pub b_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    /**
logical indices: c_indices_0; c_indices_0_1*/
    pub c: ::std::vec::Vec<(i32, i32)>,
    pub __c_ind_common: (),
    pub c_indices_0: ascent::rel::ToRelIndexType<(i32,), (i32,)>,
    pub c_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
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
            let mut __a_ind_common_delta: () = ::std::mem::take(
                &mut _self.__a_ind_common,
            );
            let mut __a_ind_common_total: () = Default::default();
            let mut __a_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __a_ind_common_new,
                &mut __a_ind_common_delta,
                &mut __a_ind_common_total,
            );
            let mut a_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.a_indices_0_1);
            let mut a_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut a_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut a_indices_0_1_new.to_rel_index_write(&mut __a_ind_common_new),
                &mut a_indices_0_1_delta.to_rel_index_write(&mut __a_ind_common_delta),
                &mut a_indices_0_1_total.to_rel_index_write(&mut __a_ind_common_total),
            );
            let mut a_indices_1_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.a_indices_1,
            );
            let mut a_indices_1_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut a_indices_1_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut a_indices_1_new.to_rel_index_write(&mut __a_ind_common_new),
                &mut a_indices_1_delta.to_rel_index_write(&mut __a_ind_common_delta),
                &mut a_indices_1_total.to_rel_index_write(&mut __a_ind_common_total),
            );
            let mut __b_ind_common_delta: () = ::std::mem::take(
                &mut _self.__b_ind_common,
            );
            let mut __b_ind_common_total: () = Default::default();
            let mut __b_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __b_ind_common_new,
                &mut __b_ind_common_delta,
                &mut __b_ind_common_total,
            );
            let mut b_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.b_indices_0,
            );
            let mut b_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut b_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut b_indices_0_new.to_rel_index_write(&mut __b_ind_common_new),
                &mut b_indices_0_delta.to_rel_index_write(&mut __b_ind_common_delta),
                &mut b_indices_0_total.to_rel_index_write(&mut __b_ind_common_total),
            );
            let mut b_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.b_indices_0_1);
            let mut b_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut b_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut b_indices_0_1_new.to_rel_index_write(&mut __b_ind_common_new),
                &mut b_indices_0_1_delta.to_rel_index_write(&mut __b_ind_common_delta),
                &mut b_indices_0_1_total.to_rel_index_write(&mut __b_ind_common_total),
            );
            let mut __c_ind_common_delta: () = ::std::mem::take(
                &mut _self.__c_ind_common,
            );
            let mut __c_ind_common_total: () = Default::default();
            let mut __c_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __c_ind_common_new,
                &mut __c_ind_common_delta,
                &mut __c_ind_common_total,
            );
            let mut c_indices_0_delta: ascent::rel::ToRelIndexType<(i32,), (i32,)> = ::std::mem::take(
                &mut _self.c_indices_0,
            );
            let mut c_indices_0_total: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            let mut c_indices_0_new: ascent::rel::ToRelIndexType<(i32,), (i32,)> = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut c_indices_0_new.to_rel_index_write(&mut __c_ind_common_new),
                &mut c_indices_0_delta.to_rel_index_write(&mut __c_ind_common_delta),
                &mut c_indices_0_total.to_rel_index_write(&mut __c_ind_common_total),
            );
            let mut c_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = ::std::mem::take(&mut _self.c_indices_0_1);
            let mut c_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut c_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut c_indices_0_1_new.to_rel_index_write(&mut __c_ind_common_new),
                &mut c_indices_0_1_delta.to_rel_index_write(&mut __c_ind_common_delta),
                &mut c_indices_0_1_total.to_rel_index_write(&mut __c_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "a, b, c <-- a_indices_1_delta, b_indices_0_total+delta, c_indices_0_total+delta [SIMPLE JOIN]",
                );
                {
                    if a_indices_1_delta.to_rel_index(&__a_ind_common_delta).len()
                        <= ascent::internal::RelIndexCombined::new(
                                &b_indices_0_total.to_rel_index(&__b_ind_common_total),
                                &b_indices_0_delta.to_rel_index(&__b_ind_common_delta),
                            )
                            .len()
                    {
                        a_indices_1_delta
                            .to_rel_index(&__a_ind_common_delta)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                        &b_indices_0_total.to_rel_index(&__b_ind_common_total),
                                        &b_indices_0_delta.to_rel_index(&__b_ind_common_delta),
                                    )
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
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total.to_rel_index(&__c_ind_common_total),
                                                            &c_indices_0_delta.to_rel_index(&__c_ind_common_delta),
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let w: &i32 = __val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total.to_rel_index(&__a_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &a_indices_0_1_delta.to_rel_index(&__a_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new
                                                                            .to_rel_index_write(&mut __a_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new
                                                                                .to_rel_index_write(&mut __a_ind_common_new),
                                                                            (__new_row.1.clone(),),
                                                                            (__new_row.0.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(z),
                                                                    ascent::internal::Convert::convert(w),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &b_indices_0_1_total.to_rel_index(&__b_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &b_indices_0_1_delta.to_rel_index(&__b_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new
                                                                            .to_rel_index_write(&mut __b_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new
                                                                                .to_rel_index_write(&mut __b_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(x),
                                                                    ascent::internal::Convert::convert(y),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &c_indices_0_1_total.to_rel_index(&__c_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &c_indices_0_1_delta.to_rel_index(&__c_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new
                                                                            .to_rel_index_write(&mut __c_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new
                                                                                .to_rel_index_write(&mut __c_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                            });
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        ascent::internal::RelIndexCombined::new(
                                &b_indices_0_total.to_rel_index(&__b_ind_common_total),
                                &b_indices_0_delta.to_rel_index(&__b_ind_common_delta),
                            )
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = a_indices_1_delta
                                    .to_rel_index(&__a_ind_common_delta)
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
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total.to_rel_index(&__c_ind_common_total),
                                                            &c_indices_0_delta.to_rel_index(&__c_ind_common_delta),
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let w: &i32 = __val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total.to_rel_index(&__a_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &a_indices_0_1_delta.to_rel_index(&__a_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new
                                                                            .to_rel_index_write(&mut __a_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new
                                                                                .to_rel_index_write(&mut __a_ind_common_new),
                                                                            (__new_row.1.clone(),),
                                                                            (__new_row.0.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(z),
                                                                    ascent::internal::Convert::convert(w),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &b_indices_0_1_total.to_rel_index(&__b_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &b_indices_0_1_delta.to_rel_index(&__b_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new
                                                                            .to_rel_index_write(&mut __b_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new
                                                                                .to_rel_index_write(&mut __b_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(x),
                                                                    ascent::internal::Convert::convert(y),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &c_indices_0_1_total.to_rel_index(&__c_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &c_indices_0_1_delta.to_rel_index(&__c_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new
                                                                            .to_rel_index_write(&mut __c_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new
                                                                                .to_rel_index_write(&mut __c_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
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
                ascent::internal::comment(
                    "a, b, c <-- a_indices_1_total, b_indices_0_delta, c_indices_0_total+delta [SIMPLE JOIN]",
                );
                {
                    if a_indices_1_total.to_rel_index(&__a_ind_common_total).len()
                        <= b_indices_0_delta.to_rel_index(&__b_ind_common_delta).len()
                    {
                        a_indices_1_total
                            .to_rel_index(&__a_ind_common_total)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = b_indices_0_delta
                                    .to_rel_index(&__b_ind_common_delta)
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
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total.to_rel_index(&__c_ind_common_total),
                                                            &c_indices_0_delta.to_rel_index(&__c_ind_common_delta),
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let w: &i32 = __val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total.to_rel_index(&__a_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &a_indices_0_1_delta.to_rel_index(&__a_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new
                                                                            .to_rel_index_write(&mut __a_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new
                                                                                .to_rel_index_write(&mut __a_ind_common_new),
                                                                            (__new_row.1.clone(),),
                                                                            (__new_row.0.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(z),
                                                                    ascent::internal::Convert::convert(w),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &b_indices_0_1_total.to_rel_index(&__b_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &b_indices_0_1_delta.to_rel_index(&__b_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new
                                                                            .to_rel_index_write(&mut __b_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new
                                                                                .to_rel_index_write(&mut __b_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(x),
                                                                    ascent::internal::Convert::convert(y),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &c_indices_0_1_total.to_rel_index(&__c_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &c_indices_0_1_delta.to_rel_index(&__c_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new
                                                                            .to_rel_index_write(&mut __c_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new
                                                                                .to_rel_index_write(&mut __c_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                            });
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        b_indices_0_delta
                            .to_rel_index(&__b_ind_common_delta)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = a_indices_1_total
                                    .to_rel_index(&__a_ind_common_total)
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
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total.to_rel_index(&__c_ind_common_total),
                                                            &c_indices_0_delta.to_rel_index(&__c_ind_common_delta),
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let w: &i32 = __val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total.to_rel_index(&__a_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &a_indices_0_1_delta.to_rel_index(&__a_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new
                                                                            .to_rel_index_write(&mut __a_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new
                                                                                .to_rel_index_write(&mut __a_ind_common_new),
                                                                            (__new_row.1.clone(),),
                                                                            (__new_row.0.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(z),
                                                                    ascent::internal::Convert::convert(w),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &b_indices_0_1_total.to_rel_index(&__b_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &b_indices_0_1_delta.to_rel_index(&__b_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new
                                                                            .to_rel_index_write(&mut __b_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new
                                                                                .to_rel_index_write(&mut __b_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(x),
                                                                    ascent::internal::Convert::convert(y),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &c_indices_0_1_total.to_rel_index(&__c_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &c_indices_0_1_delta.to_rel_index(&__c_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new
                                                                            .to_rel_index_write(&mut __c_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new
                                                                                .to_rel_index_write(&mut __c_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
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
                ascent::internal::comment(
                    "a, b, c <-- a_indices_1_total, b_indices_0_total, c_indices_0_delta [SIMPLE JOIN]",
                );
                {
                    if a_indices_1_total.to_rel_index(&__a_ind_common_total).len()
                        <= b_indices_0_total.to_rel_index(&__b_ind_common_total).len()
                    {
                        a_indices_1_total
                            .to_rel_index(&__a_ind_common_total)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = b_indices_0_total
                                    .to_rel_index(&__b_ind_common_total)
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
                                                    if let Some(__matching) = c_indices_0_delta
                                                        .to_rel_index(&__c_ind_common_delta)
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let w: &i32 = __val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total.to_rel_index(&__a_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &a_indices_0_1_delta.to_rel_index(&__a_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new
                                                                            .to_rel_index_write(&mut __a_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new
                                                                                .to_rel_index_write(&mut __a_ind_common_new),
                                                                            (__new_row.1.clone(),),
                                                                            (__new_row.0.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(z),
                                                                    ascent::internal::Convert::convert(w),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &b_indices_0_1_total.to_rel_index(&__b_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &b_indices_0_1_delta.to_rel_index(&__b_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new
                                                                            .to_rel_index_write(&mut __b_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new
                                                                                .to_rel_index_write(&mut __b_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(x),
                                                                    ascent::internal::Convert::convert(y),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &c_indices_0_1_total.to_rel_index(&__c_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &c_indices_0_1_delta.to_rel_index(&__c_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new
                                                                            .to_rel_index_write(&mut __c_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new
                                                                                .to_rel_index_write(&mut __c_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                            });
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        b_indices_0_total
                            .to_rel_index(&__b_ind_common_total)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = a_indices_1_total
                                    .to_rel_index(&__a_ind_common_total)
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
                                                    if let Some(__matching) = c_indices_0_delta
                                                        .to_rel_index(&__c_ind_common_delta)
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let __val = __val.tuple_of_borrowed();
                                                                let w: &i32 = __val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total.to_rel_index(&__a_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &a_indices_0_1_delta.to_rel_index(&__a_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new
                                                                            .to_rel_index_write(&mut __a_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new
                                                                                .to_rel_index_write(&mut __a_ind_common_new),
                                                                            (__new_row.1.clone(),),
                                                                            (__new_row.0.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(z),
                                                                    ascent::internal::Convert::convert(w),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &b_indices_0_1_total.to_rel_index(&__b_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &b_indices_0_1_delta.to_rel_index(&__b_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new
                                                                            .to_rel_index_write(&mut __b_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new
                                                                                .to_rel_index_write(&mut __b_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
                                                                    }
                                                                }
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(x),
                                                                    ascent::internal::Convert::convert(y),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &c_indices_0_1_total.to_rel_index(&__c_ind_common_total),
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        &c_indices_0_1_delta.to_rel_index(&__c_ind_common_delta),
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new
                                                                            .to_rel_index_write(&mut __c_ind_common_new),
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new
                                                                                .to_rel_index_write(&mut __c_ind_common_new),
                                                                            (__new_row.0.clone(),),
                                                                            (__new_row.1.clone(),),
                                                                        );
                                                                        __changed = true;
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
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __a_ind_common_new,
                    &mut __a_ind_common_delta,
                    &mut __a_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut a_indices_0_1_new.to_rel_index_write(&mut __a_ind_common_new),
                    &mut a_indices_0_1_delta
                        .to_rel_index_write(&mut __a_ind_common_delta),
                    &mut a_indices_0_1_total
                        .to_rel_index_write(&mut __a_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut a_indices_1_new.to_rel_index_write(&mut __a_ind_common_new),
                    &mut a_indices_1_delta.to_rel_index_write(&mut __a_ind_common_delta),
                    &mut a_indices_1_total.to_rel_index_write(&mut __a_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __b_ind_common_new,
                    &mut __b_ind_common_delta,
                    &mut __b_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut b_indices_0_new.to_rel_index_write(&mut __b_ind_common_new),
                    &mut b_indices_0_delta.to_rel_index_write(&mut __b_ind_common_delta),
                    &mut b_indices_0_total.to_rel_index_write(&mut __b_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut b_indices_0_1_new.to_rel_index_write(&mut __b_ind_common_new),
                    &mut b_indices_0_1_delta
                        .to_rel_index_write(&mut __b_ind_common_delta),
                    &mut b_indices_0_1_total
                        .to_rel_index_write(&mut __b_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __c_ind_common_new,
                    &mut __c_ind_common_delta,
                    &mut __c_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut c_indices_0_new.to_rel_index_write(&mut __c_ind_common_new),
                    &mut c_indices_0_delta.to_rel_index_write(&mut __c_ind_common_delta),
                    &mut c_indices_0_total.to_rel_index_write(&mut __c_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut c_indices_0_1_new.to_rel_index_write(&mut __c_ind_common_new),
                    &mut c_indices_0_1_delta
                        .to_rel_index_write(&mut __c_ind_common_delta),
                    &mut c_indices_0_1_total
                        .to_rel_index_write(&mut __c_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
                if !__changed {
                    break;
                }
            }
            _self.__a_ind_common = __a_ind_common_total;
            _self.a_indices_0_1 = a_indices_0_1_total;
            _self.a_indices_1 = a_indices_1_total;
            _self.__b_ind_common = __b_ind_common_total;
            _self.b_indices_0 = b_indices_0_total;
            _self.b_indices_0_1 = b_indices_0_1_total;
            _self.__c_ind_common = __c_ind_common_total;
            _self.c_indices_0 = c_indices_0_total;
            _self.c_indices_0_1 = c_indices_0_1_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.a.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.a_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__a_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = (tuple.1.clone(),);
            let rel_ind = &mut self.a_indices_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__a_ind_common),
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.b.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.b_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__b_ind_common),
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.b_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__b_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.c.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.c_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__c_ind_common),
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.c_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__c_ind_common),
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
        "scc 0, is_looping: true:\n  a, b, c <-- a_indices_1_delta, b_indices_0_total+delta, c_indices_0_total+delta [SIMPLE JOIN]\n  a, b, c <-- a_indices_1_total, b_indices_0_delta, c_indices_0_total+delta [SIMPLE JOIN]\n  a, b, c <-- a_indices_1_total, b_indices_0_total, c_indices_0_delta [SIMPLE JOIN]\n  dynamic relations: a, b, c\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "a", self.a.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "b", self.b.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "c", self.c.len()))
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
            a: Default::default(),
            __a_ind_common: Default::default(),
            a_indices_0_1: Default::default(),
            a_indices_1: Default::default(),
            b: Default::default(),
            __b_ind_common: Default::default(),
            b_indices_0: Default::default(),
            b_indices_0_1: Default::default(),
            c: Default::default(),
            __c_ind_common: Default::default(),
            c_indices_0: Default::default(),
            c_indices_0_1: Default::default(),
            scc_times: [std::time::Duration::ZERO; 1usize],
            scc_iters: [0; 1usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
