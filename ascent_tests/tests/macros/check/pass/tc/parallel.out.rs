use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
physical indices:
 a_indices_0_1; a_indices_1*/
    pub a: ::std::vec::Vec<(i32, i32)>,
    pub a_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub a_indices_1: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    /**
physical indices:
 b_indices_0; b_indices_0_1*/
    pub b: ::std::vec::Vec<(i32, i32)>,
    pub b_indices_0: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    pub b_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    /**
physical indices:
 c_indices_0; c_indices_0_1*/
    pub c: ::std::vec::Vec<(i32, i32)>,
    pub c_indices_0: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    pub c_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub scc0_duration: std::time::Duration,
    pub update_time_nanos: std::sync::atomic::AtomicU64,
}
impl AscentProgram {
    #[allow(unused_imports)]
    ///Runs the Ascent program to a fixed point.
    pub fn run(&mut self) {
        use core::cmp::PartialEq;
        use ascent::internal::RelIndexRead;
        use ascent::internal::RelIndexReadAll;
        self.update_indices_priv();
        let _self = self;
        ascent::internal::comment("scc 0");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let a_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.a_indices_0_1;
            let mut a_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut a_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let a_indices_1_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.a_indices_1;
            let mut a_indices_1_total: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let mut a_indices_1_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let b_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.b_indices_0;
            let mut b_indices_0_total: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let mut b_indices_0_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let b_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.b_indices_0_1;
            let mut b_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut b_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let c_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.c_indices_0;
            let mut c_indices_0_total: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let mut c_indices_0_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let c_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.c_indices_0_1;
            let mut c_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut c_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "a, b, c <-- a_indices_1_delta, b_indices_0_total+delta, c_indices_0_total+delta [SIMPLE JOIN]",
                );
                {
                    if a_indices_1_delta.len()
                        <= ascent::internal::RelIndexCombined::new(
                                &b_indices_0_total,
                                b_indices_0_delta,
                            )
                            .len()
                    {
                        a_indices_1_delta
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                        &b_indices_0_total,
                                        b_indices_0_delta,
                                    )
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let z = &__val.0;
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total,
                                                            c_indices_0_delta,
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let w = &__val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        a_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new,
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
                                                                    &b_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        b_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new,
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
                                                                    &c_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        c_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new,
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
                                &b_indices_0_total,
                                b_indices_0_delta,
                            )
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = a_indices_1_delta
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total,
                                                            c_indices_0_delta,
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let w = &__val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        a_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new,
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
                                                                    &b_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        b_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new,
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
                                                                    &c_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        c_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new,
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
                    if a_indices_1_total.len() <= b_indices_0_delta.len() {
                        a_indices_1_total
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = b_indices_0_delta
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let z = &__val.0;
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total,
                                                            c_indices_0_delta,
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let w = &__val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        a_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new,
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
                                                                    &b_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        b_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new,
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
                                                                    &c_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        c_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new,
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
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = a_indices_1_total
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                                            &c_indices_0_total,
                                                            c_indices_0_delta,
                                                        )
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let w = &__val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        a_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new,
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
                                                                    &b_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        b_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new,
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
                                                                    &c_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        c_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new,
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
                    if a_indices_1_total.len() <= b_indices_0_total.len() {
                        a_indices_1_total
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = b_indices_0_total
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let z = &__val.0;
                                                    if let Some(__matching) = c_indices_0_delta
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let w = &__val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        a_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new,
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
                                                                    &b_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        b_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new,
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
                                                                    &c_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        c_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new,
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
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = a_indices_1_total
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    if let Some(__matching) = c_indices_0_delta
                                                        .index_get(&(z.clone(),))
                                                    {
                                                        __matching
                                                            .for_each(|__val| {
                                                                let w = &__val.0;
                                                                let __new_row: (i32, i32) = (
                                                                    ascent::internal::Convert::convert(y),
                                                                    ascent::internal::Convert::convert(z),
                                                                );
                                                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    &a_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        a_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut a_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.a.len();
                                                                        _self.a.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut a_indices_1_new,
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
                                                                    &b_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        b_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut b_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.b.len();
                                                                        _self.b.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut b_indices_0_new,
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
                                                                    &c_indices_0_1_total,
                                                                    &__new_row,
                                                                )
                                                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                        c_indices_0_1_delta,
                                                                        &__new_row,
                                                                    )
                                                                {
                                                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                        &mut c_indices_0_1_new,
                                                                        &__new_row,
                                                                        (),
                                                                    ) {
                                                                        let __new_row_ind = _self.c.len();
                                                                        _self.c.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                        ::ascent::internal::RelIndexWrite::index_insert(
                                                                            &mut c_indices_0_new,
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
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    a_indices_0_1_delta,
                    &mut a_indices_0_1_total,
                );
                std::mem::swap(&mut a_indices_0_1_new, a_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    a_indices_1_delta,
                    &mut a_indices_1_total,
                );
                std::mem::swap(&mut a_indices_1_new, a_indices_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    b_indices_0_delta,
                    &mut b_indices_0_total,
                );
                std::mem::swap(&mut b_indices_0_new, b_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    b_indices_0_1_delta,
                    &mut b_indices_0_1_total,
                );
                std::mem::swap(&mut b_indices_0_1_new, b_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    c_indices_0_delta,
                    &mut c_indices_0_total,
                );
                std::mem::swap(&mut c_indices_0_new, c_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    c_indices_0_1_delta,
                    &mut c_indices_0_1_total,
                );
                std::mem::swap(&mut c_indices_0_1_new, c_indices_0_1_delta);
                if !__changed {
                    break;
                }
            }
            _self.a_indices_0_1 = a_indices_0_1_total;
            _self.a_indices_1 = a_indices_1_total;
            _self.b_indices_0 = b_indices_0_total;
            _self.b_indices_0_1 = b_indices_0_1_total;
            _self.c_indices_0 = c_indices_0_total;
            _self.c_indices_0_1 = c_indices_0_1_total;
            _self.scc0_duration += _scc_start_time.elapsed();
        }
    }
    fn update_indices_priv(&mut self) {
        for (_i, tuple) in self.a.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.a_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
            let selection_tuple = (tuple.1.clone(),);
            let rel_ind = &mut self.a_indices_1;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(),),
            );
        }
        for (_i, tuple) in self.b.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.b_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.b_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
        }
        for (_i, tuple) in self.c.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.c_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.c_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
        }
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
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "0", self.scc0_duration))
            .unwrap();
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            a: Default::default(),
            a_indices_0_1: Default::default(),
            a_indices_1: Default::default(),
            b: Default::default(),
            b_indices_0: Default::default(),
            b_indices_0_1: Default::default(),
            c: Default::default(),
            c_indices_0: Default::default(),
            c_indices_0_1: Default::default(),
            scc0_duration: std::time::Duration::ZERO,
            update_time_nanos: Default::default(),
        };
        _self
    }
}
fn main() {}
