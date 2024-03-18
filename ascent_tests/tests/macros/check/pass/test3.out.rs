use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
physical indices:
 bar_indices_0; bar_indices_0_1*/
    pub bar: ::std::vec::Vec<(i32, i32)>,
    pub bar_indices_0: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    pub bar_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    /**
physical indices:
 baz_indices_0_1; baz_indices_none*/
    pub baz: ::std::vec::Vec<(i32, i32)>,
    pub baz_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub baz_indices_none: ascent::internal::RelIndexType1<(), (i32, i32)>,
    /**
physical indices:
 foo_indices_0_1; foo_indices_1*/
    pub foo: ::std::vec::Vec<(i32, i32)>,
    pub foo_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub foo_indices_1: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    pub scc0_duration: std::time::Duration,
    pub scc1_duration: std::time::Duration,
    pub scc2_duration: std::time::Duration,
    pub scc3_duration: std::time::Duration,
    pub scc4_duration: std::time::Duration,
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
            let foo_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.foo_indices_0_1;
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let foo_indices_1_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.foo_indices_1;
            let mut foo_indices_1_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut foo_indices_1_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("foo <-- ");
                {
                    let __new_row: (i32, i32) = (1, 2);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &foo_indices_0_1_total,
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            foo_indices_0_1_delta,
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut foo_indices_0_1_new,
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.foo.len();
                            _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut foo_indices_1_new,
                                (__new_row.1.clone(),),
                                (__new_row.0.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_0_1_delta,
                    &mut foo_indices_0_1_total,
                );
                std::mem::swap(&mut foo_indices_0_1_new, foo_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_1_delta,
                    &mut foo_indices_1_total,
                );
                std::mem::swap(&mut foo_indices_1_new, foo_indices_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_0_1_delta,
                    &mut foo_indices_0_1_total,
                );
                std::mem::swap(&mut foo_indices_0_1_new, foo_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_1_delta,
                    &mut foo_indices_1_total,
                );
                std::mem::swap(&mut foo_indices_1_new, foo_indices_1_delta);
            }
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_1 = foo_indices_1_total;
            _self.scc0_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let foo_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.foo_indices_0_1;
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let foo_indices_1_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.foo_indices_1;
            let mut foo_indices_1_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut foo_indices_1_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("foo <-- ");
                {
                    let __new_row: (i32, i32) = (10, 2);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &foo_indices_0_1_total,
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            foo_indices_0_1_delta,
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut foo_indices_0_1_new,
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.foo.len();
                            _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut foo_indices_1_new,
                                (__new_row.1.clone(),),
                                (__new_row.0.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_0_1_delta,
                    &mut foo_indices_0_1_total,
                );
                std::mem::swap(&mut foo_indices_0_1_new, foo_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_1_delta,
                    &mut foo_indices_1_total,
                );
                std::mem::swap(&mut foo_indices_1_new, foo_indices_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_0_1_delta,
                    &mut foo_indices_0_1_total,
                );
                std::mem::swap(&mut foo_indices_0_1_new, foo_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_1_delta,
                    &mut foo_indices_1_total,
                );
                std::mem::swap(&mut foo_indices_1_new, foo_indices_1_delta);
            }
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_1 = foo_indices_1_total;
            _self.scc1_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let bar_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.bar_indices_0;
            let mut bar_indices_0_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut bar_indices_0_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.bar_indices_0_1;
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("bar <-- ");
                {
                    let __new_row: (i32, i32) = (2, 3);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &bar_indices_0_1_total,
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            bar_indices_0_1_delta,
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut bar_indices_0_1_new,
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.bar.len();
                            _self.bar.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut bar_indices_0_new,
                                (__new_row.0.clone(),),
                                (__new_row.1.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_delta,
                    &mut bar_indices_0_total,
                );
                std::mem::swap(&mut bar_indices_0_new, bar_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_1_delta,
                    &mut bar_indices_0_1_total,
                );
                std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_delta,
                    &mut bar_indices_0_total,
                );
                std::mem::swap(&mut bar_indices_0_new, bar_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_1_delta,
                    &mut bar_indices_0_1_total,
                );
                std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
            }
            _self.bar_indices_0 = bar_indices_0_total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.scc2_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 3");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let bar_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.bar_indices_0;
            let mut bar_indices_0_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut bar_indices_0_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.bar_indices_0_1;
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("bar <-- ");
                {
                    let __new_row: (i32, i32) = (2, 1);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &bar_indices_0_1_total,
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            bar_indices_0_1_delta,
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut bar_indices_0_1_new,
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.bar.len();
                            _self.bar.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut bar_indices_0_new,
                                (__new_row.0.clone(),),
                                (__new_row.1.clone(),),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_delta,
                    &mut bar_indices_0_total,
                );
                std::mem::swap(&mut bar_indices_0_new, bar_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_1_delta,
                    &mut bar_indices_0_1_total,
                );
                std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_delta,
                    &mut bar_indices_0_total,
                );
                std::mem::swap(&mut bar_indices_0_new, bar_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_1_delta,
                    &mut bar_indices_0_1_total,
                );
                std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
            }
            _self.bar_indices_0 = bar_indices_0_total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.scc3_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 4");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let bar_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.bar_indices_0;
            let mut bar_indices_0_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut bar_indices_0_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            let bar_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.bar_indices_0_1;
            let mut bar_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut bar_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let baz_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.baz_indices_0_1;
            let mut baz_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut baz_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let baz_indices_none_delta: &mut ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = &mut _self.baz_indices_none;
            let mut baz_indices_none_total: ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = Default::default();
            let mut baz_indices_none_new: ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = Default::default();
            let foo_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.foo_indices_0_1;
            let mut foo_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut foo_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let foo_indices_1_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.foo_indices_1;
            let mut foo_indices_1_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut foo_indices_1_new: ascent::internal::RelIndexType1<(i32,), (i32,)> = Default::default();
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "baz <-- foo_indices_1_delta, bar_indices_0_total+delta, if ⋯ [SIMPLE JOIN]",
                );
                {
                    if foo_indices_1_delta.len()
                        <= ascent::internal::RelIndexCombined::new(
                                &bar_indices_0_total,
                                bar_indices_0_delta,
                            )
                            .len()
                    {
                        foo_indices_1_delta
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                                        &bar_indices_0_total,
                                        bar_indices_0_delta,
                                    )
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            if *x != 10 {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let z = &__val.0;
                                                        if x != z {
                                                            let __new_row: (i32, i32) = (*x, *z);
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &baz_indices_0_1_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    baz_indices_0_1_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                    &mut baz_indices_0_1_new,
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self.baz.len();
                                                                    _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                    ::ascent::internal::RelIndexWrite::index_insert(
                                                                        &mut baz_indices_none_new,
                                                                        (),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed = true;
                                                                }
                                                            }
                                                        }
                                                    });
                                            }
                                        });
                                }
                            });
                    } else {
                        ascent::internal::RelIndexCombined::new(
                                &bar_indices_0_total,
                                bar_indices_0_delta,
                            )
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = foo_indices_1_delta
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    if *x != 10 {
                                                        if x != z {
                                                            let __new_row: (i32, i32) = (*x, *z);
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &baz_indices_0_1_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    baz_indices_0_1_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                    &mut baz_indices_0_1_new,
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self.baz.len();
                                                                    _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                    ::ascent::internal::RelIndexWrite::index_insert(
                                                                        &mut baz_indices_none_new,
                                                                        (),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed = true;
                                                                }
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
                    "baz <-- foo_indices_1_total, bar_indices_0_delta, if ⋯ [SIMPLE JOIN]",
                );
                {
                    if foo_indices_1_total.len() <= bar_indices_0_delta.len() {
                        foo_indices_1_total
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = bar_indices_0_delta
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            if *x != 10 {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let z = &__val.0;
                                                        if x != z {
                                                            let __new_row: (i32, i32) = (*x, *z);
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &baz_indices_0_1_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    baz_indices_0_1_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                    &mut baz_indices_0_1_new,
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self.baz.len();
                                                                    _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                    ::ascent::internal::RelIndexWrite::index_insert(
                                                                        &mut baz_indices_none_new,
                                                                        (),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed = true;
                                                                }
                                                            }
                                                        }
                                                    });
                                            }
                                        });
                                }
                            });
                    } else {
                        bar_indices_0_delta
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = foo_indices_1_total
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    if *x != 10 {
                                                        if x != z {
                                                            let __new_row: (i32, i32) = (*x, *z);
                                                            if !::ascent::internal::RelFullIndexRead::contains_key(
                                                                &baz_indices_0_1_total,
                                                                &__new_row,
                                                            )
                                                                && !::ascent::internal::RelFullIndexRead::contains_key(
                                                                    baz_indices_0_1_delta,
                                                                    &__new_row,
                                                                )
                                                            {
                                                                if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                                    &mut baz_indices_0_1_new,
                                                                    &__new_row,
                                                                    (),
                                                                ) {
                                                                    let __new_row_ind = _self.baz.len();
                                                                    _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                                                    ::ascent::internal::RelIndexWrite::index_insert(
                                                                        &mut baz_indices_none_new,
                                                                        (),
                                                                        (__new_row.0.clone(), __new_row.1.clone()),
                                                                    );
                                                                    __changed = true;
                                                                }
                                                            }
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    }
                }
                ascent::internal::comment("foo, bar <-- baz_indices_none_delta");
                {
                    if let Some(__matching) = baz_indices_none_delta.index_get(&()) {
                        __matching
                            .for_each(|__val| {
                                let x = &__val.0;
                                let y = &__val.1;
                                let __new_row: (i32, i32) = (*x, *y);
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &foo_indices_0_1_total,
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        foo_indices_0_1_delta,
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut foo_indices_0_1_new,
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.foo.len();
                                        _self.foo.push((__new_row.0.clone(), __new_row.1.clone()));
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut foo_indices_1_new,
                                            (__new_row.1.clone(),),
                                            (__new_row.0.clone(),),
                                        );
                                        __changed = true;
                                    }
                                }
                                let __new_row: (i32, i32) = (*x, *y);
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &bar_indices_0_1_total,
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        bar_indices_0_1_delta,
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut bar_indices_0_1_new,
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.bar.len();
                                        _self.bar.push((__new_row.0.clone(), __new_row.1.clone()));
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut bar_indices_0_new,
                                            (__new_row.0.clone(),),
                                            (__new_row.1.clone(),),
                                        );
                                        __changed = true;
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_delta,
                    &mut bar_indices_0_total,
                );
                std::mem::swap(&mut bar_indices_0_new, bar_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    bar_indices_0_1_delta,
                    &mut bar_indices_0_1_total,
                );
                std::mem::swap(&mut bar_indices_0_1_new, bar_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    baz_indices_0_1_delta,
                    &mut baz_indices_0_1_total,
                );
                std::mem::swap(&mut baz_indices_0_1_new, baz_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    baz_indices_none_delta,
                    &mut baz_indices_none_total,
                );
                std::mem::swap(&mut baz_indices_none_new, baz_indices_none_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_0_1_delta,
                    &mut foo_indices_0_1_total,
                );
                std::mem::swap(&mut foo_indices_0_1_new, foo_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    foo_indices_1_delta,
                    &mut foo_indices_1_total,
                );
                std::mem::swap(&mut foo_indices_1_new, foo_indices_1_delta);
                if !__changed {
                    break;
                }
            }
            _self.bar_indices_0 = bar_indices_0_total;
            _self.bar_indices_0_1 = bar_indices_0_1_total;
            _self.baz_indices_0_1 = baz_indices_0_1_total;
            _self.baz_indices_none = baz_indices_none_total;
            _self.foo_indices_0_1 = foo_indices_0_1_total;
            _self.foo_indices_1 = foo_indices_1_total;
            _self.scc4_duration += _scc_start_time.elapsed();
        }
    }
    fn update_indices_priv(&mut self) {
        for (_i, tuple) in self.bar.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.bar_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.bar_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
        }
        for (_i, tuple) in self.baz.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.baz_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
            let selection_tuple = ();
            let rel_ind = &mut self.baz_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone()),
            );
        }
        for (_i, tuple) in self.foo.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.foo_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
            let selection_tuple = (tuple.1.clone(),);
            let rel_ind = &mut self.foo_indices_1;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(),),
            );
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
        "scc 0, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 1, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 2, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 3, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 4, is_looping: true:\n  baz <-- foo_indices_1_delta, bar_indices_0_total+delta, if ⋯ [SIMPLE JOIN]\n  baz <-- foo_indices_1_total, bar_indices_0_delta, if ⋯ [SIMPLE JOIN]\n  foo, bar <-- baz_indices_none_delta\n  dynamic relations: bar, baz, foo\n"
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
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "0", self.scc0_duration))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "1", self.scc1_duration))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "2", self.scc2_duration))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "3", self.scc3_duration))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "4", self.scc4_duration))
            .unwrap();
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            bar: Default::default(),
            bar_indices_0: Default::default(),
            bar_indices_0_1: Default::default(),
            baz: Default::default(),
            baz_indices_0_1: Default::default(),
            baz_indices_none: Default::default(),
            foo: Default::default(),
            foo_indices_0_1: Default::default(),
            foo_indices_1: Default::default(),
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
fn main() {}
