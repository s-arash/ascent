use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
physical indices:
 edge_indices_0_1; edge_indices_1; edge_indices_none*/
    pub edge: ::std::vec::Vec<(i32, i32)>,
    pub edge_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub edge_indices_1: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    pub edge_indices_none: ascent::internal::RelIndexType1<(), (i32, i32)>,
    /**
physical indices:
 path_indices_0; path_indices_0_1*/
    pub path: ::std::vec::Vec<(i32, i32)>,
    pub path_indices_0: ascent::internal::RelIndexType1<(i32,), (i32,)>,
    pub path_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub scc0_duration: std::time::Duration,
    pub scc1_duration: std::time::Duration,
    pub scc2_duration: std::time::Duration,
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
            let edge_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.edge_indices_0_1;
            let mut edge_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut edge_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let edge_indices_1_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.edge_indices_1;
            let mut edge_indices_1_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut edge_indices_1_new: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let edge_indices_none_delta: &mut ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = &mut _self.edge_indices_none;
            let mut edge_indices_none_total: ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = Default::default();
            let mut edge_indices_none_new: ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = Default::default();
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
                            &edge_indices_0_1_total,
                            &__new_row,
                        )
                            && !::ascent::internal::RelFullIndexRead::contains_key(
                                edge_indices_0_1_delta,
                                &__new_row,
                            )
                        {
                            if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                &mut edge_indices_0_1_new,
                                &__new_row,
                                (),
                            ) {
                                let __new_row_ind = _self.edge.len();
                                _self.edge.push((__new_row.0.clone(), __new_row.1.clone()));
                                ::ascent::internal::RelIndexWrite::index_insert(
                                    &mut edge_indices_1_new,
                                    (__new_row.1.clone(),),
                                    (__new_row.0.clone(),),
                                );
                                ::ascent::internal::RelIndexWrite::index_insert(
                                    &mut edge_indices_none_new,
                                    (),
                                    (__new_row.0.clone(), __new_row.1.clone()),
                                );
                                __changed = true;
                            }
                        }
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    edge_indices_0_1_delta,
                    &mut edge_indices_0_1_total,
                );
                std::mem::swap(&mut edge_indices_0_1_new, edge_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    edge_indices_1_delta,
                    &mut edge_indices_1_total,
                );
                std::mem::swap(&mut edge_indices_1_new, edge_indices_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    edge_indices_none_delta,
                    &mut edge_indices_none_total,
                );
                std::mem::swap(&mut edge_indices_none_new, edge_indices_none_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    edge_indices_0_1_delta,
                    &mut edge_indices_0_1_total,
                );
                std::mem::swap(&mut edge_indices_0_1_new, edge_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    edge_indices_1_delta,
                    &mut edge_indices_1_total,
                );
                std::mem::swap(&mut edge_indices_1_new, edge_indices_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    edge_indices_none_delta,
                    &mut edge_indices_none_total,
                );
                std::mem::swap(&mut edge_indices_none_new, edge_indices_none_delta);
            }
            _self.edge_indices_0_1 = edge_indices_0_1_total;
            _self.edge_indices_1 = edge_indices_1_total;
            _self.edge_indices_none = edge_indices_none_total;
            _self.scc0_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let path_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.path_indices_0;
            let mut path_indices_0_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut path_indices_0_new: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let path_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.path_indices_0_1;
            let mut path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let edge_indices_none_total: &mut ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = &mut _self.edge_indices_none;
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("path <-- edge_indices_none_total");
                {
                    if let Some(__matching) = edge_indices_none_total.index_get(&()) {
                        __matching
                            .for_each(|__val| {
                                let x = &__val.0;
                                let y = &__val.1;
                                let __new_row: (i32, i32) = (*x, *y);
                                if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &path_indices_0_1_total,
                                    &__new_row,
                                )
                                    && !::ascent::internal::RelFullIndexRead::contains_key(
                                        path_indices_0_1_delta,
                                        &__new_row,
                                    )
                                {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                        &mut path_indices_0_1_new,
                                        &__new_row,
                                        (),
                                    ) {
                                        let __new_row_ind = _self.path.len();
                                        _self.path.push((__new_row.0.clone(), __new_row.1.clone()));
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut path_indices_0_new,
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
                    path_indices_0_delta,
                    &mut path_indices_0_total,
                );
                std::mem::swap(&mut path_indices_0_new, path_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_1_delta,
                    &mut path_indices_0_1_total,
                );
                std::mem::swap(&mut path_indices_0_1_new, path_indices_0_1_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_delta,
                    &mut path_indices_0_total,
                );
                std::mem::swap(&mut path_indices_0_new, path_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_1_delta,
                    &mut path_indices_0_1_total,
                );
                std::mem::swap(&mut path_indices_0_1_new, path_indices_0_1_delta);
            }
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.scc1_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 2");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let path_indices_0_delta: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.path_indices_0;
            let mut path_indices_0_total: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let mut path_indices_0_new: ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = Default::default();
            let path_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = &mut _self.path_indices_0_1;
            let mut path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let mut path_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                (),
            > = Default::default();
            let edge_indices_1_total: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32,),
            > = &mut _self.edge_indices_1;
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "path <-- edge_indices_1_total, path_indices_0_delta [SIMPLE JOIN]",
                );
                {
                    if edge_indices_1_total.len() <= path_indices_0_delta.len() {
                        edge_indices_1_total
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = path_indices_0_delta
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let z = &__val.0;
                                                    let __new_row: (i32, i32) = (*x, *z);
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total,
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            path_indices_0_1_delta,
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut path_indices_0_1_new,
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.path.len();
                                                            _self.path.push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut path_indices_0_new,
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
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = edge_indices_1_total
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let z = &cl1_val.0;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let x = &__val.0;
                                                    let __new_row: (i32, i32) = (*x, *z);
                                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                                        &path_indices_0_1_total,
                                                        &__new_row,
                                                    )
                                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                                            path_indices_0_1_delta,
                                                            &__new_row,
                                                        )
                                                    {
                                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                                            &mut path_indices_0_1_new,
                                                            &__new_row,
                                                            (),
                                                        ) {
                                                            let __new_row_ind = _self.path.len();
                                                            _self.path.push((__new_row.0.clone(), __new_row.1.clone()));
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut path_indices_0_new,
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
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_delta,
                    &mut path_indices_0_total,
                );
                std::mem::swap(&mut path_indices_0_new, path_indices_0_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    path_indices_0_1_delta,
                    &mut path_indices_0_1_total,
                );
                std::mem::swap(&mut path_indices_0_1_new, path_indices_0_1_delta);
                if !__changed {
                    break;
                }
            }
            _self.path_indices_0 = path_indices_0_total;
            _self.path_indices_0_1 = path_indices_0_1_total;
            _self.scc2_duration += _scc_start_time.elapsed();
        }
    }
    fn update_indices_priv(&mut self) {
        for (_i, tuple) in self.edge.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.edge_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
            let selection_tuple = (tuple.1.clone(),);
            let rel_ind = &mut self.edge_indices_1;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(),),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.edge_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone()),
            );
        }
        for (_i, tuple) in self.path.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.path_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.1.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.path_indices_0_1;
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
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "0", self.scc0_duration))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "1", self.scc1_duration))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("scc {0} time: {1:?}\n", "2", self.scc2_duration))
            .unwrap();
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            edge: Default::default(),
            edge_indices_0_1: Default::default(),
            edge_indices_1: Default::default(),
            edge_indices_none: Default::default(),
            path: Default::default(),
            path_indices_0: Default::default(),
            path_indices_0_1: Default::default(),
            scc0_duration: std::time::Duration::ZERO,
            scc1_duration: std::time::Duration::ZERO,
            scc2_duration: std::time::Duration::ZERO,
            update_time_nanos: Default::default(),
        };
        _self
    }
}
fn main() {}
