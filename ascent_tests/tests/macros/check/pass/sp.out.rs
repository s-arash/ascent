use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent::Dual;
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
physical indices:
 edge_indices_0_1_2; edge_indices_1; edge_indices_none*/
    pub edge: ::std::vec::Vec<(i32, i32, u32)>,
    pub edge_indices_0_1_2: ascent::internal::RelFullIndexType<(i32, i32, u32), ()>,
    pub edge_indices_1: ascent::internal::RelIndexType1<(i32,), (i32, u32)>,
    pub edge_indices_none: ascent::internal::RelIndexType1<(), (i32, i32, u32)>,
    /**
physical indices:
 shortest_path_indices_0; shortest_path_indices_0_1; shortest_path_indices_0_1_2*/
    pub shortest_path: ::std::vec::Vec<(i32, i32, Dual<u32>)>,
    pub shortest_path_indices_0: ascent::internal::LatticeIndexType<(i32,), usize>,
    pub shortest_path_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), usize>,
    pub shortest_path_indices_0_1_2: ascent::internal::RelFullIndexType<
        (i32, i32, Dual<u32>),
        usize,
    >,
    pub scc0_duration: std::time::Duration,
    pub scc1_duration: std::time::Duration,
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
            let shortest_path_indices_0_delta: &mut ascent::internal::LatticeIndexType<
                (i32,),
                usize,
            > = &mut _self.shortest_path_indices_0;
            let mut shortest_path_indices_0_total: ascent::internal::LatticeIndexType<
                (i32,),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_new: ascent::internal::LatticeIndexType<
                (i32,),
                usize,
            > = Default::default();
            let shortest_path_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                usize,
            > = &mut _self.shortest_path_indices_0_1;
            let mut shortest_path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                usize,
            > = Default::default();
            let shortest_path_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32, Dual<u32>),
                usize,
            > = &mut _self.shortest_path_indices_0_1_2;
            let mut shortest_path_indices_0_1_2_total: ascent::internal::RelFullIndexType<
                (i32, i32, Dual<u32>),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_2_new: ascent::internal::RelFullIndexType<
                (i32, i32, Dual<u32>),
                usize,
            > = Default::default();
            let edge_indices_none_total: &mut ascent::internal::RelIndexType1<
                (),
                (i32, i32, u32),
            > = &mut _self.edge_indices_none;
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("shortest_path <-- edge_indices_none_total");
                {
                    if let Some(__matching) = edge_indices_none_total.index_get(&()) {
                        __matching
                            .for_each(|__val| {
                                let x = &__val.0;
                                let y = &__val.1;
                                let len = &__val.2;
                                let __new_row: (i32, i32, Dual<u32>) = (
                                    ascent::internal::Convert::convert(x),
                                    ascent::internal::Convert::convert(y),
                                    Dual(*len),
                                );
                                let __lattice_key = (
                                    __new_row.0.clone(),
                                    __new_row.1.clone(),
                                );
                                if let Some(mut __existing_ind) = shortest_path_indices_0_1_new
                                    .index_get(&__lattice_key)
                                    .or_else(|| {
                                        shortest_path_indices_0_1_delta.index_get(&__lattice_key)
                                    })
                                    .or_else(|| {
                                        shortest_path_indices_0_1_total.index_get(&__lattice_key)
                                    })
                                {
                                    let __existing_ind = *__existing_ind.next().unwrap();
                                    let __lat_changed = ::ascent::Lattice::join_mut(
                                        &mut _self.shortest_path[__existing_ind].2,
                                        __new_row.2.clone(),
                                    );
                                    if __lat_changed {
                                        let __new_row_ind = __existing_ind;
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut shortest_path_indices_0_new,
                                            (__new_row.0.clone(),),
                                            __new_row_ind,
                                        );
                                        ::ascent::internal::RelIndexWrite::index_insert(
                                            &mut shortest_path_indices_0_1_new,
                                            (__new_row.0.clone(), __new_row.1.clone()),
                                            __new_row_ind,
                                        );
                                        __changed = true;
                                    }
                                } else {
                                    let __new_row_ind = _self.shortest_path.len();
                                    ::ascent::internal::RelIndexWrite::index_insert(
                                        &mut shortest_path_indices_0_new,
                                        (__new_row.0.clone(),),
                                        __new_row_ind,
                                    );
                                    ::ascent::internal::RelIndexWrite::index_insert(
                                        &mut shortest_path_indices_0_1_new,
                                        (__new_row.0.clone(), __new_row.1.clone()),
                                        __new_row_ind,
                                    );
                                    _self
                                        .shortest_path
                                        .push((
                                            __new_row.0.clone(),
                                            __new_row.1.clone(),
                                            __new_row.2,
                                        ));
                                    __changed = true;
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_delta,
                    &mut shortest_path_indices_0_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_new,
                    shortest_path_indices_0_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_1_delta,
                    &mut shortest_path_indices_0_1_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_1_new,
                    shortest_path_indices_0_1_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_1_2_delta,
                    &mut shortest_path_indices_0_1_2_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_1_2_new,
                    shortest_path_indices_0_1_2_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_delta,
                    &mut shortest_path_indices_0_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_new,
                    shortest_path_indices_0_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_1_delta,
                    &mut shortest_path_indices_0_1_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_1_new,
                    shortest_path_indices_0_1_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_1_2_delta,
                    &mut shortest_path_indices_0_1_2_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_1_2_new,
                    shortest_path_indices_0_1_2_delta,
                );
            }
            _self.shortest_path_indices_0 = shortest_path_indices_0_total;
            _self.shortest_path_indices_0_1 = shortest_path_indices_0_1_total;
            _self.shortest_path_indices_0_1_2 = shortest_path_indices_0_1_2_total;
            _self.scc0_duration += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let shortest_path_indices_0_delta: &mut ascent::internal::LatticeIndexType<
                (i32,),
                usize,
            > = &mut _self.shortest_path_indices_0;
            let mut shortest_path_indices_0_total: ascent::internal::LatticeIndexType<
                (i32,),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_new: ascent::internal::LatticeIndexType<
                (i32,),
                usize,
            > = Default::default();
            let shortest_path_indices_0_1_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32),
                usize,
            > = &mut _self.shortest_path_indices_0_1;
            let mut shortest_path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (i32, i32),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_new: ascent::internal::RelFullIndexType<
                (i32, i32),
                usize,
            > = Default::default();
            let shortest_path_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32, Dual<u32>),
                usize,
            > = &mut _self.shortest_path_indices_0_1_2;
            let mut shortest_path_indices_0_1_2_total: ascent::internal::RelFullIndexType<
                (i32, i32, Dual<u32>),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_2_new: ascent::internal::RelFullIndexType<
                (i32, i32, Dual<u32>),
                usize,
            > = Default::default();
            let edge_indices_1_total: &mut ascent::internal::RelIndexType1<
                (i32,),
                (i32, u32),
            > = &mut _self.edge_indices_1;
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "shortest_path <-- edge_indices_1_total, shortest_path_indices_0_delta [SIMPLE JOIN]",
                );
                {
                    if edge_indices_1_total.len() <= shortest_path_indices_0_delta.len()
                    {
                        edge_indices_1_total
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = shortest_path_indices_0_delta
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let x = &cl1_val.0;
                                            let len = &cl1_val.1;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let __row = &_self.shortest_path[*__val].clone();
                                                    let z = &__row.1;
                                                    let __arg_pattern_ = &__row.2;
                                                    if let Dual(plen) = __arg_pattern_ {
                                                        let __new_row: (i32, i32, Dual<u32>) = (
                                                            ascent::internal::Convert::convert(x),
                                                            ascent::internal::Convert::convert(z),
                                                            Dual(len + plen),
                                                        );
                                                        let __lattice_key = (
                                                            __new_row.0.clone(),
                                                            __new_row.1.clone(),
                                                        );
                                                        if let Some(mut __existing_ind) = shortest_path_indices_0_1_new
                                                            .index_get(&__lattice_key)
                                                            .or_else(|| {
                                                                shortest_path_indices_0_1_delta.index_get(&__lattice_key)
                                                            })
                                                            .or_else(|| {
                                                                shortest_path_indices_0_1_total.index_get(&__lattice_key)
                                                            })
                                                        {
                                                            let __existing_ind = *__existing_ind.next().unwrap();
                                                            let __lat_changed = ::ascent::Lattice::join_mut(
                                                                &mut _self.shortest_path[__existing_ind].2,
                                                                __new_row.2.clone(),
                                                            );
                                                            if __lat_changed {
                                                                let __new_row_ind = __existing_ind;
                                                                ::ascent::internal::RelIndexWrite::index_insert(
                                                                    &mut shortest_path_indices_0_new,
                                                                    (__new_row.0.clone(),),
                                                                    __new_row_ind,
                                                                );
                                                                ::ascent::internal::RelIndexWrite::index_insert(
                                                                    &mut shortest_path_indices_0_1_new,
                                                                    (__new_row.0.clone(), __new_row.1.clone()),
                                                                    __new_row_ind,
                                                                );
                                                                __changed = true;
                                                            }
                                                        } else {
                                                            let __new_row_ind = _self.shortest_path.len();
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut shortest_path_indices_0_new,
                                                                (__new_row.0.clone(),),
                                                                __new_row_ind,
                                                            );
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut shortest_path_indices_0_1_new,
                                                                (__new_row.0.clone(), __new_row.1.clone()),
                                                                __new_row_ind,
                                                            );
                                                            _self
                                                                .shortest_path
                                                                .push((
                                                                    __new_row.0.clone(),
                                                                    __new_row.1.clone(),
                                                                    __new_row.2,
                                                                ));
                                                            __changed = true;
                                                        }
                                                    }
                                                });
                                        });
                                }
                            });
                    } else {
                        shortest_path_indices_0_delta
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let y = &__cl1_joined_columns.0;
                                if let Some(__matching) = edge_indices_1_total
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let __row = &_self.shortest_path[*cl1_val].clone();
                                            let z = &__row.1;
                                            let __arg_pattern_ = &__row.2;
                                            if let Dual(plen) = __arg_pattern_ {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let x = &__val.0;
                                                        let len = &__val.1;
                                                        let __new_row: (i32, i32, Dual<u32>) = (
                                                            ascent::internal::Convert::convert(x),
                                                            ascent::internal::Convert::convert(z),
                                                            Dual(len + plen),
                                                        );
                                                        let __lattice_key = (
                                                            __new_row.0.clone(),
                                                            __new_row.1.clone(),
                                                        );
                                                        if let Some(mut __existing_ind) = shortest_path_indices_0_1_new
                                                            .index_get(&__lattice_key)
                                                            .or_else(|| {
                                                                shortest_path_indices_0_1_delta.index_get(&__lattice_key)
                                                            })
                                                            .or_else(|| {
                                                                shortest_path_indices_0_1_total.index_get(&__lattice_key)
                                                            })
                                                        {
                                                            let __existing_ind = *__existing_ind.next().unwrap();
                                                            let __lat_changed = ::ascent::Lattice::join_mut(
                                                                &mut _self.shortest_path[__existing_ind].2,
                                                                __new_row.2.clone(),
                                                            );
                                                            if __lat_changed {
                                                                let __new_row_ind = __existing_ind;
                                                                ::ascent::internal::RelIndexWrite::index_insert(
                                                                    &mut shortest_path_indices_0_new,
                                                                    (__new_row.0.clone(),),
                                                                    __new_row_ind,
                                                                );
                                                                ::ascent::internal::RelIndexWrite::index_insert(
                                                                    &mut shortest_path_indices_0_1_new,
                                                                    (__new_row.0.clone(), __new_row.1.clone()),
                                                                    __new_row_ind,
                                                                );
                                                                __changed = true;
                                                            }
                                                        } else {
                                                            let __new_row_ind = _self.shortest_path.len();
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut shortest_path_indices_0_new,
                                                                (__new_row.0.clone(),),
                                                                __new_row_ind,
                                                            );
                                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                                &mut shortest_path_indices_0_1_new,
                                                                (__new_row.0.clone(), __new_row.1.clone()),
                                                                __new_row_ind,
                                                            );
                                                            _self
                                                                .shortest_path
                                                                .push((
                                                                    __new_row.0.clone(),
                                                                    __new_row.1.clone(),
                                                                    __new_row.2,
                                                                ));
                                                            __changed = true;
                                                        }
                                                    });
                                            }
                                        });
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_delta,
                    &mut shortest_path_indices_0_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_new,
                    shortest_path_indices_0_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_1_delta,
                    &mut shortest_path_indices_0_1_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_1_new,
                    shortest_path_indices_0_1_delta,
                );
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    shortest_path_indices_0_1_2_delta,
                    &mut shortest_path_indices_0_1_2_total,
                );
                std::mem::swap(
                    &mut shortest_path_indices_0_1_2_new,
                    shortest_path_indices_0_1_2_delta,
                );
                if !__changed {
                    break;
                }
            }
            _self.shortest_path_indices_0 = shortest_path_indices_0_total;
            _self.shortest_path_indices_0_1 = shortest_path_indices_0_1_total;
            _self.shortest_path_indices_0_1_2 = shortest_path_indices_0_1_2_total;
            _self.scc1_duration += _scc_start_time.elapsed();
        }
    }
    fn update_indices_priv(&mut self) {
        for (_i, tuple) in self.edge.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            let rel_ind = &mut self.edge_indices_0_1_2;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
            let selection_tuple = (tuple.1.clone(),);
            let rel_ind = &mut self.edge_indices_1;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(), tuple.2.clone()),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.edge_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone(), tuple.2.clone()),
            );
        }
        for (_i, tuple) in self.shortest_path.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.shortest_path_indices_0;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, _i);
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.shortest_path_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, _i);
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            let rel_ind = &mut self.shortest_path_indices_0_1_2;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, _i);
        }
    }
    #[deprecated = "Explicit call to update_indices not required anymore."]
    pub fn update_indices(&mut self) {
        self.update_indices_priv();
    }
    fn type_constraints() {
        let _type_constraints: ascent::internal::TypeConstraints<i32>;
        let _type_constraints: ascent::internal::TypeConstraints<u32>;
        let _type_constraints: ascent::internal::LatTypeConstraints<Dual<u32>>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  shortest_path <-- edge_indices_none_total\n  dynamic relations: shortest_path\nscc 1, is_looping: true:\n  shortest_path <-- edge_indices_1_total, shortest_path_indices_0_delta [SIMPLE JOIN]\n  dynamic relations: shortest_path\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "edge", self.edge.len()))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!(
                    "{0} size: {1}\n", "shortest_path", self.shortest_path.len(),
                ),
            )
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            edge: Default::default(),
            edge_indices_0_1_2: Default::default(),
            edge_indices_1: Default::default(),
            edge_indices_none: Default::default(),
            shortest_path: Default::default(),
            shortest_path_indices_0: Default::default(),
            shortest_path_indices_0_1: Default::default(),
            shortest_path_indices_0_1_2: Default::default(),
            scc0_duration: std::time::Duration::ZERO,
            scc1_duration: std::time::Duration::ZERO,
            update_time_nanos: Default::default(),
        };
        _self
    }
}
fn main() {}
