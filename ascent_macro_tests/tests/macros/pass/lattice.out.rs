//! Aggregate `agg` clause
use ascent::{ascent, Dual};
pub type Node = &'static str;
pub struct AscentProgram {
    /**
logical indices: edge_indices_0_1_2; edge_indices_1; edge_indices_none*/
    pub edge: ::std::vec::Vec<(Node, Node, u32)>,
    pub __edge_ind_common: (),
    pub edge_indices_0_1_2: ascent::internal::RelFullIndexType<(Node, Node, u32), ()>,
    pub edge_indices_1: ascent::rel::ToRelIndexType<(Node,), (Node, u32)>,
    pub edge_indices_none: ascent::rel::ToRelIndexType<(), (Node, Node, u32)>,
    /**
logical indices: shortest_path_indices_0; shortest_path_indices_0_1; shortest_path_indices_0_1_2*/
    pub shortest_path: ::std::vec::Vec<(Node, Node, Dual<u32>)>,
    pub __shortest_path_ind_common: (),
    pub shortest_path_indices_0: ascent::internal::LatticeIndexType<(Node,), usize>,
    pub shortest_path_indices_0_1: ascent::internal::RelFullIndexType<
        (Node, Node),
        usize,
    >,
    pub shortest_path_indices_0_1_2: ascent::internal::LatticeIndexType<
        (Node, Node, Dual<u32>),
        usize,
    >,
    scc_times: [std::time::Duration; 2usize],
    scc_iters: [usize; 2usize],
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
            let mut __shortest_path_ind_common_delta: () = ::std::mem::take(
                &mut _self.__shortest_path_ind_common,
            );
            let mut __shortest_path_ind_common_total: () = Default::default();
            let mut __shortest_path_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __shortest_path_ind_common_new,
                &mut __shortest_path_ind_common_delta,
                &mut __shortest_path_ind_common_total,
            );
            let mut shortest_path_indices_0_delta: ascent::internal::LatticeIndexType<
                (Node,),
                usize,
            > = ::std::mem::take(&mut _self.shortest_path_indices_0);
            let mut shortest_path_indices_0_total: ascent::internal::LatticeIndexType<
                (Node,),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_new: ascent::internal::LatticeIndexType<
                (Node,),
                usize,
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut shortest_path_indices_0_new,
                &mut shortest_path_indices_0_delta,
                &mut shortest_path_indices_0_total,
            );
            let mut shortest_path_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (Node, Node),
                usize,
            > = ::std::mem::take(&mut _self.shortest_path_indices_0_1);
            let mut shortest_path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (Node, Node),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_new: ascent::internal::RelFullIndexType<
                (Node, Node),
                usize,
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut shortest_path_indices_0_1_new,
                &mut shortest_path_indices_0_1_delta,
                &mut shortest_path_indices_0_1_total,
            );
            let mut shortest_path_indices_0_1_2_delta: ascent::internal::LatticeIndexType<
                (Node, Node, Dual<u32>),
                usize,
            > = ::std::mem::take(&mut _self.shortest_path_indices_0_1_2);
            let mut shortest_path_indices_0_1_2_total: ascent::internal::LatticeIndexType<
                (Node, Node, Dual<u32>),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_2_new: ascent::internal::LatticeIndexType<
                (Node, Node, Dual<u32>),
                usize,
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut shortest_path_indices_0_1_2_new,
                &mut shortest_path_indices_0_1_2_delta,
                &mut shortest_path_indices_0_1_2_total,
            );
            let __edge_ind_common_total: () = std::mem::take(
                &mut _self.__edge_ind_common,
            );
            let edge_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (Node, Node, u32),
            > = std::mem::take(&mut _self.edge_indices_none);
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("shortest_path <-- edge_indices_none_total");
                {
                    if let Some(__matching) = edge_indices_none_total
                        .to_rel_index(&__edge_ind_common_total)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let x: &Node = __val.0;
                                let y: &Node = __val.1;
                                let w: &u32 = __val.2;
                                let __new_row: (Node, Node, Dual<u32>) = (
                                    ascent::internal::Convert::convert(x),
                                    ascent::internal::Convert::convert(y),
                                    Dual(*w),
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
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __shortest_path_ind_common_new,
                    &mut __shortest_path_ind_common_delta,
                    &mut __shortest_path_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_new,
                    &mut shortest_path_indices_0_delta,
                    &mut shortest_path_indices_0_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_1_new,
                    &mut shortest_path_indices_0_1_delta,
                    &mut shortest_path_indices_0_1_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_1_2_new,
                    &mut shortest_path_indices_0_1_2_delta,
                    &mut shortest_path_indices_0_1_2_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __shortest_path_ind_common_new,
                    &mut __shortest_path_ind_common_delta,
                    &mut __shortest_path_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_new,
                    &mut shortest_path_indices_0_delta,
                    &mut shortest_path_indices_0_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_1_new,
                    &mut shortest_path_indices_0_1_delta,
                    &mut shortest_path_indices_0_1_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_1_2_new,
                    &mut shortest_path_indices_0_1_2_delta,
                    &mut shortest_path_indices_0_1_2_total,
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__shortest_path_ind_common = __shortest_path_ind_common_total;
            _self.shortest_path_indices_0 = shortest_path_indices_0_total;
            _self.shortest_path_indices_0_1 = shortest_path_indices_0_1_total;
            _self.shortest_path_indices_0_1_2 = shortest_path_indices_0_1_2_total;
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_none = edge_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __shortest_path_ind_common_delta: () = ::std::mem::take(
                &mut _self.__shortest_path_ind_common,
            );
            let mut __shortest_path_ind_common_total: () = Default::default();
            let mut __shortest_path_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __shortest_path_ind_common_new,
                &mut __shortest_path_ind_common_delta,
                &mut __shortest_path_ind_common_total,
            );
            let mut shortest_path_indices_0_delta: ascent::internal::LatticeIndexType<
                (Node,),
                usize,
            > = ::std::mem::take(&mut _self.shortest_path_indices_0);
            let mut shortest_path_indices_0_total: ascent::internal::LatticeIndexType<
                (Node,),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_new: ascent::internal::LatticeIndexType<
                (Node,),
                usize,
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut shortest_path_indices_0_new,
                &mut shortest_path_indices_0_delta,
                &mut shortest_path_indices_0_total,
            );
            let mut shortest_path_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (Node, Node),
                usize,
            > = ::std::mem::take(&mut _self.shortest_path_indices_0_1);
            let mut shortest_path_indices_0_1_total: ascent::internal::RelFullIndexType<
                (Node, Node),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_new: ascent::internal::RelFullIndexType<
                (Node, Node),
                usize,
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut shortest_path_indices_0_1_new,
                &mut shortest_path_indices_0_1_delta,
                &mut shortest_path_indices_0_1_total,
            );
            let mut shortest_path_indices_0_1_2_delta: ascent::internal::LatticeIndexType<
                (Node, Node, Dual<u32>),
                usize,
            > = ::std::mem::take(&mut _self.shortest_path_indices_0_1_2);
            let mut shortest_path_indices_0_1_2_total: ascent::internal::LatticeIndexType<
                (Node, Node, Dual<u32>),
                usize,
            > = Default::default();
            let mut shortest_path_indices_0_1_2_new: ascent::internal::LatticeIndexType<
                (Node, Node, Dual<u32>),
                usize,
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut shortest_path_indices_0_1_2_new,
                &mut shortest_path_indices_0_1_2_delta,
                &mut shortest_path_indices_0_1_2_total,
            );
            let __edge_ind_common_total: () = std::mem::take(
                &mut _self.__edge_ind_common,
            );
            let edge_indices_1_total: ascent::rel::ToRelIndexType<
                (Node,),
                (Node, u32),
            > = std::mem::take(&mut _self.edge_indices_1);
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "shortest_path <-- edge_indices_1_total, shortest_path_indices_0_delta [SIMPLE JOIN]",
                );
                {
                    if edge_indices_1_total.to_rel_index(&__edge_ind_common_total).len()
                        <= (&shortest_path_indices_0_delta).len()
                    {
                        edge_indices_1_total
                            .to_rel_index(&__edge_ind_common_total)
                            .iter_all()
                            .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                                let __cl1_joined_columns = __cl1_joined_columns
                                    .tuple_of_borrowed();
                                let y = __cl1_joined_columns.0;
                                if let Some(__matching) = (&shortest_path_indices_0_delta)
                                    .index_get(&(y.clone(),))
                                {
                                    __cl1_tuple_indices
                                        .for_each(|cl1_val| {
                                            let cl1_val = cl1_val.tuple_of_borrowed();
                                            let x: &Node = cl1_val.0;
                                            let w: &u32 = cl1_val.1;
                                            __matching
                                                .clone()
                                                .for_each(|__val| {
                                                    let __row = &_self.shortest_path[*__val].clone();
                                                    let z: &Node = &__row.1;
                                                    let __arg_pattern_: &Dual<u32> = &__row.2;
                                                    if let Dual(l) = __arg_pattern_ {
                                                        let __new_row: (Node, Node, Dual<u32>) = (
                                                            ascent::internal::Convert::convert(x),
                                                            ascent::internal::Convert::convert(z),
                                                            Dual(w + l),
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
                        (&shortest_path_indices_0_delta)
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
                                            let __row = &_self.shortest_path[*cl1_val].clone();
                                            let z: &Node = &__row.1;
                                            let __arg_pattern_: &Dual<u32> = &__row.2;
                                            if let Dual(l) = __arg_pattern_ {
                                                __matching
                                                    .clone()
                                                    .for_each(|__val| {
                                                        let __val = __val.tuple_of_borrowed();
                                                        let x: &Node = __val.0;
                                                        let w: &u32 = __val.1;
                                                        let __new_row: (Node, Node, Dual<u32>) = (
                                                            ascent::internal::Convert::convert(x),
                                                            ascent::internal::Convert::convert(z),
                                                            Dual(w + l),
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
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __shortest_path_ind_common_new,
                    &mut __shortest_path_ind_common_delta,
                    &mut __shortest_path_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_new,
                    &mut shortest_path_indices_0_delta,
                    &mut shortest_path_indices_0_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_1_new,
                    &mut shortest_path_indices_0_1_delta,
                    &mut shortest_path_indices_0_1_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut shortest_path_indices_0_1_2_new,
                    &mut shortest_path_indices_0_1_2_delta,
                    &mut shortest_path_indices_0_1_2_total,
                );
                _self.scc_iters[1usize] += 1;
                if !__changed {
                    break;
                }
            }
            _self.__shortest_path_ind_common = __shortest_path_ind_common_total;
            _self.shortest_path_indices_0 = shortest_path_indices_0_total;
            _self.shortest_path_indices_0_1 = shortest_path_indices_0_1_total;
            _self.shortest_path_indices_0_1_2 = shortest_path_indices_0_1_2_total;
            _self.__edge_ind_common = __edge_ind_common_total;
            _self.edge_indices_1 = edge_indices_1_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.edge.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            let rel_ind = &mut self.edge_indices_0_1_2;
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
                (tuple.0.clone(), tuple.2.clone()),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.edge_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__edge_ind_common),
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
        self.update_indices_duration += before.elapsed();
    }
    #[deprecated = "Explicit call to update_indices not required anymore."]
    pub fn update_indices(&mut self) {
        self.update_indices_priv();
    }
    fn type_constraints() {
        let _type_constraints: ascent::internal::TypeConstraints<Node>;
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            edge: Default::default(),
            __edge_ind_common: Default::default(),
            edge_indices_0_1_2: Default::default(),
            edge_indices_1: Default::default(),
            edge_indices_none: Default::default(),
            shortest_path: Default::default(),
            __shortest_path_ind_common: Default::default(),
            shortest_path_indices_0: Default::default(),
            shortest_path_indices_0_1: Default::default(),
            shortest_path_indices_0_1_2: Default::default(),
            scc_times: [std::time::Duration::ZERO; 2usize],
            scc_iters: [0; 2usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
