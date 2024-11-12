//! Binding `let` clause
use ascent::ascent;
pub enum List {
    Nil,
    Cons(usize, Box<List>),
}
#[automatically_derived]
impl ::core::clone::Clone for List {
    #[inline]
    fn clone(&self) -> List {
        match self {
            List::Nil => List::Nil,
            List::Cons(__self_0, __self_1) => {
                List::Cons(
                    ::core::clone::Clone::clone(__self_0),
                    ::core::clone::Clone::clone(__self_1),
                )
            }
        }
    }
}
#[automatically_derived]
impl ::core::marker::StructuralEq for List {}
#[automatically_derived]
impl ::core::cmp::Eq for List {
    #[inline]
    #[doc(hidden)]
    #[coverage(off)]
    fn assert_receiver_is_total_eq(&self) -> () {
        let _: ::core::cmp::AssertParamIsEq<usize>;
        let _: ::core::cmp::AssertParamIsEq<Box<List>>;
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for List {}
#[automatically_derived]
impl ::core::cmp::PartialEq for List {
    #[inline]
    fn eq(&self, other: &List) -> bool {
        let __self_tag = ::core::intrinsics::discriminant_value(self);
        let __arg1_tag = ::core::intrinsics::discriminant_value(other);
        __self_tag == __arg1_tag
            && match (self, other) {
                (List::Cons(__self_0, __self_1), List::Cons(__arg1_0, __arg1_1)) => {
                    *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                }
                _ => true,
            }
    }
}
#[automatically_derived]
impl ::core::hash::Hash for List {
    #[inline]
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        let __self_tag = ::core::intrinsics::discriminant_value(self);
        ::core::hash::Hash::hash(&__self_tag, state);
        match self {
            List::Cons(__self_0, __self_1) => {
                ::core::hash::Hash::hash(__self_0, state);
                ::core::hash::Hash::hash(__self_1, state)
            }
            _ => {}
        }
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for List {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            List::Nil => ::core::fmt::Formatter::write_str(f, "Nil"),
            List::Cons(__self_0, __self_1) => {
                ::core::fmt::Formatter::debug_tuple_field2_finish(
                    f,
                    "Cons",
                    __self_0,
                    &__self_1,
                )
            }
        }
    }
}
impl List {
    fn as_vec(&self) -> Vec<usize> {
        let mut items = ::alloc::vec::Vec::new();
        let mut list = self;
        while let Self::Cons(head, tail) = list {
            items.push(*head);
            list = tail;
        }
        items
    }
}
pub struct AscentProgram {
    /**
logical indices: list_indices_0_1; list_indices_none*/
    pub list: ::std::vec::Vec<(List, usize)>,
    pub __list_ind_common: (),
    pub list_indices_0_1: ascent::internal::RelFullIndexType<(List, usize), ()>,
    pub list_indices_none: ascent::rel::ToRelIndexType<(), (List, usize)>,
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
            let mut __list_ind_common_delta: () = ::std::mem::take(
                &mut _self.__list_ind_common,
            );
            let mut __list_ind_common_total: () = Default::default();
            let mut __list_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __list_ind_common_new,
                &mut __list_ind_common_delta,
                &mut __list_ind_common_total,
            );
            let mut list_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (List, usize),
                (),
            > = ::std::mem::take(&mut _self.list_indices_0_1);
            let mut list_indices_0_1_total: ascent::internal::RelFullIndexType<
                (List, usize),
                (),
            > = Default::default();
            let mut list_indices_0_1_new: ascent::internal::RelFullIndexType<
                (List, usize),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut list_indices_0_1_new.to_rel_index_write(&mut __list_ind_common_new),
                &mut list_indices_0_1_delta
                    .to_rel_index_write(&mut __list_ind_common_delta),
                &mut list_indices_0_1_total
                    .to_rel_index_write(&mut __list_ind_common_total),
            );
            let mut list_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (List, usize),
            > = ::std::mem::take(&mut _self.list_indices_none);
            let mut list_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (List, usize),
            > = Default::default();
            let mut list_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (List, usize),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut list_indices_none_new
                    .to_rel_index_write(&mut __list_ind_common_new),
                &mut list_indices_none_delta
                    .to_rel_index_write(&mut __list_ind_common_delta),
                &mut list_indices_none_total
                    .to_rel_index_write(&mut __list_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment("list <-- ");
                {
                    let __new_row: (List, usize) = (List::Nil, 0);
                    if !::ascent::internal::RelFullIndexRead::contains_key(
                        &list_indices_0_1_total.to_rel_index(&__list_ind_common_total),
                        &__new_row,
                    )
                        && !::ascent::internal::RelFullIndexRead::contains_key(
                            &list_indices_0_1_delta
                                .to_rel_index(&__list_ind_common_delta),
                            &__new_row,
                        )
                    {
                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                            &mut list_indices_0_1_new
                                .to_rel_index_write(&mut __list_ind_common_new),
                            &__new_row,
                            (),
                        ) {
                            let __new_row_ind = _self.list.len();
                            _self.list.push((__new_row.0.clone(), __new_row.1.clone()));
                            ::ascent::internal::RelIndexWrite::index_insert(
                                &mut list_indices_none_new
                                    .to_rel_index_write(&mut __list_ind_common_new),
                                (),
                                (__new_row.0.clone(), __new_row.1.clone()),
                            );
                            __changed = true;
                        }
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __list_ind_common_new,
                    &mut __list_ind_common_delta,
                    &mut __list_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut list_indices_0_1_new
                        .to_rel_index_write(&mut __list_ind_common_new),
                    &mut list_indices_0_1_delta
                        .to_rel_index_write(&mut __list_ind_common_delta),
                    &mut list_indices_0_1_total
                        .to_rel_index_write(&mut __list_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut list_indices_none_new
                        .to_rel_index_write(&mut __list_ind_common_new),
                    &mut list_indices_none_delta
                        .to_rel_index_write(&mut __list_ind_common_delta),
                    &mut list_indices_none_total
                        .to_rel_index_write(&mut __list_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __list_ind_common_new,
                    &mut __list_ind_common_delta,
                    &mut __list_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut list_indices_0_1_new
                        .to_rel_index_write(&mut __list_ind_common_new),
                    &mut list_indices_0_1_delta
                        .to_rel_index_write(&mut __list_ind_common_delta),
                    &mut list_indices_0_1_total
                        .to_rel_index_write(&mut __list_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut list_indices_none_new
                        .to_rel_index_write(&mut __list_ind_common_new),
                    &mut list_indices_none_delta
                        .to_rel_index_write(&mut __list_ind_common_delta),
                    &mut list_indices_none_total
                        .to_rel_index_write(&mut __list_ind_common_total),
                );
                _self.scc_iters[0usize] += 1;
            }
            _self.__list_ind_common = __list_ind_common_total;
            _self.list_indices_0_1 = list_indices_0_1_total;
            _self.list_indices_none = list_indices_none_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
        }
        ascent::internal::comment("scc 1");
        {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __list_ind_common_delta: () = ::std::mem::take(
                &mut _self.__list_ind_common,
            );
            let mut __list_ind_common_total: () = Default::default();
            let mut __list_ind_common_new: () = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut __list_ind_common_new,
                &mut __list_ind_common_delta,
                &mut __list_ind_common_total,
            );
            let mut list_indices_0_1_delta: ascent::internal::RelFullIndexType<
                (List, usize),
                (),
            > = ::std::mem::take(&mut _self.list_indices_0_1);
            let mut list_indices_0_1_total: ascent::internal::RelFullIndexType<
                (List, usize),
                (),
            > = Default::default();
            let mut list_indices_0_1_new: ascent::internal::RelFullIndexType<
                (List, usize),
                (),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut list_indices_0_1_new.to_rel_index_write(&mut __list_ind_common_new),
                &mut list_indices_0_1_delta
                    .to_rel_index_write(&mut __list_ind_common_delta),
                &mut list_indices_0_1_total
                    .to_rel_index_write(&mut __list_ind_common_total),
            );
            let mut list_indices_none_delta: ascent::rel::ToRelIndexType<
                (),
                (List, usize),
            > = ::std::mem::take(&mut _self.list_indices_none);
            let mut list_indices_none_total: ascent::rel::ToRelIndexType<
                (),
                (List, usize),
            > = Default::default();
            let mut list_indices_none_new: ascent::rel::ToRelIndexType<
                (),
                (List, usize),
            > = Default::default();
            ::ascent::internal::RelIndexMerge::init(
                &mut list_indices_none_new
                    .to_rel_index_write(&mut __list_ind_common_new),
                &mut list_indices_none_delta
                    .to_rel_index_write(&mut __list_ind_common_delta),
                &mut list_indices_none_total
                    .to_rel_index_write(&mut __list_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            loop {
                let mut __changed = false;
                ascent::internal::comment(
                    "list <-- list_indices_none_delta, let ⋯, if ⋯",
                );
                {
                    if let Some(__matching) = list_indices_none_delta
                        .to_rel_index(&__list_ind_common_delta)
                        .index_get(&())
                    {
                        __matching
                            .for_each(|__val| {
                                let __val = __val.tuple_of_borrowed();
                                let t: &List = __val.0;
                                let l: &usize = __val.1;
                                let h = *l + 1;
                                if h <= 5 {
                                    let __new_row: (List, usize) = (
                                        List::Cons(*l, Box::new(t.clone())),
                                        ascent::internal::Convert::convert(h),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &list_indices_0_1_total
                                            .to_rel_index(&__list_ind_common_total),
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            &list_indices_0_1_delta
                                                .to_rel_index(&__list_ind_common_delta),
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut list_indices_0_1_new
                                                .to_rel_index_write(&mut __list_ind_common_new),
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.list.len();
                                            _self.list.push((__new_row.0.clone(), __new_row.1.clone()));
                                            ::ascent::internal::RelIndexWrite::index_insert(
                                                &mut list_indices_none_new
                                                    .to_rel_index_write(&mut __list_ind_common_new),
                                                (),
                                                (__new_row.0.clone(), __new_row.1.clone()),
                                            );
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut __list_ind_common_new,
                    &mut __list_ind_common_delta,
                    &mut __list_ind_common_total,
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut list_indices_0_1_new
                        .to_rel_index_write(&mut __list_ind_common_new),
                    &mut list_indices_0_1_delta
                        .to_rel_index_write(&mut __list_ind_common_delta),
                    &mut list_indices_0_1_total
                        .to_rel_index_write(&mut __list_ind_common_total),
                );
                ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                    &mut list_indices_none_new
                        .to_rel_index_write(&mut __list_ind_common_new),
                    &mut list_indices_none_delta
                        .to_rel_index_write(&mut __list_ind_common_delta),
                    &mut list_indices_none_total
                        .to_rel_index_write(&mut __list_ind_common_total),
                );
                _self.scc_iters[1usize] += 1;
                if !__changed {
                    break;
                }
            }
            _self.__list_ind_common = __list_ind_common_total;
            _self.list_indices_0_1 = list_indices_0_1_total;
            _self.list_indices_none = list_indices_none_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
        }
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.list.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.list_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__list_ind_common),
                selection_tuple,
                (),
            );
            let selection_tuple = ();
            let rel_ind = &mut self.list_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__list_ind_common),
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
        let _type_constraints: ascent::internal::TypeConstraints<List>;
        let _type_constraints: ascent::internal::TypeConstraints<usize>;
    }
    pub fn summary() -> &'static str {
        "scc 0, is_looping: false:\n  list <-- \n  dynamic relations: list\nscc 1, is_looping: true:\n  list <-- list_indices_none_delta, let ⋯, if ⋯\n  dynamic relations: list\n"
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "list", self.list.len()))
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
            list: Default::default(),
            __list_ind_common: Default::default(),
            list_indices_0_1: Default::default(),
            list_indices_none: Default::default(),
            scc_times: [std::time::Duration::ZERO; 2usize],
            scc_iters: [0; 2usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
