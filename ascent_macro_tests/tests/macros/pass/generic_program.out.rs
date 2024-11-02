//! Generic program
use std::hash::Hash;
use ascent::ascent;
pub struct Node(&'static str);
#[automatically_derived]
impl ::core::marker::Copy for Node {}
#[automatically_derived]
impl ::core::clone::Clone for Node {
    #[inline]
    fn clone(&self) -> Node {
        let _: ::core::clone::AssertParamIsClone<&'static str>;
        *self
    }
}
#[automatically_derived]
impl ::core::marker::StructuralEq for Node {}
#[automatically_derived]
impl ::core::cmp::Eq for Node {
    #[inline]
    #[doc(hidden)]
    #[coverage(off)]
    fn assert_receiver_is_total_eq(&self) -> () {
        let _: ::core::cmp::AssertParamIsEq<&'static str>;
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for Node {}
#[automatically_derived]
impl ::core::cmp::PartialEq for Node {
    #[inline]
    fn eq(&self, other: &Node) -> bool {
        self.0 == other.0
    }
}
#[automatically_derived]
impl ::core::hash::Hash for Node {
    #[inline]
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        ::core::hash::Hash::hash(&self.0, state)
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Node {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Node", &&self.0)
    }
}
struct AscentProgram<T>
where
    T: Clone + Eq + Hash,
{
    /**
logical indices: value_indices_0*/
    pub value: ::std::vec::Vec<(T,)>,
    pub __value_ind_common: (),
    pub value_indices_0: ascent::internal::RelFullIndexType<(T,), ()>,
    scc_times: [std::time::Duration; 0usize],
    scc_iters: [usize; 0usize],
    pub update_time_nanos: std::sync::atomic::AtomicU64,
    pub update_indices_duration: std::time::Duration,
}
impl<T> AscentProgram<T>
where
    T: Clone + Eq + Hash,
{
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
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.value.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.value_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__value_ind_common),
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
        let _type_constraints: ascent::internal::TypeConstraints<T>;
    }
    pub fn summary() -> &'static str {
        ""
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "value", self.value.len()))
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
        res
    }
}
impl<T> Default for AscentProgram<T>
where
    T: Clone + Eq + Hash,
{
    fn default() -> Self {
        let mut _self = AscentProgram {
            value: Default::default(),
            __value_ind_common: Default::default(),
            value_indices_0: Default::default(),
            scc_times: [std::time::Duration::ZERO; 0usize],
            scc_iters: [0; 0usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
