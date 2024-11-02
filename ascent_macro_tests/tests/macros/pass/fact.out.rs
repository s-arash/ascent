//! Simple fact relation
use ascent::ascent;
pub struct AscentProgram {
    /**
logical indices: fact_one_indices_0*/
    pub fact_one: ::std::vec::Vec<(isize,)>,
    pub __fact_one_ind_common: (),
    pub fact_one_indices_0: ascent::internal::RelFullIndexType<(isize,), ()>,
    /**
logical indices: fact_two_indices_0_1*/
    pub fact_two: ::std::vec::Vec<(isize, isize)>,
    pub __fact_two_ind_common: (),
    pub fact_two_indices_0_1: ascent::internal::RelFullIndexType<(isize, isize), ()>,
    /**
logical indices: fact_zero_indices_none*/
    pub fact_zero: ::std::vec::Vec<()>,
    pub __fact_zero_ind_common: (),
    pub fact_zero_indices_none: ascent::internal::RelFullIndexType<(), ()>,
    scc_times: [std::time::Duration; 0usize],
    scc_iters: [usize; 0usize],
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
    }
    #[allow(noop_method_call, suspicious_double_ref_op)]
    fn update_indices_priv(&mut self) {
        let before = ::ascent::internal::Instant::now();
        use ascent::internal::ToRelIndex0;
        use ascent::internal::RelIndexWrite;
        for (_i, tuple) in self.fact_one.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.fact_one_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_one_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.fact_two.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.fact_two_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_two_ind_common),
                selection_tuple,
                (),
            );
        }
        for (_i, tuple) in self.fact_zero.iter().enumerate() {
            let selection_tuple = ();
            let rel_ind = &mut self.fact_zero_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                &mut rel_ind.to_rel_index_write(&mut self.__fact_zero_ind_common),
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
        let _type_constraints: ascent::internal::TypeConstraints<isize>;
    }
    pub fn summary() -> &'static str {
        ""
    }
    pub fn relation_sizes_summary(&self) -> String {
        use std::fmt::Write;
        let mut res = String::new();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "fact_one", self.fact_one.len()))
            .unwrap();
        (&mut res)
            .write_fmt(format_args!("{0} size: {1}\n", "fact_two", self.fact_two.len()))
            .unwrap();
        (&mut res)
            .write_fmt(
                format_args!("{0} size: {1}\n", "fact_zero", self.fact_zero.len()),
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            fact_one: Default::default(),
            __fact_one_ind_common: Default::default(),
            fact_one_indices_0: Default::default(),
            fact_two: Default::default(),
            __fact_two_ind_common: Default::default(),
            fact_two_indices_0_1: Default::default(),
            fact_zero: Default::default(),
            __fact_zero_ind_common: Default::default(),
            fact_zero_indices_none: Default::default(),
            scc_times: [std::time::Duration::ZERO; 0usize],
            scc_iters: [0; 0usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
        };
        _self
    }
}
fn main() {}
