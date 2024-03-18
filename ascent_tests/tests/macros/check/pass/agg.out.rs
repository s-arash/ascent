use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};
use ascent::aggregators::min;
use ascent_tests::{Atom, FactTypes};
pub struct AscentProgram {
    /**
physical indices:
 bar_indices_0_1; bar_indices_0_1_2*/
    pub bar: ::std::vec::Vec<(i32, i32, i32)>,
    pub bar_indices_0_1: ascent::internal::RelIndexType1<(i32, i32), (i32,)>,
    pub bar_indices_0_1_2: ascent::internal::RelFullIndexType<(i32, i32, i32), ()>,
    /**
physical indices:
 baz_indices_0_1_2*/
    pub baz: ::std::vec::Vec<(i32, i32, i32)>,
    pub baz_indices_0_1_2: ascent::internal::RelFullIndexType<(i32, i32, i32), ()>,
    /**
physical indices:
 foo_indices_0_1; foo_indices_none*/
    pub foo: ::std::vec::Vec<(i32, i32)>,
    pub foo_indices_0_1: ascent::internal::RelFullIndexType<(i32, i32), ()>,
    pub foo_indices_none: ascent::internal::RelIndexType1<(), (i32, i32)>,
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
            let baz_indices_0_1_2_delta: &mut ascent::internal::RelFullIndexType<
                (i32, i32, i32),
                (),
            > = &mut _self.baz_indices_0_1_2;
            let mut baz_indices_0_1_2_total: ascent::internal::RelFullIndexType<
                (i32, i32, i32),
                (),
            > = Default::default();
            let mut baz_indices_0_1_2_new: ascent::internal::RelFullIndexType<
                (i32, i32, i32),
                (),
            > = Default::default();
            let bar_indices_0_1_total: &mut ascent::internal::RelIndexType1<
                (i32, i32),
                (i32,),
            > = &mut _self.bar_indices_0_1;
            let foo_indices_none_total: &mut ascent::internal::RelIndexType1<
                (),
                (i32, i32),
            > = &mut _self.foo_indices_none;
            #[allow(unused_assignments, unused_variables)]
            {
                let mut __changed = false;
                ascent::internal::comment(
                    "baz <-- foo_indices_none_total, agg bar_indices_0_1",
                );
                {
                    if let Some(__matching) = foo_indices_none_total.index_get(&()) {
                        __matching
                            .for_each(|__val| {
                                let x = &__val.0;
                                let y = &__val.1;
                                let __matching = bar_indices_0_1_total
                                    .index_get(&(x.clone(), y.clone()));
                                let __aggregated_rel = &_self.bar;
                                let __agg_args = __matching
                                    .into_iter()
                                    .flatten()
                                    .map(|__val| {
                                        let z = &__val.0;
                                        (z,)
                                    });
                                for min_z in min(__agg_args) {
                                    let __new_row: (i32, i32, i32) = (
                                        ascent::internal::Convert::convert(x),
                                        ascent::internal::Convert::convert(y),
                                        ascent::internal::Convert::convert(min_z),
                                    );
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                        &baz_indices_0_1_2_total,
                                        &__new_row,
                                    )
                                        && !::ascent::internal::RelFullIndexRead::contains_key(
                                            baz_indices_0_1_2_delta,
                                            &__new_row,
                                        )
                                    {
                                        if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                            &mut baz_indices_0_1_2_new,
                                            &__new_row,
                                            (),
                                        ) {
                                            let __new_row_ind = _self.baz.len();
                                            _self.baz.push((__new_row.0, __new_row.1, __new_row.2));
                                            __changed = true;
                                        }
                                    }
                                }
                            });
                    }
                }
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    baz_indices_0_1_2_delta,
                    &mut baz_indices_0_1_2_total,
                );
                std::mem::swap(&mut baz_indices_0_1_2_new, baz_indices_0_1_2_delta);
                ::ascent::internal::RelIndexWrite::move_index_contents(
                    baz_indices_0_1_2_delta,
                    &mut baz_indices_0_1_2_total,
                );
                std::mem::swap(&mut baz_indices_0_1_2_new, baz_indices_0_1_2_delta);
            }
            _self.baz_indices_0_1_2 = baz_indices_0_1_2_total;
            _self.scc0_duration += _scc_start_time.elapsed();
        }
    }
    fn update_indices_priv(&mut self) {
        for (_i, tuple) in self.bar.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.bar_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.2.clone(),),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            let rel_ind = &mut self.bar_indices_0_1_2;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
        }
        for (_i, tuple) in self.baz.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone(), tuple.2.clone());
            let rel_ind = &mut self.baz_indices_0_1_2;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
        }
        for (_i, tuple) in self.foo.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.foo_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(rel_ind, selection_tuple, ());
            let selection_tuple = ();
            let rel_ind = &mut self.foo_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
                rel_ind,
                selection_tuple,
                (tuple.0.clone(), tuple.1.clone()),
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
        "scc 0, is_looping: false:\n  baz <-- foo_indices_none_total, agg bar_indices_0_1\n  dynamic relations: baz\n"
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
        res
    }
}
impl Default for AscentProgram {
    fn default() -> Self {
        let mut _self = AscentProgram {
            bar: Default::default(),
            bar_indices_0_1: Default::default(),
            bar_indices_0_1_2: Default::default(),
            baz: Default::default(),
            baz_indices_0_1_2: Default::default(),
            foo: Default::default(),
            foo_indices_0_1: Default::default(),
            foo_indices_none: Default::default(),
            scc0_duration: std::time::Duration::ZERO,
            update_time_nanos: Default::default(),
        };
        _self
    }
}
fn main() {}
