use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Deref;
use std::{clone, cmp::max, rc::Rc};

#[allow(dead_code)]
pub trait Atom:
   From<usize> + Into<usize> + Copy + Clone + std::fmt::Debug + Eq + Ord + Hash + Sync + Send + 'static
{
   fn index(self) -> usize;
}

#[allow(dead_code)]
pub trait FactTypes: Copy + Clone + Debug {
   type Origin: Atom;
   type Loan: Atom;
   type Point: Atom;
   type Variable: Atom;
   type Path: Atom;
}

#[warn(warnings)]
#[allow(unused_imports)]
#[allow(dead_code)]
#[allow(redundant_semicolons)]
#[cfg(test)]
fn _test<T: FactTypes>() {
   use ascent::aggregators::*;
   use ascent::lattice::set::Set;
   use ascent::Dual;

   use ascent::rel as custom_ds;
   ::ascent::rel::rel_codegen! { AscentProgram_bar , (i32 ,) , [[0]] , ser , () }
   ::ascent::rel::rel_codegen! { AscentProgram_foo , (i32 ,) , [[0]] , ser , () }
   ::ascent::rel::rel_codegen! { AscentProgram_baz , (i32 , i32) , [[] , [0 , 1]] , ser , () }
   pub struct AscentProgram {
      #[doc = "\nlogical indices: bar_indices_0"]
      pub bar: ::ascent::rel::rel!(AscentProgram_bar, (i32,), [[0]], ser, ()),
      pub __bar_ind_common: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()),
      pub bar_indices_0: ::ascent::rel::rel_full_ind!(AscentProgram_bar, (i32,), [[0]], ser, (), (i32,), ()),
      #[doc = "\nlogical indices: foo_indices_0"]
      pub foo: ::ascent::rel::rel!(AscentProgram_foo, (i32,), [[0]], ser, ()),
      pub __foo_ind_common: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()),
      pub foo_indices_0: ::ascent::rel::rel_full_ind!(AscentProgram_foo, (i32,), [[0]], ser, (), (i32,), ()),
      #[doc = "\nlogical indices: baz_indices_none; baz_indices_0_1"]
      pub baz: ::ascent::rel::rel!(AscentProgram_baz, (i32, i32), [[], [0, 1]], ser, ()),
      pub __baz_ind_common: ::ascent::rel::rel_ind_common!(AscentProgram_baz, (i32, i32), [[], [0, 1]], ser, ()),
      pub baz_indices_none:
         ::ascent::rel::rel_ind!(AscentProgram_baz, (i32, i32), [[], [0, 1]], ser, (), [], (), (i32, i32)),
      pub baz_indices_0_1:
         ::ascent::rel::rel_full_ind!(AscentProgram_baz, (i32, i32), [[], [0, 1]], ser, (), (i32, i32), ()),
      scc_times: [std::time::Duration; 3usize],
      scc_iters: [usize; 3usize],
      pub update_time_nanos: std::sync::atomic::AtomicU64,
      pub update_indices_duration: std::time::Duration,
   }
   impl AscentProgram {
      #[allow(unused_imports)]
      #[doc = "Runs the Ascent program to a fixed point."]
      pub fn run(&mut self) {
         macro_rules! __check_return_conditions {
            () => {};
         }
         use ascent::internal::RelIndexRead;
         use ascent::internal::RelIndexReadAll;
         use ascent::internal::RelIndexWrite;
         use ascent::to_rel_index::ToRelIndex0;
         use ascent::tuple_of_borrowed::TupleOfBorrowed;
         use core::cmp::PartialEq;
         self.update_indices_priv();
         let _self = self;
         ascent::internal::comment("scc 0");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __foo_ind_common_delta: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()) =
               ::std::mem::take(&mut _self.__foo_ind_common);
            let mut __foo_ind_common_total: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()) =
               Default::default();
            let mut __foo_ind_common_new: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()) =
               Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut __foo_ind_common_new,
               &mut __foo_ind_common_delta,
               &mut __foo_ind_common_total,
            );
            let mut foo_indices_0_delta: ::ascent::rel::rel_full_ind!(
               AscentProgram_foo,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = ::std::mem::take(&mut _self.foo_indices_0);
            let mut foo_indices_0_total: ::ascent::rel::rel_full_ind!(
               AscentProgram_foo,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            let mut foo_indices_0_new: ::ascent::rel::rel_full_ind!(
               AscentProgram_foo,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
               &mut foo_indices_0_delta.to_rel_index_write(&mut __foo_ind_common_delta),
               &mut foo_indices_0_total.to_rel_index_write(&mut __foo_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("foo <-- ");
               {
                  let __new_row: (i32,) = (1,);
                  if !::ascent::internal::RelFullIndexRead::contains_key(
                     &foo_indices_0_total.to_rel_index(&__foo_ind_common_total),
                     &__new_row,
                  ) && !::ascent::internal::RelFullIndexRead::contains_key(
                     &foo_indices_0_delta.to_rel_index(&__foo_ind_common_delta),
                     &__new_row,
                  ) {
                     if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                        &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
                        &__new_row,
                        (),
                     ) {
                        let __new_row_ind = _self.foo.len();
                        _self.foo.push((__new_row.0,));
                        __changed = true;
                     }
                  }
               }
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __foo_ind_common_new,
                  &mut __foo_ind_common_delta,
                  &mut __foo_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
                  &mut foo_indices_0_delta.to_rel_index_write(&mut __foo_ind_common_delta),
                  &mut foo_indices_0_total.to_rel_index_write(&mut __foo_ind_common_total),
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __foo_ind_common_new,
                  &mut __foo_ind_common_delta,
                  &mut __foo_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
                  &mut foo_indices_0_delta.to_rel_index_write(&mut __foo_ind_common_delta),
                  &mut foo_indices_0_total.to_rel_index_write(&mut __foo_ind_common_total),
               );
               _self.scc_iters[0usize] += 1;
               __check_return_conditions!();
            }
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_0 = foo_indices_0_total;
            _self.scc_times[0usize] += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 1");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __bar_ind_common_delta: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()) =
               ::std::mem::take(&mut _self.__bar_ind_common);
            let mut __bar_ind_common_total: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()) =
               Default::default();
            let mut __bar_ind_common_new: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()) =
               Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut __bar_ind_common_new,
               &mut __bar_ind_common_delta,
               &mut __bar_ind_common_total,
            );
            let mut bar_indices_0_delta: ::ascent::rel::rel_full_ind!(
               AscentProgram_bar,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = ::std::mem::take(&mut _self.bar_indices_0);
            let mut bar_indices_0_total: ::ascent::rel::rel_full_ind!(
               AscentProgram_bar,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            let mut bar_indices_0_new: ::ascent::rel::rel_full_ind!(
               AscentProgram_bar,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
               &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
               &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            {
               let mut __changed = false;
               ascent::internal::comment("bar <-- ");
               {
                  let __new_row: (i32,) = (3,);
                  if !::ascent::internal::RelFullIndexRead::contains_key(
                     &bar_indices_0_total.to_rel_index(&__bar_ind_common_total),
                     &__new_row,
                  ) && !::ascent::internal::RelFullIndexRead::contains_key(
                     &bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta),
                     &__new_row,
                  ) {
                     if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                        &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                        &__new_row,
                        (),
                     ) {
                        let __new_row_ind = _self.bar.len();
                        _self.bar.push((__new_row.0,));
                        __changed = true;
                     }
                  }
               }
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __bar_ind_common_new,
                  &mut __bar_ind_common_delta,
                  &mut __bar_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                  &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
                  &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __bar_ind_common_new,
                  &mut __bar_ind_common_delta,
                  &mut __bar_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                  &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
                  &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
               );
               _self.scc_iters[1usize] += 1;
               __check_return_conditions!();
            }
            _self.__bar_ind_common = __bar_ind_common_total;
            _self.bar_indices_0 = bar_indices_0_total;
            _self.scc_times[1usize] += _scc_start_time.elapsed();
         }
         ascent::internal::comment("scc 2");
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            let mut __bar_ind_common_delta: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()) =
               ::std::mem::take(&mut _self.__bar_ind_common);
            let mut __bar_ind_common_total: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()) =
               Default::default();
            let mut __bar_ind_common_new: ::ascent::rel::rel_ind_common!(AscentProgram_bar, (i32,), [[0]], ser, ()) =
               Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut __bar_ind_common_new,
               &mut __bar_ind_common_delta,
               &mut __bar_ind_common_total,
            );
            let mut bar_indices_0_delta: ::ascent::rel::rel_full_ind!(
               AscentProgram_bar,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = ::std::mem::take(&mut _self.bar_indices_0);
            let mut bar_indices_0_total: ::ascent::rel::rel_full_ind!(
               AscentProgram_bar,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            let mut bar_indices_0_new: ::ascent::rel::rel_full_ind!(
               AscentProgram_bar,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
               &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
               &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
            );
            let mut __baz_ind_common_delta: ::ascent::rel::rel_ind_common!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               ()
            ) = ::std::mem::take(&mut _self.__baz_ind_common);
            let mut __baz_ind_common_total: ::ascent::rel::rel_ind_common!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               ()
            ) = Default::default();
            let mut __baz_ind_common_new: ::ascent::rel::rel_ind_common!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               ()
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut __baz_ind_common_new,
               &mut __baz_ind_common_delta,
               &mut __baz_ind_common_total,
            );
            let mut baz_indices_none_delta: ::ascent::rel::rel_ind!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               (),
               [],
               (),
               (i32, i32)
            ) = ::std::mem::take(&mut _self.baz_indices_none);
            let mut baz_indices_none_total: ::ascent::rel::rel_ind!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               (),
               [],
               (),
               (i32, i32)
            ) = Default::default();
            let mut baz_indices_none_new: ::ascent::rel::rel_ind!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               (),
               [],
               (),
               (i32, i32)
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut baz_indices_none_new.to_rel_index_write(&mut __baz_ind_common_new),
               &mut baz_indices_none_delta.to_rel_index_write(&mut __baz_ind_common_delta),
               &mut baz_indices_none_total.to_rel_index_write(&mut __baz_ind_common_total),
            );
            let mut baz_indices_0_1_delta: ::ascent::rel::rel_full_ind!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               (),
               (i32, i32),
               ()
            ) = ::std::mem::take(&mut _self.baz_indices_0_1);
            let mut baz_indices_0_1_total: ::ascent::rel::rel_full_ind!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               (),
               (i32, i32),
               ()
            ) = Default::default();
            let mut baz_indices_0_1_new: ::ascent::rel::rel_full_ind!(
               AscentProgram_baz,
               (i32, i32),
               [[], [0, 1]],
               ser,
               (),
               (i32, i32),
               ()
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut baz_indices_0_1_new.to_rel_index_write(&mut __baz_ind_common_new),
               &mut baz_indices_0_1_delta.to_rel_index_write(&mut __baz_ind_common_delta),
               &mut baz_indices_0_1_total.to_rel_index_write(&mut __baz_ind_common_total),
            );
            let mut __foo_ind_common_delta: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()) =
               ::std::mem::take(&mut _self.__foo_ind_common);
            let mut __foo_ind_common_total: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()) =
               Default::default();
            let mut __foo_ind_common_new: ::ascent::rel::rel_ind_common!(AscentProgram_foo, (i32,), [[0]], ser, ()) =
               Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut __foo_ind_common_new,
               &mut __foo_ind_common_delta,
               &mut __foo_ind_common_total,
            );
            let mut foo_indices_0_delta: ::ascent::rel::rel_full_ind!(
               AscentProgram_foo,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = ::std::mem::take(&mut _self.foo_indices_0);
            let mut foo_indices_0_total: ::ascent::rel::rel_full_ind!(
               AscentProgram_foo,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            let mut foo_indices_0_new: ::ascent::rel::rel_full_ind!(
               AscentProgram_foo,
               (i32,),
               [[0]],
               ser,
               (),
               (i32,),
               ()
            ) = Default::default();
            ::ascent::internal::RelIndexMerge::init(
               &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
               &mut foo_indices_0_delta.to_rel_index_write(&mut __foo_ind_common_delta),
               &mut foo_indices_0_total.to_rel_index_write(&mut __foo_ind_common_total),
            );
            #[allow(unused_assignments, unused_variables)]
            loop {
               let mut __changed = false;
               ascent::internal::comment("baz <-- foo_indices_0_delta, bar_indices_0_total+delta [SIMPLE JOIN]");
               {
                  if foo_indices_0_delta.to_rel_index(&__foo_ind_common_delta).len()
                     <= ascent::internal::RelIndexCombined::new(
                        &bar_indices_0_total.to_rel_index(&__bar_ind_common_total),
                        &bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta),
                     )
                     .len()
                  {
                     foo_indices_0_delta.to_rel_index(&__foo_ind_common_delta).iter_all().for_each(
                        |(__cl1_joined_columns, __cl1_tuple_indices)| {
                           let __cl1_joined_columns = __cl1_joined_columns.tuple_of_borrowed();
                           let x = __cl1_joined_columns.0;
                           if let Some(__matching) = ascent::internal::RelIndexCombined::new(
                              &bar_indices_0_total.to_rel_index(&__bar_ind_common_total),
                              &bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta),
                           )
                           .index_get(&(x.clone(),))
                           {
                              __cl1_tuple_indices.for_each(|cl1_val| {
                                 __matching.clone().for_each(|__val| {
                                    let __new_row: (i32, i32) = (ascent::internal::Convert::convert(x), x + 1);
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                       &baz_indices_0_1_total.to_rel_index(&__baz_ind_common_total),
                                       &__new_row,
                                    ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                       &baz_indices_0_1_delta.to_rel_index(&__baz_ind_common_delta),
                                       &__new_row,
                                    ) {
                                       if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut baz_indices_0_1_new.to_rel_index_write(&mut __baz_ind_common_new),
                                          &__new_row,
                                          (),
                                       ) {
                                          let __new_row_ind = _self.baz.len();
                                          _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut baz_indices_none_new.to_rel_index_write(&mut __baz_ind_common_new),
                                             (),
                                             (__new_row.0.clone(), __new_row.1.clone()),
                                          );
                                          __changed = true;
                                       }
                                    }
                                 });
                              });
                           }
                        },
                     );
                  } else {
                     ascent::internal::RelIndexCombined::new(
                        &bar_indices_0_total.to_rel_index(&__bar_ind_common_total),
                        &bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta),
                     )
                     .iter_all()
                     .for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                        let __cl1_joined_columns = __cl1_joined_columns.tuple_of_borrowed();
                        let x = __cl1_joined_columns.0;
                        if let Some(__matching) =
                           foo_indices_0_delta.to_rel_index(&__foo_ind_common_delta).index_get(&(x.clone(),))
                        {
                           __cl1_tuple_indices.for_each(|cl1_val| {
                              __matching.clone().for_each(|__val| {
                                 let __new_row: (i32, i32) = (ascent::internal::Convert::convert(x), x + 1);
                                 if !::ascent::internal::RelFullIndexRead::contains_key(
                                    &baz_indices_0_1_total.to_rel_index(&__baz_ind_common_total),
                                    &__new_row,
                                 ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                    &baz_indices_0_1_delta.to_rel_index(&__baz_ind_common_delta),
                                    &__new_row,
                                 ) {
                                    if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                       &mut baz_indices_0_1_new.to_rel_index_write(&mut __baz_ind_common_new),
                                       &__new_row,
                                       (),
                                    ) {
                                       let __new_row_ind = _self.baz.len();
                                       _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                       ::ascent::internal::RelIndexWrite::index_insert(
                                          &mut baz_indices_none_new.to_rel_index_write(&mut __baz_ind_common_new),
                                          (),
                                          (__new_row.0.clone(), __new_row.1.clone()),
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
               ascent::internal::comment("baz <-- foo_indices_0_total, bar_indices_0_delta [SIMPLE JOIN]");
               {
                  if foo_indices_0_total.to_rel_index(&__foo_ind_common_total).len()
                     <= bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta).len()
                  {
                     foo_indices_0_total.to_rel_index(&__foo_ind_common_total).iter_all().for_each(
                        |(__cl1_joined_columns, __cl1_tuple_indices)| {
                           let __cl1_joined_columns = __cl1_joined_columns.tuple_of_borrowed();
                           let x = __cl1_joined_columns.0;
                           if let Some(__matching) =
                              bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta).index_get(&(x.clone(),))
                           {
                              __cl1_tuple_indices.for_each(|cl1_val| {
                                 __matching.clone().for_each(|__val| {
                                    let __new_row: (i32, i32) = (ascent::internal::Convert::convert(x), x + 1);
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                       &baz_indices_0_1_total.to_rel_index(&__baz_ind_common_total),
                                       &__new_row,
                                    ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                       &baz_indices_0_1_delta.to_rel_index(&__baz_ind_common_delta),
                                       &__new_row,
                                    ) {
                                       if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut baz_indices_0_1_new.to_rel_index_write(&mut __baz_ind_common_new),
                                          &__new_row,
                                          (),
                                       ) {
                                          let __new_row_ind = _self.baz.len();
                                          _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut baz_indices_none_new.to_rel_index_write(&mut __baz_ind_common_new),
                                             (),
                                             (__new_row.0.clone(), __new_row.1.clone()),
                                          );
                                          __changed = true;
                                       }
                                    }
                                 });
                              });
                           }
                        },
                     );
                  } else {
                     bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta).iter_all().for_each(
                        |(__cl1_joined_columns, __cl1_tuple_indices)| {
                           let __cl1_joined_columns = __cl1_joined_columns.tuple_of_borrowed();
                           let x = __cl1_joined_columns.0;
                           if let Some(__matching) =
                              foo_indices_0_total.to_rel_index(&__foo_ind_common_total).index_get(&(x.clone(),))
                           {
                              __cl1_tuple_indices.for_each(|cl1_val| {
                                 __matching.clone().for_each(|__val| {
                                    let __new_row: (i32, i32) = (ascent::internal::Convert::convert(x), x + 1);
                                    if !::ascent::internal::RelFullIndexRead::contains_key(
                                       &baz_indices_0_1_total.to_rel_index(&__baz_ind_common_total),
                                       &__new_row,
                                    ) && !::ascent::internal::RelFullIndexRead::contains_key(
                                       &baz_indices_0_1_delta.to_rel_index(&__baz_ind_common_delta),
                                       &__new_row,
                                    ) {
                                       if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                                          &mut baz_indices_0_1_new.to_rel_index_write(&mut __baz_ind_common_new),
                                          &__new_row,
                                          (),
                                       ) {
                                          let __new_row_ind = _self.baz.len();
                                          _self.baz.push((__new_row.0.clone(), __new_row.1.clone()));
                                          ::ascent::internal::RelIndexWrite::index_insert(
                                             &mut baz_indices_none_new.to_rel_index_write(&mut __baz_ind_common_new),
                                             (),
                                             (__new_row.0.clone(), __new_row.1.clone()),
                                          );
                                          __changed = true;
                                       }
                                    }
                                 });
                              });
                           }
                        },
                     );
                  }
               }
               ascent::internal::comment("foo, bar <-- baz_indices_none_delta");
               {
                  if let Some(__matching) = baz_indices_none_delta.to_rel_index(&__baz_ind_common_delta).index_get(&())
                  {
                     __matching.for_each(|__val| {
                        let __val = __val.tuple_of_borrowed();
                        let x: &i32 = __val.0;
                        let y: &i32 = __val.1;
                        let __new_row: (i32,) = (ascent::internal::Convert::convert(x),);
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                           &foo_indices_0_total.to_rel_index(&__foo_ind_common_total),
                           &__new_row,
                        ) && !::ascent::internal::RelFullIndexRead::contains_key(
                           &foo_indices_0_delta.to_rel_index(&__foo_ind_common_delta),
                           &__new_row,
                        ) {
                           if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                              &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
                              &__new_row,
                              (),
                           ) {
                              let __new_row_ind = _self.foo.len();
                              _self.foo.push((__new_row.0,));
                              __changed = true;
                           }
                        }
                        let __new_row: (i32,) = (ascent::internal::Convert::convert(y),);
                        if !::ascent::internal::RelFullIndexRead::contains_key(
                           &bar_indices_0_total.to_rel_index(&__bar_ind_common_total),
                           &__new_row,
                        ) && !::ascent::internal::RelFullIndexRead::contains_key(
                           &bar_indices_0_delta.to_rel_index(&__bar_ind_common_delta),
                           &__new_row,
                        ) {
                           if ::ascent::internal::RelFullIndexWrite::insert_if_not_present(
                              &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                              &__new_row,
                              (),
                           ) {
                              let __new_row_ind = _self.bar.len();
                              _self.bar.push((__new_row.0,));
                              __changed = true;
                           }
                        }
                     });
                  }
               }
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __bar_ind_common_new,
                  &mut __bar_ind_common_delta,
                  &mut __bar_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut bar_indices_0_new.to_rel_index_write(&mut __bar_ind_common_new),
                  &mut bar_indices_0_delta.to_rel_index_write(&mut __bar_ind_common_delta),
                  &mut bar_indices_0_total.to_rel_index_write(&mut __bar_ind_common_total),
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __baz_ind_common_new,
                  &mut __baz_ind_common_delta,
                  &mut __baz_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut baz_indices_none_new.to_rel_index_write(&mut __baz_ind_common_new),
                  &mut baz_indices_none_delta.to_rel_index_write(&mut __baz_ind_common_delta),
                  &mut baz_indices_none_total.to_rel_index_write(&mut __baz_ind_common_total),
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut baz_indices_0_1_new.to_rel_index_write(&mut __baz_ind_common_new),
                  &mut baz_indices_0_1_delta.to_rel_index_write(&mut __baz_ind_common_delta),
                  &mut baz_indices_0_1_total.to_rel_index_write(&mut __baz_ind_common_total),
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut __foo_ind_common_new,
                  &mut __foo_ind_common_delta,
                  &mut __foo_ind_common_total,
               );
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(
                  &mut foo_indices_0_new.to_rel_index_write(&mut __foo_ind_common_new),
                  &mut foo_indices_0_delta.to_rel_index_write(&mut __foo_ind_common_delta),
                  &mut foo_indices_0_total.to_rel_index_write(&mut __foo_ind_common_total),
               );
               _self.scc_iters[2usize] += 1;
               if !__changed {
                  break;
               }
               __check_return_conditions!();
            }
            _self.__bar_ind_common = __bar_ind_common_total;
            _self.bar_indices_0 = bar_indices_0_total;
            _self.__baz_ind_common = __baz_ind_common_total;
            _self.baz_indices_none = baz_indices_none_total;
            _self.baz_indices_0_1 = baz_indices_0_1_total;
            _self.__foo_ind_common = __foo_ind_common_total;
            _self.foo_indices_0 = foo_indices_0_total;
            _self.scc_times[2usize] += _scc_start_time.elapsed();
         }
      }
      fn update_indices_priv(&mut self) {
         let before = ::ascent::internal::Instant::now();
         use ascent::internal::RelIndexWrite;
         use ascent::to_rel_index::ToRelIndex0;
         for (_i, tuple) in self.bar.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.bar_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
               &mut rel_ind.to_rel_index_write(&mut self.__bar_ind_common),
               selection_tuple,
               (),
            );
         }
         for (_i, tuple) in self.foo.iter().enumerate() {
            let selection_tuple = (tuple.0.clone(),);
            let rel_ind = &mut self.foo_indices_0;
            ascent::internal::RelIndexWrite::index_insert(
               &mut rel_ind.to_rel_index_write(&mut self.__foo_ind_common),
               selection_tuple,
               (),
            );
         }
         for (_i, tuple) in self.baz.iter().enumerate() {
            let selection_tuple = ();
            let rel_ind = &mut self.baz_indices_none;
            ascent::internal::RelIndexWrite::index_insert(
               &mut rel_ind.to_rel_index_write(&mut self.__baz_ind_common),
               selection_tuple,
               (tuple.0.clone(), tuple.1.clone()),
            );
            let selection_tuple = (tuple.0.clone(), tuple.1.clone());
            let rel_ind = &mut self.baz_indices_0_1;
            ascent::internal::RelIndexWrite::index_insert(
               &mut rel_ind.to_rel_index_write(&mut self.__baz_ind_common),
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
         "scc 0, is_looping: false:\n  foo <-- \n  dynamic relations: foo\nscc 1, is_looping: false:\n  bar <-- \n  dynamic relations: bar\nscc 2, is_looping: true:\n  baz <-- foo_indices_0_delta, bar_indices_0_total+delta [SIMPLE JOIN]\n  baz <-- foo_indices_0_total, bar_indices_0_delta [SIMPLE JOIN]\n  foo, bar <-- baz_indices_none_delta\n  dynamic relations: bar, baz, foo\n"
      }
      pub fn relation_sizes_summary(&self) -> String {
         use std::fmt::Write;
         let mut res = String::new();
         writeln!(&mut res, "{} size: {}", "bar", self.bar.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "baz", self.baz.len()).unwrap();
         writeln!(&mut res, "{} size: {}", "foo", self.foo.len()).unwrap();
         res
      }
      pub fn scc_times_summary(&self) -> String {
         use std::fmt::Write;
         let mut res = String::new();
         writeln!(&mut res, "update_indices time: {:?}", self.update_indices_duration).unwrap();
         writeln!(&mut res, "scc {}: iterations: {}, time: {:?}", "0", self.scc_iters[0usize], self.scc_times[0usize])
            .unwrap();
         writeln!(&mut res, "scc {}: iterations: {}, time: {:?}", "1", self.scc_iters[1usize], self.scc_times[1usize])
            .unwrap();
         writeln!(&mut res, "scc {}: iterations: {}, time: {:?}", "2", self.scc_iters[2usize], self.scc_times[2usize])
            .unwrap();
         res
      }
   }
   impl Default for AscentProgram {
      fn default() -> Self {
         let mut _self = AscentProgram {
            bar: Default::default(),
            __bar_ind_common: Default::default(),
            bar_indices_0: Default::default(),
            foo: Default::default(),
            __foo_ind_common: Default::default(),
            foo_indices_0: Default::default(),
            baz: Default::default(),
            __baz_ind_common: Default::default(),
            baz_indices_none: Default::default(),
            baz_indices_0_1: Default::default(),
            scc_times: [std::time::Duration::ZERO; 3usize],
            scc_iters: [0; 3usize],
            update_time_nanos: Default::default(),
            update_indices_duration: std::time::Duration::default(),
         };
         _self
      }
   };
}
