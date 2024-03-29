#![cfg(test)]

use std::alloc::System;
use std::collections::BTreeSet;
use std::hash::BuildHasherDefault;
use std::time::Instant;

#[allow(unused_imports)]
use ascent::{ascent, ascent_run, ascent_par};
use hashbrown::HashSet;
use itertools::Itertools;
use rustc_hash::FxHasher;
use separator::Separatable;

use crate::tracking_alloc::{TrackingAllocator, self};
use crate::utils::move_hash_set_contents_disjoint;

#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator(System);


#[allow(dead_code)]
// #[test]
fn bench_eq_rel_in_ascent() {

   type User = u32;
   type Tx = u32;

   ascent! {
      struct BlockChainAnalysis;

      relation transaction(Tx, User);

      #[ds(crate::eqrel)]
      relation same_user(User, User);

      same_user(u1, u2) <--
         transaction(tx, u1),
         transaction(tx, u2);

      relation same_user_materialized(User, User);
      // same_user_materialized(u1, u2) <-- same_user(u1, u2);

      // same_user(y, y) <-- same_user(&42, y);
      // same_user(x, z) <-- same_user(x, y), same_user(y, z);
   }

   ascent! {
      struct BlockChainAnalysisExplicit;

      relation transaction(Tx, User);
      relation same_user_explicit(User, User);

      same_user_explicit(u1, u2) <--
         transaction(tx, u1),
         transaction(tx, u2);

      // symmetry, reflexivity:
      same_user_explicit(u2, u1),
      same_user_explicit(u1, u1) <-- same_user_explicit(u1, u2);

      //transitivity:
      same_user_explicit(u1, u3) <-- same_user_explicit(u1, u2), same_user_explicit(u2, u3);
   }

   fn mem_pretty(bytes: usize) -> String {
      format!("{}KiB", bytes / 1024)
   }
   for count in [100, 200, 400, 600, 800, 1200, 1600, 2000] {
      println!("\n=========== COUNT: {count} ============");
      let mem_use_before = tracking_alloc::current_alloc();
      let transaction = (0..count).flat_map(|tx| (tx..(tx + 2).min(count)).map(move |u| (tx, u))).collect_vec();

      let mut prog = BlockChainAnalysis::default();
      prog.transaction = transaction.iter().cloned().collect();

      let before = Instant::now();
      prog.run();
      println!("prog took {:?} s", before.elapsed().as_secs_f32());
      // println!("same_user len: {}", prog.same_user.len());
      println!("same_user_materialized len: {}", prog.same_user_materialized.len());
      println!("mem use by eqrel version: {}", mem_pretty(tracking_alloc::current_alloc() - mem_use_before));


      let mut prog_explicit = BlockChainAnalysisExplicit::default();
      prog_explicit.transaction = transaction.iter().cloned().collect();

      let before = Instant::now(); 
      let mem_use_before = tracking_alloc::current_alloc();
      prog_explicit.run();
      println!("prog_explicit took {:?} s", before.elapsed().as_secs_f32());
      println!("same_user_explicit len: {}", prog_explicit.same_user_explicit.len());
      println!("mem use by explicit version: {}", mem_pretty(tracking_alloc::current_alloc() - mem_use_before));

      // println!("same_user_explicit: {:?}", prog.same_user_explicit);
   }
   println!("\nmax mem use: {}", tracking_alloc::max_alloc());

}

#[test]
fn test_eq_rel2_in_ascent() {
   ascent! {
      struct EqRel2TestProg;

      #[ds(crate::eqrel)]
      relation indexed_eq_rel(u32, u64, u64);
      
      relation seed(u32);
      seed(x) <-- for x in 0..2;
      
      relation seed2(u32, u64, u64);
      
      seed2(a, 1, 2) <-- seed(a);
      seed2(a, x + 1, y + 1) <-- seed2(a, x, y), if *y < 20;

      indexed_eq_rel(a, x, y) <-- seed2(a, x, y);

      relation indexed_eq_rel_materialized(u32, u64, u64);
      indexed_eq_rel_materialized(a, x, y) <-- indexed_eq_rel(a, x, y);
   }

   let mut prog = EqRel2TestProg::default();

   prog.run();

   println!("indexed_eq_rel_materialized len: {}", prog.indexed_eq_rel_materialized.len());
   assert_eq!(prog.indexed_eq_rel_materialized.len(), 2 * 20usize.pow(2));
}

#[test]
fn test_eq_rel2_in_ascent2() {

   let test_cases = vec![
      (0..2).flat_map(|a| (3..5).flat_map(move |x| (9..11).map(move |y| (a, x, y)))).collect_vec(),
      vec![(0, 10, 11), (0, 12, 13), (0, 13, 14)],
      vec![(0, 10, 11), (0, 12, 13), (0, 13, 14), (0, 12, 11), (1, 11, 14), (1, 12, 13), (1, 13, 14)],
   ];
   for (i, seed_rel) in test_cases.into_iter().enumerate() {
      println!("test {}", i);
      let res = ascent_run! {
         struct EqRel2TestProg;
         
         #[ds(crate::eqrel)]
         relation indexed_eq_rel(u32, u64, u64);

         relation indexed_eq_rel_explicit(u32, u64, u64);

         indexed_eq_rel_explicit(a, x, x), indexed_eq_rel_explicit(a, y, x) <-- indexed_eq_rel_explicit(a, x, y);
         indexed_eq_rel_explicit(a, x, z) <-- indexed_eq_rel_explicit(a, x, y), indexed_eq_rel_explicit(a, y, z);
         
         indexed_eq_rel(a, x, y), indexed_eq_rel_explicit(a, x, y) <-- for (a, x, y) in seed_rel;


         relation indexed_eq_rel_materialized(u32, u64, u64);
         indexed_eq_rel_materialized(a, x, y) <-- indexed_eq_rel(a, x, y);

         relation foo(u32, u64, u64);
         foo(a, x, y) <-- for a in 0..2, for x in 8..12, for y in 10..15;

         relation test1_expected(u32, u64, u64);
         relation test1_actual(u32, u64, u64);

         test1_expected(a, x, y) <-- foo(a, x, y), indexed_eq_rel_explicit(a, x, y);
         test1_actual(a, x, y) <-- foo(a, x, y), indexed_eq_rel(a, x, y);

         relation bar(u32, u64);
         bar(a, x) <-- for a in 0..3, for x in 10..16;

         relation test2_expected(u32, u64, u64);
         relation test2_actual(u32, u64, u64);

         test2_expected(a, x, y) <-- bar(a, x), indexed_eq_rel_explicit(a, x, y);
         test2_actual(a, x, y) <-- bar(a, x), indexed_eq_rel(a, x, y);

         relation test3_expected(u32, u64, u64);
         relation test3_actual(u32, u64, u64);

         test3_expected(a, x, y) <-- bar(a, _), indexed_eq_rel_explicit(a, x, y);
         test3_actual(a, x, y) <-- bar(a, _), indexed_eq_rel(a, x, y);

         relation test4_expected(u32, u64, u64);
         relation test4_actual(u32, u64, u64);

         test4_expected(a, x, y) <-- bar(_, x), indexed_eq_rel_explicit(a, x, y);
         test4_actual(a, x, y) <-- bar(_, x), indexed_eq_rel_explicit(a, x, y);

         relation test5_expected(u32, u64, u64);
         relation test5_actual(u32, u64, u64);

         test5_expected(a, x, x) <-- for x in [5, 7, 9, 11, 13], indexed_eq_rel_explicit(a, x, x);
         test5_actual(a, x, x) <-- for x in [5, 7, 9, 11, 13], indexed_eq_rel(a, x, x);

         relation test6_expected(u32, u64, u64);
         relation test6_actual(u32, u64, u64);

         test6_expected(a, x, y) <-- for x in 8..14, indexed_eq_rel_explicit(a, x, y);
         test6_actual(a, x, y) <-- for x in 8..14, indexed_eq_rel(a, x, y);
      };

      println!("indexed_eq_rel_materialized len: {}", res.indexed_eq_rel_materialized.len());
      println!("test1_actual len: {}", res.test1_actual.len());
      println!("test2_actual len: {}", res.test2_actual.len());
      println!("test3_actual len: {}", res.test3_actual.len());
      println!("test4_actual len: {}", res.test4_actual.len());
      println!("test5_actual len: {}", res.test5_actual.len());
      println!("test6_actual len: {}", res.test6_actual.len());

      assert_eq!(res.indexed_eq_rel_materialized.len(), res.indexed_eq_rel_explicit.len());
      assert_eq!(res.test1_actual.len(), res.test1_expected.len());
      assert_eq!(res.test2_actual.len(), res.test2_expected.len());
      assert_eq!(res.test3_actual.len(), res.test3_expected.len());
      assert_eq!(res.test4_actual.len(), res.test4_expected.len());
      assert_eq!(res.test5_actual.len(), res.test5_expected.len());
      assert_eq!(res.test6_actual.len(), res.test6_expected.len());
   }
}

// #[test]
#[allow(dead_code)]
fn bench_trrel_in_ascent() {
   ascent! {
      struct TrRelTest;

      #[ds(crate::trrel)]
      relation tr(u32, u32);

      relation inp(u32, u32);

      tr(x, y) <-- inp(x, y);

      relation tr_materialized(u32, u32);
      // tr_materialized(x, y) <-- tr(x, y);
   }

   ascent! {
      struct TrRelTestExplicitOpt;
      
      relation inp(u32, u32);

      relation tr_explicit(u32, u32);
      
      tr_explicit(x, z) <-- inp(x, y), tr_explicit(y, z);
      tr_explicit(x, y) <-- inp(x, y);
   }

   ascent! {
      struct TrRelTestExplicit;

      relation tr_explicit(u32, u32);
      tr_explicit(x, z) <-- tr_explicit(x, y), tr_explicit(y, z);

      relation inp(u32, u32);

      tr_explicit(x, y) <-- inp(x, y);
   }

   for inp_size in [1000, 10000] {
      println!("\ninp size: {}", inp_size);
      
      let before = Instant::now();
      let mut tr_prog = TrRelTest::default();
      tr_prog.inp = (0..inp_size).map(|x| (x, x + 1)).collect();
      tr_prog.run();
      println!("tr took {}ms", before.elapsed().as_millis().separated_string());
      
      let before = Instant::now();
      let mut tr_explicit_opt_prog = TrRelTestExplicitOpt::default();
      tr_explicit_opt_prog.inp = (0..inp_size).map(|x| (x, x + 1)).collect();
      tr_explicit_opt_prog.run();
      println!("tr_explicit_opt took {}ms", before.elapsed().as_millis().separated_string());
      
      // let before = Instant::now();
      // let mut tr_explicit_prog = TrRelTestExplicit::default();
      // tr_explicit_prog.inp = (0..inp_size).map(|x| (x, x + 1)).collect();
      // tr_explicit_prog.run();
      // println!("tr_explicit took {}ms", before.elapsed().as_millis().separated_string());
      
      println!("tr_explicit len: {}", tr_explicit_opt_prog.tr_explicit.len());
      println!("tr len: {}", tr_prog.tr.len());
      println!("tr_materialized len: {}", tr_prog.tr_materialized.len());
      println!("tr_materialized: {:?}", tr_prog.tr_materialized);

      println!("========================");
   }

}

// #[test]
#[allow(dead_code)]
fn bench_trrel_uf_in_ascent1() {
   ascent! {
      struct TrRelUFTest;

      relation inp(u32, u32);

      #[ds(crate::trrel_uf)]
      relation tr(u32, u32);

      tr(x, y) <-- inp(x, y);

      relation tr_materialized(u32, u32);
      // tr_materialized(x, y) <-- tr(x, y);
   }

   ascent! {
      struct TrRelUFTestExplicit;

      relation inp(u32, u32);

      relation tr_explicit(u32, u32);
      relation tr_explicit_proto(u32, u32);

      tr_explicit_proto(x, y) <-- inp(x, y);

      tr_explicit(x, y) <-- tr_explicit_proto(x, y);
      tr_explicit(x, z) <-- tr_explicit_proto(x, y), tr_explicit(y, z);
   }

   let test_cases = vec![
      ((0..5000).map(|x| (x, x + 1)).collect_vec(), "chain 5000"),
      ((0..5000).map(|x| (x, x + 1)).chain([(5000, 0)]).collect_vec(), "loop 5000"),
      ((0..20).flat_map(|x| {
         let start = x * 500;
         (start..start + 500).map(|x| (x, x + 1)).chain([(start + 500, start)])
      }).collect_vec(), "loops")
   ];
   
   for (inp, name) in test_cases {
      println!("\n======= {name} =======");

      tracking_alloc::reset_max_alloc();
      let mem_use_before = tracking_alloc::current_alloc();
      let mut prog = TrRelUFTest::default();
      prog.inp = inp.clone();
      let before = Instant::now();
      prog.run();
      println!("tr_materialized len: {}", prog.tr_materialized.len());
      println!("tr with union find took {:?}", before.elapsed());
      println!("tr with union find max mem use: {}MB", (tracking_alloc::max_alloc() - mem_use_before) / (1 << 20));

      tracking_alloc::reset_max_alloc();
      let mem_use_before = tracking_alloc::current_alloc();
      let mut explicit_prog = TrRelUFTestExplicit::default();
      explicit_prog.inp = inp.clone();
      let before = Instant::now();
      explicit_prog.run();
      println!("tr_explicit len: {}", explicit_prog.tr_explicit.len());
      println!("tr explicit took {:?}", before.elapsed());
      println!("tr explicit max mem use: {}MB", (tracking_alloc::max_alloc() - mem_use_before) / (1 << 20));

   }
}

#[test]
fn test_trrel_uf_in_ascent() {
   let ub = 15;
   let res = ascent_run! {
      relation seed(u32);
      seed(1);
      seed(x + 1) <-- seed(x), if x < &ub;

      #[ds(crate::trrel_uf)]
      relation tr(u32, u32);

      tr(x, x + 1), tr(x + 1, x) <-- seed(x);

      // included to pull all rules into the same SCC
      seed(0) <-- if false, tr(1000, 2000);
   };
   assert_eq!(res.__tr_ind_common.count_exact() as u32, (ub + 1).pow(2));
}

#[test]
fn test_trrel_uf_ternary_in_ascent() {
   let ub = 15;
   let res = ascent_run! {
      relation seed(u32);
      seed(1);
      seed(x + 1) <-- seed(x), if x < &ub;

      #[ds(crate::trrel_uf)]
      relation tr(u8, u32, u32);

      tr(i, x, x + 1), tr(i, x + 1, x) <-- seed(x), for i in 0..10;

      // included to pull all rules into the same SCC
      seed(0) <-- if false, tr(_, 1000, 2000);
   };
   assert_eq!(res.__tr_ind_common.0.map[&1].count_exact() as u32, (ub + 1).pow(2));
}


#[test]
fn test_trrel1() {

   let test_cases = vec![
      vec![(1, 2), (2, 3)],
      vec![(1, 2)],
      (4..6).flat_map(|x| (6..9).map(move |y| (x, y))).collect(),
      (0..5).map(|x| (x, x + 1)).collect()
   ];

   for (i, seed_rel) in test_cases.into_iter().enumerate() {

      let res = ascent_run! {
         #[ds(crate::trrel)]
         relation tr(u32, u32);
         
         relation tr_explicit(u32, u32);
         tr_explicit(x, z) <-- tr_explicit(x, y), tr_explicit(y, z);
         
         tr(x, y), tr_explicit(x, y) <-- for (x, y) in seed_rel;
         
         tr(x, y) <-- tr(x, y);
         relation tr_materialized(u32, u32);
         tr_materialized(x, y) <-- tr(x, y);

         // testing New, Old variants of `TrRelIndCommon`
         #[ds(crate::trrel)]
         relation dummy1(u32, u32);
         
         #[ds(crate::trrel)]
         relation dummy2(u32, u32);

         dummy2(x, y) <-- dummy1(x, y);
      };
   
      println!("TEST {}", i);
      println!("explicit len: {}", res.tr_explicit.len());
      println!("materialized len: {}", res.tr_materialized.len());

      assert_eq!(res.tr_explicit.len(), res.tr_materialized.len());
   }
}

#[test]
fn test_trrel_reflexive_facts() {

   let test_cases = vec![
      vec![(1, 1), (1, 2)],
      vec![(1, 1)],
      (4..6).flat_map(|x| (5..7).map(move |y| (x, y))).collect(),
      (0..5).map(|x| (x, x)).collect()
   ];

   for (i, seed_rel) in test_cases.into_iter().enumerate() {

      let res = ascent_run! {
         #[ds(crate::trrel)]
         relation tr(u32, u32);
         
         relation tr_explicit(u32, u32);
         tr_explicit(x, z) <-- tr_explicit(x, y), tr_explicit(y, z);
         
         tr(x, y), tr_explicit(x, y) <-- for (x, y) in seed_rel;
         
         relation tr_materialized(u32, u32);
         tr_materialized(x, y) <-- tr(x, y);
      };
   
      println!("TEST {}", i);
      println!("explicit len: {}", res.tr_explicit.len());
      assert_eq!(res.tr_explicit.len(), res.tr_materialized.len());
   }
}

#[test]
fn test_trrel2_in_ascent() {
   let test_cases = vec![
      (1..4).flat_map(|a| (3..7).flat_map(move |x| (11..15).map(move |y| (a, x, y)))).collect_vec(),
      (0..3).flat_map(|a| (0..15).filter(move |x| x % 3 == a as u64 % 3).flat_map(move |x| (a as u64 + 14..19).map(move |y| (a, x, y)))).collect_vec(),
      (1..4).flat_map(|a| (5..10).flat_map(move |x| (12..17).map(move |y| (a, x, y)))).collect_vec(),
      vec![(0, 10, 11), (0, 5, 13), (0, 6, 13), (0, 7, 14), (0, 12, 13), (0, 13, 14)],
      vec![(0, 7, 8), (0, 8, 10), (0, 8, 13), (0, 9, 11), (1, 9, 14), (1, 9, 13), (1, 10, 14)]
         .into_iter().flat_map(|(a, x, y)| (0..3).map(move |o| (a + o, x, y - o as u64))).collect(),
   ];

   for (i, seed_rel) in test_cases.into_iter().enumerate() {
      let res = ascent_run! {
         #[ds(crate::trrel)]
         relation tr_indexed(u32, u64, u64);

         relation tr_indexed_explicit(u32, u64, u64);
         tr_indexed_explicit(a, x, z) <-- tr_indexed_explicit(a, x, y), tr_indexed_explicit(a, y, z);

         relation empty(u32, u64);
         
         relation seed(u32, u64, u64) = seed_rel;

         tr_indexed(a, x, y), tr_indexed_explicit(a, x, y) <-- seed(a, x, y);

         // checking join works, should add no tuples
         tr_indexed(a, x, y) <-- tr_indexed(a, x, y), empty(a, y);

         relation tr_indexed_materialized(u32, u64, u64);

         tr_indexed_materialized(a, x, y) <-- tr_indexed(a, x, y);

         relation foo(u32, u64);
         foo(a, x) <-- for a in 0..1, for x in 4..12;
         foo(a, x) <-- for a in 0..1, for x in (0..20).filter(|x| x % 3 == 2);


         relation test1_actual(u32, u64);
         relation test1_expected(u32, u64);

         test1_actual(a, x) <-- tr_indexed(a, x, _), foo(a, x);
         test1_expected(a, x) <-- tr_indexed_explicit(a, x, _), foo(a, x);

         relation bar(u32, u64, u64);
         bar(a, x, y) <-- for a in 0..1, for x in 5..6, for y in 10..12;
         bar(a, x, y) <-- 
            for a in 1..5, 
            for x in (5..21).filter(|x| x % 3 == a as u64 % 3), 
            for y in (12..18).filter(|y| y % 5 == a as u64 % 5);


         relation test2_actual(u32, u64, u64);
         relation test2_expected(u32, u64, u64);

         test2_actual(a, x, y) <-- tr_indexed(a, x, y), bar(a, x, y);
         test2_expected(a, x, y) <-- tr_indexed_explicit(a, x, y), bar(a, x, y);

         relation test3_actual(u32, u64, u64);
         relation test3_expected(u32, u64, u64);

         test3_actual(a, x, y) <-- tr_indexed(a, x, y), bar(_, x, y);
         test3_expected(a, x, y) <-- tr_indexed_explicit(a, x, y), bar(_, x, y);

         relation test4_actual(u32, u64, u64);
         relation test4_expected(u32, u64, u64);

         test4_actual(a, x, y) <-- tr_indexed(a, x, y), bar(_, x, _);
         test4_expected(a, x, y) <-- tr_indexed_explicit(a, x, y), bar(_, x, _);

         relation test5_actual(u32, u64, u64);
         relation test5_expected(u32, u64, u64);

         test5_actual(a, x, y) <-- tr_indexed(a, x, y), bar(_, _, y);
         test5_expected(a, x, y) <-- tr_indexed_explicit(a, x, y), bar(_, _, y);

      };

      println!("============ TEST CASE {} ============", i);
      println!("tr_indexed_explicit len: {}", res.tr_indexed_explicit.len());
      println!("test1_expected len: {}", res.test1_expected.len());
      println!("test2_expected len: {}", res.test2_expected.len());
      println!("test3_expected len: {}", res.test3_expected.len());
      println!("test4_expected len: {}", res.test4_expected.len());
      println!("test5_expected len: {}", res.test5_expected.len());

      // println!("test4_expected: {:?}", res.test4_expected);
      // println!("test4_actual: {:?}", res.test4_actual);
      // use hashbrown::HashSet;
      // println!("test_4 diff: {:?}", 
      //    res.test4_expected.iter().cloned().collect::<HashSet<_>>()
      //    .symmetric_difference(&res.test4_actual.iter().cloned().collect::<HashSet<_>>()));


      assert_eq!(res.test1_expected.len(), res.test1_actual.len());
      assert_eq!(res.test2_expected.len(), res.test2_actual.len());
      assert_eq!(res.test3_expected.len(), res.test3_actual.len());
      assert_eq!(res.test4_expected.len(), res.test4_actual.len());
      assert_eq!(res.test5_expected.len(), res.test5_actual.len());

      assert_eq!(res.tr_indexed_materialized.len(), res.tr_indexed_explicit.len());

   }
}

// #[test]
#[allow(dead_code)]
fn bench_btree() {

   let count = 10_000_000;
   let set1 = rand::seq::index::sample(&mut rand::thread_rng(), count, count / 2).into_vec();
   let set1 = set1.iter().cloned().collect::<HashSet<_, BuildHasherDefault<FxHasher>>>();
   let set2 = (0..count).filter(|x| !set1.contains(x)).collect::<HashSet<_, BuildHasherDefault<FxHasher>>>();
   // let set1 = (0..count).filter(|x| x % 5 == 0);
   // let set2 = (0..count).filter(|x| x % 5 != 0);

   let before = Instant::now();
   let mut hs1 = set1.iter().collect::<HashSet<_, BuildHasherDefault<FxHasher>>>();
   let mut hs2 = set2.iter().collect::<HashSet<_, BuildHasherDefault<FxHasher>>>();
   println!("constructing hashsets took {:?}", before.elapsed());

   let before = Instant::now();
   let mut bs1 = set1.iter().collect::<BTreeSet<_>>();
   let mut bs2 = set2.iter().collect::<BTreeSet<_>>();
   println!("constructing btrees took {:?}", before.elapsed());

   let before = Instant::now();
   let mut v1 = set1.iter().collect::<Vec<_>>();
   let mut v2 = set2.iter().collect::<Vec<_>>();
   println!("constructing vecs took {:?}", before.elapsed());

   let before = Instant::now();
   move_hash_set_contents_disjoint(&mut hs1, &mut hs2);
   println!("hs combined len: {}", hs2.len());
   println!("moving hash set contents took {:?}", before.elapsed());


   let before = Instant::now();
   bs2.append(&mut bs1);
   println!("bs combined len: {}", bs2.len());
   println!("moving btree set contents took {:?}", before.elapsed());

   let before = Instant::now();
   if v1.len() > v2.len() { std::mem::swap(&mut v1, &mut v2) }
   v2.append(&mut v1);
   println!("vec combined len: {}", v2.len());
   println!("moving btree set contents took {:?}", before.elapsed());
}
