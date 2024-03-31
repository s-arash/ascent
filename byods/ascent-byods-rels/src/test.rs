#![cfg(test)]

#[allow(unused_imports)]
use ascent::{ascent, ascent_run, ascent_par};
use itertools::Itertools;

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
