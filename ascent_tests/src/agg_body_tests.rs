#![cfg(test)]
//! Aggregation clauses whose `in` part is a parenthesized body
//! (`agg s = sum(x) in (foo(y, x), bar(y), if *x > 0)`), and aggregation atoms
//! with repeated variables (`agg s = sum(y) in foo(y, y)`).
use std::collections::HashSet;
use std::hash::Hash;

use ascent::aggregators::{count, max, mean, min, not, sum};
use ascent::{ascent, ascent_run};

fn sorted<T: Ord + Clone>(v: &[T]) -> Vec<T> {
   let mut v = v.to_vec();
   v.sort();
   v
}

#[cfg(feature = "par")]
fn par_sorted<'a, T: Ord + Clone + 'a>(v: impl IntoIterator<Item = &'a T>) -> Vec<T> {
   let mut v: Vec<T> = v.into_iter().cloned().collect();
   v.sort();
   v
}

/// Counts distinct values it receives (order independent).
fn distinct_count<'a, T: 'a + Eq + Hash>(inp: impl Iterator<Item = (&'a T,)>) -> impl Iterator<Item = usize> {
   std::iter::once(inp.map(|(v,)| v).collect::<HashSet<_>>().len())
}

/// Counts the tuples it receives (order independent).
fn count_values<'a, T: 'a>(inp: impl Iterator<Item = (&'a T,)>) -> impl Iterator<Item = usize> {
   std::iter::once(inp.count())
}

// ---------------------------------------------------------------------------
// parenthesized bodies
// ---------------------------------------------------------------------------

#[test]
fn two_atom_body_joins_on_local_variable() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 10), (2, 20), (3, 30), (1, 5)];
      relation bar(i32) = vec![(1,), (3,)];
      relation total(i32);
      total(s) <-- agg s = sum(x) in (foo(y, x), bar(y));
   };
   assert_eq!(res.total, vec![(45,)]);
}

#[test]
fn equal_values_from_distinct_bindings_are_all_aggregated() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 5), (2, 5), (3, 5)];
      relation bar(i32) = vec![(1,), (2,)];
      relation total(i32);
      total(s) <-- agg s = sum(x) in (foo(y, x), bar(y));
   };
   assert_eq!(res.total, vec![(10,)]);
}

#[test]
fn three_atom_body_chains_locals() {
   let res = ascent_run! {
      relation a(i32, i32) = vec![(1, 10), (2, 20), (3, 30)];
      relation b(i32, i32) = vec![(10, 100), (10, 101), (20, 200), (30, 300)];
      relation c(i32) = vec![(100,), (101,), (300,)];
      relation total(i32);
      total(s) <-- agg s = sum(x) in (a(x, y), b(y, z), c(z));
   };
   assert_eq!(res.total, vec![(5,)]);
}

#[test]
fn group_by_outer_variable_counts_with_empty_groups() {
   let res = ascent_run! {
      relation node(i32) = vec![(1,), (2,), (3,), (4,)];
      relation edge(i32, i32) = vec![(1, 2), (1, 3), (2, 3), (3, 4), (4, 1)];
      relation active(i32) = vec![(2,), (3,)];
      relation degree(i32, usize);
      degree(n, c) <-- node(n), agg c = count() in (edge(n, m), active(m));
   };
   assert_eq!(sorted(&res.degree), vec![(1, 2), (2, 1), (3, 0), (4, 0)]);
}

#[test]
fn if_filter_on_local_variable() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, -5), (1, 7), (2, 3), (3, 100)];
      relation bar(i32) = vec![(1,), (2,)];
      relation total(i32);
      total(s) <-- agg s = sum(x) in (foo(y, x), bar(y), if *x > 0);
   };
   assert_eq!(res.total, vec![(10,)]);
}

#[test]
fn let_item_value_is_aggregated() {
   let res = ascent_run! {
      relation val(i32) = vec![(1,), (2,), (3,)];
      relation total(i32);
      total(s) <-- agg s = sum(d) in (val(x), let d = x * x);
   };
   assert_eq!(res.total, vec![(14,)]);
}

#[test]
fn question_some_pattern_argument() {
   let res = ascent_run! {
      relation key(i32) = vec![(1,), (2,), (3,)];
      relation opt(i32, Option<i32>) = vec![(1, Some(3)), (1, None), (1, Some(4)), (2, None), (3, Some(9))];
      relation wanted(i32) = vec![(1,), (2,)];
      relation total(i32, i32);
      total(k, s) <-- key(k), agg s = sum(v) in (opt(k, ?Some(v)), wanted(k));
   };
   assert_eq!(sorted(&res.total), vec![(1, 7), (2, 0), (3, 0)]);
}

#[test]
fn negation_inside_body_excludes_rows() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 10), (2, 20), (3, 30)];
      relation blocked(i32) = vec![(2,)];
      relation total(i32);
      total(s) <-- agg s = sum(x) in (foo(y, x), !blocked(y));
   };
   assert_eq!(res.total, vec![(40,)]);
}

#[test]
fn disjunction_inside_body_counts_each_satisfied_branch() {
   let res = ascent_run! {
      relation val(i32) = vec![(1,), (2,), (3,), (4,), (5,), (6,)];
      relation small(i32) = vec![(1,), (2,), (3,)];
      relation even(i32) = vec![(2,), (4,), (6,)];
      relation n(usize);
      n(c) <-- agg c = count() in (val(x), (small(x) | even(x)));
   };
   assert_eq!(res.n, vec![(6,)]);
}

#[test]
fn min_over_body_per_group() {
   let res = ascent_run! {
      relation key(i32) = vec![(1,), (2,), (3,)];
      relation foo(i32, i32) = vec![(1, 9), (1, 4), (1, 1), (2, 8), (2, 6), (3, 2)];
      relation ok(i32) = vec![(9,), (4,), (8,), (6,)];
      relation lowest(i32, i32);
      lowest(k, m) <-- key(k), agg m = min(x) in (foo(k, x), ok(x));
   };
   // group 3 has no satisfying row: min yields nothing
   assert_eq!(sorted(&res.lowest), vec![(1, 4), (2, 6)]);
}

#[test]
fn mean_counts_each_binding_of_equal_values() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 2), (2, 2), (3, 8)];
      relation bar(i32) = vec![(1,), (2,), (3,)];
      relation avg(i64);
      avg((m * 10.0).round() as i64) <-- agg m = mean(x) in (foo(y, x), bar(y));
   };
   assert_eq!(res.avg, vec![(40,)]);
}

#[test]
fn min_max_mean_of_empty_body_yield_nothing() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 10)];
      relation bar(i32) = vec![(2,)];
      relation lowest(i32);
      relation highest(i32);
      relation avg(i64);
      lowest(m) <-- agg m = min(x) in (foo(y, x), bar(y));
      highest(m) <-- agg m = max(x) in (foo(y, x), bar(y));
      avg(m.round() as i64) <-- agg m = mean(x) in (foo(y, x), bar(y));
   };
   assert!(res.lowest.is_empty());
   assert!(res.highest.is_empty());
   assert!(res.avg.is_empty());
}

#[test]
fn user_defined_aggregator_receives_every_binding() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 5), (2, 5), (3, 6)];
      relation bar(i32) = vec![(1,), (2,), (3,)];
      relation distinct(usize);
      relation all(usize);
      distinct(c) <-- agg c = distinct_count(x) in (foo(y, x), bar(y));
      all(c) <-- agg c = count_values(x) in (foo(y, x), bar(y));
   };
   assert_eq!(res.distinct, vec![(2,)]);
   assert_eq!(res.all, vec![(3,)]);
}

#[test]
fn local_variable_name_rebound_after_clause() {
   let res = ascent_run! {
      relation val(i32) = vec![(1,), (2,), (3,), (4,)];
      relation other(i32) = vec![(10,), (20,)];
      relation out(usize, i32);
      out(c, x) <-- agg c = count() in (val(x), if *x > 2), other(x);
   };
   assert_eq!(sorted(&res.out), vec![(2, 10), (2, 20)]);
}

#[test]
fn multiple_agg_clauses_in_one_rule() {
   let res = ascent_run! {
      relation key(i32) = vec![(1,), (2,)];
      relation foo(i32, i32) = vec![(1, 1), (1, 2), (1, 3), (2, 4), (2, 5)];
      relation bar(i32) = vec![(1,), (3,), (5,)];
      relation stats(i32, usize, i32);
      stats(k, c, s) <--
         key(k),
         agg c = count() in (foo(k, x), bar(x)),
         agg s = sum(x) in (foo(k, x), !bar(x));
   };
   assert_eq!(sorted(&res.stats), vec![(1, 2, 2), (2, 1, 4)]);
}

#[test]
fn counts_reachable_nodes_over_recursive_lower_stratum() {
   let res = ascent_run! {
      relation node(i32) = vec![(1,), (2,), (3,), (4,), (5,)];
      relation edge(i32, i32) = vec![(1, 2), (2, 3), (3, 1), (3, 4)];
      relation path(i32, i32);
      path(x, y) <-- edge(x, y);
      path(x, z) <-- edge(x, y), path(y, z);
      relation reach_count(i32, usize);
      reach_count(n, c) <-- node(n), agg c = count() in (path(n, m), if m != n);
   };
   assert_eq!(sorted(&res.reach_count), vec![(1, 3), (2, 3), (3, 3), (4, 0), (5, 0)]);
}

#[test]
fn body_aggregation_in_ascent_struct_program() {
   ascent! {
      struct BodyAggProgram;
      relation key(i32);
      relation foo(i32, i32);
      relation bar(i32);
      relation total(i32, i32);
      total(k, s) <-- key(k), agg s = sum(x) in (foo(k, x), bar(x), if *x > 1);
   }
   let mut prog = BodyAggProgram::default();
   prog.key = vec![(1,), (2,), (3,)];
   prog.foo = vec![(1, 1), (1, 2), (1, 3), (2, 2), (3, 4)];
   prog.bar = vec![(1,), (2,), (3,)];
   prog.run();
   assert_eq!(sorted(&prog.total), vec![(1, 5), (2, 2), (3, 0)]);
}

#[test]
fn parenthesized_single_atom_with_repeated_variable() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 1), (2, 3), (4, 4)];
      relation total(i32);
      total(s) <-- agg s = sum(y) in (foo(y, y));
   };
   assert_eq!(res.total, vec![(5,)]);
}

#[cfg(feature = "par")]
#[test]
fn par_two_atom_body_joins_on_local_variable() {
   let res = ascent::ascent_run_par! {
      relation foo(i32, i32) = vec![(1, 10), (2, 20), (3, 30), (1, 5)].into_iter().collect();
      relation bar(i32) = vec![(1,), (3,)].into_iter().collect();
      relation total(i32);
      total(s) <-- agg s = sum(x) in (foo(y, x), bar(y));
   };
   assert_eq!(par_sorted(res.total.iter()), vec![(45,)]);
}

#[cfg(feature = "par")]
#[test]
fn par_negation_and_disjunction_inside_body() {
   let res = ascent::ascent_run_par! {
      relation val(i32) = vec![(1,), (2,), (3,), (4,), (5,), (6,)].into_iter().collect();
      relation small(i32) = vec![(1,), (2,), (3,)].into_iter().collect();
      relation even(i32) = vec![(2,), (4,), (6,)].into_iter().collect();
      relation blocked(i32) = vec![(4,)].into_iter().collect();
      relation n(usize);
      n(c) <-- agg c = count() in (val(x), !blocked(x), (small(x) | even(x)));
   };
   assert_eq!(par_sorted(res.n.iter()), vec![(5,)]);
}

// ---------------------------------------------------------------------------
// repeated variables in the aggregated atom
// ---------------------------------------------------------------------------

#[test]
fn sum_over_repeated_variable_uses_only_equal_columns() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 1), (2, 3), (4, 4), (10, 20)];
      relation total(i32);
      total(s) <-- agg s = sum(y) in foo(y, y);
   };
   assert_eq!(res.total, vec![(5,)]);
}

#[test]
fn sum_over_repeated_variable_with_no_equal_columns_is_zero() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 2), (3, 4)];
      relation total(i32);
      total(s) <-- agg s = sum(y) in foo(y, y);
   };
   assert_eq!(res.total, vec![(0,)]);
}

#[test]
fn min_over_repeated_variable_without_equal_rows_yields_nothing() {
   let res = ascent_run! {
      relation foo(i32, i32) = vec![(1, 2), (3, 4)];
      relation lowest(i32);
      lowest(m) <-- agg m = min(y) in foo(y, y);
   };
   assert!(res.lowest.is_empty());
}

#[test]
fn repeated_variable_with_group_key_sums_per_group() {
   let res = ascent_run! {
      relation key(i32) = vec![(1,), (2,), (3,)];
      relation foo(i32, i32, i32) = vec![
         (1, 10, 10), (1, 20, 21), (1, 30, 30),
         (2, 5, 6), (2, 7, 7),
         (3, 1, 2),
      ];
      relation total(i32, i32);
      total(k, s) <-- key(k), agg s = sum(y) in foo(k, y, y);
   };
   assert_eq!(sorted(&res.total), vec![(1, 40), (2, 7), (3, 0)]);
}

#[test]
fn repeated_variable_three_occurrences() {
   let res = ascent_run! {
      relation foo(i32, i32, i32) = vec![(1, 1, 1), (2, 2, 3), (4, 4, 4), (5, 6, 5), (7, 8, 8)];
      relation total(i32);
      total(s) <-- agg s = sum(y) in foo(y, y, y);
   };
   assert_eq!(res.total, vec![(5,)]);
}

#[test]
fn repeated_variable_combined_with_constant() {
   let res = ascent_run! {
      relation foo(i32, i32, i32) = vec![(1, 3, 3), (1, 4, 5), (2, 6, 6), (1, 9, 9)];
      relation total(i32);
      total(s) <-- agg s = sum(y) in foo(1, y, y);
   };
   assert_eq!(res.total, vec![(12,)]);
}

#[test]
fn repeated_variable_around_outer_bound_variable() {
   let res = ascent_run! {
      relation key(i32) = vec![(1,), (2,)];
      relation foo(i32, i32, i32) = vec![
         (3, 1, 3), (4, 1, 5), (6, 2, 6), (7, 2, 7), (8, 2, 9), (1, 3, 1),
      ];
      relation total(i32, i32);
      total(k, s) <-- key(k), agg s = sum(y) in foo(y, k, y);
   };
   assert_eq!(sorted(&res.total), vec![(1, 3), (2, 13)]);
}

#[test]
fn repeated_variable_over_recursive_lower_stratum() {
   // nodes lying on a cycle are exactly those with path(x, x)
   let res = ascent_run! {
      relation edge(i32, i32) = vec![(1, 2), (2, 1), (2, 3), (3, 4), (4, 3), (4, 5)];
      relation path(i32, i32);
      path(x, y) <-- edge(x, y);
      path(x, z) <-- edge(x, y), path(y, z);
      relation on_cycle_count(usize);
      on_cycle_count(c) <-- agg c = count_values(x) in path(x, x);
   };
   assert_eq!(res.on_cycle_count, vec![(4,)]);
}

#[test]
fn repeated_variable_in_ascent_struct_program() {
   ascent! {
      struct RepeatedVarProgram;
      relation foo(i32, i32);
      relation total(i32);
      total(s) <-- agg s = sum(y) in foo(y, y);
   }
   let mut prog = RepeatedVarProgram::default();
   prog.foo = vec![(3, 3), (4, 5), (6, 6)];
   prog.run();
   assert_eq!(prog.total, vec![(9,)]);
}

#[cfg(feature = "par")]
#[test]
fn par_sum_over_repeated_variable_uses_only_equal_columns() {
   let res = ascent::ascent_run_par! {
      relation foo(i32, i32) = vec![(1, 1), (2, 3), (4, 4), (10, 20)].into_iter().collect();
      relation total(i32);
      total(s) <-- agg s = sum(y) in foo(y, y);
   };
   assert_eq!(res.total.iter().cloned().collect::<Vec<_>>(), vec![(5,)]);
}
