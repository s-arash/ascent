#![allow(dead_code)]
use std::collections::HashSet;


pub fn collect_set<T: Eq + std::hash::Hash>(iter : impl Iterator<Item = T>) -> HashSet<T> {
   iter.collect()
}

pub fn into_set<T: Eq + std::hash::Hash>(iter : impl IntoIterator<Item = T>) -> HashSet<T> {
   iter.into_iter().collect()
}

pub fn rels_equal<T : Eq + std::hash::Hash>(rel1: impl IntoIterator<Item = T>, rel2: impl IntoIterator<Item = T>) -> bool {
   rel1.into_iter().collect::<HashSet<_>>() == rel2.into_iter().collect::<HashSet<_>>()
}

#[macro_export]
macro_rules! assert_rels_eq {
   ($rel1: expr, $rel2: expr) => {
      let (rel1, rel2) = ($rel1.into_iter().collect::<std::collections::HashSet<_>>(), $rel2.into_iter().collect::<std::collections::HashSet<_>>());
      if rel1 != rel2 {
         panic!("Expected rels to be equal. \nrel1: {:?} \nrel2: {:?}",
            rel1, rel2);
      }
   };
}