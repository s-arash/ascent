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