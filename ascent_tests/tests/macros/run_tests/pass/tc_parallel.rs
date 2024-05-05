use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent_par! {
   #![measure_rule_times]
   struct TC;
   relation edge(i32, i32);
   relation path(i32, i32);

   path(x, y) <-- edge(x, y);
   path(x, z) <-- edge(x, y), path(y, z);
}

#[test]
fn it_works() {
   let mut prog = TC::default();
   
   prog.edge = ascent::boxcar::vec![(1, 2), (2, 3)];
   prog.run();

   let mut paths = Vec::from_iter(prog.path);
   paths.sort();

   assert_eq!(paths, vec![(1, 2), (1, 3), (2, 3)]);
}
