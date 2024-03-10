use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent_par! {
   #![measure_rule_times]
      struct TC;
      relation edge(i32, i32);
      relation path(i32, i32);

      path(x, y) <-- edge(x, y);
      // path(x, z) <-- edge(x, y), path(y, z);
      path(x, z) <-- path(x, y), path(y, z);
}

fn main() {}
