use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   lattice longest_path(i32, i32, u32);
   relation edge(i32, i32, u32);

   longest_path(x, y, ew) <-- edge(x, y, ew);
   longest_path(x, z, *ew + *w) <-- edge(x, y, ew), longest_path(y, z, w);

   // edge(1,2, 3);
   // edge(2,3, 5);
   // edge(1,3, 4);
   // edge(2,4, 10);
}

fn main() {}
