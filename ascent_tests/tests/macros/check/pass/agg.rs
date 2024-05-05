use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent::aggregators::min;

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation foo(i32, i32);
   relation bar(i32, i32, i32);
   relation baz(i32, i32, i32);

   baz(x, y, min_z) <--
      foo(x, y),
      agg min_z = min(z) in bar(x, y, z);
}

fn main() {}
