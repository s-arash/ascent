use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation a(i32, i32);
   relation b(i32, i32);
   relation c(i32, i32);

   a(y, z),
   b(z, w),
   c(x, y) <--
      a(x, y),
      b(y, z),
      c(z, w);
}

fn main() {}
