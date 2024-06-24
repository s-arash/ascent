use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation bar(i32, i32);
   relation foo(i32, i32);
   relation baz(i32, i32);

   foo(1, 2);
   foo(10, 2);
   bar(2, 3);
   bar(2, 1);

   baz(*x, *z) <-- foo(x, y) if *x != 10, bar(y, z), if x != z;
   foo(*x, *y), bar(*x, *y) <-- baz(x, y);
}

fn main() {}
