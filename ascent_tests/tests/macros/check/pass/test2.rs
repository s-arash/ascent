use std::{clone, cmp::max, fmt::Debug, hash::Hash, ops::Deref, rc::Rc};

use ascent_tests::{Atom, FactTypes};

ascent::ascent! {
   relation foo(i32, Option<i32>);
   relation bar(i32, i32);
   relation baz(i32, i32, i32);

   foo(1, Some(2));
   foo(2, None);
   foo(3, Some(5));
   foo(4, Some(10));

   bar(3, 6);
   bar(5, 10);
   bar(10, 20);

   baz(*x, *y, *z) <-- foo(x, ?Some(y)), bar(y , z);
}

fn main() {}
