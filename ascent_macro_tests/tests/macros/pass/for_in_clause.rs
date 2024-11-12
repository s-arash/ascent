//! Generative `for … in` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation seed(i32);

    // Rules:

    relation number(i32);

    number(x + y) <-- seed(x), for y in 0..3;
}

fn main() {}
