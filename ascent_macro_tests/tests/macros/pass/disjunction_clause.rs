//! Disjunction clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(i32);

    // Rules:

    relation square(i32);

    square(y * y) <-- number(y), number(y * y);

    relation even(i32);

    even(x) <-- number(x) if x % 2 == 0;

    relation even_or_square(i32);

    even_or_square(x) <-- (even(x) || square(x));
}

fn main() {}
