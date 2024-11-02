//! Negation clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(i32);

    // Rules:

    relation even(i32);

    even(x) <-- number(x), if x % 2 == 0;

    relation odd(i32);

    odd(x) <-- number(x), !even(x);
}

fn main() {}
