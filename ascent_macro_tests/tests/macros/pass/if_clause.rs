//! Conditional `if` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(isize);

    // Rules:

    relation even(isize);

    even(x) <-- number(x), if x % 2 == 0;

    relation odd(isize);

    odd(x) <-- number(x), if x % 2 != 0;
}

fn main() {}
