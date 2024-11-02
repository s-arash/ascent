//! Conditional `if let` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation option(Option<isize>);

    // Rules:

    relation some(isize);

    some(y) <-- option(x), if let Some(y) = x;
}

fn main() {}
