//! Missing `relation` keyword

use ascent::ascent;

ascent! {
    // Invalid: This rule is missing the `relation` keyword:
    fact(usize);
}

fn main() {}
