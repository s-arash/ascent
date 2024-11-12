//! Invalid head arity

use ascent::ascent;

ascent! {
    relation fact(usize);

    relation rule(usize, usize);

    // Invalid: This rule uses a wrong arity in its head:
    rule(x) <-- fact(x);
}

fn main() {}
