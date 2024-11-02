//! Invalid body arity

use ascent::ascent;

ascent! {
    relation fact(usize);

    relation rule(usize);

    // Invalid: This rule uses a wrong arity in its body:
    rule(42) <-- fact(x, y);
}

fn main() {}
