//! Incompatible variable type

use ascent::ascent;

ascent! {
    relation fact(usize);

    relation rule(String);

    // Invalid: This rule tries to assign a variable to a value of incompatible type:
    rule(x) <-- fact(x);
}

fn main() {}
