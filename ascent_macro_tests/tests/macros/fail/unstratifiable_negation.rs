//! Use of unstratifiable negation

use ascent::ascent;

ascent! {
    relation a(i32);
    relation b(i32);

    // Invalid: These rules creates a cycle that involves negation:
    a(x) <-- !b(x);
    b(x) <-- !a(x);
}

fn main() {}
