//! Use of non-existent rule in head

use ascent::ascent;

ascent! {
    relation rule(usize);

    // Invalid: this rule references a non-existent rule in its head:
    non_existent_rule(n) <-- rule(n);
}

fn main() {}
