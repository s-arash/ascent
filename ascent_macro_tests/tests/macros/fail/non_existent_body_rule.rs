//! Use of non-existent rule in body

use ascent::ascent;

ascent! {
    relation rule(usize);

    // Invalid: this rule references a non-existent relation:
    rule(n) <-- non_existent_rule();
}

fn main() {}
