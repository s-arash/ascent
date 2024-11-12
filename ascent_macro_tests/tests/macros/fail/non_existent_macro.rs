//! Use of non-existent macro

use ascent::ascent;

ascent! {
    // Invalid: this rule references a non-existent macro:
    non_existent_macro!();
}

fn main() {}
