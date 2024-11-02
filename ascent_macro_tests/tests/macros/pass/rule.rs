//! Simple rule relation

use ascent::ascent;

ascent! {
    relation fact_zero();
    relation fact_one(isize);
    relation fact_two(isize, isize);

    relation rule_zero();
    relation rule_one(isize);
    relation rule_two(isize, isize);

    rule_zero() <-- fact_zero();
    rule_one(n) <-- fact_one(n);
    rule_two(a, b) <-- fact_two(a, b);
}

fn main() {}
