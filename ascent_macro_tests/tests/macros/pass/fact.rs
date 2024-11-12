//! Simple fact relation

use ascent::ascent;

ascent! {
    relation fact_zero();
    relation fact_one(isize);
    relation fact_two(isize, isize);
}

fn main() {}
