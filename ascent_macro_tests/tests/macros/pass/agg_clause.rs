//! Aggregate `agg` clause

use ascent::{
   aggregators::{count, max, mean, min, sum},
   ascent,
};

ascent! {
    // Facts:

    relation number(i32);

    // Rules:

    relation lowest(i32);

    lowest(y) <-- agg y = min(x) in number(x);

    relation greatest(i32);

    greatest(y) <-- agg y = max(x) in number(x);

    relation average(i32);

    average(y.round() as i32) <-- agg y = mean(x) in number(x);

    relation total(i32);

    total(y) <-- agg y = sum(x) in number(x);

    relation cardinality(usize);

    cardinality(y) <-- agg y = count() in number(_);
}

fn main() {}
