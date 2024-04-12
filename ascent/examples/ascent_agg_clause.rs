//! Aggregate `agg` clause

use ascent::{aggregators::{count, max, mean, min, sum}, ascent};

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

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.number = vec![
        (1,),
        (2,),
        (3,),
        (4,),
        (5,),
    ];

    prog.run();

    let AscentProgram { lowest, greatest, average, total, cardinality, ..} = prog;

    assert_eq!(lowest, vec![(1,)]);
    assert_eq!(greatest, vec![(5,)]);
    assert_eq!(average, vec![(3,)]);
    assert_eq!(total, vec![(15,)]);
    assert_eq!(cardinality, vec![(5,)]);
}
