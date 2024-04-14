//! Generative `for … in` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation seed(i32);

    // Rules:
    
    relation number(i32);
    
    number(x + y) <-- seed(x), for y in 0..3;
}

fn main() {
    let mut prog = AscentProgram::default();
    
    prog.seed = vec![
        (0,),
        (10,),
    ];

    prog.run();

    let AscentProgram { mut number, ..} = prog;

    number.sort_by_key(|(key,)| *key);

    assert_eq!(number, vec![
        (0,),
        (1,),
        (2,),
        (10,),
        (11,),
        (12,),
    ]);
}
