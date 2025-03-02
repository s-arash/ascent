//! Disjunction clause

use ascent::ascent;

ascent! {
    // Facts:

    relation number(i32);

    // Rules:

    relation square(i32);

    square(y * y) <-- number(y), number(y * y);

    relation even(i32);

    even(x) <-- number(x) if x % 2 == 0;

    relation even_or_square(i32);

    even_or_square(x) <-- (even(x) | square(x));
}

fn main() {
   let mut prog = AscentProgram::default();

   prog.number = (1..=10).map(|n| (n,)).collect();

   prog.run();

   let AscentProgram { mut even_or_square, .. } = prog;

   even_or_square.sort_by_key(|(key,)| *key);

   assert_eq!(even_or_square, vec![(1,), (2,), (4,), (6,), (8,), (9,), (10,),]);
}
