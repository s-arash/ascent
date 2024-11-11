//! Conditional `if let` clause

use ascent::ascent;

ascent! {
    // Facts:

    relation option(Option<isize>);

    // Rules:

    relation some(isize);

    some(y) <-- option(x), if let Some(y) = x;

    // The rule above could alternatively also be written
    // using the following short-hand syntax:
    some(y) <-- option(?Some(y));
}

fn main() {
   let mut prog = AscentProgram::default();

   prog.option = vec![(None,), (Some(1),), (Some(2),), (Some(3),)];

   prog.run();

   let AscentProgram { mut some, .. } = prog;

   some.sort_by_key(|(key,)| *key);

   assert_eq!(some, vec![(1,), (2,), (3,),]);
}
