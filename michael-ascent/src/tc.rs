use ascent::ascent;
use rand::Rng;
use std::collections::HashSet;

ascent! {
   relation edge(i32, i32);
   // i32 is 32 bit integer
   // Rust should make sure the data is 32 bit
   relation path(i32, i32);

   path(x, y) <-- edge(x, y);
   path(x, z) <-- edge(x, y), path(y, z);
}

// too tired to be writing the paths
fn gen_pairs(amnt: i32, edge_amnt: usize) -> Vec<(i32, i32)> {
   /*
      https://www.reddit.com/r/rust/comments/2552cj/let_vs_let_mut_syntax_rather_verbose/

      mut is a key to say it is mutable
   */
   let mut rng = rand::thread_rng();
   let mut edges = HashSet::new();

   while edges.len() < edge_amnt {
      let x = rng.gen_range(1..=amnt);
      let y = rng.gen_range(1..=amnt);

      if x != y { // prob dont want self loops
         edges.insert((x, y));
      }
   }

   edges.into_iter().collect() // should turn the hashset to vec

   // also since the last statement does not have a semicolon, it should indicate that it is a return (which is a CRAZY thing)
}

fn main() {
   let mut prog = AscentProgram::default();
   prog.edge = gen_pairs(10000000, 10000000);
   prog.run();
   println!("Path: {:?}", prog.path);
}