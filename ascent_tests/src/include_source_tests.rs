use ascent::{ascent_run, ascent_source};

mod base {
   ascent::ascent_source! {
      /// Defines `edge` and `path`, the transitive closure of `edge`.
      /// The type of a node is `Node`
      tc ():

      /// `edge` degined in `tc`
      relation edge(Node, Node);
      relation path(Node, Node);
      path(x, y) <-- edge(x, y);
      path(x, z) <-- edge(x, y), path(y, z);
   }
}

ascent_source! { symmetric_edges ():
   edge(x, y) <-- edge(y, x);
}

#[test]
fn include_ascent_source() {
   type Node = usize;
   let res = ascent_run! {
      include_source!(base::tc);
      include_source!(symmetric_edges);
      /// `edge` defined in `ascent_run`
      relation edge(Node, Node);

      edge(1, 2), edge(2, 3), edge(3, 4);
   };

   assert!(res.path.contains(&(4, 1)));
}

#[test]
fn test_macro_with_binding() {
   let z = 1;
   mod foom {
      use super::*;
      ascent_source! {
         foo_gen (z):
         foo(x, y) <-- foo(y, x), if y == &$z;
      }
   }
   let prog = ascent_run! {
      // struct MacroTest;
      relation foo(usize, usize);
      foo(1, 2);
      include_source!(foom::foo_gen, z);
   };
   assert!(prog.foo.contains(&(2, 1)));
}
