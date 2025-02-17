use ascent::{ascent_run, ascent_source};

mod base {
   ascent::ascent_source! {
      /// Defines `edge` and `path`, the transitive closure of `edge`.
      /// The type of a node is `Node`
      tc:

      relation edge(Node, Node);
      relation path(Node, Node);
      path(x, y) <-- edge(x, y);
      path(x, z) <-- edge(x, y), path(y, z);
   }
}

ascent_source! { edges:
   edge(1, 2);
   edge(2, 3);
}

#[test]
fn include_ascent_source() {
   type Node = usize;
   let res = ascent_run! {
      include_source!(base::tc);
      include_source!(edges);
      edge(3, 4);
   };

   assert!(res.path.contains(&(1, 4)));
}
