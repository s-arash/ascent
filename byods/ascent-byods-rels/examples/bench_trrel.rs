use ascent::ascent;
use ascent_byods_rels::trrel;
use separator::Separatable;
use std::time::Instant;

ascent! {
   struct TrRelTest;

   #[ds(crate::trrel)]
   relation tr(u32, u32);

   relation inp(u32, u32);

   tr(x, y) <-- inp(x, y);

   relation tr_materialized(u32, u32);
   // tr_materialized(x, y) <-- tr(x, y);
}

ascent! {
   struct TrRelTestExplicitOpt;

   relation inp(u32, u32);

   relation tr_explicit(u32, u32);

   tr_explicit(x, z) <-- inp(x, y), tr_explicit(y, z);
   tr_explicit(x, y) <-- inp(x, y);
}

ascent! {
   struct TrRelTestExplicit;

   relation tr_explicit(u32, u32);
   tr_explicit(x, z) <-- tr_explicit(x, y), tr_explicit(y, z);

   relation inp(u32, u32);

   tr_explicit(x, y) <-- inp(x, y);
}

fn main() {
   for inp_size in [1000, 10000] {
      println!("\ninp size: {}", inp_size);

      let before = Instant::now();
      let mut tr_prog = TrRelTest::default();
      tr_prog.inp = (0..inp_size).map(|x| (x, x + 1)).collect();
      tr_prog.run();
      println!("tr took {}ms", before.elapsed().as_millis().separated_string());

      let before = Instant::now();
      let mut tr_explicit_opt_prog = TrRelTestExplicitOpt::default();
      tr_explicit_opt_prog.inp = (0..inp_size).map(|x| (x, x + 1)).collect();
      tr_explicit_opt_prog.run();
      println!("tr_explicit_opt took {}ms", before.elapsed().as_millis().separated_string());

      // let before = Instant::now();
      // let mut tr_explicit_prog = TrRelTestExplicit::default();
      // tr_explicit_prog.inp = (0..inp_size).map(|x| (x, x + 1)).collect();
      // tr_explicit_prog.run();
      // println!("tr_explicit took {}ms", before.elapsed().as_millis().separated_string());

      println!("tr_explicit len: {}", tr_explicit_opt_prog.tr_explicit.len());
      println!("tr len: {}", tr_prog.tr.len());
      println!("tr_materialized len: {}", tr_prog.tr_materialized.len());
      println!("tr_materialized: {:?}", tr_prog.tr_materialized);

      println!("========================");
   }
}
