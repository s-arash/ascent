use stopwatch::Stopwatch;

mod tc {
   use infer::infer;

   infer! {
      relation edge(i32, i32);
      relation path(i32, i32);
      // edge(x, x + 1) <-- for x in (0..1000);
      path(*x, *y) <-- edge(x,y);
      path(*x, *z) <-- edge(x,y), path(y, z);
      // path(*x, *z) <-- path(x,y), edge(y, z);

   }
}

fn bench_tc(nodes_count: i32) {
   let mut tc = tc::InferProgram::default();

   for i in 0..nodes_count {
      tc.edge.push((i, i + 1));
   }
   tc.update_indices();

   let mut stopwatch = Stopwatch::start_new();
   tc.run();
   stopwatch.stop();

   println!("tc for {} nodes took {:?}", nodes_count, stopwatch.elapsed());
   println!("path size: {}", tc.path.len());
}

fn main() {
   bench_tc(1000);
}
