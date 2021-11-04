#![allow(dead_code)]

use std::time::Instant;
use stopwatch::Stopwatch;
use infer::infer;
use infer::lattice::Dual;

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

fn test_dl_lattice1(){
   infer!{
      lattice shortest_path(i32, i32, Dual<u32>);
      relation edge(i32, i32, u32);

      shortest_path(*x, *y, Dual(*w)) <-- edge(x, y, w);
      shortest_path(*x, *z, Dual(w + l.0)) <-- edge(x, y, w), shortest_path(y, z, l);

      edge(1, 2, x + 30)  <-- for x in 0..10000;
      edge(2, 3, x + 50)  <-- for x in 0..10000;
      edge(1, 3, x + 40)  <-- for x in 0..10000;
      edge(2, 4, x + 100) <-- for x in 0..10000;
      edge(1, 4, x + 200) <-- for x in 0..10000;
   }
   let mut prog = InferProgram::default();
   prog.run();
   // println!("shortest_path ({} tuples):", prog.shortest_path.len());
   //println!("{:?}", prog.shortest_path);
   for _i in prog.shortest_path.iter() {

   }
   // println!("{}", InferProgram::summary());
   // assert!(rels_equal(prog.shortest_path, [(1,2, Dual(30)), (1, 3, Dual(40)), (1,4, Dual(130)), (2,3, Dual(50)), (2, 4, Dual(100))]))
}

fn bench_lattice(){
   let iterations = 100;
   let before = Instant::now();
   for _ in 0..iterations {
      test_dl_lattice1();
   }
   let elapsed = before.elapsed();
   println!("average time: {:?}", elapsed / iterations);
}


fn bench_tc_path_join_path(nodes_count: i32) {
   infer! {
      struct TCPathJoinPath;
      relation edge(i32, i32);
      relation path(i32, i32);
      path(*x, *y) <-- edge(x,y);
      path(*x, *z) <-- path(x,y), path(y, z);
   }
   let mut tc = TCPathJoinPath::default();
   println!("{}", TCPathJoinPath::summary());

   for i in 0..nodes_count {
      tc.edge.push((i, i + 1));
   }
   tc.update_indices();

   let mut stopwatch = Stopwatch::start_new();
   tc.run();
   stopwatch.stop();

   println!("tc path_join_path for {} nodes took {:?}", nodes_count, stopwatch.elapsed());
   println!("path size: {}", tc.path.len());
}

fn main() {
   // bench_tc(1000);
   // bench_tc_path_join_path(1000);
   bench_lattice();
}
