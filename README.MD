# Logic programming in Rust
[![Rust](https://github.com/s-arash/infer/actions/workflows/rust.yml/badge.svg)](https://github.com/s-arash/infer/actions/workflows/rust.yml)

Infer is a logic programming language (similar to Datalog) embedded in Rust via macros.

## Examples

### Computing all the connected nodes in a graph
```Rust
infer!{
   relation edge(i32, i32);
   relation path(i32, i32);
   
   path(x, y) <-- edge(x, y);
   path(x, z) <-- edge(x, y), path(y, z);
}
```

## Using Infer
1. [Install Rust](https://www.rust-lang.org/tools/install).
2. Make a new Rust project:
   ```bash
   cargo new my-infer-project
   cd my-infer-project
   ```
3. Add `infer` as a dependency in `Cargo.toml`:
   ```toml
   [dependencies]
   infer = {git = "https://github.com/s-arash/infer"}
   ```
4. Write some Infer code in `main.rs`. Here is a complete example:
   ```rust
   use infer::infer;
   infer!{
      relation edge(i32, i32);
      relation path(i32, i32);
      
      path(x, y) <-- edge(x, y);
      path(x, z) <-- edge(x, y), path(y, z);
   }

   fn main() {
      let mut prog = InferProgram::default();
      prog.edge = vec![(1, 2), (2, 3)];
      prog.run();
      println!("path: {:?}", prog.path);
   }
   ```
5. Run the program:
   ```bash
   cargo run
   ```
## Features

### Lattices

Infer supports computing fixed points of user-defined lattices. The `lattice` keyword defines a lattice in Infer. The type of the final column of a `lattice` must implement the `Lattice` trait. A `lattice` is like a relation, except that when a new `lattice` fact (v<sub>1</sub>, v<sub>2</sub>, ..., v<sub>(n-1)</sub>, v<sub>n</sub>) is discovered, and a fact  (v<sub>1</sub>, v<sub>2</sub>, ..., v<sub>(n-1)</sub>, v'<sub>n</sub>) is already present in the database, v<sub>n</sub> and v'<sub>n</sub> are `join`ed together to produce a single fact.

This feature enables writing programs not expressible in Datalog. For example we can use this feature to compute the lengths of shortest paths between nodes in a graph.

```Rust
infer!{
   lattice shortest_path(i32, i32, Dual<u32>);
   relation edge(i32, i32, u32);

   shortest_path(x, y, Dual(*w)) <-- edge(x, y, w);

   shortest_path(x, z, Dual(w + l)) <-- 
      edge(x, y, w), 
      shortest_path(y, z, ?Dual(l));
}
```

In this example, `Dual<T>` is the dual of the lattice T. We use `Dual<T>` because we are interested in shortest paths, given two path lengths `l1` and `l2` for any given pair of nodes, we only store `min(l1, l2)`.

### Conditions and Generative clauses
The syntax is designed to be familiar to Rust users. In this example, `edge` is populated with non-reflexive edges from `node`. Note that any type that implements `Clone + Eq + Hash` can be used as a relation column.

```Rust
infer! {
   relation node(i32, Rc<Vec<i32>>);
   relation edge(i32, i32);
   
   edge(x, y) <--
      node(x, neighbors),
      for &y in neighbors.iter(),
      if x != y;
}
```

### Negation and Aggregation
Infer supports stratified negation and aggregation. Aggregators are defined in `infer::aggregators`. You can find `sum`, `min`, `max`, `count`, and `mean` there.

In the following example, the average grade of students is stored in `avg_grade`:

```Rust
use infer::aggregators::*;
type Student = u32;
type Course = u32;
type Grade = u16;
infer! {
   relation student(Student);
   relation course_grade(Student, Course, Grade);
   relation avg_grade(Student, Grade);

   avg_grade(s, avg as Grade) <--
      student(s),
      agg avg = mean(g) in course_grade(s, _, g);
}
```

You can define your own aggregators if the provided aggregators are not sufficient. For example, an aggregator for getting the 2nd highest value of a column can have the following signature: 

```Rust
fn second_highest<'a, N: 'a>(inp: impl Iterator<Item = (&'a N,)>) -> impl Iterator<Item = N>
   where N: Ord + Clone
```
Aggregators can even be parameterized! For an example of a parameterized aggregator, lookup the definition of `percentile` in [`infer::aggregators`](./infer/src/aggregators.rs).

### `infer_run!`

In addition to `infer!`, we provide the `infer_run!` macro. Unlike `infer!`, this macro evaluates the infer program when invoked. The main advantage of `infer_run!` is that local variables are in scope inside the Infer program. For example, we can define a function for discovering triangles in a graph like so:

```Rust
fn triangles(edges: &[(i32, i32)]) -> Vec<(i32, i32, i32)> {
   infer_run!{
      relation edge(i32, i32) = edges.iter().cloned().collect();
      relation triangle(i32, i32, i32);
      triangle(x, y, z) <-- edge(x, y), edge(y, z), edge(z, x);
   }.triangle
}
```
In the above example, we initialize the `edge` relation directly to shorten the program.