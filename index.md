# Logic programming in Rust
[![Rust](https://github.com/s-arash/ascent/actions/workflows/rust.yml/badge.svg)](https://github.com/s-arash/ascent/actions/workflows/rust.yml) [![Crates.io](https://img.shields.io/crates/v/ascent?color=blue)](https://crates.io/crates/ascent)

Ascent is a logic programming language (similar to Datalog) embedded in Rust via macros.

For more information, check out [the CC paper](https://s-arash.github.io/ascent/cc22main-p95-seamless-deductive-inference-via-macros.pdf) on Ascent.

In addition, [this OOPSLA paper](https://dl.acm.org/doi/pdf/10.1145/3622840) describes the "Bring Your Own Data Structures to Datalog" aspect of Ascent.

## Examples

### Computing all the connected nodes in a graph
```Rust
ascent! {
   relation edge(i32, i32);
   relation path(i32, i32);
   
   path(x, y) <-- edge(x, y);
   path(x, z) <-- edge(x, y), path(y, z);
}
```

## Using Ascent
1. [Install Rust](https://www.rust-lang.org/tools/install).
2. Make a new Rust project:
   ```bash
   cargo new my-ascent-project
   cd my-ascent-project
   ```
3. Add `ascent` as a dependency in `Cargo.toml`:
   ```toml
   [dependencies]
   ascent = "*"
   ```
4. Write some Ascent code in `main.rs`. Here is a complete example:
   ```rust
   use ascent::ascent;
   ascent! {
      relation edge(i32, i32);
      relation path(i32, i32);
      
      path(x, y) <-- edge(x, y);
      path(x, z) <-- edge(x, y), path(y, z);
   }

   fn main() {
      let mut prog = AscentProgram::default();
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

Ascent supports computing fixed points of user-defined lattices. The `lattice` keyword defines a lattice in Ascent. The type of the final column of a `lattice` must implement the `Lattice` trait. A `lattice` is like a relation, except that when a new `lattice` fact (v<sub>1</sub>, v<sub>2</sub>, ..., v<sub>(n-1)</sub>, v<sub>n</sub>) is discovered, and a fact  (v<sub>1</sub>, v<sub>2</sub>, ..., v<sub>(n-1)</sub>, v'<sub>n</sub>) is already present in the database, v<sub>n</sub> and v'<sub>n</sub> are `join`ed together to produce a single fact.

This feature enables writing programs not expressible in Datalog. For example we can use this feature to compute the lengths of shortest paths between nodes in a graph.

```Rust
ascent! {
   lattice shortest_path(i32, i32, Dual<u32>);
   relation edge(i32, i32, u32);

   shortest_path(x, y, Dual(*w)) <-- edge(x, y, w);

   shortest_path(x, z, Dual(w + l)) <-- 
      edge(x, y, w), 
      shortest_path(y, z, ?Dual(l));
}
```

In this example, `Dual<T>` is the dual of the lattice `T`. We use `Dual<T>` because we are interested in shortest paths, given two path lengths `l1` and `l2` for any given pair of nodes, we only store `min(l1, l2)`.

### Conditions and Generative Clauses
The syntax is designed to be familiar to Rust users. In this example, `edge` is populated with non-reflexive edges from `node`. Note that any type that implements `Clone + Eq + Hash` can be used as a relation column.

```Rust
ascent! {
   relation node(i32, Rc<Vec<i32>>);
   relation edge(i32, i32);
   
   edge(x, y) <--
      node(x, neighbors),
      for &y in neighbors.iter(),
      if x != y;
}
```

### Negation and Aggregation
Ascent supports stratified negation and aggregation. Aggregators are defined in `ascent::aggregators`. You can find `sum`, `min`, `max`, `count`, and `mean` there.

In the following example, the average grade of students is stored in `avg_grade`:

```Rust
use ascent::aggregators::*;
type Student = u32;
type Course = u32;
type Grade = u16;
ascent! {
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
Aggregators can even be parameterized! For an example of a parameterized aggregator, lookup the definition of `percentile` in [`ascent::aggregators`](./ascent/src/aggregators.rs).

### Parallel Ascent

Ascent is capable of producing parallel code. The macros `ascent_par!` and `ascent_run_par!` produce parallelized code. 
Naturally, column types must be `Send + Sync` to work in parallel Ascent. 

Parallel Ascent utilizes [`rayon`](https://crates.io/crates/rayon), so the parallelism level can be controlled either via `rayon`'s [`ThreadPoolBuilder`](https://docs.rs/rayon/latest/rayon/struct.ThreadPoolBuilder.html) or using the `RAYON_NUM_THREADS` environment variable (see [here](https://github.com/rayon-rs/rayon/blob/master/FAQ.md#how-many-threads-will-rayon-spawn) for more info).
### `ascent_run!`

In addition to `ascent!`, we provide the `ascent_run!` macro. Unlike `ascent!`, this macro evaluates the ascent program when invoked. The main advantage of `ascent_run!` is that local variables are in scope inside the Ascent program. For example, we can define a function for discovering the (optionally reflexive) transitive closure of a relation like so:

```Rust
fn tc(r: Vec<(i32, i32)>, reflexive: bool) -> Vec<(i32, i32)> {
   ascent_run! {
      relation r(i32, i32) = r;
      relation tc(i32, i32);
      tc(x, y) <-- r(x, y);
      tc(x, z) <-- r(x, y), tc(y, z);
      tc(x, x), tc(y, y) <-- if reflexive, r(x, y);
   }.tc
}
```
In the above example, we initialize the relation `r` directly to shorten the program.


### Macro definitions
It may be useful to define macros that expand to either body items or head items. Ascent allows you to do this.

You can find more about macros in Ascent macros [here](MACROS.MD).

### BYODS
BYODS (short for Bring Your Own Data Structures to Datalog) is an extension of Ascent that enables relations to be backed by custom data structures. This feature allows improving the algorithmic complexity of Ascent programs by optimizing the data structures used to back relations. For example, a program that requires transitive relation computation of a large graph could improve its performance by choosing a union-find based data structure for the transitive closure relation:

```Rust
ascent! {
   #[ds(trrel_uf)]
   relation path(Node, Node);
   
   path(x, y) <-- edge(x, y);
}
```

The `#[ds(trrel_uf)]` attibute directs the Ascent compiler to use the data structure provider defined in the module `trrrel_uf` for the `path` relation. See [BYODS.MD](BYODS.MD) for more information on BYODS.

(Note: custom data structure providers like `trrel_uf` have not been merged into the master branch yet.)


### Misc
- **`#![measure_rule_times]`** causes execution times of individual rules to be measured. Example: 
   ```Rust
   ascent! {
      #![measure_rule_times]
      // ...
   }
   let mut prog = AscentProgram::default();
   prog.run();
   println!("{}", prog.scc_times_summary());
   ```
   Note that **`scc_times_summary()`** is generated for all Ascent programs. With `#![measure_rule_times]`
   it reports execution times of individual rules too.

- With **`#![generate_run_timeout]`**, a `run_timeout` function is generated that stops after
  the given amount of time.

- The feature **`wasm-bindgen`** allows Ascent programs to run in WASM environments.

- **`struct`** declarations can be added to the top of `ascent!` definitions. This allows changing the
  name and visibility of the generated type and introduction of type/lifetime parameters and constraints.
   ```Rust
   ascent! {
      struct GenericTC<N: Clone + Eq + Hash>;
      relation edge(N, N);
      // ...
   }
   ```

   *Hint*: If you get a "private type ... in public interface (error E0446)" warning, you can fix it by
   making the generated Ascent type private, as done in the above example.