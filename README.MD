# Logic programming in Rust
[![Rust](https://github.com/s-arash/infer/actions/workflows/rust.yml/badge.svg)](https://github.com/s-arash/infer/actions/workflows/rust.yml)

Infer is a logic programming language (similar to Datalog) embedded in Rust via macros.

## Examples

**Computing all the connected nodes in a graph**
```Rust
infer!{
   relation edge(i32, i32);
   relation path(i32, i32);
   
   path(x, y) <-- edge(x, y);
   path(x, z) <-- edge(x, y), path(y, z);
}
```

**Computing shortest paths in a graph** 

Infer supports computing fixed points of user-defined lattices. In this example we use this feature to compute the lengths of shortest paths between nodes in a graph.

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

Notice the `lattice` keyword. Infer works with arbitrary lattices (any type that implements the `Lattice` trait). In this example, `Dual<T>` is the dual of the lattice T. We use `Dual<T>` because we are interested in shortest paths, given two path lengths `l1` and `l2` for any given pair of nodes, we only store `min(l1, l2)`.

## Using Infer
1. [Insall Rust](https://www.rust-lang.org/tools/install).
2. Make a new Rust project:
   ```bash
   cargo new my-infer-project
   cd my-infer-project
   ```
3. Edit `Cargo.toml` to add `infer` as a dependency:
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
      prog.edge = vec![(1,2), (2, 3)];
      prog.run();
      println!("path: {:?}", prog.path);
   }
   ```
5. Run the program:
   ```bash
   cargo run
   ```