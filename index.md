# Logic programming in Rust
[![Rust](https://github.com/s-arash/ascent/actions/workflows/rust.yml/badge.svg)](https://github.com/s-arash/ascent/actions/workflows/rust.yml)

Ascent is a logic programming language (similar to Datalog) embedded in Rust via macros.

## Example

### Computing all the connected nodes in a graph
```Rust
ascent!{
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
   ascent = {git = "https://github.com/s-arash/ascent"}
   ```
4. Write some Ascent code in `main.rs`. Here is a complete example:
   ```rust
   use ascent::ascent;
   ascent!{
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
## Ascent features
Visit the [Ascent repository page](https://github.com/s-arash/ascent) for an overview of Ascent features. 
