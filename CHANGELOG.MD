## 0.7.0

- Fixed unsoundness in `util::update` (#41)
- Moved `ascent_par!` and required dependencies behind Cargo feature `par` (enabled by default)

## 0.6.0

- BYODS (Bring Your Own Data Structures to Datalog): this version allows relations in Ascent programs to
be backed by custom data structures (for improved algorithmic efficiency, modified semantics, etc.). 
- Introduction of the crate [`ascent-byods-rels`](https://crates.io/crates/ascent-byods-rels), which contains a number of such data structures (for equivalence and transitive relations).

- Introduction of `impl` directives with different bounds from `struct` declarations for generic parameters (#25)

## 0.5.0

- Parallel evaluation: with introduction of `ascent_par!` and `ascent_run_par!` macros, Ascent programs can utilize multiple CPU cores to speed up evaluation.

- Made improvements to handling of rules that start with `if`, `let`, or `for` (they can be simple joins).

## 0.4.0
- Reduced the size of generated code for a rule with N dynamic relations from O(N * 2^N) to O(N^2); 
  dramatically improving compile times for some programs.

## 0.3.0
- Introduced macro-in-macros!

## 0.2.1
- Bug fix

## 0.2.0
- Added run_timeout
- Disallowed shadowed variables