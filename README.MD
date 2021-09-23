# Logic programming in Rust
[![Rust](https://github.com/s-arash/infer/actions/workflows/rust.yml/badge.svg)](https://github.com/s-arash/infer/actions/workflows/rust.yml)

**Example** Computing all the connected nodes in a graph:
```Rust
dl!{
   relation edge(i32, i32);
   relation path(i32, i32);
   
   path(*x, *y) <-- edge(x,y);
   path(*x, *z) <-- edge(x,y), path(y, z);
}
```