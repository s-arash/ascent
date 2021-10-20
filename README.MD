# Logic programming in Rust
[![Rust](https://github.com/s-arash/infer/actions/workflows/rust.yml/badge.svg)](https://github.com/s-arash/infer/actions/workflows/rust.yml)

## Examples

**Computing all the connected nodes in a graph**
```Rust
infer!{
   relation edge(i32, i32);
   relation path(i32, i32);
   
   path(*x, *y) <-- edge(x,y);
   path(*x, *z) <-- edge(x,y), path(y, z);
}
```

**Computing shortest paths in a graph** 

Notice the use of `lattice`. Infer works with arbitrary lattices. In this example, `Dual<T>` is the dual of the lattice T. We use `Dual<T>` because we are interested in shortest paths, given two path lengths `l1` and `l2` for any given pair of nodes, we only store the smaller one.
```Rust
infer!{
   lattice shortest_path(i32, i32, Dual<u32>);
   relation edge(i32, i32, u32);

   shortest_path(*x, *y, Dual(*w)) <-- edge(x, y, w);
   shortest_path(*x, *z, Dual(w + l.0)) <-- edge(x, y, w), shortest_path(y, z, l);
}
```