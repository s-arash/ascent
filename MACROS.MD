# macros!

Macros can be defined inside Ascent programs to avoid having to repeat code.

The syntax resembles (unstable) Rust macros 2.0.

Example: Finding out what languages can be compiled to what languages *competently*.
```Rust
type Lang = &'static str;
type Compiler = &'static str;
ascent! {
   relation compiler(Compiler, Lang, Lang);
   relation bad_compiler(Compiler);

   relation can_compile_to(Lang, Lang);

   macro compiler($from: expr, $to: expr) {
      compiler(name, $from, $to), !bad_compiler(name)
   }

   can_compile_to(a, b) <-- compiler!(a, b);
   can_compile_to(a, c) <-- compiler!(a, b), can_compile_to(b, c);
}
```

Identifiers bound in macro bodies, like `name` in the example above, remain private to the macro invocation. For example, to find out what languages can be compiled in two steps, we can add the following:
```Rust
ascent! {
   //...
   relation compiles_in_two_steps(Lang, Lang);
   compiles_in_two_steps(a, c) <-- compiler!(a, b), compiler!(b, c);
}
```
And it won't require the `a -> b` compiler to be the same as the `b -> c` compiler!