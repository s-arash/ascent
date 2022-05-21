### macros!

Macros can be defined inside Ascent programs to avoid having to repeat code.

The syntax resembels (unstable) Rust macros 2.0.

Example: Finding out what languages can be compiled to what languages *competently*.
```Rust
type Lang = &'static str;
type Compiler = &'static str;
ascent! {
   relation compiler(Compiler, Lang, Lang);
   relation bad_compiler(Compiler);

   relation can_compile_to(Lang, Lang);

   macro compiler($from: expr, $to: expr) {
      compiler(_name, $from, $to), !bad_compiler(_name)
   }

   can_compile_to(a, b) <-- compiler!(a, b);
   can_compile_to(a, c) <-- compiler!(a, b), can_compile_to(b, c);
}
```