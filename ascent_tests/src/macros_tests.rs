#![cfg(test)]

use ascent::ascent;

use crate::assert_rels_eq;

#[test]
fn test_macro_in_macro() {
   ascent!{
      relation foo1(i32, i32);
      relation foo2(i32, i32);
      relation bar(i32 , i32);

      foo1(1, 2);
      foo1(2, 1);

      foo2(11, 12);
      foo2(12, 11);

      macro foo($x: ident, $y: ident){
         (foo1($x, $y) || foo2($x, $y)), if $x < $y,
      }

      bar(x, y) <-- foo!(x, y);

      relation baz(i32);
      relation quax(i32);
      baz(1); baz(2);
      baz(11); baz(12);

      quax(y) <-- baz(x), foo!(x, y);
   };
   let mut prog = AscentProgram::default();
   prog.run();
   assert_rels_eq!(prog.bar, [(1, 2), (11, 12)]);
   assert_rels_eq!(prog.quax, [(2,), (12,)]);
}

#[test]
fn test_macro_in_macro2() {

   type Var = String;
   type Val = isize;
   #[derive(Clone, Eq, PartialEq, Hash)]
   enum Atomic {
      Val(Val),
      Var(Var),
   }
   ascent! {
      struct AscentProgram;
      relation σ(Var, Val);
      relation res(Atomic);

      macro ae($x: ident) {
         (res(?Atomic::Var(_var)), σ(_var, $x) ||
          res(?Atomic::Val($x)))
      }

      relation res_val(Val);
      res_val(x) <-- ae!(x);

      relation res_val2(Val, Val);
      res_val2(x, y) <-- ae!(x), ae!(y);

   }

   let mut prog = AscentProgram::default();

   prog.σ = vec![("x1".into(), 100), ("x2".into(), 200)];
   prog.res = vec![(Atomic::Val(1000),), (Atomic::Var("x1".into()),)];
   prog.run();

   println!("res_val: {}\n{:?}" , prog.res_val.len(), prog.res_val);
   println!("res_val2: {}\n{:?}", prog.res_val2.len(), prog.res_val2);

   assert_eq!(prog.res_val2.len(), prog.res_val.len().pow(2));
   assert_rels_eq!(prog.res_val, [(100, ), (1000, )]);
}

#[test]
fn test_macro_in_macro3() {

   ascent! {
      relation edge(i32, i32);
      relation edge_rev(i32, i32);

      macro edge($x: expr, $y: expr) {
         edge($x, $y), edge_rev($y, $x)
      }

      edge!(1, 2);

      edge!(x, x + 1) <-- for x in 0..10;
   }

   let mut prog = AscentProgram::default();
   prog.run();

   assert_eq!(prog.edge.len(), prog.edge_rev.len());
}

#[test]
fn test_macro_in_macro4() {

   ascent! {
      relation foo(i32, i32);
      relation bar(i32, i32);

      macro foo_($x: expr, $y: expr) { foo($x, $y) }

      macro foo($x: expr, $y: expr) {
         let _x = $x, let _y = $y, foo_!(_x, _y)
      }

      foo(0, 1), foo(1, 2), foo(2, 3), foo(3, 4);
      bar(x, y) <-- foo(x, y), foo!(x + 1, y + 1), foo!(x + 2, y + 2), foo!(x + 3, y + 3);
   }

   let mut prog = AscentProgram::default();
   prog.run();

   assert_rels_eq!(prog.bar, [(0, 1)]);
}

#[test]
fn test_macro_in_macro5() {

   type Lang = &'static str;
   type CompilerName = &'static str;
   ascent! {
      relation compiler(CompilerName, Lang, Lang);
      relation bad_compiler(CompilerName);

      relation can_compile_to(Lang, Lang);

      macro compiler($from: expr, $to: expr) {
         compiler(_name, $from, $to), !bad_compiler(_name)
      }

      can_compile_to(a, b) <-- compiler!(a, b);
      can_compile_to(a, c) <-- compiler!(a, b), can_compile_to(b, c);

      relation compiles_in_two_steps(Lang, Lang);
      compiles_in_two_steps(a, c) <-- compiler!(a, b), compiler!(b, c);
   }

   let mut prog = AscentProgram::default();
   prog.compiler = vec![("Rustc", "Rust", "X86"), ("Rustc", "Rust", "WASM"), 
                        ("MyRandomCompiler", "Python", "Rust"),
                        ("Cython", "Python", "C"), ("Clang", "C", "X86")];
   prog.bad_compiler = vec![("MyRandomCompiler",)];
   prog.run();

   println!("can_compile_to: {:?}", prog.can_compile_to);
   println!("compiles_in_two_steps: {:?}", prog.compiles_in_two_steps);

   assert!(prog.can_compile_to.contains(&("Python", "X86")));
   assert!(!prog.can_compile_to.contains(&("Python", "Rust")));
}

#[test]
fn test_macro_in_macro6() {

   ascent! {
      relation foo(i32, i32) = vec![(0, 1), (1, 2), (2, 3), (3, 4)];

      macro foo_rev($y: expr, $x: expr) {
         foo!($x, $y), let x = $x, let y = $y
      }

      macro foo_($x: expr, $y: expr) {
         foo($x, $y)
      }

      macro foo($x: expr, $y: expr) {
         foo_!($x, $y), let x = $x, let y = $y, foo_!(x, y)
      }

      relation baz(i32, i32);
      relation baz_e(i32, i32);

      baz(x, z) <-- foo_rev!(y, x), foo_rev!(z, y);
      baz_e(x, z) <-- foo(x, y), foo(y, z);
   }

   let mut prog = AscentProgram::default();
   prog.run();

   println!("baz: {:?}", prog.baz);
   println!("baz_e: {:?}", prog.baz_e);

   assert_rels_eq!(prog.baz, prog.baz_e);
}

#[test]
fn test_macro_in_macro7() {

   ascent! {
      relation foo(i32, i32) = vec![(0, 1), (1, 2), (2, 3), (3, 4), (4, 6), (3, 7)];
      relation bar(Option<i32>, i32) = vec![(Some(1), 2), (Some(2), 3), (None, 4)];

      macro foo($x: expr, $y: expr) { foo($x, $y), }
      macro bar($x: ident, $y: expr) { bar(?Some($x), $y) }

      macro foo2($x: expr, $y: expr) { 
         foo!($x, $y), let x = $x, for x2 in [1, 2], ((foo(x, x2), let y = $y, let _ = println!("{}", y)) || if true, for y in [$y, $y]), 
         foo!(x + 0, y - 0), foo(x, y), foo!(x, y),
         let z = |x: i32| {x}, foo(z(*x), z(*y))
      }

      relation baz(i32, i32);
      macro baz($x: expr, $y: expr) {baz($x, $y)}
      relation baz_e(i32, i32);

      baz!(x, z) <-- bar!(x, y), foo!(y, z);
      baz!(a, c) <-- bar!(a, b), foo!(b, c);
      baz_e(x, z) <-- bar(?Some(x), y), foo(y, z);
      
      relation quax(i32, i32);
      relation quax_e(i32, i32);

      quax(x, z) <-- foo2!(x, y), foo2!(y, z);
      quax_e(x, z) <-- foo(x, y), foo(y, z);
   }

   let mut prog = AscentProgram::default();
   prog.run();

   println!("baz  : {:?}", prog.baz);
   assert_rels_eq!(prog.baz, prog.baz_e);
   
   println!("quax: {:?}", prog.quax);
   assert_rels_eq!(prog.quax, prog.quax_e);

}

#[test]
fn test_macro_in_macro8() {

   macro_rules! id {
      ($($inp: tt)*) => { $($inp)* };
   }

   ascent! {
      relation foo(i32, i32) = vec![(0, 1), (1, 2), (2, 3), (3, 4), (4, 6), (3, 7)];
      relation bar(Option<i32>, i32) = vec![(Some(1), 2), (Some(2), 3), (None, 4)];

      macro foo2($x: expr, $y: expr) { foo($x, $y), }
      macro bar($x: ident, $y: expr) { bar(?Some($x), $y) }

      macro foo($xx: expr, $yy: expr) { 
         foo2!($xx, $yy), let x = id!($xx), let y = id!($yy), for x2 in [1, 2],
         let _ = assert!(x == $xx && y == $yy),
         foo2!(id!(id!(x) + 0), id!(y - 0)), foo(x, y), foo2!(x, y),
         let z = id!(|x: i32| {x}), foo(z(*x), z(*y))
      }

      relation baz(i32, i32);
      relation baz_e(i32, i32);
      baz(x, z) <-- bar!(x, y), foo!(y, z);
      
      relation baz2(i32, i32);
      baz2(a, c) <-- bar!(a, b), foo!(b, c);

      baz_e(x, z) <-- bar(?Some(x), y), foo(y, z);
   }

   let mut prog = AscentProgram::default();
   prog.run();

   println!("baz  : {:?}", prog.baz);
   assert_rels_eq!(&prog.baz, &prog.baz_e);
   assert_rels_eq!(prog.baz2, prog.baz_e);
}
