#![cfg(test)]
use crate::{AscentMacroKind, ascent_impl};

#[test]
fn test_agg_not_stratifiable() {
   let inp = quote! {
      relation foo(i32, i32, i32);
      relation bar(i32, i32);
      relation baz(i32);

      baz(x) <--
         foo(x, _, _),
         !bar(_, x);

      bar(x, x + 1) <-- baz(x);
   };
   let res = ascent_impl(inp, AscentMacroKind::default());
   println!("res: {:?}", res);
   assert!(res.is_err());
   assert!(res.unwrap_err().to_string().contains("bar"));
}

#[test]
fn test_agg_unbound_var() {
   let inp = quote! {
      relation student_grade(u64, u8, i32);
      relation student_avg_grade(u64, i32);
      student_avg_grade(student, grade) <-- agg avg = average(gradeX) in student_grade(student, _, grade);
   };
   let err = ascent_impl(inp, AscentMacroKind::default()).unwrap_err();
   println!("error: {err}");
}
