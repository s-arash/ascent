#![allow(warnings)]
#![allow(unused_imports)]
mod tests;
mod ascent_mir;
mod utils;
mod ascent_hir;
mod scratchpad;
mod ascent_codegen;
mod ascent_syntax;
mod test_errors;
mod syn_utils;
mod index_selection;
mod index_shape;
mod ascent_hir2;

#[macro_use]
extern crate quote;

extern crate proc_macro;
use ascent_hir2::compile_hir_to_hir2;
use ascent_syntax::{AscentProgram, desugar_ascent_program};
use proc_macro::TokenStream;
use syn::Result;
use crate::{ascent_codegen::compile_mir, ascent_hir::{compile_ascent_program_to_hir}, ascent_mir::{compile_hir_to_mir}};

/// The main macro of the ascent library. Allows writing logical inference rules similar to Datalog.
/// 
/// Example:
/// ```
/// # #[macro_use] extern crate ascent_macro;
/// # use ascent::ascent;
/// ascent!{
///   relation edge(i32, i32);
///   relation path(i32, i32);
///   
///   path(x, y) <-- edge(x,y);
///   path(x, z) <-- edge(x,y), path(y, z);
/// }
/// 
/// fn main() {
///   let mut tc_comp = AscentProgram::default();
///   tc_comp.edge = vec![(1,2), (2,3)];
///   tc_comp.run();
///   println!("{:?}", tc_comp.path);
/// }
/// ```
/// this macro creates a type named `AscentProgram` that can be instantiated using `AscentProgram::default()`.
/// The type has a `run()` method, which runs the computation to a fixed point.
#[proc_macro]
pub fn ascent(input: TokenStream) -> TokenStream {
   let res = ascent_impl(input.into(), false);
   
   match res {
      Ok(res) => res.into(),
      Err(err) => TokenStream::from(err.to_compile_error()),
   }
}


/// Like `ascent`, except that the result of an `ascent_run` invocation is a value containing all the relations
/// defined inside the macro body, and computed to a fixed point.
/// 
/// The advantage of `ascent_run` compared to `ascent` is the fact that `ascent_run` has access to local variables
/// in scope:
/// ```
/// # #[macro_use] extern crate ascent;
/// # use ascent::ascent_run;
/// let r = vec![(1,2), (2,3)];
/// let r_tc = ascent_run!{
///    relation tc(i32, i32);
///    tc(x, y) <-- for (x, y) in r.iter();
///    tc(x, z) <-- for (x, y) in r.iter(), tc(y, z);
/// }.tc;
///
/// ```
#[proc_macro]
pub fn ascent_run(input: TokenStream) -> TokenStream {
   let res = ascent_impl(input.into(), true);
   
   match res {
      Ok(res) => res.into(),
      Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

pub(crate) fn ascent_impl(input: proc_macro2::TokenStream, is_ascent_run: bool) -> Result<proc_macro2::TokenStream> {
   let prog: AscentProgram = syn::parse2(input)?;
   // println!("prog relations: {}", prog.relations.len());
   // println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   let prog = desugar_ascent_program(prog)?;
   
   let hir = compile_ascent_program_to_hir(&prog)?;
   // println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let hir2 = compile_hir_to_hir2(hir);

   let mir = compile_hir_to_mir(&hir2)?;

   // println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir, is_ascent_run);

   Ok(code)
}
