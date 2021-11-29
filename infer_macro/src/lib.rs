#![allow(warnings)]
#![allow(unused_imports)]
mod tests;
mod infer_mir;
mod utils;
mod infer_hir;
mod scratchpad;
mod infer_codegen;
mod infer_syntax;
mod test_errors;

#[macro_use]
extern crate quote;

extern crate proc_macro;
use infer_syntax::{InferProgram, desugar_infer_program};
use proc_macro::TokenStream;
use syn::Result;
use crate::{infer_codegen::compile_mir, infer_hir::{compile_infer_program_to_hir}, infer_mir::{compile_hir_to_mir}};

/// The main macro of the infer library. Allows writing logical inference rules similar to Datalog.
/// Example:
/// ```
/// # #[macro_use] extern crate infer_macro;
/// # use infer::infer;
/// infer!{
///   relation edge(i32, i32);
///   relation path(i32, i32);
///   
///   path(*x, *y) <-- edge(x,y);
///   path(*x, *z) <-- edge(x,y), path(y, z);
/// }
/// 
/// fn main() {
///   let mut tc_comp = InferProgram::default();
///   tc_comp.edge = vec![(1,2), (2,3)];
///   tc_comp.update_indices();
///   tc_comp.run();
///   println!("{:?}", tc_comp.path);
/// }
/// ```
/// this macro creates a type named `InferProgram` that can be instantiated using `InferProgram::default()`.
/// The type has a `run()` method, which runs the computation to a fixed point.
#[proc_macro]
pub fn infer(input: TokenStream) -> TokenStream {
   let res = infer_impl(input.into(), false);
   
   match res {
    Ok(res) => res.into(),
    Err(err) => TokenStream::from(err.to_compile_error()),
   }
}


/// Like `infer`, except that invocations of this macro evaluate to an expression containing all the relations
/// defined inside the macro body, and evaluated to a fixed point.
/// The advantage of `infer_run` compared to `infer` is the fact that `infer_run` has access to local variables
/// in scope:
/// ```
/// # #[macro_use] extern crate infer;
/// # use infer::infer_run;
/// let r = vec![(1,2), (2,3)];
/// let r_tc = infer_run!{
///    relation tc(i32, i32);
///    tc(*x, *y) <-- for (x, y) in r.iter();
///    tc(*x, *z) <-- for (x, y) in r.iter(), tc(y, z);
/// }.tc;
///
/// ```
#[proc_macro]
pub fn infer_run(input: TokenStream) -> TokenStream {
   let res = infer_impl(input.into(), true);
   
   match res {
      Ok(res) => res.into(),
      Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

pub(crate) fn infer_impl(input: proc_macro2::TokenStream, is_infer_run: bool) -> Result<proc_macro2::TokenStream> {
   let prog: InferProgram = syn::parse2(input)?;
   // println!("prog relations: {}", prog.relations.len());
   // println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   let prog = desugar_infer_program(prog);
   
   let hir = compile_infer_program_to_hir(&prog)?;
   // println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let mir = compile_hir_to_mir(&hir)?;

   // println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir, is_infer_run);

   Ok(code)
}
