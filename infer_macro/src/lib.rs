#![allow(warnings)]
#![allow(unused_imports)]
mod tests;
mod infer_mir;
mod utils;
mod infer_hir;
mod scratchpad;
mod infer_codegen;
mod infer_syntax;

#[macro_use]
extern crate quote;

extern crate proc_macro;
use infer_syntax::{InferProgram, desugar_infer_program};
use proc_macro::TokenStream;
use syn::Result;
use crate::{infer_codegen::compile_mir, infer_hir::{compile_infer_program_to_hir}, infer_mir::{compile_hir_to_mir}};

#[proc_macro]
pub fn infer(input: TokenStream) -> TokenStream {
   let res = infer_impl(input.into());
   
   match res {
    Ok(res) => res.into(),
    Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

#[proc_macro]
pub fn dl(input: TokenStream) -> TokenStream{
   infer(input)
}

fn infer_impl(input: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream> {
   let prog: InferProgram = syn::parse2(input)?;
   // println!("prog relations: {}", prog.relations.len());
   // println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   let prog = desugar_infer_program(prog);
   
   let hir = compile_infer_program_to_hir(&prog)?;
   // println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let mir = compile_hir_to_mir(&hir);

   // println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir);

   Ok(code)
}
