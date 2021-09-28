#![allow(warnings)]
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
use infer_syntax::{InferProgram, desugar_pattern_args};
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};
use proc_macro2::Span;
use syn::{Expr, Ident, Pat, Result, Token, Type, parenthesized, parse::{self, Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Token};
use std::{clone, collections::{HashMap, HashSet}, fmt::Pointer, sync::Mutex};
use quote::{ToTokens, TokenStreamExt};
use itertools::{Either, Itertools};
use derive_syn_parse::Parse;

use crate::{infer_codegen::compile_mir, infer_hir::{compile_infer_program_to_hir}, infer_mir::{compile_hir_to_mir}, utils::{map_punctuated, tuple_type}};


#[proc_macro]
pub fn dl(input: TokenStream) -> TokenStream{
   let res = dl_impl(input.into());
   
   match res {
    Ok(res) => res.into(),
    Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

fn dl_impl(input: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream> {
   let prog: InferProgram = syn::parse2(input)?;
   // println!("prog relations: {}", prog.relations.len());
   // println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   let prog = desugar_pattern_args(prog);
   
   let hir = compile_infer_program_to_hir(&prog)?;
   // println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let mir = compile_hir_to_mir(&hir);

   // println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir);

   Ok(code)
}
