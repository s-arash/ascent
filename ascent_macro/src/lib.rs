#![allow(clippy::useless_format, clippy::redundant_static_lifetimes, clippy::get_first)]
#![cfg_attr(not(test), deny(unused_crate_dependencies))]
mod tests;
mod ascent_mir;
mod utils;
mod ascent_hir;
mod scratchpad;
mod ascent_codegen;
mod ascent_syntax;
mod test_errors;
mod syn_utils;

#[macro_use]
extern crate quote;

extern crate proc_macro;
use ascent_syntax::{AscentProgram, desugar_ascent_program};
use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{parse_quote, Ident, Result};
use syn::parse::{ParseStream, Parser};

use crate::ascent_codegen::compile_mir;
use crate::ascent_hir::compile_ascent_program_to_hir;
use crate::ascent_mir::compile_hir_to_mir;

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
   let res = ascent_impl(input.into(), AscentMacroKind { is_ascent_run: false, is_parallel: false });

   match res {
      Ok(res) => res.into(),
      Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

/// Similar to `ascent`, allows writing logic programs in Rust.
///
/// The difference is that `ascent_par` generates parallelized code.
#[proc_macro]
pub fn ascent_par(input: TokenStream) -> TokenStream {
   let res = ascent_impl(input.into(), AscentMacroKind { is_ascent_run: false, is_parallel: true });

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
   let res = ascent_impl(input.into(), AscentMacroKind { is_ascent_run: true, is_parallel: false });

   match res {
      Ok(res) => res.into(),
      Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

/// The parallelized version of `ascent_run`
#[proc_macro]
pub fn ascent_run_par(input: TokenStream) -> TokenStream {
   let res = ascent_impl(input.into(), AscentMacroKind { is_ascent_run: true, is_parallel: true });

   match res {
      Ok(res) => res.into(),
      Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct AscentMacroKind {
   pub is_ascent_run: bool,
   pub is_parallel: bool,
}

impl AscentMacroKind {
   pub fn name(&self) -> &'static str {
      match (self.is_ascent_run, self.is_parallel) {
         (false, false) => "ascent",
         (false, true) => "ascent_par",
         (true, false) => "ascent_run",
         (true, true) => "ascent_run_par",
      }
   }

   pub fn macro_path(&self) -> syn::Path {
      let name_ident = Ident::new(self.name(), Span::call_site());
      parse_quote! {
         ::ascent::#name_ident
      }
   }
}

pub(crate) fn ascent_impl(
   input: proc_macro2::TokenStream, kind : AscentMacroKind,
) -> Result<proc_macro2::TokenStream> {
   let AscentMacroKind { is_ascent_run, is_parallel } = kind;
   // let prog: AscentProgram = syn::parse2(input)?;
   let prog = match Parser::parse2(move |input: ParseStream| ascent_syntax::parse_ascent_program(input, kind), input)? {
      itertools::Either::Left(prog) => prog,
      itertools::Either::Right(macro_back) => {
         println!("{} output to include ascent source:\n{}", kind.name(), macro_back.0);
         return Ok(macro_back.0)
      },
   };
   // println!("prog relations: {}", prog.relations.len());
   // println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   let prog = desugar_ascent_program(prog)?;

   let hir = compile_ascent_program_to_hir(&prog, is_parallel)?;
   // println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let mir = compile_hir_to_mir(&hir)?;

   // println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir, is_ascent_run);

   Ok(code)
}
