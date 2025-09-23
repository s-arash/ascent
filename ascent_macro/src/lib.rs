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

use ascent_syntax::{AscentProgram, desugar_ascent_program, parse_ascent_program};
use derive_syn_parse::Parse;
use itertools::Either;
use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::parse::{ParseStream, Parser};
use syn::spanned::Spanned;
use syn::{Attribute, Ident, Result, Token, parse_quote_spanned};
use syn_utils::ResTokenStream2Ext;

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
   ascent_impl(input.into(), AscentMacroKind { is_ascent_run: false, is_parallel: false }).into_token_stream()
}

/// Similar to `ascent`, allows writing logic programs in Rust.
///
/// The difference is that `ascent_par` generates parallelized code.
#[proc_macro]
pub fn ascent_par(input: TokenStream) -> TokenStream {
   ascent_impl(input.into(), AscentMacroKind { is_ascent_run: false, is_parallel: true }).into_token_stream()
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
   ascent_impl(input.into(), AscentMacroKind { is_ascent_run: true, is_parallel: false }).into_token_stream()
}

/// The parallelized version of `ascent_run`
#[proc_macro]
pub fn ascent_run_par(input: TokenStream) -> TokenStream {
   ascent_impl(input.into(), AscentMacroKind { is_ascent_run: true, is_parallel: true }).into_token_stream()
}

/// This macro allows writing Ascent code that can later be included in an actual Ascent program.
///
/// In an Ascent program, you can include the contents of an `ascent_source` using the `include_source!(path);`
/// syntax. It will look like the following:
/// ```
/// # #[macro_use] extern crate ascent;
/// let capatures = ...; // ...
/// mod my_ascent_sources {
///   ascent_source! { secret_sauce (capatures, ...):
///     // secret Ascent code ...
///   }
/// }
/// // somewhere else, we define an actual Ascent program:
/// ascent! {
///   include_source!(my_ascent_sources::secret_sauce, capatures, ...);
///   // More Ascent code ...
/// }
/// ```
/// ascent_source will compile the included source code to a deferred macro definition, where code after `:`
/// will become the body of compiled macro.
/// To avoid hygiene issues, we provide additional arguments to the `ascent_source` macro before `:`.
/// These arguments are bound to the macro variables in the code after `:`. You can use `$` to refer to them.
/// By providing these arguments as additional arguments to the `include_source!` macro, these macro variables
/// are bounded to the actual variable outside of the `ascent_source` macro.
///
/// # Be warned!
/// This feature comes with all the caveats of C's `#include`, or Rust's `include!()` macro:
/// when `include_source!` is used, it is as if the contents of the included `ascent_source` was copy-pasted
/// into the containing Ascent program. This means every type, function, etc. will be resolved relative to
/// the containing Ascent program, and **not** relative to where the included `ascent_source` is defined.
///
/// # Example
/// ```
/// # #[macro_use] extern crate ascent;
/// mod base {
///    ascent::ascent_source! {
///       /// Defines `edge` and `path`, the transitive closure of `edge`
///       tc ():
///       relation edge(usize, usize);
///       relation path(usize, usize);
///       path(x, y) <-- edge(x, y);
///       path(x, z) <-- edge(x, y), path(y, z);
///    }
/// }
///
/// ascent! {
///    struct Tc;
///    include_source!(base::tc);
/// }
///
/// ascent! {
///    struct Rtc;
///    include_source!(base::tc);
///    path(x, x), path(y, y) <-- edge(x, y);  
/// }
///
/// ascent_par! {
///    struct ParallelTc;
///    include_source!(base::tc);
/// }
/// ```
/// # Example for binding macro variables
/// ```
/// mod foo_mod {
/// ascent_source! { foo_gen (z):
///    foo(x, y) <-- foo(y, x), let _ = $z;
/// }
/// }
/// ascent_run! {
///    relation foo(i32, i32);
///    include_source!(foo_mod::foo_gen, z);
/// }
/// ```
#[proc_macro]
pub fn ascent_source(input: TokenStream) -> TokenStream { ascent_source_impl(input.into()).into_token_stream() }

fn ascent_source_impl(input: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream> {
   #[derive(Parse)]
   struct AscentSourceInput {
      #[call(Attribute::parse_outer)]
      attrs: Vec<Attribute>,
      name: Ident,
      #[paren]
      _arg_paren: syn::token::Paren,
      #[inside(_arg_paren)]
      #[call(syn::punctuated::Punctuated::parse_terminated)]
      caps: syn::punctuated::Punctuated<Ident, Token![,]>,
      _colon: Token![:],
      ascent_code: proc_macro2::TokenStream,
   }

   let AscentSourceInput { attrs, name, _arg_paren, caps, _colon, ascent_code } = syn::parse2(input.into())?;

   let caps : Vec<Ident> = caps.into_iter().collect();
   // WARNING: comment out for now because I allow `$` in the ascent_source for binding macro variables
   // match Parser::parse2(
   //    |input: ParseStream| parse_ascent_program(input, parse_quote!(::ascent::ascent_source)),
   //    ascent_code.clone(),
   // )? {
   //    itertools::Either::Left(_prog) => (),
   //    itertools::Either::Right(mut include_source_call) => {
   //       let before_tokens = include_source_call.before_tokens;
         
   //       include_source_call.before_tokens = quote! {
   //          #(#attrs)*
   //          #name (#(#caps),*) #_colon
   //          #before_tokens
   //       };
   //       // This allows transitive `include_source`s. Disabled for now ...
   //       // return Ok(include_source_call.macro_call_output())
   //       return Err(syn::Error::new(
   //          include_source_call.include_node.include_source_kw.span,
   //          "`ascent_source`s cannot contain `include_source!`",
   //       ))
   //    },
   // };

   if let Some(bad_attr) = attrs.iter().find(|attr| attr.path().get_ident().map_or(true, |ident| ident != "doc")) {
      return Err(syn::Error::new(bad_attr.span(), "unexpected attribute. Only `doc` attribute is allowed"))
   }

   let macro_name = Ident::new(&format!("ascent_source_{name}"), name.span());

   Ok(quote! {
      #(#attrs)*
      #[macro_export]
      macro_rules! #macro_name {
         ( (#($#caps:ident),*), {$($cb: tt)*}, {$($before: tt)*}, {$($after: tt)*}) => {
            $($cb)*! {
               $($before)*
               #ascent_code
               $($after)*
            }
         }
      }
      pub use #macro_name as #name;
   })
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

   pub fn macro_path(&self, span: Span) -> syn::Path {
      let name_ident = Ident::new(self.name(), span);
      parse_quote_spanned! {span=>
         ::ascent::#name_ident
      }
   }
}

pub(crate) fn ascent_impl(input: proc_macro2::TokenStream, kind: AscentMacroKind) -> Result<proc_macro2::TokenStream> {
   let AscentMacroKind { is_ascent_run, is_parallel } = kind;
   let prog = match Parser::parse2(
      |input: ParseStream| parse_ascent_program(input, kind.macro_path(Span::call_site())),
      input,
   )? {
      Either::Left(prog) => prog,
      Either::Right(include_source_call) => return Ok(include_source_call.macro_call_output()),
   };

   let prog = desugar_ascent_program(prog)?;

   let hir = compile_ascent_program_to_hir(&prog, is_parallel)?;

   let mir = compile_hir_to_mir(&hir)?;

   let code = compile_mir(&mir, is_ascent_run);

   Ok(code)
}
