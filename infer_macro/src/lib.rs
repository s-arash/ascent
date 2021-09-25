#![allow(warnings)]
mod tests;
mod infer_mir;
mod utils;
mod infer_hir;
mod scratchpad;
mod infer_codegen;

extern crate proc_macro;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};
use proc_macro2::Span;
use syn::{Expr, Ident, Pat, Result, Token, Type, parenthesized, parse::{self, Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Token};
use std::{clone, collections::{HashMap, HashSet}, fmt::Pointer, sync::Mutex};
#[macro_use]
extern crate quote;
use quote::{ToTokens, TokenStreamExt};
use itertools::{Either, Itertools};
use derive_syn_parse::Parse;

use crate::{infer_codegen::compile_mir, infer_hir::{compile_infer_program_to_hir}, infer_mir::{compile_hir_to_mir}, utils::{map_punctuated, tuple_type}};

// resources:
// https://blog.rust-lang.org/2018/12/21/Procedural-Macros-in-Rust-2018.html
// https://github.com/dtolnay/syn/blob/master/examples/lazy-static/lazy-static/src/lib.rs
// https://crates.io/crates/quote
// example: https://gitlab.gnome.org/federico/gnome-class/-/blob/master/src/parser/mod.rs

mod kw {
   syn::custom_keyword!(relation);
   // syn::custom_keyword!(when);

   // syn::custom_keyword!(<--);
}

// #[derive(Clone)]
struct RelationNode{
   name: Ident,
   field_types : Punctuated<Type, Token![,]>,
   semi_colon: Token![;],
}
impl Parse for RelationNode {
   fn parse(input: ParseStream) -> Result<Self> {
      input.parse::<kw::relation>()?;
      let name : Ident = input.parse()?;
      let content;
      parenthesized!(content in input);
      let field_types = content.parse_terminated(Type::parse)?;
      let semi_colon = input.parse::<Token![;]>()?;
      Ok(RelationNode{name, field_types, semi_colon})
   }
}

#[derive(Parse)]
enum BodyItemNode {
   #[peek(Token![for], name = "GeneratorNode")]
   Generator(GeneratorNode),
   #[peek(Ident, name = "BodyClauseNode")]
   Clause(BodyClauseNode),
}



#[derive(Parse, Clone)]
struct GeneratorNode {
   for_keyword: Token![for],
   pattern: Pat,
   in_keyword: Token![in],
   expr: Expr
}

// #[derive(Debug)]
struct BodyClauseNode {
   rel : Ident,
   args : Punctuated<BodyClauseArg, Token![,]>,
   cond_clauses: Vec<CondClause>
}

#[derive(Parse, Clone, PartialEq, Eq)]
enum BodyClauseArg {
   #[peek(Token![?], name = "Pattern arg")]
   Pat(ClauseArgPattern),
   #[peek_with(|x| true, name = "Expression arg")]
   Expr(Expr),
}

impl BodyClauseArg {
   fn unwrap_expr(self) -> Expr {
      match self {
         Self::Expr(exp) => exp,
         Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr")
      }
   }

   fn unwrap_expr_ref(&self) -> &Expr {
      match self {
         Self::Expr(exp) => exp,
         Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr")
      }
   }
}

#[derive(Parse, Clone, PartialEq, Eq)]
struct ClauseArgPattern {
   huh_token: Token![?],
   pattern : Pat,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
struct IfLetClause {
   if_keyword: Token![if],
   let_keyword: Token![let],
   pattern: syn::Pat,
   eq_symbol : Token![=],
   exp: syn::Expr,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
struct IfClause {
   if_keyword: Token![if],
   cond: Expr 
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum CondClause {
   IfLet(IfLetClause),
   If(IfClause),
}

impl Parse for CondClause {
   fn parse(input: ParseStream) -> Result<Self> {
      if input.peek(Token![if]) {
         if input.peek(Token![let]) {
            let cl: IfLetClause = input.parse()?;
            return Ok(Self::IfLet(cl));
         } else {
            let cl: IfClause = input.parse()?;
            return Ok(Self::If(cl));
         }
      } else {
         Err(input.error("expected either if clause or if let clause"))
      }
   }
}

// impl ToTokens for BodyClauseNode {
//    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
//       self.rel.to_tokens(tokens);
//       self.args.to_tokens(tokens);
//    }
// }

impl Parse for BodyClauseNode{
   fn parse(input: ParseStream) -> Result<Self> {
      let rel : Ident = input.parse()?;
      let args_content;
      parenthesized!(args_content in input);
      let args = args_content.parse_terminated(BodyClauseArg::parse)?;
      let mut cond_clauses = vec![];
      while let Ok(cl) = input.parse(){
         cond_clauses.push(cl);
      }
      Ok(BodyClauseNode{rel, args, cond_clauses})
   }
}

// #[derive(Debug)]
struct HeadClauseNode {
   rel : Ident,
   args : Punctuated<Expr, Token![,]>,
}
impl ToTokens for HeadClauseNode {
   fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      self.rel.to_tokens(tokens);
      self.args.to_tokens(tokens);
   }
}

impl Parse for HeadClauseNode{
   fn parse(input: ParseStream) -> Result<Self> {
      let rel : Ident = input.parse()?;
      let args_content;
      parenthesized!(args_content in input);
      let args = args_content.parse_terminated(Expr::parse)?;
      Ok(HeadClauseNode{rel, args})
   }
}

struct RuleNode {
   head_clause: HeadClauseNode,
   body_items: Punctuated<BodyItemNode, Token![,]>,
}

// impl ToTokens for RuleNode {
//    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
//       self.head_clause.to_tokens(tokens);
//       self.body_items.to_tokens(tokens);
//    }
// }

impl Parse for RuleNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let head_clause : HeadClauseNode = input.parse()?;

      if input.peek(Token![;]){
         // println!("fact rule!!!");
         input.parse::<Token![;]>()?;
         Ok(RuleNode{head_clause, body_items: Punctuated::default()})
      } else {
         input.parse::<Token![<]>()?;
         input.parse::<Token![-]>()?;
         input.parse::<Token![-]>()?;
         let body_items = Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty(input)?;
         input.parse::<Token![;]>()?;
         Ok(RuleNode{ head_clause, body_items})
      }
   }
}

// #[derive(Clone)]
pub(crate) struct InferProgram {
   rules : Vec<RuleNode>,
   relations : Vec<RelationNode>
}

impl Parse for InferProgram {
   fn parse(input: ParseStream) -> Result<Self> {
      let mut rules = vec![];
      let mut relations = vec![];
      while !input.is_empty() {
         if let Ok(rel) = RelationNode::parse(input) {
            relations.push(rel);
         } else if let Ok(rule) = RuleNode::parse(input) {
            rules.push(rule);
         } else {
            return Err(input.error("bad dl syntax!"));
         }
      }
      Ok(InferProgram{rules, relations})
   }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct RelationIdentity {
   name: Ident,
   field_types: Vec<Type>
}

impl From<&RelationNode> for RelationIdentity{
   fn from(relation_node: &RelationNode) -> Self {
      RelationIdentity {
         name: relation_node.name.clone(),
         field_types: relation_node.field_types.iter().cloned().collect()
      }
   }
} 


fn ir_name_for_rel_indices(rel: &Ident, indices: &[usize]) -> Ident {
   let name = format!("{}_indices_{}", rel, indices.iter().join("_"));
   Ident::new(&name, rel.span())
}

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
   
   let hir = compile_infer_program_to_hir(&prog);
   // println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let mir = compile_hir_to_mir(&hir);

   // println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir);

   Ok(code)
}

fn expr_to_ident(expr: &Expr) -> Option<Ident> {
   match expr {
      Expr::Path(p) if p.path.get_ident().is_some() => p.path.get_ident().cloned(),
      _ => None
   }
}

fn pat_to_ident(pat: &Pat) -> Option<Ident> {
   match pat {
      Pat::Ident(ident) => Some(ident.ident.clone()),
      _ => None
   }
}

fn desugar_pattern_args(prog: InferProgram) -> InferProgram {

   fn clause_desugar_pattern_args(body_clause: BodyClauseNode) -> BodyClauseNode {
      let mut new_args = Punctuated::new();
      let mut new_cond_clauses = vec![];
      for arg in body_clause.args.into_pairs() {
         let (arg, punc) = arg.into_tuple();
         let new_arg = match arg {
            BodyClauseArg::Expr(_) => arg,
            BodyClauseArg::Pat(pat) => {
               let pattern = pat.pattern;
               let ident = fresh_ident("_arg_pattern", pattern.span());
               let new_cond_clause = quote!{ if let #pattern = #ident};
               let new_cond_clause = CondClause::IfLet(syn::parse2(new_cond_clause).unwrap());
               new_cond_clauses.push(new_cond_clause);
               BodyClauseArg::Expr(syn::parse2(quote!{#ident}).unwrap())
            }
         };
         new_args.push_value(new_arg);
         if let Some(punc) = punc {new_args.push_punct(punc)}
      }
      new_cond_clauses.extend(body_clause.cond_clauses.into_iter());
      BodyClauseNode{
         args: new_args,
         cond_clauses: new_cond_clauses,
         rel: body_clause.rel
      }
   }
   fn rule_desugar_pattern_args(rule: RuleNode) -> RuleNode {
      RuleNode {
         body_items: map_punctuated(rule.body_items, 
                                    |bi| match bi {BodyItemNode::Clause(cl) => BodyItemNode::Clause(clause_desugar_pattern_args(cl)),
                                                   _ => bi}),
         head_clause: rule.head_clause
      }
   }

   InferProgram{
      relations: prog.relations,
      rules: prog.rules.into_iter().map(rule_desugar_pattern_args).collect()
   }
}


lazy_static::lazy_static! {
   static ref ident_counters: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::default());
}
fn fresh_ident(prefix: &str, span: Span) -> Ident {
   let mut ident_counters_lock = ident_counters.lock().unwrap();
   let counter = if let Some(entry) = ident_counters_lock.get_mut(prefix) {
      let counter = *entry;
      *entry = counter + 1;
      format!("{}", counter)
   } else {
      ident_counters_lock.insert(prefix.to_string(), 1);
      "".to_string()
   };
   
   Ident::new(&format!("{}_{}", prefix, counter), span)
}