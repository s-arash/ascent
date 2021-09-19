#![allow(warnings)]
#![feature(proc_macro)]
mod tests;
mod infer_mir;
mod utils;
mod infer_hir;
mod scratchpad;

extern crate proc_macro;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};
use syn::{Expr, Ident, Result, Token, Type, parenthesized, parse::{self, Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, token::Token};
use infer_hir::dl_impl_hir_to_code;
use std::{clone, collections::{HashMap, HashSet}, fmt::Pointer};
#[macro_use]
extern crate quote;
use quote::{ToTokens, TokenStreamExt};
use itertools::Itertools;

use crate::{utils::tuple_type, infer_hir::{compile_ir_rule, compile_yadl_program_to_hir}, infer_mir::{compile_hir_to_mir, compile_mir}};

// resources:
// https://blog.rust-lang.org/2018/12/21/Procedural-Macros-in-Rust-2018.html
// https://github.com/dtolnay/syn/blob/master/examples/lazy-static/lazy-static/src/lib.rs
// https://crates.io/crates/quote
// example: https://gitlab.gnome.org/federico/gnome-class/-/blob/master/src/parser/mod.rs

mod kw {
   syn::custom_keyword!(relation);
   syn::custom_keyword!(when);

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

// #[derive(Debug)]
struct BodyClauseNode {
   rel : Ident,
   args : Punctuated<Expr, Token![,]>,
}

impl ToTokens for BodyClauseNode {
   fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
      self.rel.to_tokens(tokens);
      self.args.to_tokens(tokens);
   }
}

impl Parse for BodyClauseNode{
   fn parse(input: ParseStream) -> Result<Self> {
      let rel : Ident = input.parse()?;
      let args_content;
      parenthesized!(args_content in input);
      let args = args_content.parse_terminated(Expr::parse)?;
      Ok(BodyClauseNode{rel, args})
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
   body_clauses: Punctuated<BodyClauseNode, Token![,]>,
   when_clause: Option<Expr>,

}

impl ToTokens for RuleNode {
   fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
      self.head_clause.to_tokens(tokens);
      self.body_clauses.to_tokens(tokens);
   }
}

impl Parse for RuleNode {
   fn parse(input: ParseStream) -> Result<Self> {
       let head_clause : HeadClauseNode = input.parse()?;
       input.parse::<Token![<]>()?;
       input.parse::<Token![-]>()?;
       input.parse::<Token![-]>()?;
       let body_clauses = Punctuated::<BodyClauseNode, Token![,]>::parse_separated_nonempty(input)?;
       let mut when_clause = None;
       if input.peek(kw::when) {
          input.parse::<kw::when>()?;
          when_clause = Some(input.parse::<Expr>()?);
          
       }
       input.parse::<Token![;]>()?;
       Ok(RuleNode{ head_clause, body_clauses, when_clause})
   }
}

// #[derive(Clone)]
pub(crate) struct YadlProgram {
   rules : Vec<RuleNode>,
   relations : Vec<RelationNode>
}

impl Parse for YadlProgram {
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
      Ok(YadlProgram{rules, relations})
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
pub fn dl_old(input: TokenStream) -> TokenStream{
   let prog = parse_macro_input!(input as YadlProgram);
   println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());

   let mut relation_fields = vec![];
   
   for rel in prog.relations.iter(){
      let name = &rel.name;
      let field_types = &rel.field_types;
      relation_fields.push(quote! {
         pub #name : Vec<(#field_types)>,

      });
   }

   let mut compiled_rules = vec![];
   for rule in prog.rules.iter() {
      compiled_rules.push(compile_rule(&prog, rule, 0));
   }

   let mut merge_rels = vec![];
   for rel in prog.relations.iter(){
      let name = &rel.name;
      merge_rels.push(quote! {
         Self::extend_no_dup(&mut self.#name, & other.#name);
      });
   }
   let res = quote! {
      #[derive(Default)]
      struct DLProgram {
         #(#relation_fields)*
      }

      impl DLProgram {
         pub fn run(&mut self) {
            loop {
               let mut new_db = DLProgram::default();
               #(#compiled_rules)*
               if !self.merge(&new_db) {break;}
            }
         }

         fn merge(&mut self, other: &Self) -> bool{
            let changed = false;
            #(#merge_rels)*
            changed
         }

         fn extend_no_dup<T : Clone + Eq>(vec1 : &mut Vec<T>, vec2: & Vec<T>) -> bool {
            let mut res = false;
            for item in vec2.iter(){
               if !vec1.contains(item) {
                  vec1.push(item.clone());
                  res = true;
               }
            }
            res
         }
      }
   };
   println!("res:\n {}", res);

   TokenStream::from(res)
}


#[proc_macro]
pub fn dl(input: TokenStream) -> TokenStream{
   // let prog = parse_macro_input!(input as YadlProgram);
   // let res = dl_impl_hir_to_code(input.into());
   let res = dl_impl(input.into());
   
   match res {
    Ok(res) => res.into(),
    Err(err) => TokenStream::from(err.to_compile_error()),
   }
}

fn dl_impl(input: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream> {
   let prog: YadlProgram = syn::parse2(input)?;
   // let prog = YadlProgram::parse(input.into());// parse_macro_input!(input as YadlProgram);
   println!("prog relations: {}", prog.relations.len());
   println!("parse res: {} relations, {} rules", prog.relations.len(), prog.rules.len());
   
   let hir = compile_yadl_program_to_hir(&prog);
   println!("hir relations: {}", hir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let mir = compile_hir_to_mir(&hir);

   println!("mir relations: {}", mir.relations_ir_relations.keys().map(|r| &r.name).join(", "));

   let code = compile_mir(&mir);

   Ok(code)
}

fn expr_to_ident(expr: &Expr) -> Option<Ident> {
   match expr {
      Expr::Path(p) if p.path.get_ident().is_some() => p.path.get_ident().cloned(),
      _ => None
   }
}


fn compile_rule(prog: &YadlProgram, rule: &RuleNode, clause_ind: usize) -> proc_macro2::TokenStream {
   if clause_ind < rule.body_clauses.len() {

      let bclause = &rule.body_clauses[clause_ind];
      let bclause_rel = &bclause.rel;
      
      let pre_clause_vars = rule.body_clauses.iter().take(clause_ind)
                              .flat_map(|cl| cl.args.iter().filter_map(expr_to_ident))
                              .collect::<Vec<_>>();
      
      let clause_vars = rule.body_clauses[clause_ind].args.iter().enumerate()
                           .filter_map(|(i,v)| expr_to_ident(v).map(|v| (i, v)))
                           .collect::<Vec<_>>();
      
      let common_vars = clause_vars.iter().filter(|(i,v)| pre_clause_vars.contains(v)).collect::<Vec<_>>();
      let common_vars_no_indices = common_vars.iter().map(|(i,v)| v.clone()).collect::<Vec<_>>();
      
      println!("common_vars: {:?}", common_vars);
      
      let mut consistency_check = vec![quote! {true}];
      for (i, var) in common_vars.iter(){
         let i_ind = syn::Index::from(*i);
         consistency_check.push(quote! {&& #var == row.#i_ind})
      }
      
      let mut assignments = vec![];
      for (i, var) in clause_vars.iter() {
         if common_vars_no_indices.contains(var) {continue};
         let i_ind = syn::Index::from(*i);
         assignments.push(quote! {let #var = row.#i_ind.clone();})
      }
      
      let next_loop = compile_rule(prog, rule, clause_ind + 1);
      let res = quote! {
         for row in self.#bclause_rel.iter() {
            if !(#(#consistency_check)*) { continue };
            #(#assignments)*
            #next_loop
         }
      };
      res
   } else {
      let head_rel = &rule.head_clause.rel;
      let mut assignments = vec![];
      for (i, expr) in rule.head_clause.args.iter().enumerate(){
         let i_ind = syn::Index::from(i);
         assignments.push(quote! {
            #expr
         });
      }
      let head_relation = prog.relations.iter().filter(|v| v.name == rule.head_clause.rel).next().unwrap();
      let row_type = &head_relation.field_types;
      let cond = rule.when_clause.as_ref().map(|x| x.to_token_stream()).unwrap_or( quote! {true}).clone();
      quote! {
         if #cond {
            let new_row: (#row_type) = (#(#assignments),*) ;
            new_db.#head_rel.push(new_row);
         }
      }

   }
}

