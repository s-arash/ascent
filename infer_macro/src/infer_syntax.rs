extern crate proc_macro;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};
use proc_macro2::Span;
use syn::{Expr, Ident, Pat, Result, Token, Type, braced, parenthesized, parse::{self, Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Token};
use std::{clone, collections::{HashMap, HashSet}, fmt::Pointer, sync::Mutex};

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
}

// #[derive(Clone)]
pub struct RelationNode{
   pub name: Ident,
   pub field_types : Punctuated<Type, Token![,]>,
   pub semi_colon: Token![;],
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

#[derive(Parse, Clone)]
pub enum BodyItemNode {
   #[peek(Token![for], name = "GeneratorNode")]
   Generator(GeneratorNode),
   #[peek(Ident, name = "BodyClauseNode")]
   Clause(BodyClauseNode),
   #[peek(syn::token::Paren, name = "Dsjunction node")]
   Disjunction(DisjunctionNode)
}

#[derive(Clone)]
pub struct DisjunctionNode {
   paren: syn::token::Paren,
   disjuncts: Punctuated<Punctuated<BodyItemNode, Token![,]>, Token![||]>,
}

impl Parse for DisjunctionNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let content;
      let paren = parenthesized!(content in input);
      let res: Punctuated<Punctuated<BodyItemNode, Token![,]>, Token![||]>;
      res = Punctuated::<Punctuated<BodyItemNode, Token![,]>, Token![||]>::parse_separated_nonempty_with(&content, Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty)?;
      Ok(DisjunctionNode{paren, disjuncts: res})
   }
}




#[derive(Parse, Clone)]
pub struct GeneratorNode {
   pub for_keyword: Token![for],
   pub pattern: Pat,
   pub in_keyword: Token![in],
   pub expr: Expr
}

#[derive(Clone)]
pub struct BodyClauseNode {
   pub rel : Ident,
   pub args : Punctuated<BodyClauseArg, Token![,]>,
   pub cond_clauses: Vec<CondClause>
}

#[derive(Parse, Clone, PartialEq, Eq)]
pub enum BodyClauseArg {
   #[peek(Token![?], name = "Pattern arg")]
   Pat(ClauseArgPattern),
   #[peek_with(|x| true, name = "Expression arg")]
   Expr(Expr),
}

impl BodyClauseArg {
   pub fn unwrap_expr(self) -> Expr {
      match self {
         Self::Expr(exp) => exp,
         Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr")
      }
   }

   pub fn unwrap_expr_ref(&self) -> &Expr {
      match self {
         Self::Expr(exp) => exp,
         Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr")
      }
   }
}

#[derive(Parse, Clone, PartialEq, Eq)]
pub struct ClauseArgPattern {
   pub huh_token: Token![?],
   pub pattern : Pat,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IfLetClause {
   pub if_keyword: Token![if],
   pub let_keyword: Token![let],
   pub pattern: syn::Pat,
   pub eq_symbol : Token![=],
   pub exp: syn::Expr,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IfClause {
   pub if_keyword: Token![if],
   pub cond: Expr 
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum CondClause {
   IfLet(IfLetClause),
   If(IfClause),
}

impl Parse for CondClause {
   fn parse(input: ParseStream) -> Result<Self> {
      if input.peek(Token![if]) {
         if input.peek2(Token![let]) {
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

#[derive(Clone)]
pub struct HeadClauseNode {
   pub rel : Ident,
   pub args : Punctuated<Expr, Token![,]>,
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

pub struct RuleNode {
   pub head_clauses: Punctuated<HeadClauseNode, Token![,]>,
   pub body_items: Vec<BodyItemNode>// Punctuated<BodyItemNode, Token![,]>,
}

// impl ToTokens for RuleNode {
//    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
//       self.head_clause.to_tokens(tokens);
//       self.body_items.to_tokens(tokens);
//    }
// }

impl Parse for RuleNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let head_clauses = if input.peek(syn::token::Brace) {
         let content;
         braced!(content in input);
         Punctuated::<HeadClauseNode, Token![,]>::parse_terminated(&content)?
      } else {
         // let mut res = Punctuated::<HeadClauseNode, Token![,]>::new();
         // res.push(HeadClauseNode::parse(&input)?);
         // res
         // TODO this seems to work fine for parsing multiple head clauses...
         Punctuated::<HeadClauseNode, Token![,]>::parse_separated_nonempty(&input)?
      };

      if input.peek(Token![;]){
         // println!("fact rule!!!");
         input.parse::<Token![;]>()?;
         Ok(RuleNode{head_clauses, body_items: vec![] /*Punctuated::default()*/})
      } else {
         input.parse::<Token![<]>()?;
         input.parse::<Token![-]>()?;
         input.parse::<Token![-]>()?;
         let body_items = Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty(input)?;
         input.parse::<Token![;]>()?;
         Ok(RuleNode{ head_clauses, body_items: body_items.into_iter().collect()})
      }
   }
}

// #[derive(Clone)]
pub(crate) struct InferProgram {
   pub rules : Vec<RuleNode>,
   pub relations : Vec<RelationNode>
}

impl Parse for InferProgram {
   fn parse(input: ParseStream) -> Result<Self> {
      let mut rules = vec![];
      let mut relations = vec![];
      while !input.is_empty() {
         if input.peek(kw::relation){
            relations.push(RelationNode::parse(input)?);
         } else {
            rules.push(RuleNode::parse(input)?);
         }
      }
      Ok(InferProgram{rules, relations})
   }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct RelationIdentity {
   pub name: Ident,
   pub field_types: Vec<Type>
}

impl From<&RelationNode> for RelationIdentity{
   fn from(relation_node: &RelationNode) -> Self {
      RelationIdentity {
         name: relation_node.name.clone(),
         field_types: relation_node.field_types.iter().cloned().collect()
      }
   }
} 

fn rule_desugar_disjunction_nodes(rule: RuleNode) -> Vec<RuleNode> {
   fn bitem_desugar(bitem: &BodyItemNode) -> Vec<Vec<BodyItemNode>> {
      match bitem {
         BodyItemNode::Generator(g) => vec![vec![bitem.clone()]],
         BodyItemNode::Clause(c) => vec![vec![bitem.clone()]],
         BodyItemNode::Disjunction(d) => {
            let mut res = vec![];
            for disjunt in d.disjuncts.iter() {
               for conjunction in bitems_desugar(&disjunt.iter().cloned().collect_vec()){
                  res.push(conjunction);
               }
            } 
           res
         },
      }
   }
   fn bitems_desugar(bitems: &[BodyItemNode]) -> Vec<Vec<BodyItemNode>> {
      let mut res = vec![];
      if bitems.len() > 0 {
         let sub_res = bitems_desugar(&bitems[0 .. bitems.len() - 1]);
         let last_desugared = bitem_desugar(&bitems[bitems.len() - 1]);
         for sub_res_item in sub_res.into_iter() {
            for last_item in last_desugared.iter() {
               let mut res_item = sub_res_item.clone();
               res_item.extend(last_item.clone());
               res.push(res_item);
            }
         }
      } else {
         res.push(vec![]);
      }

      res
   }

   let mut res = vec![];
   for conjunction in bitems_desugar(&rule.body_items){
      res.push(RuleNode {
         body_items: conjunction,
         head_clauses: rule.head_clauses.clone()
      })
   }
   res
}

fn desugar_disjunction_nodes(prog: InferProgram) -> InferProgram {
   let mut new_rules = vec![];
   for rule in prog.rules.into_iter() {
      new_rules.extend(rule_desugar_disjunction_nodes(rule));
   }
   InferProgram {
      relations : prog.relations,
      rules: new_rules
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
         body_items: rule.body_items.into_iter().map(
                           |bi| match bi {BodyItemNode::Clause(cl) => BodyItemNode::Clause(clause_desugar_pattern_args(cl)),
                                          _ => bi}).collect(),
         head_clauses: rule.head_clauses
      }
   }

   InferProgram{
      relations: prog.relations,
      rules: prog.rules.into_iter().map(rule_desugar_pattern_args).collect()
   }
}

pub(crate) fn desugar_infer_program(prog: InferProgram) -> InferProgram {
   desugar_pattern_args(desugar_disjunction_nodes(prog))
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