extern crate proc_macro;
use proc_macro2::{Span, TokenStream};
use syn::{Attribute, Expr, ExprPath, GenericParam, Generics, Ident, Pat, Result, Token, Type, Visibility, WhereClause, braced, parenthesized, parse::{Parse, ParseStream}, parse2, punctuated::Punctuated, spanned::Spanned, token::{Comma, Gt, Lt}, Error};
use std::{collections::{HashMap}, sync::Mutex};

use quote::{ToTokens};
use itertools::{Itertools};
use derive_syn_parse::Parse;

use crate::utils::{expr_get_vars, expr_to_ident, is_wild_card, pat_to_ident, pattern_get_vars};


// resources:
// https://blog.rust-lang.org/2018/12/21/Procedural-Macros-in-Rust-2018.html
// https://github.com/dtolnay/syn/blob/master/examples/lazy-static/lazy-static/src/lib.rs
// https://crates.io/crates/quote
// example: https://gitlab.gnome.org/federico/gnome-class/-/blob/master/src/parser/mod.rs

mod kw {
   syn::custom_keyword!(relation);
   syn::custom_keyword!(lattice);
   syn::custom_punctuation!(LongLeftArrow, <--);
   syn::custom_keyword!(agg);
}


#[derive(Clone, Parse)]
pub struct Declaration {
   // We don't actually use the Parse impl to parse attrs.
   #[call(Attribute::parse_outer)]
   pub attrs: Vec<Attribute>,
   pub visibility: Visibility,
   pub struct_kw: Token![struct],
   pub ident: Ident,
   #[call(parse_generics_with_where_clause)]
   pub generics: Generics,
   pub where_clause: Option<WhereClause>,
   pub semi: Token![;]
}

/// Parse impl on Generics does not parse WhereClauses, hence this function
fn parse_generics_with_where_clause(input: ParseStream) -> Result<Generics> {
   let mut res = Generics::parse(input)?;
   if input.peek(Token![where]) {
      res.where_clause = Some(input.parse()?);
   }
   Ok(res)
}

// #[derive(Clone)]
pub struct RelationNode{
   pub attrs : Vec<Attribute>,
   pub name: Ident,
   pub field_types : Punctuated<Type, Token![,]>,
   pub initialization: Option<Expr>,
   pub semi_colon: Token![;],
   pub is_lattice: bool,
}
impl Parse for RelationNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let is_lattice = input.peek(kw::lattice);
      if is_lattice {input.parse::<kw::lattice>()?;} else {input.parse::<kw::relation>()?;}
      let name : Ident = input.parse()?;
      let content;
      parenthesized!(content in input);
      let field_types = content.parse_terminated(Type::parse)?;
      let initialization = if input.peek(Token![=]) {
         input.parse::<Token![=]>()?;
         Some(input.parse::<Expr>()?)
      } else {None};

      let semi_colon = input.parse::<Token![;]>()?;
      if is_lattice && field_types.empty_or_trailing() {
         return Err(input.error(format!("empty lattice is not allowed")));
      }
      Ok(RelationNode{attrs: vec![], name, field_types, semi_colon, is_lattice, initialization})
   }
}

#[derive(Parse, Clone)]
pub enum BodyItemNode {
   #[peek(Token![for], name = "generative clause")]
   Generator(GeneratorNode),
   #[peek(kw::agg, name = "aggregate clause")]
   Agg(AggClauseNode),
   #[peek(Ident, name = "body clause")]
   Clause(BodyClauseNode),
   #[peek(Token![!], name = "negation clause")]
   Negation(NegationClauseNode),
   #[peek(syn::token::Paren, name = "Dsjunction node")]
   Disjunction(DisjunctionNode),
   #[peek_with(peek_if_or_let, name = "if condition")]
   Cond(CondClause),
}
fn peek_if_or_let(parse_stream: ParseStream) -> bool {
   parse_stream.peek(Token![if]) || parse_stream.peek(Token![let])
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
      res = Punctuated::<Punctuated<BodyItemNode, Token![,]>, Token![||]>::parse_terminated_with(&content, Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty)?;
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
   #[peek_with(|_| true, name = "Expression arg")]
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
impl ToTokens for BodyClauseArg {
   fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      match self{
         BodyClauseArg::Pat(pat) => {
            pat.huh_token.to_tokens(tokens);
            pat.pattern.to_tokens(tokens);
         },
         BodyClauseArg::Expr(exp) => exp.to_tokens(tokens),
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

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LetClause {
   pub let_keyword: Token![let],
   pub pattern: syn::Pat,
   pub eq_symbol : Token![=],
   pub exp: syn::Expr,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum CondClause {
   IfLet(IfLetClause),
   If(IfClause),
   Let(LetClause),
}

impl CondClause {
   pub fn bound_vars(&self) -> Vec<Ident> {
      match self {
        CondClause::IfLet(cl) => pattern_get_vars(&cl.pattern),
        CondClause::If(_) => vec![],
        CondClause::Let(cl) => pattern_get_vars(&cl.pattern),
      }
   }

   /// returns the expression associated with the CondClause. 
   /// Useful for determining clause dependencies
   pub fn expr(&self) -> &Expr {
         match self {
         CondClause::IfLet(cl) => &cl.exp,
         CondClause::If(cl) => &cl.cond,
         CondClause::Let(cl) => &cl.exp,
      }
   }
}
impl Parse for CondClause {
   fn parse(input: ParseStream) -> Result<Self> {
      if input.peek(Token![if]) {
         if input.peek2(Token![let]) {
            let cl: IfLetClause = input.parse()?;
            Ok(Self::IfLet(cl))
         } else {
            let cl: IfClause = input.parse()?;
            Ok(Self::If(cl))
         }
      } else if input.peek(Token![let]) {
         let cl: LetClause = input.parse()?;
         Ok(Self::Let(cl))
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

#[derive(Parse, Clone)]
pub struct NegationClauseNode {
   neg_token: Token![!],
   pub rel : Ident,
   #[paren]
   rel_arg_paren: syn::token::Paren,
   #[inside(rel_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   pub args : Punctuated<Expr, Token![,]>,
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

#[derive(Clone, Parse)]
pub struct AggClauseNode {
   pub agg_kw: kw::agg,
   pub pat: Pat,
   pub eq_token: Token![=],
   pub aggregator: AggregatorNode,
   #[paren]
   pub agg_arg_paren: syn::token::Paren,
   #[inside(agg_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   pub bound_args: Punctuated<Ident, Token![,]>,
   pub in_kw: Token![in],
   pub rel : Ident,
   #[paren]
   rel_arg_paren: syn::token::Paren,
   #[inside(rel_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   pub rel_args : Punctuated<Expr, Token![,]>
}

#[derive(Clone)]
pub enum AggregatorNode {
   Path(syn::Path),
   Expr(Expr)
}

impl Parse for AggregatorNode {
   fn parse(input: ParseStream) -> Result<Self> {
      if input.peek(syn::token::Paren) {
         let inside_parens;
         parenthesized!(inside_parens in input);
         Ok(AggregatorNode::Expr(inside_parens.parse()?))
      } else {
         Ok(AggregatorNode::Path(input.parse()?))
      }
   }
}
impl AggregatorNode {
   pub fn get_expr(&self) -> Expr {
      match self {
         AggregatorNode::Path(path) => parse2(quote!{#path}).unwrap(),
         AggregatorNode::Expr(expr) => expr.clone(),
      }
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
         // TODO this does not work with quote!
         // input.parse::<kw::LongLeftArrow>()?;

         let body_items = Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty(input)?;
         input.parse::<Token![;]>()?;
         Ok(RuleNode{ head_clauses, body_items: body_items.into_iter().collect()})
      }
   }
}

pub(crate) fn rule_node_summary(rule: &RuleNode) -> String {
   fn bitem_to_str(bitem: &BodyItemNode) -> String {
      match bitem {
         BodyItemNode::Generator(gen) => format!("for_{}", pat_to_ident(&gen.pattern).map(|x| x.to_string()).unwrap_or_default()),
         BodyItemNode::Clause(bcl) => format!("{}", bcl.rel.to_string()),
         BodyItemNode::Disjunction(_) => todo!(),
         BodyItemNode::Cond(cl) => format!("if_"),
         BodyItemNode::Agg(agg) => format!("agg {}", agg.rel),
         BodyItemNode::Negation(neg) => format!("! {}", neg.rel),
      }
   }
   format!("{} <-- {}",
            rule.head_clauses.iter().map(|hcl| hcl.rel.to_string()).join(", "),
            rule.body_items.iter().map(bitem_to_str).join(", "))
}

// #[derive(Clone)]
pub(crate) struct AscentProgram {
   pub rules : Vec<RuleNode>,
   pub relations : Vec<RelationNode>,
   pub declaration: Option<Declaration>,
   pub attributes: Vec<syn::Attribute>,
}

impl Parse for AscentProgram {
   fn parse(input: ParseStream) -> Result<Self> {
      let attributes = Attribute::parse_inner(input)?;
      let mut struct_attrs = Attribute::parse_outer(input)?;
      let declaration = if input.peek(Token![pub]) || input.peek(Token![struct]) {
         Some(Declaration{ attrs: std::mem::take(&mut struct_attrs), .. Declaration::parse(input)?})
      } else {None};
      let mut rules = vec![];
      let mut relations = vec![];
      while !input.is_empty() {
         let attrs = if !struct_attrs.is_empty() {std::mem::take(&mut struct_attrs)} else {Attribute::parse_outer(input)?};
         if input.peek(kw::relation) || input.peek(kw::lattice){
            let mut relation_node = RelationNode::parse(input)?;
            relation_node.attrs = attrs;
            relations.push(relation_node);
         } else {
            if !attrs.is_empty() {
               return Err(Error::new(attrs[0].span(), "unexpected attribute(s)"));
            }
            rules.push(RuleNode::parse(input)?);
         }
      }
      Ok(AscentProgram{rules, relations, declaration, attributes})
   }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct RelationIdentity {
   pub name: Ident,
   pub field_types: Vec<Type>,
   pub is_lattice: bool,
}

impl From<&RelationNode> for RelationIdentity{
   fn from(relation_node: &RelationNode) -> Self {
      RelationIdentity {
         name: relation_node.name.clone(),
         field_types: relation_node.field_types.iter().cloned().collect(),
         is_lattice: relation_node.is_lattice
      }
   }
} 

fn rule_desugar_disjunction_nodes(rule: RuleNode) -> Vec<RuleNode> {
   fn bitem_desugar(bitem: &BodyItemNode) -> Vec<Vec<BodyItemNode>> {
      match bitem {
         BodyItemNode::Generator(_) => vec![vec![bitem.clone()]],
         BodyItemNode::Clause(_) => vec![vec![bitem.clone()]],
         BodyItemNode::Cond(_) => vec![vec![bitem.clone()]],
         BodyItemNode::Agg(_) => vec![vec![bitem.clone()]],
         BodyItemNode::Negation(_) => vec![vec![bitem.clone()]],
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

fn rule_desugar_pattern_args(rule: RuleNode) -> RuleNode {
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
   use BodyItemNode::*;
   RuleNode {
      body_items: rule.body_items.into_iter().map(|bi| match bi {
         Clause(cl) => Clause(clause_desugar_pattern_args(cl)),
         _ => bi}).collect(),
      head_clauses: rule.head_clauses
   }
}

fn rule_desugar_repeated_vars(mut rule: RuleNode) -> RuleNode {
   
   let mut grounded_vars = HashMap::<Ident, usize>::new();
   for i in 0..rule.body_items.len(){
      let bitem = &mut rule.body_items[i];
      match bitem {
         BodyItemNode::Clause(cl) => {
            let mut new_cond_clauses = vec![];
            for arg_ind in 0..cl.args.len() {
               let expr = cl.args[arg_ind].unwrap_expr_ref();
               let expr_has_vars_from_same_caluse =
                  expr_get_vars(&expr).iter()
                  .any(|var| if let Some(cl_ind) = grounded_vars.get(&var) {*cl_ind == i} else {false});
               if expr_has_vars_from_same_caluse {
                  let new_ident = fresh_ident(&expr_to_ident(expr).map(|e| e.to_string()).unwrap_or("expr_replaced".to_string()), expr.span());
                  new_cond_clauses.push(CondClause::If(
                     parse2(quote_spanned! {expr.span()=> if #new_ident.eq(&(#expr))}).unwrap()
                  ));
                  cl.args[arg_ind] = BodyClauseArg::Expr(parse2(new_ident.to_token_stream()).unwrap());
               } else if let Some(ident) = expr_to_ident(expr) {
                  grounded_vars.entry(ident).or_insert(i);
               }
            }
            for new_cond_cl in new_cond_clauses.into_iter().rev() {
               cl.cond_clauses.insert(0, new_cond_cl);
            }
         },
         BodyItemNode::Generator(gen) => {
            for ident in pattern_get_vars(&gen.pattern) {
               grounded_vars.entry(ident).or_insert(i);
            }
         },
         BodyItemNode::Cond(ref cond_cl @ CondClause::IfLet(_)) |
         BodyItemNode::Cond(ref cond_cl @ CondClause::Let(_)) => {
            for ident in cond_cl.bound_vars() {
               grounded_vars.entry(ident).or_insert(i);
            }
         }
         BodyItemNode::Cond(CondClause::If(_)) => (),
         BodyItemNode::Agg(agg) => {
            for ident in pattern_get_vars(&agg.pat){
               grounded_vars.entry(ident).or_insert(i);
            }
         },
         BodyItemNode::Negation(_) => (),
         BodyItemNode::Disjunction(_) => panic!("unrecognized BodyItemNode variant")
      }
   }
   rule
}

fn rule_desugar_wildcards(mut rule: RuleNode) -> RuleNode {
   for bi in &mut rule.body_items[..] {
      match bi {
         BodyItemNode::Clause(bcl) => {
            for arg in &mut bcl.args.iter_mut() {
               match arg {
                  BodyClauseArg::Expr(expr) => {
                     if is_wild_card(&expr) {
                        let new_ident = fresh_ident("_", expr.span());
                        *expr = parse2(quote! {#new_ident}).unwrap();
                     }
                  },
                  BodyClauseArg::Pat(_) => (),
               }
            }
         },
         _ => ()
      }
   }
   rule
}

fn rule_desugar_negation(mut rule: RuleNode) -> RuleNode {
   for bi in &mut rule.body_items[..] {
      match bi {
         BodyItemNode::Negation(neg) => {
            let rel = &neg.rel;
            let args = &neg.args;
            let replacement = quote_spanned! {neg.neg_token.span=> 
               agg () = ::ascent::aggregators::not() in #rel(#args)
            };
            let replacement: AggClauseNode = parse2(replacement).unwrap();
            *bi = BodyItemNode::Agg(replacement);
         },
         _ => ()
      }      
   }
   rule
}

pub(crate) fn desugar_ascent_program(mut prog: AscentProgram) -> AscentProgram {
   prog.rules = 
      prog.rules.into_iter()
      .flat_map(rule_desugar_disjunction_nodes)
      .map(rule_desugar_pattern_args)
      .map(rule_desugar_wildcards)
      .map(rule_desugar_negation)
      .map(rule_desugar_repeated_vars)
      .collect_vec();
   prog
}

lazy_static::lazy_static! {
   static ref IDENT_COUNTERS: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::default());
}
fn fresh_ident(prefix: &str, span: Span) -> Ident {
   let mut ident_counters_lock = IDENT_COUNTERS.lock().unwrap();
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