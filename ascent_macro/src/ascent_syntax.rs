#![deny(warnings)]
extern crate proc_macro;
use std::collections::{HashMap, HashSet};
use std::sync::Mutex;

use ascent_base::util::update;
use derive_syn_parse::Parse;
use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
   Attribute, Error, Expr, ExprMacro, Generics, Ident, ImplGenerics, Pat, Result, Token, Type, TypeGenerics,
   Visibility, WhereClause, braced, parenthesized, parse2,
};

use crate::syn_utils::{
   expr_get_vars, expr_visit_free_vars_mut, expr_visit_idents_in_macros_mut, pattern_get_vars, pattern_visit_vars_mut,
   token_stream_idents, token_stream_replace_ident,
};
use crate::utils::{
   Piper, expr_to_ident, expr_to_ident_mut, flatten_punctuated, is_wild_card, pat_to_ident, punctuated_map,
   punctuated_singleton, punctuated_try_map, punctuated_try_unwrap, spans_eq, token_stream_replace_macro_idents,
};

// resources:
// https://blog.rust-lang.org/2018/12/21/Procedural-Macros-in-Rust-2018.html
// https://github.com/dtolnay/syn/blob/master/examples/lazy-static/lazy-static/src/lib.rs
// https://crates.io/crates/quote
// example: https://gitlab.gnome.org/federico/gnome-class/-/blob/master/src/parser/mod.rs

mod kw {
   use derive_syn_parse::Parse;
   use proc_macro2::Span;
   use syn::Token;

   use crate::utils::join_spans;

   syn::custom_keyword!(relation);
   syn::custom_keyword!(lattice);
   #[allow(dead_code)] // for unused fields of LongLeftArrow
   #[derive(Parse)]
   pub struct LongLeftArrow(Token![<], Token![-], Token![-]);
   #[allow(unused)]
   impl LongLeftArrow {
      pub fn span(&self) -> Span {
         join_spans([self.0.span, self.1.span, self.2.span])
      }
   }
   syn::custom_keyword!(agg);
   syn::custom_keyword!(ident);
   syn::custom_keyword!(expr);
}

#[derive(Clone, Debug)]
pub(crate) struct Signatures {
   pub(crate) declaration: TypeSignature,
   pub(crate) implementation: Option<ImplSignature>,
}

impl Signatures {
   pub fn split_ty_generics_for_impl(&self) -> (ImplGenerics<'_>, TypeGenerics<'_>, Option<&'_ WhereClause>) {
      self.declaration.generics.split_for_impl()
   }

   pub fn split_impl_generics_for_impl(&self) -> (ImplGenerics<'_>, TypeGenerics<'_>, Option<&'_ WhereClause>) {
      let Some(signature) = &self.implementation else {
         return self.split_ty_generics_for_impl();
      };

      let (impl_generics, _, _) = signature.impl_generics.split_for_impl();
      let (_, ty_generics, where_clause) = signature.generics.split_for_impl();

      (impl_generics, ty_generics, where_clause)
   }
}

impl Parse for Signatures {
   fn parse(input: ParseStream) -> Result<Self> {
      let declaration = TypeSignature::parse(input)?;
      let implementation = if input.peek(Token![impl]) { Some(ImplSignature::parse(input)?) } else { None };
      Ok(Signatures { declaration, implementation })
   }
}

#[derive(Clone, Parse, Debug)]
pub struct TypeSignature {
   // We don't actually use the Parse impl to parse attrs.
   #[call(Attribute::parse_outer)]
   pub attrs: Vec<Attribute>,
   pub visibility: Visibility,
   pub _struct_kw: Token![struct],
   pub ident: Ident,
   #[call(parse_generics_with_where_clause)]
   pub generics: Generics,
   pub _semi: Token![;],
}

#[derive(Clone, Parse, Debug)]
pub struct ImplSignature {
   pub _impl_kw: Token![impl],
   pub impl_generics: Generics,
   pub ident: Ident,
   #[call(parse_generics_with_where_clause)]
   pub generics: Generics,
   pub _semi: Token![;],
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
pub struct RelationNode {
   pub attrs: Vec<Attribute>,
   pub name: Ident,
   pub field_types: Punctuated<Type, Token![,]>,
   pub initialization: Option<Expr>,
   pub _semi_colon: Token![;],
   pub is_lattice: bool,
}
impl Parse for RelationNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let is_lattice = input.peek(kw::lattice);
      if is_lattice {
         input.parse::<kw::lattice>()?;
      } else {
         input.parse::<kw::relation>()?;
      }
      let name: Ident = input.parse()?;
      let content;
      parenthesized!(content in input);
      let field_types = content.parse_terminated(Type::parse, Token![,])?;
      let initialization = if input.peek(Token![=]) {
         input.parse::<Token![=]>()?;
         Some(input.parse::<Expr>()?)
      } else {
         None
      };

      let _semi_colon = input.parse::<Token![;]>()?;
      if is_lattice && field_types.empty_or_trailing() {
         return Err(input.error("empty lattice is not allowed"));
      }
      Ok(RelationNode { attrs: vec![], name, field_types, _semi_colon, is_lattice, initialization })
   }
}

#[derive(Parse, Clone)]
pub enum BodyItemNode {
   #[peek(Token![for], name = "generative clause")]
   Generator(GeneratorNode),
   #[peek(kw::agg, name = "aggregate clause")]
   Agg(AggClauseNode),
   #[peek_with(peek_macro_invocation, name = "macro invocation")]
   MacroInvocation(syn::ExprMacro),
   #[peek(Ident, name = "body clause")]
   Clause(BodyClauseNode),
   #[peek(Token![!], name = "negation clause")]
   Negation(NegationClauseNode),
   #[peek(syn::token::Paren, name = "disjunction node")]
   Disjunction(DisjunctionNode),
   #[peek_with(peek_if_or_let, name = "if condition or let binding")]
   Cond(CondClause),
}

fn peek_macro_invocation(parse_stream: ParseStream) -> bool {
   parse_stream.peek(Ident) && parse_stream.peek2(Token![!])
}

fn peek_if_or_let(parse_stream: ParseStream) -> bool { parse_stream.peek(Token![if]) || parse_stream.peek(Token![let]) }

#[derive(Parse, Clone)]
enum DisjunctionToken {
   #[allow(unused)]
   #[peek(Token![||], name = "||")]
   OrOr(Token![||]),
   #[allow(unused)]
   #[peek(Token![|], name = "|")]
   Or(Token![|]),
}

#[derive(Clone)]
pub struct DisjunctionNode {
   paren: syn::token::Paren,
   disjuncts: Punctuated<Punctuated<BodyItemNode, Token![,]>, DisjunctionToken>,
}

impl Parse for DisjunctionNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let content;
      let paren = parenthesized!(content in input);
      let res: Punctuated<Punctuated<BodyItemNode, Token![,]>, DisjunctionToken> =
         Punctuated::<Punctuated<BodyItemNode, Token![,]>, DisjunctionToken>::parse_terminated_with(
            &content,
            Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty,
         )?;
      if res.pairs().any(|pair| matches!(pair.punct(), Some(DisjunctionToken::OrOr(_)))) {
         eprintln!("WARNING: In Ascent rules, use `|` as the disjunction token instead of `||`")
      }
      Ok(DisjunctionNode { paren, disjuncts: res })
   }
}

#[derive(Parse, Clone)]
pub struct GeneratorNode {
   pub for_keyword: Token![for],
   #[call(Pat::parse_multi)]
   pub pattern: Pat,
   pub _in_keyword: Token![in],
   pub expr: Expr,
}

#[derive(Clone)]
pub struct BodyClauseNode {
   pub rel: Ident,
   pub args: Punctuated<BodyClauseArg, Token![,]>,
   pub cond_clauses: Vec<CondClause>,
}

#[derive(Parse, Clone, PartialEq, Eq, Debug)]
pub enum BodyClauseArg {
   #[peek(Token![?], name = "Pattern arg")]
   Pat(ClauseArgPattern),
   #[peek_with({ |_| true }, name = "Expression arg")]
   Expr(Expr),
}

impl BodyClauseArg {
   pub fn unwrap_expr(self) -> Expr {
      match self {
         Self::Expr(exp) => exp,
         Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr"),
      }
   }

   pub fn unwrap_expr_ref(&self) -> &Expr {
      match self {
         Self::Expr(exp) => exp,
         Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr"),
      }
   }

   pub fn get_vars(&self) -> Vec<Ident> {
      match self {
         BodyClauseArg::Pat(p) => pattern_get_vars(&p.pattern),
         BodyClauseArg::Expr(e) => expr_to_ident(e).into_iter().collect(),
      }
   }
}
impl ToTokens for BodyClauseArg {
   fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      match self {
         BodyClauseArg::Pat(pat) => {
            pat.huh_token.to_tokens(tokens);
            pat.pattern.to_tokens(tokens);
         },
         BodyClauseArg::Expr(exp) => exp.to_tokens(tokens),
      }
   }
}

#[derive(Parse, Clone, PartialEq, Eq, Debug)]
pub struct ClauseArgPattern {
   pub huh_token: Token![?],
   #[call(Pat::parse_multi)]
   pub pattern: Pat,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IfLetClause {
   pub if_keyword: Token![if],
   pub let_keyword: Token![let],
   #[call(Pat::parse_multi)]
   pub pattern: Pat,
   pub eq_symbol: Token![=],
   pub exp: syn::Expr,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IfClause {
   pub if_keyword: Token![if],
   pub cond: Expr,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LetClause {
   pub let_keyword: Token![let],
   #[call(Pat::parse_multi)]
   pub pattern: Pat,
   pub eq_symbol: Token![=],
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

impl Parse for BodyClauseNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let rel: Ident = input.parse()?;
      let args_content;
      parenthesized!(args_content in input);
      let args = args_content.parse_terminated(BodyClauseArg::parse, Token![,])?;
      let mut cond_clauses = vec![];
      while let Ok(cl) = input.parse() {
         cond_clauses.push(cl);
      }
      Ok(BodyClauseNode { rel, args, cond_clauses })
   }
}

#[derive(Parse, Clone)]
pub struct NegationClauseNode {
   neg_token: Token![!],
   pub rel: Ident,
   #[paren]
   _rel_arg_paren: syn::token::Paren,
   #[inside(_rel_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   pub args: Punctuated<Expr, Token![,]>,
}

#[derive(Clone, Parse)]
pub enum HeadItemNode {
   #[peek_with(peek_macro_invocation, name = "macro invocation")]
   MacroInvocation(syn::ExprMacro),
   #[peek(Ident, name = "head clause")]
   HeadClause(HeadClauseNode),
}

impl HeadItemNode {
   pub fn clause(&self) -> &HeadClauseNode {
      match self {
         HeadItemNode::HeadClause(cl) => cl,
         HeadItemNode::MacroInvocation(_) => panic!("unexpected macro invocation"),
      }
   }
}

#[derive(Clone)]
pub struct HeadClauseNode {
   pub rel: Ident,
   pub args: Punctuated<Expr, Token![,]>,
}
impl ToTokens for HeadClauseNode {
   fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      self.rel.to_tokens(tokens);
      self.args.to_tokens(tokens);
   }
}

impl Parse for HeadClauseNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let rel: Ident = input.parse()?;
      let args_content;
      parenthesized!(args_content in input);
      let args = args_content.parse_terminated(Expr::parse, Token![,])?;
      Ok(HeadClauseNode { rel, args })
   }
}

#[derive(Clone, Parse)]
pub struct AggClauseNode {
   pub agg_kw: kw::agg,
   #[call(Pat::parse_multi)]
   pub pat: Pat,
   pub _eq_token: Token![=],
   pub aggregator: AggregatorNode,
   #[paren]
   pub _agg_arg_paren: syn::token::Paren,
   #[inside(_agg_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   pub bound_args: Punctuated<Ident, Token![,]>,
   pub _in_kw: Token![in],
   pub rel: Ident,
   #[paren]
   _rel_arg_paren: syn::token::Paren,
   #[inside(_rel_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   pub rel_args: Punctuated<Expr, Token![,]>,
}

#[derive(Clone)]
pub enum AggregatorNode {
   Path(syn::Path),
   Expr(Expr),
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
         AggregatorNode::Path(path) => parse2(quote! {#path}).unwrap(),
         AggregatorNode::Expr(expr) => expr.clone(),
      }
   }
}

pub struct RuleNode {
   pub head_clauses: Punctuated<HeadItemNode, Token![,]>,
   pub body_items: Vec<BodyItemNode>, // Punctuated<BodyItemNode, Token![,]>,
}

impl Parse for RuleNode {
   fn parse(input: ParseStream) -> Result<Self> {
      let head_clauses = if input.peek(syn::token::Brace) {
         let content;
         braced!(content in input);
         Punctuated::<HeadItemNode, Token![,]>::parse_terminated(&content)?
      } else {
         Punctuated::<HeadItemNode, Token![,]>::parse_separated_nonempty(input)?
      };

      if input.peek(Token![;]) {
         input.parse::<Token![;]>()?;
         Ok(RuleNode { head_clauses, body_items: vec![] /*Punctuated::default()*/ })
      } else {
         input.parse::<kw::LongLeftArrow>()?;
         let body_items = Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty(input)?;
         input.parse::<Token![;]>()?;
         Ok(RuleNode { head_clauses, body_items: body_items.into_iter().collect() })
      }
   }
}

// TODO maybe remove?
#[allow(dead_code)]
pub(crate) fn rule_node_summary(rule: &RuleNode) -> String {
   fn bitem_to_str(bitem: &BodyItemNode) -> String {
      match bitem {
         BodyItemNode::Generator(gen) =>
            format!("for_{}", pat_to_ident(&gen.pattern).map(|x| x.to_string()).unwrap_or_default()),
         BodyItemNode::Clause(bcl) => format!("{}", bcl.rel),
         BodyItemNode::Disjunction(_) => todo!(),
         BodyItemNode::Cond(_cl) => format!("if_"),
         BodyItemNode::Agg(agg) => format!("agg {}", agg.rel),
         BodyItemNode::Negation(neg) => format!("! {}", neg.rel),
         BodyItemNode::MacroInvocation(m) => format!("{:?}!(..)", m.mac.path),
      }
   }
   fn hitem_to_str(hitem: &HeadItemNode) -> String {
      match hitem {
         HeadItemNode::MacroInvocation(m) => format!("{:?}!(..)", m.mac.path),
         HeadItemNode::HeadClause(cl) => cl.rel.to_string(),
      }
   }
   format!(
      "{} <-- {}",
      rule.head_clauses.iter().map(hitem_to_str).join(", "),
      rule.body_items.iter().map(bitem_to_str).join(", ")
   )
}

#[derive(Parse)]
pub struct MacroDefParam {
   _dollar: Token![$],
   name: Ident,
   _colon: Token![:],
   kind: MacroParamKind,
}

#[derive(Parse)]
#[allow(unused)]
pub enum MacroParamKind {
   #[peek(kw::ident, name = "ident")]
   Expr(Ident),
   #[peek(kw::expr, name = "expr")]
   Ident(Ident),
}

#[derive(Parse)]
pub struct MacroDefNode {
   _mac: Token![macro],
   name: Ident,
   #[paren]
   _arg_paren: syn::token::Paren,
   #[inside(_arg_paren)]
   #[call(Punctuated::parse_terminated)]
   params: Punctuated<MacroDefParam, Token![,]>,
   #[brace]
   _body_brace: syn::token::Brace,
   #[inside(_body_brace)]
   body: TokenStream,
}

// #[derive(Clone)]
pub(crate) struct AscentProgram {
   pub rules: Vec<RuleNode>,
   pub relations: Vec<RelationNode>,
   pub signatures: Option<Signatures>,
   pub attributes: Vec<syn::Attribute>,
   pub macros: Vec<MacroDefNode>,
}

impl Parse for AscentProgram {
   fn parse(input: ParseStream) -> Result<Self> {
      let attributes = Attribute::parse_inner(input)?;
      let mut struct_attrs = Attribute::parse_outer(input)?;
      let signatures = if input.peek(Token![pub]) || input.peek(Token![struct]) {
         let mut signatures = Signatures::parse(input)?;
         signatures.declaration.attrs = std::mem::take(&mut struct_attrs);
         Some(signatures)
      } else {
         None
      };
      let mut rules = vec![];
      let mut relations = vec![];
      let mut macros = vec![];
      while !input.is_empty() {
         let attrs =
            if !struct_attrs.is_empty() { std::mem::take(&mut struct_attrs) } else { Attribute::parse_outer(input)? };
         if input.peek(kw::relation) || input.peek(kw::lattice) {
            let mut relation_node = RelationNode::parse(input)?;
            relation_node.attrs = attrs;
            relations.push(relation_node);
         } else if input.peek(Token![macro]) {
            if !attrs.is_empty() {
               return Err(Error::new(attrs[0].span(), "unexpected attribute(s)"));
            }
            macros.push(MacroDefNode::parse(input)?);
         } else {
            if !attrs.is_empty() {
               return Err(Error::new(attrs[0].span(), "unexpected attribute(s)"));
            }
            rules.push(RuleNode::parse(input)?);
         }
      }
      Ok(AscentProgram { rules, relations, signatures, attributes, macros })
   }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct RelationIdentity {
   pub name: Ident,
   pub field_types: Vec<Type>,
   pub is_lattice: bool,
}

impl From<&RelationNode> for RelationIdentity {
   fn from(relation_node: &RelationNode) -> Self {
      RelationIdentity {
         name: relation_node.name.clone(),
         field_types: relation_node.field_types.iter().cloned().collect(),
         is_lattice: relation_node.is_lattice,
      }
   }
}

#[derive(Clone)]
pub(crate) struct DsAttributeContents {
   pub path: syn::Path,
   pub args: TokenStream,
}

impl Parse for DsAttributeContents {
   fn parse(input: ParseStream) -> Result<Self> {
      let path = syn::Path::parse_mod_style(input)?;
      let args = if input.peek(Token![:]) {
         input.parse::<Token![:]>()?;
         TokenStream::parse(input)?
      } else {
         TokenStream::default()
      };

      Ok(Self { path, args })
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
               for conjunction in bitems_desugar(&disjunt.iter().cloned().collect_vec()) {
                  res.push(conjunction);
               }
            }
            res
         },
         BodyItemNode::MacroInvocation(m) => panic!("unexpected macro invocation: {:?}", m.mac.path),
      }
   }
   fn bitems_desugar(bitems: &[BodyItemNode]) -> Vec<Vec<BodyItemNode>> {
      let mut res = vec![];
      if !bitems.is_empty() {
         let sub_res = bitems_desugar(&bitems[0..bitems.len() - 1]);
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
   for conjunction in bitems_desugar(&rule.body_items) {
      res.push(RuleNode { body_items: conjunction, head_clauses: rule.head_clauses.clone() })
   }
   res
}

fn body_item_get_bound_vars(bi: &BodyItemNode) -> Vec<Ident> {
   match bi {
      BodyItemNode::Generator(gen) => pattern_get_vars(&gen.pattern),
      BodyItemNode::Agg(agg) => pattern_get_vars(&agg.pat),
      BodyItemNode::Clause(cl) => cl.args.iter().flat_map(|arg| arg.get_vars()).collect(),
      BodyItemNode::Negation(_cl) => vec![],
      BodyItemNode::Disjunction(disj) =>
         disj.disjuncts.iter().flat_map(|conj| conj.iter().flat_map(body_item_get_bound_vars)).collect(),
      BodyItemNode::Cond(cl) => cl.bound_vars(),
      BodyItemNode::MacroInvocation(_) => vec![],
   }
}

fn body_item_visit_bound_vars_mut(bi: &mut BodyItemNode, visitor: &mut dyn FnMut(&mut Ident)) {
   match bi {
      BodyItemNode::Generator(gen) => pattern_visit_vars_mut(&mut gen.pattern, visitor),
      BodyItemNode::Agg(agg) => pattern_visit_vars_mut(&mut agg.pat, visitor),
      BodyItemNode::Clause(cl) =>
         for arg in cl.args.iter_mut() {
            match arg {
               BodyClauseArg::Pat(p) => pattern_visit_vars_mut(&mut p.pattern, visitor),
               BodyClauseArg::Expr(e) =>
                  if let Some(ident) = expr_to_ident_mut(e) {
                     visitor(ident)
                  },
            }
         },
      BodyItemNode::Negation(_cl) => (),
      BodyItemNode::Disjunction(disj) =>
         for conj in disj.disjuncts.iter_mut() {
            for bi in conj.iter_mut() {
               body_item_visit_bound_vars_mut(bi, visitor)
            }
         },
      BodyItemNode::Cond(cl) => match cl {
         CondClause::IfLet(cl) => pattern_visit_vars_mut(&mut cl.pattern, visitor),
         CondClause::If(_cl) => (),
         CondClause::Let(cl) => pattern_visit_vars_mut(&mut cl.pattern, visitor),
      },
      BodyItemNode::MacroInvocation(_) => (),
   }
}

fn body_item_visit_exprs_free_vars_mut(
   bi: &mut BodyItemNode, visitor: &mut dyn FnMut(&mut Ident), visit_macro_idents: bool,
) {
   let mut visit = |expr: &mut Expr| {
      expr_visit_free_vars_mut(expr, visitor);
      if visit_macro_idents {
         expr_visit_idents_in_macros_mut(expr, visitor);
      }
   };
   match bi {
      BodyItemNode::Generator(gen) => visit(&mut gen.expr),
      BodyItemNode::Agg(agg) => {
         for arg in agg.rel_args.iter_mut() {
            visit(arg)
         }
         if let AggregatorNode::Expr(e) = &mut agg.aggregator {
            visit(e)
         }
      },
      BodyItemNode::Clause(cl) =>
         for arg in cl.args.iter_mut() {
            if let BodyClauseArg::Expr(e) = arg {
               visit(e);
            }
         },
      BodyItemNode::Negation(cl) =>
         for arg in cl.args.iter_mut() {
            visit(arg);
         },
      BodyItemNode::Disjunction(disj) =>
         for conj in disj.disjuncts.iter_mut() {
            for bi in conj.iter_mut() {
               body_item_visit_exprs_free_vars_mut(bi, visitor, visit_macro_idents);
            }
         },
      BodyItemNode::Cond(cl) => match cl {
         CondClause::IfLet(cl) => visit(&mut cl.exp),
         CondClause::If(cl) => visit(&mut cl.cond),
         CondClause::Let(cl) => visit(&mut cl.exp),
      },
      BodyItemNode::MacroInvocation(m) => {
         update(&mut m.mac.tokens, |ts| token_stream_replace_ident(ts, visitor));
      },
   }
}

#[derive(Clone)]
struct GenSym(HashMap<String, u32>, fn(&str) -> String);
impl GenSym {
   pub fn next(&mut self, ident: &str) -> String {
      match self.0.get_mut(ident) {
         Some(n) => {
            *n += 1;
            format!("{}{}", self.1(ident), *n - 1)
         },
         None => {
            self.0.insert(ident.into(), 1);
            self.1(ident)
         },
      }
   }
   pub fn next_ident(&mut self, ident: &str, span: Span) -> Ident { Ident::new(&self.next(ident), span) }
   pub fn new(transformer: fn(&str) -> String) -> Self { Self(Default::default(), transformer) }
}

impl Default for GenSym {
   fn default() -> Self { Self(Default::default(), |x| format!("{}_", x)) }
}

fn body_items_rename_macro_originated_vars(
   bis: &mut [&mut BodyItemNode], macro_def: &MacroDefNode, gensym: &mut GenSym,
) {
   let bi_vars = bis.iter().flat_map(|bi| body_item_get_bound_vars(bi)).collect_vec();
   let mut mac_body_idents = token_stream_idents(macro_def.body.clone());
   mac_body_idents.retain(|ident| bi_vars.contains(ident));

   let macro_originated_vars = bi_vars
      .iter()
      .filter(|v| mac_body_idents.iter().any(|ident| spans_eq(&v.span(), &ident.span())))
      .cloned()
      .collect::<HashSet<_>>();

   let var_mappings = macro_originated_vars.iter().map(|v| (v, gensym.next(&v.to_string()))).collect::<HashMap<_, _>>();
   let mut visitor = |ident: &mut Ident| {
      if let Some(replacement) = var_mappings.get(ident) {
         if mac_body_idents.iter().any(|mac_ident| spans_eq(&mac_ident.span(), &ident.span())) {
            *ident = Ident::new(replacement, ident.span())
         }
      }
   };
   for bi in bis.iter_mut() {
      body_item_visit_bound_vars_mut(bi, &mut visitor);
      body_item_visit_exprs_free_vars_mut(bi, &mut visitor, true);
   }
}

fn rule_desugar_pattern_args(rule: RuleNode) -> RuleNode {
   fn clause_desugar_pattern_args(body_clause: BodyClauseNode, gensym: &mut GenSym) -> BodyClauseNode {
      let mut new_args = Punctuated::new();
      let mut new_cond_clauses = vec![];
      for arg in body_clause.args.into_pairs() {
         let (arg, punc) = arg.into_tuple();
         let new_arg = match arg {
            BodyClauseArg::Expr(_) => arg,
            BodyClauseArg::Pat(pat) => {
               let pattern = pat.pattern;
               let ident = gensym.next_ident("__arg_pattern", pattern.span());
               let new_cond_clause = quote! { if let #pattern = #ident};
               let new_cond_clause = CondClause::IfLet(syn::parse2(new_cond_clause).unwrap());
               new_cond_clauses.push(new_cond_clause);
               BodyClauseArg::Expr(syn::parse2(quote! {#ident}).unwrap())
            },
         };
         new_args.push_value(new_arg);
         if let Some(punc) = punc {
            new_args.push_punct(punc)
         }
      }
      new_cond_clauses.extend(body_clause.cond_clauses);
      BodyClauseNode { args: new_args, cond_clauses: new_cond_clauses, rel: body_clause.rel }
   }
   let mut gensym = GenSym::default();
   RuleNode {
      body_items: rule
         .body_items
         .into_iter()
         .map(|bi| match bi {
            BodyItemNode::Clause(cl) => BodyItemNode::Clause(clause_desugar_pattern_args(cl, &mut gensym)),
            _ => bi,
         })
         .collect(),
      head_clauses: rule.head_clauses,
   }
}

fn rule_desugar_repeated_vars(mut rule: RuleNode) -> RuleNode {
   let mut grounded_vars = HashMap::<Ident, usize>::new();
   for i in 0..rule.body_items.len() {
      let bitem = &mut rule.body_items[i];
      match bitem {
         BodyItemNode::Clause(cl) => {
            let mut new_cond_clauses = vec![];
            for arg_ind in 0..cl.args.len() {
               let expr = cl.args[arg_ind].unwrap_expr_ref();
               let expr_has_vars_from_same_clause = expr_get_vars(expr)
                  .iter()
                  .any(|var| if let Some(cl_ind) = grounded_vars.get(var) { *cl_ind == i } else { false });
               if expr_has_vars_from_same_clause {
                  let new_ident = fresh_ident(
                     &expr_to_ident(expr).map(|e| e.to_string()).unwrap_or_else(|| "expr_replaced".to_string()),
                     expr.span(),
                  );
                  new_cond_clauses
                     .push(CondClause::If(parse2(quote_spanned! {expr.span()=> if #new_ident.eq(&(#expr))}).unwrap()));
                  cl.args[arg_ind] = BodyClauseArg::Expr(parse2(new_ident.to_token_stream()).unwrap());
               } else if let Some(ident) = expr_to_ident(expr) {
                  grounded_vars.entry(ident).or_insert(i);
               }
            }
            for new_cond_cl in new_cond_clauses.into_iter().rev() {
               cl.cond_clauses.insert(0, new_cond_cl);
            }
         },
         BodyItemNode::Generator(gen) =>
            for ident in pattern_get_vars(&gen.pattern) {
               grounded_vars.entry(ident).or_insert(i);
            },
         BodyItemNode::Cond(ref cond_cl @ CondClause::IfLet(_))
         | BodyItemNode::Cond(ref cond_cl @ CondClause::Let(_)) =>
            for ident in cond_cl.bound_vars() {
               grounded_vars.entry(ident).or_insert(i);
            },
         BodyItemNode::Cond(CondClause::If(_)) => (),
         BodyItemNode::Agg(agg) =>
            for ident in pattern_get_vars(&agg.pat) {
               grounded_vars.entry(ident).or_insert(i);
            },
         BodyItemNode::Negation(_) => (),
         BodyItemNode::Disjunction(_) => panic!("unrecognized BodyItemNode variant"),
         BodyItemNode::MacroInvocation(m) => panic!("unexpected macro invocation: {:?}", m.mac.path),
      }
   }
   rule
}

fn rule_desugar_wildcards(mut rule: RuleNode) -> RuleNode {
   let mut gensym = GenSym::default();
   gensym.next("_"); // to move past "_"
   for bi in &mut rule.body_items[..] {
      if let BodyItemNode::Clause(bcl) = bi {
         for arg in bcl.args.iter_mut() {
            match arg {
               BodyClauseArg::Expr(expr) =>
                  if is_wild_card(expr) {
                     let new_ident = gensym.next_ident("_", expr.span());
                     *expr = parse2(quote! {#new_ident}).unwrap();
                  },
               BodyClauseArg::Pat(_) => (),
            }
         }
      }
   }
   rule
}

fn rule_desugar_negation(mut rule: RuleNode) -> RuleNode {
   for bi in &mut rule.body_items[..] {
      if let BodyItemNode::Negation(neg) = bi {
         let rel = &neg.rel;
         let args = &neg.args;
         let replacement = quote_spanned! {neg.neg_token.span=>
            agg () = ::ascent::aggregators::not() in #rel(#args)
         };
         let replacement: AggClauseNode = parse2(replacement).unwrap();
         *bi = BodyItemNode::Agg(replacement);
      }
   }
   rule
}

fn invoke_macro(invocation: &ExprMacro, definition: &MacroDefNode) -> Result<TokenStream> {
   let tokens = invocation.mac.tokens.clone();

   fn parse_args(definition: &MacroDefNode, args: ParseStream, span: Span) -> Result<HashMap<Ident, TokenStream>> {
      let mut ident_replacement = HashMap::new();

      for pair in definition.params.pairs() {
         if args.is_empty() {
            return Err(Error::new(span, "expected more arguments"));
         }
         let (param, comma) = pair.into_tuple();
         let arg = match param.kind {
            MacroParamKind::Expr(_) => args.parse::<Ident>()?.into_token_stream(),
            MacroParamKind::Ident(_) => args.parse::<Expr>()?.into_token_stream(),
         };

         ident_replacement.insert(param.name.clone(), arg);
         if comma.is_some() {
            if args.is_empty() {
               return Err(Error::new(span, "expected more arguments"));
            }
            args.parse::<Token![,]>()?;
         }
      }

      Ok(ident_replacement)
   }

   let args_parser = |inp: ParseStream| parse_args(definition, inp, invocation.mac.span());
   let args_parsed = Parser::parse2(args_parser, tokens)?;

   let replaced_body = token_stream_replace_macro_idents(definition.body.clone(), &args_parsed);
   Ok(replaced_body)
}

fn rule_expand_macro_invocations(rule: RuleNode, macros: &HashMap<Ident, &MacroDefNode>) -> Result<RuleNode> {
   const RECURSIVE_MACRO_ERROR: &'static str = "recursively defined Ascent macro";
   fn body_item_expand_macros(
      bi: BodyItemNode, macros: &HashMap<Ident, &MacroDefNode>, gensym: &mut GenSym, depth: i16, span: Option<Span>,
   ) -> Result<Punctuated<BodyItemNode, Token![,]>> {
      if depth <= 0 {
         return Err(Error::new(span.unwrap_or_else(Span::call_site), RECURSIVE_MACRO_ERROR))
      }
      match bi {
         BodyItemNode::MacroInvocation(m) => {
            let mac_def =
               macros.get(m.mac.path.get_ident().unwrap()).ok_or_else(|| Error::new(m.span(), "undefined macro"))?;
            let macro_invoked = invoke_macro(&m, mac_def)?;
            let expanded_bis = Parser::parse2(Punctuated::<BodyItemNode, Token![,]>::parse_terminated, macro_invoked)?;
            let mut recursively_expanded = punctuated_try_map(expanded_bis, |ebi| {
               body_item_expand_macros(ebi, macros, gensym, depth - 1, Some(m.span()))
            })?
            .pipe(flatten_punctuated);
            body_items_rename_macro_originated_vars(
               &mut recursively_expanded.iter_mut().collect_vec(),
               mac_def,
               gensym,
            );
            Ok(recursively_expanded)
         },
         BodyItemNode::Disjunction(disj) => {
            let new_disj: Punctuated<Result<_>, _> = punctuated_map(disj.disjuncts, |bis| {
               let new_bis = punctuated_map(bis, |bi| {
                  body_item_expand_macros(bi, macros, gensym, depth - 1, Some(disj.paren.span.join()))
               });
               Ok(flatten_punctuated(punctuated_try_unwrap(new_bis)?))
            });

            Ok(punctuated_singleton(BodyItemNode::Disjunction(DisjunctionNode {
               disjuncts: punctuated_try_unwrap(new_disj)?,
               ..disj
            })))
         },
         _ => Ok(punctuated_singleton(bi)),
      }
   }

   fn head_item_expand_macros(
      hi: HeadItemNode, macros: &HashMap<Ident, &MacroDefNode>, depth: i16, span: Option<Span>,
   ) -> Result<Punctuated<HeadItemNode, Token![,]>> {
      if depth <= 0 {
         return Err(Error::new(span.unwrap_or_else(Span::call_site), RECURSIVE_MACRO_ERROR))
      }
      match hi {
         HeadItemNode::MacroInvocation(m) => {
            let mac_def =
               macros.get(m.mac.path.get_ident().unwrap()).ok_or_else(|| Error::new(m.span(), "undefined macro"))?;
            let macro_invoked = invoke_macro(&m, mac_def)?;
            let expanded_his = Parser::parse2(Punctuated::<HeadItemNode, Token![,]>::parse_terminated, macro_invoked)?;

            Ok(punctuated_map(expanded_his, |ehi| head_item_expand_macros(ehi, macros, depth - 1, Some(m.span())))
               .pipe(punctuated_try_unwrap)?
               .pipe(flatten_punctuated))
         },
         HeadItemNode::HeadClause(_) => Ok(punctuated_singleton(hi)),
      }
   }

   let mut gensym = GenSym::new(|s| format!("__{}_", s));

   let new_body_items = rule
      .body_items
      .into_iter()
      .map(|bi| body_item_expand_macros(bi, macros, &mut gensym, 100, None))
      .collect::<Result<Vec<_>>>()?
      .into_iter()
      .flatten()
      .collect_vec();

   let new_head_items = punctuated_map(rule.head_clauses, |hi| head_item_expand_macros(hi, macros, 100, None))
      .pipe(punctuated_try_unwrap)?
      .pipe(flatten_punctuated);

   Ok(RuleNode { body_items: new_body_items, head_clauses: new_head_items })
}

pub(crate) fn desugar_ascent_program(mut prog: AscentProgram) -> Result<AscentProgram> {
   let macros = prog.macros.iter().map(|m| (m.name.clone(), m)).collect::<HashMap<_, _>>();
   let rules_macro_expanded =
      prog.rules.into_iter().map(|r| rule_expand_macro_invocations(r, &macros)).collect::<Result<Vec<_>>>()?;

   prog.rules = rules_macro_expanded
      .into_iter()
      .flat_map(rule_desugar_disjunction_nodes)
      .map(rule_desugar_pattern_args)
      .map(rule_desugar_wildcards)
      .map(rule_desugar_negation)
      .map(rule_desugar_repeated_vars)
      .collect_vec();

   Ok(prog)
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
