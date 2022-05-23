use std::borrow::Borrow;
use std::collections::HashMap;
use std::{collections::HashSet};

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree, Group};

use quote::ToTokens;
use syn::{Block, Stmt, ExprMacro, ItemMacro2};
use syn::parse::{Parse, ParseBuffer};
use syn::{Expr, Pat, Path, Type, parse::ParseStream, parse2, punctuated::Punctuated, spanned::Spanned};

use crate::syn_utils::{stmt_get_vars, pattern_get_vars, path_get_ident_mut};



pub fn tuple_type(types: &[Type]) -> Type {
   let res = if types.len() == 0 {
      quote! {()}
   } else if types.len() == 1 {
      let ty = &types[0];
      quote! { ( #ty, ) }
   } else {
      quote! { ( #(#types),* ) }
   };
   syn::parse2(res).unwrap()
}

pub fn tuple(exprs: &[Expr]) -> Expr {
   let span = if exprs.len() > 0 {exprs[0].span()} else {Span::call_site()};
   tuple_spanned(exprs, span)
}
pub fn tuple_spanned(exprs: &[Expr], span: Span) -> Expr {
   let res = if exprs.len() == 0 {
      quote_spanned! {span=>()}
   } else if exprs.len() == 1 {
      let exp = &exprs[0];
      quote_spanned! {span=> ( #exp, ) }
   } else {
      quote_spanned! {span=> ( #(#exprs),* ) }
   };
   syn::parse2(res).unwrap()
}

pub fn exp_cloned(exp: &Expr) -> Expr {
   let exp_span = exp.span();
   let res = match exp {
      Expr::Path(_) |
      Expr::Field(_) |
      Expr::Paren(_) => quote_spanned! {exp_span=> #exp.clone()},
      _ => quote_spanned! {exp_span=> (#exp).clone()}
   };
   syn::parse2(res).unwrap()
}

pub fn collect_set<T: Eq + std::hash::Hash>(iter : impl Iterator<Item = T>) -> HashSet<T> {
   iter.collect()
}

pub fn into_set<T: Eq + std::hash::Hash>(iter : impl IntoIterator<Item = T>) -> HashSet<T> {
   iter.into_iter().collect()
}

pub fn map_punctuated<T, P, U>(punc: Punctuated<T,P>, mut f: impl FnMut (T) -> U) -> Punctuated<U,P> {
   let mut res = Punctuated::new();
   for pair in punc.into_pairs() {
      let (t, p) = pair.into_tuple();
      res.push_value(f(t));
      if let Some(p) = p {res.push_punct(p)}
   };
   res
}

pub fn flatten_punctuated<T,P>(punc: Punctuated<Punctuated<T,P>, P>) -> Punctuated<T,P> {
   let mut res = Punctuated::new();
   for inner_punc in punc.into_pairs() {
      let (inner_punc, p) = inner_punc.into_tuple();
      let inner_punc_len = inner_punc.len();
      for (ind, item) in inner_punc.into_pairs().enumerate() {
         let (t, p) = item.into_tuple();
         res.push_value(t);
         if ind != inner_punc_len - 1 {
            res.push_punct(p.unwrap())
         }
      }
      if let Some(p) = p {res.push_punct(p)}
   }
   res
}

pub fn punctuated_try_unwrap<T,P, E>(punc: Punctuated<Result<T, E>,P>) -> Result<Punctuated<T,P>, E> {
   let mut res = Punctuated::new();
   for pair in punc.into_pairs() {
      let (t, p) = pair.into_tuple();
      res.push_value(t?);
      if let Some(p) = p {res.push_punct(p)}
   }
   Ok(res)
}

pub fn punctuated_singleton<T, P>(item: T) -> Punctuated<T, P> {
   let mut res = Punctuated::new();
   res.push_value(item);
   res
}


pub fn replace_free_vars(expr: &Expr, subs: &HashMap<Ident, Ident>) -> Expr {
   struct Visitor<'a> {
      subs: &'a HashMap<Ident, Ident>
   }

   impl <'a> syn::visit_mut::VisitMut for Visitor<'a> {
      fn visit_expr_path_mut(&mut self, i: &mut syn::ExprPath) {
         syn::visit_mut::visit_expr_path_mut(self, i);
      }
   }
   todo!()
}


pub fn expr_to_ident(expr: &Expr) -> Option<Ident> {
   match expr {
      Expr::Path(p) => p.path.get_ident().cloned(),
      _ => None
   }
}
pub fn expr_to_ident_mut(expr: &mut Expr) -> Option<&mut Ident> {
   match expr {
      Expr::Path(p) => path_get_ident_mut(&mut p.path),
      _ => None
   }
}

pub fn pat_to_ident(pat: &Pat) -> Option<Ident> {
   match pat {
      Pat::Ident(ident) => Some(ident.ident.clone()),
      _ => None
   }
}

pub fn is_wild_card(expr: &Expr) -> bool {
   match expr {
      Expr::Verbatim(ts) => ts.to_string() == "_",
      _ => false
   }
}


pub fn token_stream_replace_macro_idents(input: TokenStream, ident_replacements: &HashMap<Ident, TokenStream>) -> TokenStream {

   fn ts_replace(ts: TokenStream, ident_replacements: &HashMap<Ident, TokenStream>, res: &mut Vec<TokenTree>){
      

      let mut last_dollar = None;
      for tt in ts {
         if let Some(dollar) = last_dollar.take() {
            let is_match = match &tt {
               TokenTree::Ident(after_dollor_ident) => ident_replacements.get(after_dollor_ident),
               _ => None
            };
            if let Some(replacement) = is_match {
               res.extend(replacement.clone());
               continue;
            } else {
               res.push(dollar);
            }
         }
         let is_dollar = match &tt {
            TokenTree::Punct(punct) => punct.as_char() == '$',
            _ => false
         };
         if is_dollar {
            last_dollar = Some(tt);
         } else {
            match tt {
               TokenTree::Group(grp) => {
                  let replaced = token_stream_replace_macro_idents(grp.stream(), ident_replacements);
                  let updated_group = Group::new(grp.delimiter(), replaced);
                  res.push(TokenTree::Group(updated_group));
               },
               _ => {
                  res.push(tt)
               }
            }
         }
      }
      if let Some(dollar) = last_dollar {
         res.push(dollar);
      }
   }

   let mut res = vec![];
   ts_replace(input, ident_replacements, &mut res);

   res.into_iter().collect()
}

pub fn token_stream_replace_macro_ident(input: TokenStream, ident: &Ident, replacement: &TokenStream) -> TokenStream {

   fn ts_replace(ts: TokenStream, ident: &Ident, replacement: &TokenStream, res: &mut Vec<TokenTree>){
      

      let mut last_dollar = None;
      for tt in ts {
         if let Some(dollar) = last_dollar.take() {
            let is_match = match &tt {
               TokenTree::Ident(after_dollor_ident) => ident == after_dollor_ident,
               _ => false
            };
            if is_match {
               res.extend(replacement.clone());
               continue;
            } else {
               res.push(dollar);
            }
         }
         let is_dollar = match &tt {
            TokenTree::Punct(punct) => punct.as_char() == '$',
            _ => false
         };
         if is_dollar {
            last_dollar = Some(tt);
         } else {
            match tt {
               TokenTree::Group(grp) => {
                  let replaced = token_stream_replace_macro_ident(grp.stream(), ident, replacement);
                  let updated_group = Group::new(grp.delimiter(), replaced);
                  res.push(TokenTree::Group(updated_group));
               },
               _ => {
                  res.push(tt)
               }
            }
         }
      }
      if let Some(dollar) = last_dollar {
         res.push(dollar);
      }
   }

   let mut res = vec![];
   ts_replace(input, ident, replacement, &mut res);

   res.into_iter().collect()
}


pub fn punctuated_into_parts<T,P>(punctuated: Punctuated<T,P>) -> (Vec<T>, Vec<P>) {
   let mut items = vec![];
   let mut punctuations = vec![];
   for p in punctuated.into_pairs() {
      match p {
         syn::punctuated::Pair::Punctuated(t, p) => {items.push(t); punctuations.push(p)},
         syn::punctuated::Pair::End(t) => items.push(t),
      }
   }
   (items, punctuations)
}

pub fn punctuated_from_parts<T,P>(mut items: impl Iterator<Item = T>, mut punctuations: impl Iterator<Item = P>)
-> Punctuated<T, P> {
   let mut res = Punctuated::new();
   for pair in items.by_ref().zip_longest(punctuations) {
      match pair {
         itertools::EitherOrBoth::Both(item, punct) => {res.push_value(item); res.push_punct(punct)},
         itertools::EitherOrBoth::Left(item) => res.push_value(item),
         itertools::EitherOrBoth::Right(_) => panic!("extra punctuations"),
      }
   }
   res
}

pub fn spans_eq(span1: &Span, span2: &Span) -> bool {
   format!("{:?}", span1) == format!("{:?}", span2)
}

// I don't know why I'm like this
pub trait Piper : Sized {
   /// applies `f` to `self`, i.e., `f(self)`
   fn pipe<Res>(self, f: impl FnOnce(Self) -> Res) -> Res;
}

impl <T> Piper for T where T: Sized {
   #[inline(always)]
   fn pipe<Res>(self, f: impl FnOnce(Self) -> Res) -> Res {
      f(self)
   }
}
