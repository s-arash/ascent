use std::borrow::Borrow;
use std::collections::HashMap;
use std::{collections::HashSet};
use std::hash::Hash;

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree, Group};

use quote::ToTokens;
use syn::visit_mut::VisitMut;
use syn::{Block, Stmt, ExprMacro, ItemMacro2, GenericParam, parse_quote_spanned, Lifetime};
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

pub fn punctuated_map<T, P, U>(punc: Punctuated<T,P>, mut f: impl FnMut (T) -> U) -> Punctuated<U,P> {
   let mut res = Punctuated::new();
   for pair in punc.into_pairs() {
      let (t, p) = pair.into_tuple();
      res.push_value(f(t));
      if let Some(p) = p {res.push_punct(p)}
   };
   res
}

pub fn punctuated_try_map<T, P, U, E>(punc: Punctuated<T,P>, mut f: impl FnMut (T) -> Result<U,E>) -> Result<Punctuated<U,P>, E> {
   let mut res = Punctuated::new();
   for pair in punc.into_pairs() {
      let (t, p) = pair.into_tuple();
      res.push_value(f(t)?);
      if let Some(p) = p {res.push_punct(p)}
   };
   Ok(res)
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


/// sets the span of only top-level tokens to `span`
pub fn with_span(ts: TokenStream, span: Span) -> TokenStream {
   ts.into_iter().map(|mut tt| {
      tt.set_span(span);
      tt
   }).collect()
}

pub(crate) trait TokenStreamExtensions {
   fn with_span(self, span: Span) -> TokenStream;
}

impl TokenStreamExtensions for TokenStream {
   fn with_span(self, span: Span) -> TokenStream { with_span(self, span) }
}

// TODO unused, probably not useful
pub fn erase_lifetimes(ty: &mut Type){
   struct Visitor { bound_lifetimes: Vec<Ident> };
   impl Visitor {
      fn transform_lt(&self, lt: &mut Lifetime) {
         let ident = lt.ident.to_string();
         if ident != "_" && ident != "static" && !self.bound_lifetimes.contains(&lt.ident){
            *lt = parse_quote_spanned! {lt.span()=> '_}
         }
      }
   }
   impl syn::visit_mut::VisitMut for Visitor {
      fn visit_generic_argument_mut(&mut self, i: &mut syn::GenericArgument) {
         match i {
            syn::GenericArgument::Lifetime(lt) => self.transform_lt(lt),
            syn::GenericArgument::Type(_) => (),
            syn::GenericArgument::Binding(_) => (),
            syn::GenericArgument::Constraint(_) => (),
            syn::GenericArgument::Const(_) => (),
         }
         syn::visit_mut::visit_generic_argument_mut(self, i);
      }

      fn visit_type_reference_mut(&mut self, i: &mut syn::TypeReference) {
         if let Some(lt) = &mut i.lifetime {
            self.transform_lt(lt);
         }
         syn::visit_mut::visit_type_reference_mut(self, i);
      }

      fn visit_type_bare_fn_mut(&mut self, i: &mut syn::TypeBareFn) {
         if let Some(lifetimes) = &mut i.lifetimes {
            let bound_lifetimes = lifetimes.lifetimes.iter().map(|lt| &lt.lifetime.ident).join(", ");
            let bound_len = self.bound_lifetimes.len();
            self.bound_lifetimes.extend(lifetimes.lifetimes.iter().map(|lt| lt.lifetime.ident.clone()));
            syn::visit_mut::visit_type_bare_fn_mut(self, i);
            self.bound_lifetimes.truncate(bound_len);
         } else {
            syn::visit_mut::visit_type_bare_fn_mut(self, i);
         }
      }
   }

   Visitor{ bound_lifetimes: vec![] }.visit_type_mut(ty);

   type T = for <'a, 'b> fn(&'a i32, fn(&'b i32) -> &'b i32) -> &'a i32;
}

#[test]
fn test_erase_lifetimes() {

   // let ty: Type = syn::parse_quote! { for <'a> fn(&'a i32) -> &'a i32 };
   // println!("ty: {:?}", ty);
   let cases = vec![
      (quote!{ Vec<&'a T> }, quote!{ Vec<&'_ T> }),
      (quote!{ Ref<'a, 'b, T> }, quote!{ Ref<'_, '_, T> }),
      (quote!{ fn(&'a T) -> &'a i32 }, quote!{ fn(&'_ T) -> &'_ i32 }),
      (quote!{ for<'a, 'b> fn(&'a i32) -> &'c i32 }, quote!{ for<'a, 'b> fn(&'a i32) -> &'_ i32 }),
      (quote!{ for<'a, 'b> fn(&'a &'c &'b i32) -> &'c i32 }, quote!{ for<'a, 'b> fn(&'a &'_ &'b i32) -> &'_ i32 }),
      (quote!{ Cow<'static, Ref<&'a &'static Foo<'b>>> }, quote!{ Cow<'static, Ref<&'_ &'static Foo<'_>>> })
   ];

   for (inp, expected) in cases {
      let inp_ty: Type = parse2(inp).unwrap();
      let expected_ty: Type = parse2(expected).unwrap();

      let mut transformed = inp_ty.clone();
      erase_lifetimes(&mut transformed);
      println!("inp: {}, output: {}", inp_ty.to_token_stream(), transformed.to_token_stream());
      assert_eq!(transformed, expected_ty);
   }
}

fn check_lazy_set_contains<T: Hash + Eq>(hs: &mut HashSet<T>, iter: &mut impl Iterator<Item = T>, x: T) -> bool {
   if hs.contains(&x) { return true }
   
   for item in iter {
      let eq = item == x;
      hs.insert(item);
      if eq { return true }
   }
   false
}
pub fn subsumes<T, Iter1, Iter2>(set1: Iter1, set2: Iter2) -> bool 
where T: Hash + Eq, Iter1: IntoIterator<Item = T>, Iter2: IntoIterator<Item = T> 
{
   let mut hs = HashSet::default();
   let mut set1 = set1.into_iter();
   for x in set2 {
      if !check_lazy_set_contains(&mut hs, &mut set1, x) { return false }
   }
   true
}

pub fn intersects<T, Iter1, Iter2>(set1: Iter1, set2: Iter2) -> bool 
where T: Hash + Eq, Iter1: IntoIterator<Item = T>, Iter2: IntoIterator<Item = T> 
{
   let mut hs = HashSet::default();
   let mut set1 = set1.into_iter();
   for x in set2 {
      if check_lazy_set_contains(&mut hs, &mut set1, x) { return true }
   }
   false
}

#[test]
fn test_subsumes_and_intersects() {
   let cases = [
      (vec![1, 2, 3], vec![3, 3, 4], false, true),
      (vec![1, 2, 3], vec![3, 3, 2], true, true),
      (vec![1, 2, 3, 4], vec![1, 3, 4, 2], true, true),
      (vec![1, 2, 3, 4], vec![], true, false),
      (vec![1, 2, 3, 4], vec![4, 2, 3, 1, 1, 2, 3, 4], true, true),
      (vec![1, 2, 3], vec![4], false, false),
      (vec![], vec![4], false, false),
      (vec![], vec![], true, false),
      (vec![1, 2], vec![3, 4], false, false),
      (vec![1, 2, 3, 4], vec![5, 6, 7, 1], false, true)
   ];
   for (s1, s2, subsumes_expected, intersects_expected) in cases {
      println!("s1: {:?}, s2: {:?}", s1, s2);
      assert_eq!(subsumes(s1.iter(), s2.iter()), subsumes_expected);
      assert_eq!(intersects(s1.iter(), s2.iter()), intersects_expected);
   }
}