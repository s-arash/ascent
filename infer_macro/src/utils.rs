use std::collections::HashSet;

use itertools::Itertools;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{Expr, Pat, PatBox, Type, parse2, punctuated::Punctuated, spanned::Spanned};



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
   let res = if exprs.len() == 0 {
      quote! {()}
   } else if exprs.len() == 1 {
      let ty = &exprs[0];
      quote! { ( #ty, ) }
   } else {
      quote! { ( #(#exprs),* ) }
   };
   syn::parse2(res).unwrap()
}

pub fn exp_cloned(exp: &Expr) -> Expr {
   let exp_span = exp.span();
   let res = match exp {
      Expr::Path(_) |
      Expr::Field(_) |
      Expr::Paren(_) =>
         quote_spanned! {exp_span=> #exp.clone()},
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

pub fn pattern_get_vars(pat: &Pat) -> Vec<Ident> {
   let mut res = vec![];
   match pat {
      Pat::Box(patBox) => res.extend(pattern_get_vars(&patBox.pat)),
      Pat::Ident(patIdent) => {
         res.push(patIdent.ident.clone()); 
         if let Some(subpat) = &patIdent.subpat {
            res.extend(pattern_get_vars(&subpat.1))
         }
      },
      Pat::Lit(_) => {},
      Pat::Macro(_) => {},
      Pat::Or(orPat) => {
         let cases_vars = orPat.cases.iter().map(pattern_get_vars).map(into_set);
         let intersection = cases_vars.fold1(|case_vars, accu| collect_set(case_vars.intersection(&accu).cloned()));
         if let Some(intersection) = intersection {
            res.extend(intersection);
         }
      },
      Pat::Path(_) => {},
      Pat::Range(_) => {},
      Pat::Reference(refPat) => res.extend(pattern_get_vars(&refPat.pat)),
      Pat::Rest(_) => {},
      Pat::Slice(slicePat) => {
         for subPat in slicePat.elems.iter(){
            res.extend(pattern_get_vars(subPat));
         }
      },
      Pat::Struct(struct_pat) => {
         for field_pat in struct_pat.fields.iter() {
            res.extend(pattern_get_vars(&field_pat.pat));
         }
      },
      Pat::Tuple(tuple_pat) => {
         for elem_pat in tuple_pat.elems.iter() {
            res.extend(pattern_get_vars(elem_pat));
         }
      }
      Pat::TupleStruct(tuple_strcut_pat) => {
         for elem_pat in tuple_strcut_pat.pat.elems.iter(){
            res.extend(pattern_get_vars(elem_pat));
         }
      },
      Pat::Type(type_pat) => {
         res.extend(pattern_get_vars(&type_pat.pat));
      },
      Pat::Verbatim(_) => {},
      Pat::Wild(_) => {},
      _ => {}
   }
   // println!("pattern vars {} : {}", pat.to_token_stream(), res.iter().map(|ident| ident.to_string()).join(", "));
   res
}

#[test]
fn test_pattern_get_vars(){
   let pattern = quote! {
      SomePair(x, (y, z))
   };
   let pat : syn::Pat = parse2(pattern).unwrap();
   assert_eq!(collect_set(["x", "y", "z"].into_iter().map(ToString::to_string)), 
              pattern_get_vars(&pat).into_iter().map(|id| id.to_string()).collect());
}
