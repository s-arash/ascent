use syn::{Expr, Type};



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
   let res = quote! {
      #exp.clone()
   };
   syn::parse2(res).unwrap()
}