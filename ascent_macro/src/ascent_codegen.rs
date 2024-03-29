#![deny(warnings)]
use std::collections::HashSet;

use itertools::{Itertools, Either};
use proc_macro2::{Ident, Span, TokenStream};
use syn::{Expr, Type, parse2, spanned::Spanned, parse_quote, parse_quote_spanned};

use crate::{ascent_hir::{IrRelation, IndexValType}, ascent_syntax::{CondClause, RelationIdentity}, utils::TokenStreamExtensions};
use crate::utils::{exp_cloned, expr_to_ident, tuple, tuple_spanned, tuple_type};
use crate::ascent_mir::{AscentMir, MirBodyItem, MirRelation, MirRelationVersion, MirRule, MirScc, ir_relation_version_var_name, mir_rule_summary, mir_summary};
use crate::ascent_mir::MirRelationVersion::*;

pub(crate) fn compile_mir(mir: &AscentMir, is_ascent_run: bool) -> proc_macro2::TokenStream {
   
   let mut relation_fields = vec![];
   let mut field_defaults = vec![];

   for (rel, rel_indices) in mir.relations_ir_relations.iter(){
      let name = &rel.name;
      let rel_attrs = &mir.relations_metadata[rel].attributes;
      let rel_indices_comment = format!("\nlogical indices: {}", 
         rel_indices.iter().map(|ind| format!("{}", ind.ir_name())).join("; "));
      let rel_type = rel_type(rel, mir);
      let rel_ind_common = rel_ind_common_var_name(rel);
      let rel_ind_common_type = rel_ind_common_type(rel, mir);
      relation_fields.push(quote! {
         #(#rel_attrs)*
         #[doc = #rel_indices_comment]
         pub #name: #rel_type,
         pub #rel_ind_common: #rel_ind_common_type,
      });
      field_defaults.push(quote! {#name : Default::default(), #rel_ind_common: Default::default(),});
      if rel.is_lattice && mir.is_parallel {
         let lattice_mutex_name = lattice_insertion_mutex_var_name(rel);
         relation_fields.push(quote! {
            pub #lattice_mutex_name: ::std::vec::Vec<std::sync::Mutex<()>>,
         });
         field_defaults.push(quote! {#lattice_mutex_name: {
            let len = ::ascent::internal::shards_count();
            let mut v = Vec::with_capacity(len); for _ in 0..len {v.push(Default::default())}; 
            v },
         })
      }
      for ind in rel_indices.iter(){
         let name = &ind.ir_name();
         let rel_index_type = rel_index_type(ind, mir);
         relation_fields.push(quote!{
            pub #name: #rel_index_type,
         });
         field_defaults.push(quote! {#name : Default::default(),});
      }
   }

   let sccs_ordered = &mir.sccs;
   let mut rule_time_fields = vec![];
   let mut rule_time_fields_defaults = vec![];
   for i in 0..mir.sccs.len(){
      for (rule_ind, _rule) in mir.sccs[i].rules.iter().enumerate() {
         let name = rule_time_field_name(i, rule_ind);
         rule_time_fields.push(quote!{
            pub #name: std::time::Duration,
         });
         rule_time_fields_defaults.push(quote!{
            #name: std::time::Duration::ZERO,
         });
      }
   }

   
   let mut sccs_compiled = vec![];
   for (i, _scc) in sccs_ordered.iter().enumerate() {
      let msg = format!("scc {}", i);
      let scc_compiled = compile_mir_scc(mir, i);
      sccs_compiled.push(quote!{
         ascent::internal::comment(#msg);
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #scc_compiled
            _self.scc_times[#i] += _scc_start_time.elapsed();
         }
      });
   }

   let update_indices_body = compile_update_indices_function_body(mir);
   let relation_sizes_body = compile_relation_sizes_body(mir);
   let scc_times_summary_body = compile_scc_times_summary_body(mir);

   let mut type_constraints = vec![];
   let mut field_type_names = HashSet::<String>::new();
   let mut lat_field_type_names = HashSet::<String>::new();

   
   for relation in mir.relations_ir_relations.keys() {
      use crate::quote::ToTokens;
      for (i,field_type) in relation.field_types.iter().enumerate() {
         let is_lat = relation.is_lattice && i == relation.field_types.len() - 1;
         let add = if let Type::Path(path) = field_type {
            let container = if is_lat {&mut lat_field_type_names} else {&mut field_type_names};
            container.insert(path.path.clone().into_token_stream().to_string())
         } else {true}; 
         if add {
            let type_constraints_type = 
               if is_lat {quote_spanned!(field_type.span()=>LatTypeConstraints)} 
               else {quote_spanned!(field_type.span()=>TypeConstraints)}; 
            type_constraints.push(quote_spanned!{field_type.span()=>
               let _type_constraints : ascent::internal::#type_constraints_type<#field_type>;
            });
            if mir.is_parallel {
               type_constraints.push(quote_spanned!{field_type.span()=>
                  let _par_constraints : ascent::internal::ParTypeConstraints<#field_type>;
               });
            }
         }
      }
   }

   let mut relation_initializations = vec![];
   for (rel, md) in mir.relations_metadata.iter() {
      if let Some(ref init) = md.initialization {
         let rel_name = &rel.name;
         relation_initializations.push(quote! {
            _self.#rel_name = #init;
         });
      }
   }
   if !relation_initializations.is_empty() {
      relation_initializations.push(quote! {
         _self.update_indices_priv();
      })
   }

   let par_usings = if mir.is_parallel {quote! {
      use ascent::rayon::iter::ParallelBridge;
      use ascent::rayon::iter::ParallelIterator;
      use ascent::internal::CRelIndexRead;
      use ascent::internal::CRelIndexReadAll;
      use ascent::internal::Freezable;
   }} else {quote!{}};

   let more_usings = if !mir.is_parallel {quote! {
      use ascent::internal::RelIndexWrite;
   }} else { quote! {
      use ascent::internal::CRelIndexWrite;
   }};

   let run_usings = quote! {
      use core::cmp::PartialEq;
      use ascent::internal::RelIndexRead;
      use ascent::internal::RelIndexReadAll;
      use ascent::to_rel_index::ToRelIndex0;
      use ascent::tuple_of_borrowed::TupleOfBorrowed;
      #more_usings
      #par_usings
   };
   
   let generate_run_timeout = !is_ascent_run && mir.config.generate_run_partial; 
   let run_func = if is_ascent_run {quote!{}} 
   else if generate_run_timeout {
      quote! {
         #[doc = "Runs the Ascent program to a fixed point."]
         pub fn run(&mut self) {
            self.run_timeout(::std::time::Duration::MAX);
         }
      } 
   } else {
      quote! {
         #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
         #[doc = "Runs the Ascent program to a fixed point."]
         pub fn run(&mut self) {
            macro_rules! __check_return_conditions {() => {};}
            #run_usings
            self.update_indices_priv();
            let _self = self;
            #(#sccs_compiled)*
         }
      }
   };
   let run_timeout_func = if !generate_run_timeout {quote!{}} else {
      quote! {
         #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
         #[doc = "Runs the Ascent program to a fixed point or until the timeout is reached. In case of a timeout returns false"]
         pub fn run_timeout(&mut self, timeout: ::std::time::Duration) -> bool {
            let __start_time = ::ascent::internal::Instant::now();
            macro_rules! __check_return_conditions {() => {
               if timeout < ::std::time::Duration::MAX && __start_time.elapsed() >= timeout {return false;}
            };}
            #run_usings
            self.update_indices_priv();
            let _self = self;
            #(#sccs_compiled)*
            true
         }
      }
   };
   let run_code = if !is_ascent_run {quote!{}} else {
      quote! {
         macro_rules! __check_return_conditions {() => {};}
         #run_usings
         let _self = &mut __run_res;
         #(#relation_initializations)*
         #(#sccs_compiled)*
      }
   };

   let relation_initializations_for_default_impl = 
      if is_ascent_run {vec![]} else {relation_initializations};
   let summary = mir_summary(mir);

   let (ty_impl_generics, ty_ty_generics, ty_where_clause) = mir.signatures.split_ty_generics_for_impl();
   let (impl_impl_generics, impl_ty_generics, impl_where_clause) = mir.signatures.split_impl_generics_for_impl();

   let ty_signature = &mir.signatures.declaration;
   if let Some(impl_signature) = &mir.signatures.implementation {
      assert_eq!(ty_signature.ident, impl_signature.ident, "The identifiers of struct and impl must match");
   }

   let ty_ty_generics_str = quote!(#ty_ty_generics).to_string();
   let impl_ty_generics_str = quote!(#impl_ty_generics).to_string();
   assert_eq!(ty_ty_generics_str, impl_ty_generics_str, "The generic parameters of struct ({ty_ty_generics_str}) and impl ({impl_ty_generics_str}) must match");

   let vis = &ty_signature.visibility;
   let struct_name = &ty_signature.ident;
   let struct_attrs = &ty_signature.attrs;
   let summary_fn = if is_ascent_run { quote! {
      pub fn summary(&self) -> &'static str {#summary}
   }} else { quote! {
      pub fn summary() -> &'static str {#summary}
   }};
   let rule_time_fields = if mir.config.include_rule_times {rule_time_fields} else {vec![]};
   let rule_time_fields_defaults = if mir.config.include_rule_times {rule_time_fields_defaults} else {vec![]};

   let mut rel_codegens = vec![];
   for rel in mir.relations_ir_relations.keys() {
      let macro_path = &mir.relations_metadata[rel].ds_macro_path;
      let macro_input = rel_ds_macro_input(rel, mir);
      rel_codegens.push(quote_spanned!{ macro_path.span()=> #macro_path::rel_codegen!{#macro_input} });
   }

   let sccs_count = sccs_ordered.len();
   let res = quote! {
      #(#rel_codegens)*

      #(#struct_attrs)*
      #vis struct #struct_name #ty_impl_generics #ty_where_clause {
         #(#relation_fields)*
         scc_times: [std::time::Duration; #sccs_count],
         scc_iters: [usize; #sccs_count],
         #(#rule_time_fields)*
         pub update_time_nanos: std::sync::atomic::AtomicU64,
         pub update_indices_duration: std::time::Duration,
      }
      impl #impl_impl_generics #struct_name #impl_ty_generics #impl_where_clause {
         #run_func

         #run_timeout_func
         // TODO remove pub update_indices at some point
         #[allow(noop_method_call, suspicious_double_ref_op)]
         fn update_indices_priv(&mut self) {
            let before = ::ascent::internal::Instant::now();
            #update_indices_body
            self.update_indices_duration += before.elapsed();
         }

         #[deprecated = "Explicit call to update_indices not required anymore."]
         pub fn update_indices(&mut self) {
            self.update_indices_priv();
         }
         fn type_constraints() {
            #(#type_constraints)*
         }
         #summary_fn
         
         pub fn relation_sizes_summary(&self) -> String {
            #relation_sizes_body
         }
         pub fn scc_times_summary(&self) -> String {
            #scc_times_summary_body
         }
      }
      impl #impl_impl_generics Default for #struct_name #impl_ty_generics #impl_where_clause {
         fn default() -> Self {
            let mut _self = #struct_name {
               #(#field_defaults)*
               scc_times: [std::time::Duration::ZERO; #sccs_count],
               scc_iters: [0; #sccs_count],
               #(#rule_time_fields_defaults)*
               update_time_nanos: Default::default(),
               update_indices_duration: std::time::Duration::default()
            };
            #(#relation_initializations_for_default_impl)*
            _self
         }
      }
   };
   if !is_ascent_run {res} else {
      quote! {
         {
            #res
            let mut __run_res: #struct_name #ty_ty_generics = #struct_name::default();
            #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
            {
               ascent::internal::comment("running...");
               #run_code
            }
            __run_res
         }
      }
   }
}

fn rel_ind_common_type(rel: &RelationIdentity, mir: &AscentMir) -> Type {
   if rel.is_lattice {
      parse_quote!{ () }
   } else {
      let macro_path = &mir.relations_metadata[rel].ds_macro_path;
      let macro_input = rel_ds_macro_input(rel, mir);
      parse_quote_spanned!{ macro_path.span()=> #macro_path::rel_ind_common!(#macro_input) }
   }
}

fn rel_index_type(rel: &IrRelation, mir: &AscentMir) -> Type {
   let span = rel.relation.name.span();
   let key_type = rel.key_type();
   let value_type = rel.value_type();

   let is_lat_full_index = rel.relation.is_lattice && &mir.lattices_full_indices[&rel.relation] == rel;
   
   if rel.relation.is_lattice {
      let res = if !mir.is_parallel {
         if is_lat_full_index {
            quote_spanned! { span=>ascent::internal::RelFullIndexType<#key_type, #value_type> }
         } else {
            quote_spanned!{ span=>ascent::internal::LatticeIndexType<#key_type, #value_type> }
         }
      } else {
         // parallel
         if is_lat_full_index {
            quote_spanned! { span=>ascent::internal::CRelFullIndex<#key_type, #value_type> }
         } else if rel.is_no_index() {
            quote_spanned! { span=>ascent::internal::CRelNoIndex<#value_type> }
         } else {
            quote_spanned! { span=>ascent::internal::CRelIndex<#key_type, #value_type> }
         }
      };
      syn::parse2(res).unwrap()
   } else {
      let macro_path = &mir.relations_metadata[&rel.relation].ds_macro_path;
      let span = macro_path.span();
      let macro_input = rel_ds_macro_input(&rel.relation, mir);
      if rel.is_full_index() {
         parse_quote_spanned! {span=> #macro_path::rel_full_ind!(#macro_input, #key_type, #value_type)}
      } else {
         let ind = rel_index_to_macro_input(&rel.indices);
         parse_quote_spanned! {span=> #macro_path::rel_ind!(#macro_input, #ind, #key_type, #value_type)}
      }
   }
}

fn rel_type(rel: &RelationIdentity, mir: &AscentMir) -> Type {
   let field_types = tuple_type(&rel.field_types);

   if rel.is_lattice {
      if mir.is_parallel {
         parse_quote! {::ascent::boxcar::Vec<::std::sync::RwLock<#field_types>>} 
      } else {
         parse_quote! {::std::vec::Vec<#field_types>} 
      }
   } else {
      let macro_path = &mir.relations_metadata[rel].ds_macro_path;
      let macro_input = rel_ds_macro_input(rel, mir);
      parse_quote_spanned! {macro_path.span()=> #macro_path::rel!(#macro_input) }
   }
}


fn rel_index_to_macro_input(ind: &[usize]) -> TokenStream {
   let indices =ind.iter().cloned().map(syn::Index::from);
   quote!{ [#(#indices),*] }
}

fn rel_ds_macro_input(rel: &RelationIdentity, mir: &AscentMir) -> TokenStream {
   let span = rel.name.span();
   let field_types = tuple_type(&rel.field_types);
   let indices = mir.relations_ir_relations[rel].iter()
      .sorted_by_key(|r| &r.indices)
      .map(|ir_rel| rel_index_to_macro_input(&ir_rel.indices));
   let args = &mir.relations_metadata[rel].ds_macro_args;
   let par: Ident = if mir.is_parallel { parse_quote_spanned! {span=> par} } else { parse_quote_spanned! {span=> ser} };
   let name = Ident::new(&format!("{}_{}", mir.signatures.declaration.ident, rel.name), span);
   quote! {
      #name,
      #field_types,
      [#(#indices),*],
      #par,
      (#args)
   }
}

fn rule_time_field_name(scc_ind: usize, rule_ind: usize) ->Ident {
   Ident::new(&format!("rule{}_{}_duration", scc_ind, rule_ind), Span::call_site())
}

fn compile_mir_scc(mir: &AscentMir, scc_ind: usize) -> proc_macro2::TokenStream {

   let scc = &mir.sccs[scc_ind];
   let mut move_total_to_delta = vec![];
   let mut shift_delta_to_total_new_to_delta = vec![];
   let mut move_total_to_field = vec![];
   let mut freeze_code = vec![];
   let mut unfreeze_code = vec![];

   let _self = quote! { _self };

   use std::iter::once;
   for rel in scc.dynamic_relations.iter().flat_map(|(rel, indices)| once(Either::Left(rel)).chain(indices.iter().map(Either::Right))) {
      let (ir_name, ty) = match rel {
        Either::Left(rel) => (rel_ind_common_var_name(rel), rel_ind_common_type(rel, mir)),
        Either::Right(rel_ind) => (rel_ind.ir_name(), rel_index_type(rel_ind, mir)),
      };
      let delta_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Delta);
      let total_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Total);
      let new_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::New);
      let total_field = &ir_name;
      move_total_to_delta.push(quote_spanned! {ir_name.span()=>
         let mut #delta_var_name: #ty = ::std::mem::take(&mut #_self.#total_field);
         let mut #total_var_name : #ty = Default::default();
         let mut #new_var_name : #ty = Default::default();
      });

      match rel {
         Either::Left(_rel_ind_common) => {
            shift_delta_to_total_new_to_delta.push(quote_spanned!{ir_name.span()=>
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(&mut #new_var_name, &mut #delta_var_name, &mut #total_var_name);
            });
            move_total_to_delta.push(quote_spanned! {ir_name.span()=>
               ::ascent::internal::RelIndexMerge::init(&mut #new_var_name, &mut #delta_var_name, &mut #total_var_name);
            });
         },
         Either::Right(ir_rel) => {
            let delta_expr = expr_for_rel_write(&MirRelation::from(ir_rel.clone(), Delta), mir);
            let total_expr = expr_for_rel_write(&MirRelation::from(ir_rel.clone(), Total), mir);
            let new_expr = expr_for_rel_write(&MirRelation::from(ir_rel.clone(), New), mir);

            shift_delta_to_total_new_to_delta.push(quote_spanned!{ir_name.span()=>
               ::ascent::internal::RelIndexMerge::merge_delta_to_total_new_to_delta(&mut #new_expr, &mut #delta_expr, &mut #total_expr);
            });
            move_total_to_delta.push(quote_spanned! {ir_name.span()=>
               ::ascent::internal::RelIndexMerge::init(&mut #new_expr, &mut #delta_expr, &mut #total_expr);
            });
         },
      }
      
      move_total_to_field.push(quote_spanned!{ir_name.span()=>
         #_self.#total_field = #total_var_name;
      });

      if mir.is_parallel {
         freeze_code.push(quote_spanned!{ir_name.span()=>
            #total_var_name.freeze();
            #delta_var_name.freeze();
         });

         unfreeze_code.push(quote_spanned!{ir_name.span()=>
            #total_var_name.unfreeze();
            #delta_var_name.unfreeze();
         });
      }
   }
   for rel in scc.body_only_relations.iter().flat_map(|(rel, indices)| once(Either::Left(rel)).chain(indices.iter().map(Either::Right))) {
      let (ir_name, ty) = match rel {
         Either::Left(rel) => (rel_ind_common_var_name(rel), rel_ind_common_type(rel, mir)),
         Either::Right(rel_ind) => (rel_ind.ir_name(), rel_index_type(rel_ind, mir)),
      };
      let total_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Total);
      let total_field = &ir_name;

      if mir.is_parallel {
         move_total_to_delta.push(quote_spanned!{ir_name.span()=>
            #_self.#total_field.freeze();
         });
      }

      move_total_to_delta.push(quote_spanned! {ir_name.span()=>
         let #total_var_name: #ty = std::mem::take(&mut #_self.#total_field);
      });

      move_total_to_field.push(quote_spanned!{ir_name.span()=>
         #_self.#total_field = #total_var_name;
      });
   }
   
   let rule_parallelism = mir.config.inter_rule_parallelism && mir.is_parallel;

   let mut evaluate_rules = vec![];

   for (i, rule) in scc.rules.iter().enumerate() {
      let msg = mir_rule_summary(rule);
      let rule_compiled = compile_mir_rule(rule, scc, mir);
      let rule_time_field = rule_time_field_name(scc_ind, i);
      let (before_rule_var, update_rule_time_field) = if mir.config.include_rule_times {
         (quote! {let before_rule = ::ascent::internal::Instant::now();}, 
          quote!{_self.#rule_time_field += before_rule.elapsed();})
      } else {(quote!{}, quote!{})};
      evaluate_rules.push(if rule_parallelism { quote! {
         ascent::internal::comment(#msg);
         __scope.spawn(|_| {
            #before_rule_var
            #rule_compiled
            #update_rule_time_field
         });
      }} else { quote! {
         #before_rule_var
         ascent::internal::comment(#msg);
         {
            #rule_compiled
         }
         #update_rule_time_field
      }});
   }
   
   let evaluate_rules = if rule_parallelism {
      quote! {
         ascent::rayon::scope(|__scope| {
            #(#evaluate_rules)*
         }); 
      }
   } else {
      quote! { #(#evaluate_rules)* }
   };

   let changed_var_def_code = if !mir.is_parallel {
      quote! { let mut __changed = false; }
   } else {
      quote! { let __changed = std::sync::atomic::AtomicBool::new(false); }
   };
   let check_changed_code = if !mir.is_parallel {
      quote! {__changed}
   } else {
      quote! {__changed.load(std::sync::atomic::Ordering::Relaxed)}
   };

   let evaluate_rules_loop = if scc.is_looping { quote! {
      #[allow(unused_assignments, unused_variables)]
      loop {
         #changed_var_def_code

         #(#freeze_code)*
         // evaluate rules
         #evaluate_rules

         #(#unfreeze_code)*
         #(#shift_delta_to_total_new_to_delta)*
         _self.scc_iters[#scc_ind] += 1;
         if !#check_changed_code {break;}
         __check_return_conditions!();
      }

   }} else {quote! {
      #[allow(unused_assignments, unused_variables)]
      {
         // let mut __changed = false;
         #changed_var_def_code
         #(#freeze_code)*

         #evaluate_rules

         #(#unfreeze_code)*

         #(#shift_delta_to_total_new_to_delta)*
         #(#shift_delta_to_total_new_to_delta)*
         _self.scc_iters[#scc_ind] += 1;
         __check_return_conditions!();
      }
   }};
   quote! {
      // define variables for delta and new versions of dynamic relations in the scc
      // move total versions of dynamic indices to delta
      #(#move_total_to_delta)*

      #evaluate_rules_loop

      #(#move_total_to_field)*
   }
}


fn compile_relation_sizes_body(mir: &AscentMir) -> proc_macro2::TokenStream {
   let mut write_sizes = vec![];
   for r in mir.relations_ir_relations.keys().sorted_by_key(|r| &r.name) {
      let rel_name = &r.name;
      let rel_name_str = r.name.to_string();
      write_sizes.push(quote! {
         writeln!(&mut res, "{} size: {}", #rel_name_str, self.#rel_name.len()).unwrap();
      });
   }
   quote! {
      use std::fmt::Write;
      let mut res = String::new();
      #(#write_sizes)*
      res
   }
}

fn compile_scc_times_summary_body(mir: &AscentMir) -> proc_macro2::TokenStream {
   let mut res = vec![];
   for i in 0..mir.sccs.len(){
      let i_str = format!("{}", i);
      res.push(quote!{
         writeln!(&mut res, "scc {}: iterations: {}, time: {:?}", #i_str, self.scc_iters[#i], self.scc_times[#i]).unwrap();
      });
      if mir.config.include_rule_times {
         let mut sum_of_rule_times = quote!{ std::time::Duration::ZERO };
         for (rule_ind, _rule) in mir.sccs[i].rules.iter().enumerate() {
            let rule_time_field = rule_time_field_name(i, rule_ind);
            sum_of_rule_times = quote!{ #sum_of_rule_times + self.#rule_time_field};
         }
         res.push(quote! {
            let sum_of_rule_times = #sum_of_rule_times;
            writeln!(&mut res, "  sum of rule times: {:?}", sum_of_rule_times).unwrap();
         });
         for (rule_ind, rule) in mir.sccs[i].rules.iter().enumerate() {
            let rule_time_field = rule_time_field_name(i, rule_ind);
            let rule_summary = mir_rule_summary(rule);
            res.push(quote!{
               writeln!(&mut res, "  rule {}\n    time: {:?}", #rule_summary, self.#rule_time_field).unwrap();
            });
         }
         res.push(quote!{
            writeln!(&mut res, "-----------------------------------------").unwrap();
         });
      }
   }
   let update_indices_time_code = quote! {
      writeln!(&mut res, "update_indices time: {:?}", self.update_indices_duration).unwrap();
   };
   quote!{
      use std::fmt::Write;
      let mut res = String::new();
      #update_indices_time_code
      #(#res)*
      res
   }
}

fn compile_update_indices_function_body(mir: &AscentMir) -> proc_macro2::TokenStream {
   let par = mir.is_parallel;
   let mut res = vec![];
   if par {
      res.push(quote! { use ascent::rayon::iter::{IntoParallelIterator, ParallelIterator}; })
   }
   let (rel_index_write_trait, index_insert_fn) = if !par {
      (quote! {ascent::internal::RelIndexWrite}, quote! {index_insert})
   } else {
      (quote! {ascent::internal::CRelIndexWrite}, quote!{index_insert})
   };
   for (r,indices_set) in mir.relations_ir_relations.iter(){
      
      let _ref = if !par { quote!{&mut} } else { quote!{&} }.with_span(r.name.span());
      let ind_common = rel_ind_common_var_name(r);
      let rel_index_write_trait = rel_index_write_trait.clone().with_span(r.name.span());
      let _self = quote_spanned!{r.name.span().resolved_at(Span::call_site())=> self };
      let to_rel_index_fn = if !par { quote!{to_rel_index_write} } else { quote!{to_c_rel_index_write} };
      let to_rel_index = if r.is_lattice { quote!{} } else {
         quote! {.#to_rel_index_fn(#_ref #_self.#ind_common) }
      };

      let mut update_indices = vec![];
      for ind in indices_set.iter(){
         let ind_name = &ind.ir_name();
         let selection_tuple : Vec<Expr> = ind.indices.iter().map(|&i| {
            let ind = syn::Index::from(i); 
            parse_quote_spanned! {r.name.span()=> tuple.#ind.clone()}
         }).collect_vec();
         let selection_tuple = tuple_spanned(&selection_tuple, r.name.span());
         let entry_val = index_get_entry_val_for_insert(
            ind, &parse_quote_spanned!{r.name.span()=> tuple}, &parse_quote_spanned!{r.name.span()=> _i});
         let _pre_ref = if r.is_lattice {quote!()} else {_ref.clone()};
         update_indices.push(quote_spanned! {r.name.span()=>
            let selection_tuple = #selection_tuple;
            let rel_ind = #_ref #_self.#ind_name;
            #rel_index_write_trait::#index_insert_fn(#_pre_ref rel_ind #to_rel_index, selection_tuple, #entry_val);
         });

      }
      let rel_name = &r.name;
      let maybe_lock = if r.is_lattice && mir.is_parallel {
         quote_spanned!{r.name.span()=> let tuple = tuple.read().unwrap(); }
      } else { quote!{} };
      if !par {
         res.push(quote_spanned! {r.name.span()=>
            for (_i, tuple) in #_self.#rel_name.iter().enumerate() {
               #maybe_lock
               #(#update_indices)*
            }
         });
      } else {
         res.push(quote_spanned! {r.name.span()=>
            (0..#_self.#rel_name.len()).into_par_iter().for_each(|_i| {
               let tuple = &#_self.#rel_name[_i];
               #maybe_lock
               #(#update_indices)*
            });
         });
      }
   }

   quote! {
      use ascent::to_rel_index::ToRelIndex0;
      use #rel_index_write_trait;
      #(#res)*
   }
}

fn compile_cond_clause(cond: &CondClause, body: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
   match cond {
      CondClause::IfLet(if_let_clause) => {
         let pat = &if_let_clause.pattern;
         let expr = &if_let_clause.exp;
         quote_spanned! {if_let_clause.if_keyword.span()=>
            if let #pat = #expr {
               #body
            }
         }
      }
      CondClause::Let(let_clause) => {
         let pat = &let_clause.pattern;
         let expr = &let_clause.exp;
         quote_spanned! {let_clause.let_keyword.span()=>
            let #pat = #expr;
            #body
         }
      }
      CondClause::If(if_clause) => {
         let cond = &if_clause.cond;
         quote_spanned! {if_clause.if_keyword.span()=>
            if #cond {
               #body
            }
         }
      }
   }
}

fn compile_mir_rule(rule: &MirRule, scc: &MirScc, mir: &AscentMir) -> proc_macro2::TokenStream {
   let (head_rels_structs_and_vars, head_update_code) = 
      head_clauses_structs_and_update_code(rule, scc, mir);

   const MAX_PAR_ITERS: usize = 2;

   // do parallel iteration up to this clause index (exclusive)
   let par_iter_to_ind = if mir.is_parallel {
      use itertools::FoldWhile::*;
      rule.body_items.iter().fold_while((0, 0), |(count, i), bi| {
         let new_count = count + matches!(bi, MirBodyItem::Clause(..)) as usize; 
         if new_count > MAX_PAR_ITERS { Done((new_count, i)) } else { Continue((new_count, i + 1)) }
      }).into_inner().1
   } else { 0 };
   let rule_code = compile_mir_rule_inner(rule, scc, mir, par_iter_to_ind, head_update_code, 0);
   quote!{
      #head_rels_structs_and_vars
      #rule_code
   }
}

fn compile_mir_rule_inner(rule: &MirRule, _scc: &MirScc, mir: &AscentMir, par_iter_to_ind: usize, head_update_code: proc_macro2::TokenStream, clause_ind: usize) 
-> proc_macro2::TokenStream 
{
   if Some(clause_ind) == rule.simple_join_start_index && rule.reorderable {
      let mut rule_cp1 = rule.clone();
      let mut rule_cp2 = rule.clone();
      rule_cp1.reorderable = false;
      rule_cp2.reorderable = false;
      rule_cp2.body_items.swap(clause_ind, clause_ind + 1);
      let rule_cp1_compiled = compile_mir_rule_inner(&rule_cp1, _scc, mir, par_iter_to_ind, head_update_code.clone(), clause_ind);
      let rule_cp2_compiled = compile_mir_rule_inner(&rule_cp2, _scc, mir, par_iter_to_ind, head_update_code        , clause_ind);

      if let [MirBodyItem::Clause(bcl1), MirBodyItem::Clause(bcl2)] = &rule.body_items[clause_ind..clause_ind+2]{
         let rel1_var_name = expr_for_rel(&bcl1.rel, mir);
         let rel2_var_name = expr_for_rel(&bcl2.rel, mir);

         return quote_spanned!{bcl1.rel_args_span=>
            if #rel1_var_name.len() <= #rel2_var_name.len() {
               #rule_cp1_compiled
            } else {
               #rule_cp2_compiled
            }
         };
      } else {
         panic!("unexpected body items in reorderable rule")
      }
   }
   if clause_ind < rule.body_items.len(){
      let bitem = &rule.body_items[clause_ind];
      let doing_simple_join = rule.simple_join_start_index == Some(clause_ind);
      let next_loop = if doing_simple_join {
         compile_mir_rule_inner(rule, _scc, mir, par_iter_to_ind, head_update_code, clause_ind + 2)
      } else {
         compile_mir_rule_inner(rule, _scc, mir, par_iter_to_ind, head_update_code, clause_ind + 1)
      };

      match bitem {
         MirBodyItem::Clause(bclause) => {
            let (clause_ind, bclause) = 
               if doing_simple_join {(clause_ind + 1, rule.body_items[clause_ind + 1].unwrap_clause())} 
               else {(clause_ind, bclause)};

            let bclause_rel_name = &bclause.rel.relation.name;
            let selected_args = &bclause.selected_args();
            let pre_clause_vars = rule.body_items.iter().take(clause_ind)
                                       .flat_map(MirBodyItem::bound_vars)
                                       .collect::<Vec<_>>();

            let clause_vars = bclause.vars();
            let common_vars = clause_vars.iter().filter(|(_i,v)| pre_clause_vars.contains(v)).collect::<Vec<_>>();
            let common_vars_no_indices = common_vars.iter().map(|(_i,v)| v.clone()).collect::<Vec<_>>();

            let cloning_needed = true;

            let matched_val_ident = Ident::new("__val", bclause.rel_args_span);
            let new_vars_assignments = clause_var_assignments(
               &bclause.rel, clause_vars.iter().filter(|(_i, var)| !common_vars_no_indices.contains(var)).cloned(),
               &matched_val_ident, &parse_quote!{_self.#bclause_rel_name}, cloning_needed, mir
            );

            let selected_args_cloned = selected_args.iter().map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple_spanned(&selected_args_cloned, bclause.args_span);
            let rel_version_exp = expr_for_rel(&bclause.rel, mir);
            
            let mut conds_then_next_loop = next_loop;
            for cond in bclause.cond_clauses.iter().rev() {
               conds_then_next_loop = compile_cond_clause(cond, conds_then_next_loop);
            }

            let span = bclause.rel_args_span;
            
            let matching_dot_iter = quote_spanned!{bclause.rel_args_span=> __matching};

            let (index_get, iter_all) = if clause_ind < par_iter_to_ind {
               (quote_spanned! {span=> c_index_get}, quote_spanned! {span=> c_iter_all})
            } else {
               (quote_spanned! {span=> index_get}, quote_spanned! {span=> iter_all})
            };

            // The special case where the first clause has indices, but there are no expressions
            // in the args of the first clause
            if doing_simple_join {
               let cl1 = rule.body_items[rule.simple_join_start_index.unwrap()].unwrap_clause();
               let cl2 = bclause;
               let cl1_var_name = expr_for_rel(&cl1.rel, mir);
               let cl2_var_name = expr_for_rel(&cl2.rel, mir);
               let cl1_vars = cl1.vars();

               let cl1_rel_name = &cl1.rel.relation.name;

               let mut cl1_join_vars_assignments = vec![];
               for (tuple_ind, &i) in cl1.rel.indices.iter().enumerate() {
                  let var = expr_to_ident(&cl1.args[i]).unwrap();
                  let tuple_ind = syn::Index{index: tuple_ind as u32, span: var.span()};
                  cl1_join_vars_assignments.push(quote_spanned! {var.span()=> let #var = __cl1_joined_columns.#tuple_ind;});
               }

               let cl1_matched_val_ident = syn::Ident::new("cl1_val", cl1.rel_args_span);
               let cl1_vars_assignments = clause_var_assignments(
                  &cl1.rel, cl1_vars.iter().filter(|(i, _var)| !cl1.rel.indices.contains(i)).cloned(),
                  &cl1_matched_val_ident, &parse_quote!{_self.#cl1_rel_name}, cloning_needed, mir
               );
               let cl1_vars_assignments = vec![cl1_vars_assignments];

               let joined_args_for_cl2_cloned = cl2.selected_args().iter().map(exp_cloned).collect_vec();
               let joined_args_tuple_for_cl2 = tuple_spanned(&joined_args_for_cl2_cloned, cl2.args_span);
               
               
               let cl1_tuple_indices_iter = quote_spanned!(cl1.rel_args_span=> __cl1_tuple_indices);

               let mut cl1_conds_then_rest = quote_spanned! {bclause.rel_args_span=>
                  #matching_dot_iter.clone().for_each(|__val|  {
                     // TODO we may be doing excessive cloning
                     #new_vars_assignments
                     #conds_then_next_loop
                  });
               };
               for cond in cl1.cond_clauses.iter().rev() {
                  cl1_conds_then_rest = compile_cond_clause(cond, cl1_conds_then_rest);
               }
               quote_spanned! {cl1.rel_args_span=>
                  #cl1_var_name.#iter_all().for_each(|(__cl1_joined_columns, __cl1_tuple_indices)| {
                     let __cl1_joined_columns = __cl1_joined_columns.tuple_of_borrowed();
                     #(#cl1_join_vars_assignments)*
                     if let Some(__matching) = #cl2_var_name.#index_get(&#joined_args_tuple_for_cl2) {
                        #cl1_tuple_indices_iter.for_each(|cl1_val| {
                           #(#cl1_vars_assignments)*
                           #cl1_conds_then_rest
                        });
                     }
                  });
               }
            } else {
               quote_spanned! {bclause.rel_args_span=>
                  if let Some(__matching) = #rel_version_exp.#index_get( &#selected_args_tuple) {
                     #matching_dot_iter.for_each(|__val|  {
                        // TODO we may be doing excessive cloning
                        #new_vars_assignments
                        #conds_then_next_loop
                     });
                  }
               }
            }

         },
         MirBodyItem::Generator(gen) => {
            let pat = &gen.pattern;
            let expr = &gen.expr;
            quote_spanned! {gen.for_keyword.span()=>
               for #pat in #expr {
                  #next_loop
               }
            }
         }
         MirBodyItem::Cond(cond) => compile_cond_clause(cond, next_loop),
         MirBodyItem::Agg(agg) => {
            
            let pat = &agg.pat;
            let rel_name = &agg.rel.relation.name;
            let mir_relation = MirRelation::from(agg.rel.clone(), Total);
            // let rel_version_var_name = mir_relation.var_name();
            let rel_expr = expr_for_rel(&mir_relation, mir);
            let selected_args = mir_relation.indices.iter().map(|&i| &agg.rel_args[i]);
            let selected_args_cloned = selected_args.map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple_spanned(&selected_args_cloned, agg.span);
            let agg_args_tuple_indices = 
               agg.bound_args.iter()
               .map(|arg| (agg.rel_args.iter()
                        .find_position(|rel_arg| expr_to_ident(rel_arg) == Some(arg.clone())).unwrap().0, arg.clone()));
            
            let agg_args_tuple = tuple_spanned(&agg.bound_args.iter().map(|v| parse_quote!{#v}).collect_vec(), agg.span);

            let vars_assignments = clause_var_assignments(
               &MirRelation::from(agg.rel.clone(), MirRelationVersion::Total), agg_args_tuple_indices, 
               &parse_quote_spanned!{agg.span=> __val}, &parse_quote!{_self.#rel_name},
               false, mir
            );

            let agg_func = &agg.aggregator;
            let _self = quote!{ _self };
            quote_spanned! {agg.span=>
               let __aggregated_rel = #rel_expr;
               let __matching = __aggregated_rel.index_get( &#selected_args_tuple);
               let __agg_args = __matching.into_iter().flatten().map(|__val| {
                  #vars_assignments
                  #agg_args_tuple
               });
               for #pat in #agg_func(__agg_args) {
                  #next_loop
               }
               
            }
         }
      }
   } else {
      quote! {
         // let before_update = ::ascent::internal::Instant::now();
         #head_update_code
         // let update_took = before_update.elapsed();
         // _self.update_time_nanos.fetch_add(update_took.as_nanos() as u64, std::sync::atomic::Ordering::Relaxed);
      }
   }
}

fn head_clauses_structs_and_update_code(rule: &MirRule, scc: &MirScc, mir: &AscentMir) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
   let mut add_rows = vec![];

   let set_changed_true_code = if !mir.is_parallel {
      quote! { __changed = true; }
   } else {
      quote! { __changed.store(true, std::sync::atomic::Ordering::Relaxed);}
   };

   for hcl in rule.head_clause.iter() {

      let head_rel_name = Ident::new(&hcl.rel.name.to_string(), hcl.span);
      let hcl_args_converted = hcl.args.iter().cloned().map(convert_head_arg).collect_vec();
      let new_row_tuple = tuple_spanned(&hcl_args_converted, hcl.args_span);
      
      let head_relation = &hcl.rel;
      let row_type = tuple_type(&head_relation.field_types);

      let mut update_indices = vec![];
      let rel_indices = scc.dynamic_relations.get(head_relation);
      let (rel_index_write_trait, index_insert_fn) = if !mir.is_parallel {
         (quote! { ::ascent::internal::RelIndexWrite }, quote! {index_insert})
      } else {
         (quote! { ::ascent::internal::CRelIndexWrite }, quote! {index_insert})
      };
      let (rel_index_write_trait, index_insert_fn) = (rel_index_write_trait.with_span(hcl.span), index_insert_fn.with_span(hcl.span)); 
      let new_ref = if !mir.is_parallel { quote!{&mut} } else { quote!{&} };
      let mut used_fields = HashSet::new();
      if let Some(rel_indices) = rel_indices {
         for rel_ind in rel_indices.iter(){
            if rel_ind.is_full_index() {continue};
            let var_name = if !mir.is_parallel {
               expr_for_rel_write(&MirRelation::from(rel_ind.clone(), New), mir)
            } else {
               expr_for_c_rel_write(&MirRelation::from(rel_ind.clone(), New), mir)
            };
            let args_tuple : Vec<Expr> = rel_ind.indices.iter().map(|&i| {
               let i_ind = syn::Index::from(i);
               syn::parse2(quote_spanned!{hcl.span=> __new_row.#i_ind.clone()}).unwrap()
            }).collect();
            used_fields.extend(rel_ind.indices.iter().cloned());
            if let IndexValType::Direct(direct) = &rel_ind.val_type {
               used_fields.extend(direct.iter().cloned());
            }
            let args_tuple = tuple(&args_tuple);
            let entry_val = index_get_entry_val_for_insert(
               rel_ind, &parse_quote_spanned!{hcl.span=> __new_row}, &parse_quote_spanned!{hcl.span=> __new_row_ind});
            update_indices.push(quote_spanned! {hcl.span=>
               #rel_index_write_trait::#index_insert_fn(#new_ref #var_name, #args_tuple, #entry_val);
            });
         }
      }

      let head_rel_full_index = &mir.relations_full_indices[head_relation];

      let expr_for_rel_maybe_mut = if mir.is_parallel { expr_for_c_rel_write } else { expr_for_rel_write };
      let head_rel_full_index_expr_new = expr_for_rel_maybe_mut(&MirRelation::from(head_rel_full_index.clone(), New), mir);
      let head_rel_full_index_expr_delta = expr_for_rel(&MirRelation::from(head_rel_full_index.clone(), Delta), mir);
      let head_rel_full_index_expr_total = expr_for_rel(&MirRelation::from(head_rel_full_index.clone(), Total), mir);


      let rel_full_index_write_trait = if !mir.is_parallel {
         quote! { ::ascent::internal::RelFullIndexWrite }
      } else {
         quote! { ::ascent::internal::CRelFullIndexWrite }
      }.with_span(hcl.span);

      let new_row_to_be_pushed = (0..hcl.rel.field_types.len()).map(|i| {
         let ind = syn::Index::from(i);
         let clone = if used_fields.contains(&i) { quote!{.clone()} } else { quote!{} };
         parse_quote_spanned!{hcl.span=> __new_row.#ind #clone }
      }).collect_vec();
      let new_row_to_be_pushed = tuple_spanned(&new_row_to_be_pushed, hcl.span);

      let push_code = if !mir.is_parallel { quote! {
         let __new_row_ind = _self.#head_rel_name.len();
         _self.#head_rel_name.push(#new_row_to_be_pushed);
      }} else { quote! {
         let __new_row_ind = _self.#head_rel_name.push(#new_row_to_be_pushed);
      }};

      if !hcl.rel.is_lattice { 
         let add_row = quote_spanned!{hcl.span=>
            let __new_row: #row_type = #new_row_tuple;

            if !::ascent::internal::RelFullIndexRead::contains_key(&#head_rel_full_index_expr_total, &__new_row) &&
               !::ascent::internal::RelFullIndexRead::contains_key(&#head_rel_full_index_expr_delta, &__new_row) {
               if #rel_full_index_write_trait::insert_if_not_present(#new_ref #head_rel_full_index_expr_new, 
                  &__new_row, ())
               {
                  #push_code
                  #(#update_indices)*
                  #set_changed_true_code
               }
            }
         };
         add_rows.push(add_row);
      } else {  // rel.is_lattice:
         let lattice_insertion_mutex = lattice_insertion_mutex_var_name(head_relation);
         let head_lat_full_index = &mir.lattices_full_indices[head_relation];
         let head_lat_full_index_var_name_new = ir_relation_version_var_name(&head_lat_full_index.ir_name(), New);
         let head_lat_full_index_var_name_delta = ir_relation_version_var_name(&head_lat_full_index.ir_name(), Delta);
         let head_lat_full_index_var_name_full = ir_relation_version_var_name(&head_lat_full_index.ir_name(), Total);
         let tuple_lat_index = syn::Index::from(hcl.rel.field_types.len() - 1);
         let lattice_key_args : Vec<Expr> = (0..hcl.args.len() - 1).map(|i| {
            let i_ind = syn::Index::from(i);
            syn::parse2(quote_spanned!{hcl.span=> __new_row.#i_ind}).unwrap()
         }).map(|e| exp_cloned(&e)).collect_vec();
         let lattice_key_tuple = tuple(&lattice_key_args);

         let _self = quote! { _self };
         let add_row = if !mir.is_parallel { quote_spanned! {hcl.span=>
            let __new_row: #row_type = #new_row_tuple;
            let __lattice_key = #lattice_key_tuple;
            if let Some(mut __existing_ind) = #head_lat_full_index_var_name_new.index_get(&__lattice_key)
               .or_else(|| #head_lat_full_index_var_name_delta.index_get(&__lattice_key))
               .or_else(|| #head_lat_full_index_var_name_full.index_get(&__lattice_key)) 
            {
               let __existing_ind = *__existing_ind.next().unwrap();
               // TODO possible excessive cloning here?
               let __lat_changed = ::ascent::Lattice::join_mut(&mut #_self.#head_rel_name[__existing_ind].#tuple_lat_index, __new_row.#tuple_lat_index.clone());
               if __lat_changed {
                  let __new_row_ind = __existing_ind;
                  #(#update_indices)*
                  #set_changed_true_code
               }
            } else {
               let __new_row_ind = #_self.#head_rel_name.len();
               #(#update_indices)*
               #_self.#head_rel_name.push(#new_row_to_be_pushed);
               #set_changed_true_code
            }
         }} else { quote_spanned! {hcl.span=> // mir.is_parallel:
            let __new_row: #row_type = #new_row_tuple;
            let __lattice_key = #lattice_key_tuple;
            let __existing_ind_in_new = #head_lat_full_index_var_name_new.get_cloned(&__lattice_key);
            let __new_has_ind = __existing_ind_in_new.is_some();
            if let Some(__existing_ind) = __existing_ind_in_new
               .or_else(|| #head_lat_full_index_var_name_delta.get_cloned(&__lattice_key))
               .or_else(|| #head_lat_full_index_var_name_full.get_cloned(&__lattice_key)) 
            {
               let __lat_changed = ::ascent::Lattice::join_mut(&mut #_self.#head_rel_name[__existing_ind].write().unwrap().#tuple_lat_index, 
                                                               __new_row.#tuple_lat_index.clone());
               if __lat_changed && !__new_has_ind{
                  let __new_row_ind = __existing_ind;
                  #(#update_indices)*
                  #set_changed_true_code
               }
            } else {
               let __hash = #head_lat_full_index_var_name_new.hash_usize(&__lattice_key);
               let __lock = #_self.#lattice_insertion_mutex.get(__hash % #_self.#lattice_insertion_mutex.len()).expect("lattice_insertion_mutex index out of bounds").lock().unwrap();
               if let Some(__existing_ind) = #head_lat_full_index_var_name_new.get_cloned(&__lattice_key) {
                  ::ascent::Lattice::join_mut(&mut #_self.#head_rel_name[__existing_ind].write().unwrap().#tuple_lat_index, 
                                              __new_row.#tuple_lat_index.clone());
               } else {
                  let __new_row_ind = #_self.#head_rel_name.push(::std::sync::RwLock::new(#new_row_to_be_pushed));
                  #(#update_indices)*
                  #set_changed_true_code
               }
            }
         }};
         add_rows.push(add_row);
      }
   }
   (
      quote!{
         // #(#struct_defs)*
         // #(#rel_data_vars)*
      }, 
      quote!{#(#add_rows)*}
   )
}

fn lattice_insertion_mutex_var_name(head_relation: &RelationIdentity) -> Ident {
   Ident::new(&format!("__{}_mutex", head_relation.name), head_relation.name.span())
}

fn rel_ind_common_var_name(relation: &RelationIdentity) -> Ident {
   Ident::new(&format!("__{}_ind_common", relation.name), relation.name.span())
}

fn convert_head_arg(arg: Expr) -> Expr {
   if let Some(var) = expr_to_ident(&arg){
      parse2(quote_spanned!{arg.span()=> ascent::internal::Convert::convert(#var)}).unwrap()
   } else {
      arg
   }
}

fn expr_for_rel(rel: &MirRelation, mir: &AscentMir) -> proc_macro2::TokenStream {
   fn expr_for_rel_inner(ir_name: &Ident, version: MirRelationVersion, _mir: &AscentMir, mir_rel: &MirRelation) -> (TokenStream, bool) {
      let var = ir_relation_version_var_name(ir_name, version);
      if mir_rel.relation.is_lattice {
         (quote! { & #var }, true)
      } else {
         let rel_ind_common = ir_relation_version_var_name(&rel_ind_common_var_name(&mir_rel.relation), version);
         (quote! { #var.to_rel_index(& #rel_ind_common) }, false)
      }
   }

   if rel.version == MirRelationVersion::TotalDelta {
      let total_expr = expr_for_rel_inner(&rel.ir_name, MirRelationVersion::Total, mir, rel).0;
      let delta_expr = expr_for_rel_inner(&rel.ir_name, MirRelationVersion::Delta, mir, rel).0;
      quote! {
         ascent::internal::RelIndexCombined::new(& #total_expr, & #delta_expr)
      }
   } else {
      let (res, borrowed) = expr_for_rel_inner(&rel.ir_name, rel.version, mir, rel);
      if !borrowed {res} else {quote!{(#res)}}
   }
}

fn expr_for_rel_write(mir_rel: &MirRelation, _mir: &AscentMir) -> proc_macro2::TokenStream {
   let var = mir_rel.var_name();
   if mir_rel.relation.is_lattice {
      quote!{ #var }
   } else {
      let rel_ind_common = ir_relation_version_var_name(&rel_ind_common_var_name(&mir_rel.relation), mir_rel.version);
      quote! { #var.to_rel_index_write(&mut #rel_ind_common) }
   }
}

fn expr_for_c_rel_write(mir_rel: &MirRelation, _mir: &AscentMir) -> proc_macro2::TokenStream {
   let var = mir_rel.var_name();
   if mir_rel.relation.is_lattice {
      quote!{ #var }
   } else {
      let rel_ind_common = ir_relation_version_var_name(&rel_ind_common_var_name(&mir_rel.relation), mir_rel.version);
      quote! { #var.to_c_rel_index_write(&#rel_ind_common) }
   }
}

fn clause_var_assignments(
   rel: &MirRelation, 
   vars: impl Iterator<Item = (usize, Ident)>, 
   val_ident: &Ident, 
   relation_expr: &Expr, 
   cloning_needed: bool,
   mir: &AscentMir
) -> proc_macro2::TokenStream {
   let mut assignments = vec![];

   let mut any_vars = false;
   for (ind_in_tuple, var) in vars {
      let var_type_ascription = {
         let ty = &rel.relation.field_types[ind_in_tuple];
         quote! { : & #ty}
      };
      any_vars = true;
      match &rel.val_type {
         IndexValType::Reference => {
            let ind = syn::Index::from(ind_in_tuple);
            assignments.push(quote! {
               let #var #var_type_ascription = &__row.#ind;
            })
         },
         IndexValType::Direct(inds) => {
            let ind = inds.iter().enumerate().find(|(_i, ind)| **ind == ind_in_tuple).unwrap().0;
            let ind = syn::Index::from(ind);

            assignments.push(quote! {
               let #var #var_type_ascription = #val_ident.#ind;
            })
         },
      }  
   }
   
   if any_vars {
      match &rel.val_type {
         IndexValType::Reference => {
            let maybe_lock = if rel.relation.is_lattice && mir.is_parallel {
               quote! {.read().unwrap()}
            } else { quote! {} };
            let maybe_clone = if cloning_needed {
               quote! {.clone()}
            } else { quote! {} };
            assignments.insert(0, quote! {
               let __row = &#relation_expr[*#val_ident]#maybe_lock #maybe_clone;
            });
         },
         IndexValType::Direct(_) => {
            assignments.insert(0, quote! {
               let #val_ident = #val_ident.tuple_of_borrowed();
            });
         }
      }
   }

   quote! {
      #(#assignments)*
   }
}

fn index_get_entry_val_for_insert(rel_ind: &IrRelation, tuple_expr: &Expr, ind_expr: &Expr) -> Expr {
   match &rel_ind.val_type {
      IndexValType::Reference => ind_expr.clone(),
      IndexValType::Direct(inds) => {
         let val = inds.iter().map(|&ind| {
            let ind = syn::Index::from(ind);
            parse_quote!{
               #tuple_expr.#ind.clone()
            }
         }).collect_vec();
         tuple(&val)
      },
   }
}
