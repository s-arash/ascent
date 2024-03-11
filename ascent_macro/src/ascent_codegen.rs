use std::{collections::{BTreeSet, HashSet}, fmt::Display};

use itertools::{Iterate, Itertools};
use proc_macro2::{Ident, Span};
use syn::{Expr, Type, parse2, spanned::Spanned, parse_quote, parse_quote_spanned};

use crate::{ascent_hir::{IrRelation, IndexValType}, ascent_syntax::{CondClause, RelationIdentity}, syn_utils::pattern_get_vars, utils::TokenStreamExtensions};
use crate::utils::{exp_cloned, expr_to_ident, pat_to_ident, tuple, tuple_spanned, tuple_type};
use crate::ascent_mir::{AscentMir, MirBodyItem, MirRelation, MirRelationVersion, MirRule, MirScc, ir_relation_version_var_name, mir_rule_summary, mir_summary};
use crate::ascent_mir::MirRelationVersion::*;

pub(crate) fn compile_mir(mir: &AscentMir, is_ascent_run: bool) -> proc_macro2::TokenStream {
   
   let mut relation_fields = vec![];
   let mut field_defaults = vec![];

   for (rel, rel_indices) in mir.relations_ir_relations.iter(){
      let name = &rel.name;
      let field_types = tuple_type(&rel.field_types);
      let rel_attrs = &mir.relations_metadata[rel].attributes;
      let rel_indices_comment = format!("\nphysical indices:\n {}", 
         rel_indices.iter().map(|ind| format!("{}", ind.ir_name())).join("; "));
      let rel_type = if !mir.is_parallel { 
         quote! {::std::vec::Vec<#field_types>} 
      } else if !rel.is_lattice {
         quote! {::ascent::boxcar::Vec<#field_types>} 
      } else {
         quote! {::ascent::boxcar::Vec<::std::sync::RwLock<#field_types>>} 
      };
      relation_fields.push(quote! {
         #(#rel_attrs)*
         #[doc = #rel_indices_comment]
         pub #name: #rel_type,
      });
      field_defaults.push(quote! {#name : Default::default(),});
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
         let index_type: Vec<Type> = ind.indices.iter().map(|&i| rel.field_types[i].clone()).collect();
         let index_type = tuple_type(&index_type);
         let rel_index_type = rel_index_type(ind, mir);
         relation_fields.push(quote!{
            // #[allow(non_snake_case)]
            pub #name: #rel_index_type,
         });
         field_defaults.push(quote! {#name : Default::default(),});
      }
   }

   let sccs_ordered = &mir.sccs;
   let mut scc_time_fields = vec![];
   let mut scc_time_field_defaults = vec![];
   let mut rule_time_fields = vec![];
   let mut rule_time_fields_defaults = vec![];
   for i in 0..mir.sccs.len(){
      let name = scc_time_field_name(i);
      scc_time_fields.push(quote!{
         pub #name: std::time::Duration,
      });
      scc_time_field_defaults.push(quote!{
         #name: std::time::Duration::ZERO,
      });
      for (rule_ind, rule) in mir.sccs[i].rules.iter().enumerate() {
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
   for (i, scc) in sccs_ordered.iter().enumerate() {
      let msg = format!("scc {}", i);
      let scc_compiled = compile_mir_scc(mir, i);
      let scc_time_field_name = scc_time_field_name(i);
      sccs_compiled.push(quote!{
         ascent::internal::comment(#msg);
         {
            let _scc_start_time = ::ascent::internal::Instant::now();
            #scc_compiled
            _self.#scc_time_field_name += _scc_start_time.elapsed();
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
   if relation_initializations.len() > 0 {
      relation_initializations.push(quote! {
         _self.update_indices_priv();
      })
   }

   let par_usings = if mir.is_parallel {quote! {
      use ascent::rayon::iter::ParallelBridge;
      use ascent::rayon::iter::ParallelIterator;
      use ascent::internal::CRelIndexRead;
      use ascent::internal::CRelIndexReadAll;
   }} else {quote!{}};

   let run_usings = quote! {
      use core::cmp::PartialEq;
      use ascent::internal::RelIndexRead;
      use ascent::internal::RelIndexReadAll;
      #par_usings
   };
   
   let generate_run_timeout = !is_ascent_run && mir.config.generate_run_partial; 
   let run_func = if is_ascent_run {quote!{}} 
   else if generate_run_timeout {
      quote! {
         #[allow(unused_imports)]
         #[doc = "Runs the Ascent program to a fixed point."]
         pub fn run(&mut self) {
            self.run_timeout(::std::time::Duration::MAX);
         }
      } 
   } else {
      quote! {
         #[allow(unused_imports)]
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
         #[allow(unused_imports)]
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
   let res = quote! {
      #(#struct_attrs)*
      #vis struct #struct_name #ty_impl_generics #ty_where_clause {
         #(#relation_fields)*
         #(#scc_time_fields)*
         #(#rule_time_fields)*
         pub update_time_nanos: std::sync::atomic::AtomicU64,
      }
      impl #impl_impl_generics #struct_name #impl_ty_generics #impl_where_clause {
         #run_func

         #run_timeout_func
         // TODO remove pub update_indices at some point
         fn update_indices_priv(&mut self) {
            #update_indices_body
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
               #(#scc_time_field_defaults)*
               #(#rule_time_fields_defaults)*
               update_time_nanos: Default::default(),
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
            #[allow(unused_imports)]
            {
               ascent::internal::comment("running...");
               #run_code
            }
            __run_res
         }
      }
   }
}

fn rel_index_type(rel: &IrRelation, mir: &AscentMir) -> Type {
   let span = rel.relation.name.span();

   let key_type = rel.key_type();
   let value_type = rel.value_type();

   let is_lat_full_index = rel.relation.is_lattice && &mir.lattices_full_indices[&rel.relation] == rel;
   
   let ty = if !mir.is_parallel {
      if rel.is_full_index() || is_lat_full_index {quote_spanned!{span=>
         ascent::internal::RelFullIndexType<#key_type, #value_type>
      }} else if rel.relation.is_lattice {quote_spanned!{span=>
         ascent::internal::LatticeIndexType<#key_type, #value_type>
      }} else { quote_spanned!{span=>
         ascent::internal::RelIndexType1<#key_type, #value_type>
      }}
   } else {
      if rel.is_full_index() || is_lat_full_index {quote_spanned!{span=>
         ascent::internal::CRelFullIndex<#key_type, #value_type>
      }} else if rel.is_no_index() {quote_spanned!{span=>
         ascent::internal::CRelNoIndex<#value_type>
      }} else {quote_spanned!{span=>
         ascent::internal::CRelIndex<#key_type, #value_type>
      }}
   };
   
   syn::parse2(ty).unwrap()
}

fn scc_time_field_name(i: usize) -> Ident {
   Ident::new(&format!("scc{}_duration", i), Span::call_site())
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

   for rel in scc.dynamic_relations.iter().flat_map(|(rel, indices)| indices.iter()) {
      let ir_name = rel.ir_name();
      let delta_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Delta);
      let total_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Total);
      let new_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::New);
      let total_field = &rel.ir_name();
      let ty = rel_index_type(&rel, mir);
      move_total_to_delta.push(quote! {
         // #[allow(non_snake_case)]
         let #delta_var_name : &mut #ty = &mut _self.#total_field;
         // #[allow(non_snake_case)]
         let mut #total_var_name : #ty = Default::default();
         // #[allow(non_snake_case)]
         let mut #new_var_name : #ty = Default::default();
      });

      shift_delta_to_total_new_to_delta.push(quote_spanned!{rel.relation.name.span()=>
         ::ascent::internal::RelIndexWrite::move_index_contents(#delta_var_name, &mut #total_var_name);
         // #delta_var_name.clear();
         std::mem::swap(&mut #new_var_name, #delta_var_name);
      });

      move_total_to_field.push(quote!{ //_spanned!{rel.relation.name.span()=>
         _self.#total_field = #total_var_name;
      });

      if mir.is_parallel {
         freeze_code.push(quote!{ //_spanned!{rel.relation.name.span()=>
            #total_var_name.freeze();
            #delta_var_name.freeze();
         });

         unfreeze_code.push(quote!{ //_spanned!{rel.relation.name.span()=>
            #total_var_name.unfreeze();
            #delta_var_name.unfreeze();
         });
      }
   }
   for rel in scc.body_only_relations.iter().flat_map(|(rel, indices)| indices.iter()) {
      let total_var_name = ir_relation_version_var_name(&rel.ir_name(), MirRelationVersion::Total);
      let ty = rel_index_type(&rel, mir);
      let total_field = &rel.ir_name();

      move_total_to_delta.push(quote! {
         // #[allow(non_snake_case)]
         let #total_var_name : &mut #ty = &mut _self.#total_field;
      });

      if mir.is_parallel {
         freeze_code.push(quote_spanned!{rel.relation.name.span()=>
            #total_var_name.freeze();
         });
      }
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
         // let mut __changed = false;
         #changed_var_def_code

         #(#freeze_code)*
         // evaluate rules
         #evaluate_rules

         // for dynamic indices in the scc:
         //    append delta to total.
         //    move new to delta
         #(#unfreeze_code)*
         #(#shift_delta_to_total_new_to_delta)*
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
      let scc_time_field_name = scc_time_field_name(i);
      res.push(quote!{
         writeln!(&mut res, "scc {} time: {:?}", #i_str, self.#scc_time_field_name).unwrap();
      });
      if mir.config.include_rule_times {
         let mut sum_of_rule_times = quote!{ std::time::Duration::ZERO };
         for (rule_ind, rule) in mir.sccs[i].rules.iter().enumerate() {
            let rule_time_field = rule_time_field_name(i, rule_ind);
            sum_of_rule_times = quote!{ #sum_of_rule_times + self.#rule_time_field};
         }
         res.push(quote! {
            let sum_of_rule_times = #sum_of_rule_times;
            writeln!(&mut res, "  some of rule times: {:?}", sum_of_rule_times).unwrap();
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
   quote!{
      use std::fmt::Write;
      let mut res = String::new();
      #(#res)*
      res
   }
}

fn compile_update_indices_function_body(mir: &AscentMir) -> proc_macro2::TokenStream {
   // let _self = quote!{ self }; // making sure it gets the correct span
   
   let par = mir.is_parallel;
   let mut res = vec![];
   if par {
      res.push(quote! { use ascent::rayon::iter::{IntoParallelIterator, ParallelIterator}; })
   }
   let rel_index_write_trait = if !par {
      quote! {ascent::internal::RelIndexWrite}
   } else {
      quote! {ascent::internal::CRelIndexWrite}
   };
   for (r,indices_set) in mir.relations_ir_relations.iter(){
      
      let _ref = if !par { quote!{&mut} } else { quote!{&} }.with_span(r.name.span());
      let rel_index_write_trait = rel_index_write_trait.clone().with_span(r.name.span());
      let _self = quote_spanned!{r.name.span().resolved_at(Span::call_site())=> self };

      let mut update_indices = vec![];
      for ind in indices_set.iter(){
         let ind_name = &ind.ir_name();
         let selection_tuple : Vec<Expr> = ind.indices.iter().map(|&i| {
            let ind = syn::Index::from(i); 
            parse_quote_spanned! {r.name.span()=> tuple.#ind.clone()}
         }).collect_vec();
         let selection_tuple = tuple_spanned(&selection_tuple, r.name.span());
         let entry_val = index_get_entry_val_for_insert(
            &ind, &parse_quote_spanned!{r.name.span()=> tuple}, &parse_quote_spanned!{r.name.span()=> _i});
         update_indices.push(quote_spanned! {r.name.span()=>
            let selection_tuple = #selection_tuple;
            let rel_ind = #_ref #_self.#ind_name;
            #rel_index_write_trait::index_insert(rel_ind, selection_tuple, #entry_val);
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

   quote! {#(#res)*}
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

fn compile_mir_rule_inner(rule: &MirRule, scc: &MirScc, mir: &AscentMir, par_iter_to_ind: usize, head_update_code: proc_macro2::TokenStream, clause_ind: usize) 
-> proc_macro2::TokenStream 
{
   if Some(clause_ind) == rule.simple_join_start_index && rule.reorderable {
      let mut rule_cp1 = rule.clone();
      let mut rule_cp2 = rule.clone();
      rule_cp1.reorderable = false;
      rule_cp2.reorderable = false;
      rule_cp2.body_items.swap(clause_ind, clause_ind + 1);
      let rule_cp1_compiled = compile_mir_rule_inner(&rule_cp1, scc, mir, par_iter_to_ind, head_update_code.clone(), clause_ind);
      let rule_cp2_compiled = compile_mir_rule_inner(&rule_cp2, scc, mir, par_iter_to_ind, head_update_code        , clause_ind);

      if let [MirBodyItem::Clause(bcl1), MirBodyItem::Clause(bcl2)] = &rule.body_items[clause_ind..clause_ind+2]{

         let rel1_var_name = expr_for_rel(&bcl1.rel);
         
         let rel2_var_name = expr_for_rel(&bcl2.rel);

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
         compile_mir_rule_inner(rule, scc, mir, par_iter_to_ind, head_update_code, clause_ind + 2)
      } else {
         compile_mir_rule_inner(rule, scc, mir, par_iter_to_ind, head_update_code, clause_ind + 1)
      };

      match bitem {
         MirBodyItem::Clause(bclause) => {
            if doing_simple_join {
               // println!("doing simple join for {}", mir_rule_summary(rule));
            }
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

            // let mut new_vars_assignments = vec![];
            // for (i,var) in clause_vars.iter(){
            //    if common_vars_no_indices.contains(var) {continue;}
            //    let i_ind = syn::Index{index: *i as u32, span: var.span()};
            //    new_vars_assignments.push(quote_spanned! {var.span()=> let #var = &__row.#i_ind;});
            // }

            let cloning_needed = true;

            let matched_val_ident = Ident::new("__val", bclause.rel_args_span);
            let new_vars_assignments = clause_var_assignments(
               &bclause.rel, clause_vars.iter().filter(|(i, var)| !common_vars_no_indices.contains(var)).cloned(),
               &matched_val_ident, &parse_quote!{_self.#bclause_rel_name}, cloning_needed, mir
            );

            let selected_args_cloned = selected_args.iter().map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple_spanned(&selected_args_cloned, bclause.args_span);
            let rel_version_exp = expr_for_rel(&bclause.rel);
            
            let mut conds_then_next_loop = next_loop;
            for cond in bclause.cond_clauses.iter().rev() {
               conds_then_next_loop = compile_cond_clause(cond, conds_then_next_loop);
            }

            // we need to clone if the relation appears in the head of the rule and we are not the last clause.
            // let cloning_needed = rule.body_items[(clause_ind + 1)..].iter().any(|bi| !matches!(bi, MirBodyItem::Cond(_))) &&
            //                      rule.head_clause.iter().any(|hcl| &hcl.rel.name == bclause_rel_name);

            fn maybe_clone(cloning_needed: bool, span: Span) -> proc_macro2::TokenStream {
               if cloning_needed {quote_spanned! {span=> .clone()}} 
               else {quote! { }}
            }
            let span = bclause.rel_args_span;
            let row_maybe_clone = maybe_clone(cloning_needed, bclause.rel_args_span);
            
            // let matching_dot_iter = dot_iter_for_rel_index(&bclause.rel, quote_spanned!{bclause.rel_args_span=> __matching});
            let matching_dot_iter = quote_spanned!{bclause.rel_args_span=> __matching};

            let (index_get, iter_all) = if clause_ind < par_iter_to_ind {
               (quote_spanned! {span=> c_index_get}, quote_spanned! {span=> c_iter_all})
            } else {
               (quote_spanned! {span=> index_get}, quote_spanned! {span=> iter_all})
            };

            // TODO cleanup
            // The special case where the first clause has indices, but there are no expressions
            // in the args of the first clause
            if doing_simple_join {
               let cl1 = rule.body_items[rule.simple_join_start_index.unwrap()].unwrap_clause();
               let cl2 = bclause;
               let cl1_var_name = expr_for_rel(&cl1.rel);
               let cl2_var_name = expr_for_rel(&cl2.rel);
               let cl1_vars = cl1.vars();

               let cl1_rel_name = &cl1.rel.relation.name;
               let cl2_rel_name = &cl2.rel.relation.name;

               let mut cl1_join_vars_assignments = vec![];
               for (tuple_ind, &i) in cl1.rel.indices.iter().enumerate() {
                  let var = expr_to_ident(&cl1.args[i]).unwrap();
                  let tuple_ind = syn::Index{index: tuple_ind as u32, span: var.span()};
                  cl1_join_vars_assignments.push(quote_spanned! {var.span()=> let #var = &__cl1_joined_columns.#tuple_ind;});
               }
               let cl2_vars = cl2.args.iter().filter_map(expr_to_ident).collect_vec();

               // let mut cl1_vars_assignments = vec![];
               // for (i,var) in cl1_vars.iter(){
               //    let i_ind = syn::Index{index: *i as u32, span: var.span()};
               //    if cl1.rel.indices.contains(i) {continue;}
               //    cl1_vars_assignments.push(quote_spanned! {var.span()=> let #var = &__row.#i_ind;});
               // }
               let cl1_matched_val_ident = syn::Ident::new("cl1_val", cl1.rel_args_span);
               let cl1_vars_assignments = clause_var_assignments(
                  &cl1.rel, cl1_vars.iter().filter(|(i, var)| !cl1.rel.indices.contains(i)).cloned(),
                  &cl1_matched_val_ident, &parse_quote!{_self.#cl1_rel_name}, cloning_needed, mir
               );
               let cl1_vars_assignments = vec![cl1_vars_assignments];

               let joined_args_for_cl2_cloned = cl2.selected_args().iter().map(exp_cloned).collect_vec();
               let joined_args_tuple_for_cl2 = tuple_spanned(&joined_args_for_cl2_cloned, cl2.args_span);
               
               
               let cl1_tuple_indices_iter = quote_spanned!(cl1.rel_args_span=> __cl1_tuple_indices);

               let cl1_cloning_needed = true;
               let cl1_row_maybe_clone = maybe_clone(cl1_cloning_needed, cl1.rel_args_span);

               
               let mut cl1_conds_then_rest = quote_spanned! {bclause.rel_args_span=>
                  // for __val in #matching_dot_iter.clone() {
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
                     #(#cl1_join_vars_assignments)*
                     if let Some(__matching) = #cl2_var_name.#index_get(&#joined_args_tuple_for_cl2) {
                        // for cl1_val in #cl1_tuple_indices_iter{
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
                     // for __val in #matching_dot_iter {
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
            let rel_version_var_name = mir_relation.var_name();
            let selected_args = mir_relation.indices.iter().map(|&i| &agg.rel_args[i]);
            let selected_args_cloned = selected_args.map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple_spanned(&selected_args_cloned, agg.span);
            let agg_args_tuple_indices = 
               agg.bound_args.iter()
               .map(|arg| (agg.rel_args.iter()
                        .find_position(|rel_arg| expr_to_ident(rel_arg) == Some(arg.clone())).unwrap().0, arg.clone()));
            // let agg_args_tuple = agg_args_tuple_indices.clone().map(|(ind, arg)| {
            //    let tuple_ind = syn::Index::from(ind);
            //    parse2(quote_spanned! {arg.span()=> &__row.#tuple_ind}).unwrap()
            // }).collect_vec();
            // let agg_args_tuple = tuple_spanned(&agg_args_tuple, agg.span);
            let agg_args_tuple = tuple_spanned(&agg.bound_args.iter().map(|v| parse_quote!{#v}).collect_vec(), agg.span);

            let vars_assignments = clause_var_assignments(
               &MirRelation::from(agg.rel.clone(), MirRelationVersion::Total), agg_args_tuple_indices, 
               &parse_quote_spanned!{agg.span=> __val}, &parse_quote_spanned!{agg.span=>__aggregated_rel},
               false, mir
            );

            let agg_func = &agg.aggregator;
            let matching_iter = quote_spanned!{agg.span => __matching};
            let to_iter_func = ind_val_option_to_iter_func_name_for_rel(&mir_relation);
            let _self = quote!{ _self };
            quote_spanned! {agg.span=>
               let __matching = #rel_version_var_name.index_get( &#selected_args_tuple);
               let __aggregated_rel = &#_self.#rel_name;
               let __agg_args = __matching.into_iter().flatten()
                     .map(|__val| {
                              // let __row = &__aggregated_rel[__val];
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
   
   // TODO remove if not used
   // let mut struct_defs = vec![];
   // let mut rel_data_vars = vec![];

   // fn rel_name_to_struct_name(rel_name: &Ident) -> Ident {
   //    Ident::new(&format!("__{}Data", rel_name), rel_name.span())
   // };

   // fn rel_name_to_data_var_name(rel_name: &Ident) -> Ident {
   //    Ident::new(&format!("__{}_data", rel_name), rel_name.span())
   // };

   // for rel in rule.head_clause.iter().map(|hcl| &hcl.rel).collect::<HashSet<_>>() {
   //    let mut fields = vec![];
   //    let mut var_assignments = vec![];
   //    for (i, rel_ind) in scc.dynamic_relations.get(rel).into_iter().flatten().enumerate() {
   //       let var_name = ir_relation_version_var_name(&rel_ind.ir_name(), New);
   //       // let ty = rel_index_type(rel_ind);
   //       let ty = Ident::new(&format!("T{}", i), rel.name.span());

   //       var_assignments.push(quote!{
   //          #var_name: &mut #var_name,
   //       });
   //       fields.push(quote!{
   //          #var_name: &'a mut #ty,
   //       });
   //    }

   //    let mut update_indices = vec![];
   //    for (i, rel_ind) in scc.dynamic_relations.get(rel).into_iter().flatten().enumerate(){
   //       if rel_ind.is_full_index() {continue};
   //       let var_name = ir_relation_version_var_name(&rel_ind.ir_name(), New);
   //       let args_tuple : Vec<Expr> = rel_ind.indices.iter().map(|&i| {
   //          let i_ind = syn::Index::from(i);
   //          syn::parse2(quote_spanned!{rel.name.span()=> __new_row.#i_ind.clone()}).unwrap()
   //       }).collect();
   //       let args_tuple = tuple(&args_tuple);
   //       let entry_val = index_get_entry_val_for_insert(
   //          &rel_ind, &parse_quote_spanned!{rel.name.span()=> __new_row}, &parse_quote_spanned!{rel.name.span()=> __new_row_ind});
   //       update_indices.push(quote! {
   //          ::ascent::internal::RelIndexWrite::index_insert(&mut #var_name, #args_tuple, #entry_val);
   //       });
   //    }

   //    let types = (0..fields.len()).map(|i| {
   //       Ident::new(&format!("T{}", i), rel.name.span())
   //    }).collect_vec();
   //    let struct_name = rel_name_to_struct_name(&rel.name);
   //    let rel_name = &rel.name;
   //    struct_defs.push(quote! {
   //       #[allow(non_camel_case_types)]
   //       struct #struct_name<'a, TRel, #(#types),*> {
   //          #rel_name : TRel,
   //          #(#fields)*
   //       }
   //    });
   //    let rel_data_var_name = rel_name_to_data_var_name(&rel.name);
   //    let rel_data_var_instantiation_code = quote! {
   //       #struct_name { #rel_name: &mut _self.#rel_name, #(#var_assignments)* }
   //    };
   //    if true || !mir.is_parallel {
   //       rel_data_vars.push(quote! {
   //          let mut #rel_data_var_name = #rel_data_var_instantiation_code;
   //       });
   //    } else {
   //       rel_data_vars.push(quote! {
   //          let #rel_data_var_name = std::sync::Mutex::new(#rel_data_var_instantiation_code);
   //       });
   //    }
   // }

   let mut add_rows = vec![];

   let set_changed_true_code = if !mir.is_parallel {
      quote! { __changed = true; }
   } else {
      quote! { __changed.store(true, std::sync::atomic::Ordering::Relaxed);}
   };

   for hcl in rule.head_clause.iter() {

      let head_rel_name = Ident::new(&hcl.rel.name.to_string(), hcl.span);
      // let rel_data_var_name = rel_name_to_data_var_name(&head_rel_name);
      let hcl_args_converted = hcl.args.iter().cloned().map(convert_head_arg).collect_vec();
      let new_row_tuple = tuple_spanned(&hcl_args_converted, hcl.args_span);
      
      let head_relation = &hcl.rel;
      let row_type = tuple_type(&head_relation.field_types);

      let mut update_indices = vec![];
      let rel_indices = scc.dynamic_relations.get(head_relation);
      let rel_index_write_trait = if !mir.is_parallel {
         quote! { ::ascent::internal::RelIndexWrite }
      } else {
         quote! { ::ascent::internal::CRelIndexWrite }
      }.with_span(hcl.span);
      let new_ref = if !mir.is_parallel { quote!{&mut} } else { quote!{&} };
      let mut used_fields = HashSet::new();
      if let Some(rel_indices) = rel_indices {
         for rel_ind in rel_indices.iter(){
            if rel_ind.is_full_index() {continue};
            let var_name = ir_relation_version_var_name(&rel_ind.ir_name(), New);
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
               &rel_ind, &parse_quote_spanned!{hcl.span=> __new_row}, &parse_quote_spanned!{hcl.span=> __new_row_ind});
            update_indices.push(quote_spanned! {hcl.span=>
               #rel_index_write_trait::index_insert(#new_ref #var_name, #args_tuple, #entry_val);
            });
         }
      }

      let head_rel_full_index = &mir.relations_full_indices[head_relation];
      let head_rel_full_index_var_name_new = ir_relation_version_var_name(&head_rel_full_index.ir_name(), New);
      let head_rel_full_index_var_name_delta = ir_relation_version_var_name(&head_rel_full_index.ir_name(), Delta);
      let head_rel_full_index_var_name_total = ir_relation_version_var_name(&head_rel_full_index.ir_name(), Total);

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
         let head_rel_name_string = head_rel_name.to_string();
         let add_row = quote_spanned!{hcl.span=>
            let __new_row: #row_type = #new_row_tuple;

            if !::ascent::internal::RelFullIndexRead::contains_key(&#head_rel_full_index_var_name_total, &__new_row) &&
               !::ascent::internal::RelFullIndexRead::contains_key(#head_rel_full_index_var_name_delta, &__new_row) {
               if #rel_full_index_write_trait::insert_if_not_present(#new_ref #head_rel_full_index_var_name_new, 
                  &__new_row, ())
               {
                  // let __new_row_ind = _self.#head_rel_name.len();
                  // _self.#head_rel_name.push(__new_row);
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

fn convert_head_arg(arg: Expr) -> Expr {
   if let Some(var) = expr_to_ident(&arg){
      parse2(quote_spanned!{arg.span()=> ascent::internal::Convert::convert(#var)}).unwrap()
   } else {
      arg
   }
}

fn ind_val_option_to_iter_func_name_for_rel(rel: &MirRelation) -> proc_macro2::TokenStream {
   if rel.is_full_index {
      quote! {ascent::internal::rel_full_ind_val_option_to_iter}
   } else if !rel.relation.is_lattice {
      quote! {ascent::internal::rel_ind_val_option_to_iter}
   } else {
      quote! {ascent::internal::lat_ind_val_option_to_iter}
   }
}

fn expr_for_rel(rel: &MirRelation) -> proc_macro2::TokenStream {
   if rel.version == MirRelationVersion::TotalDelta {
      let total_var_name = ir_relation_version_var_name(&rel.ir_name, MirRelationVersion::Total);
      let delta_var_name = ir_relation_version_var_name(&rel.ir_name, MirRelationVersion::Delta);
      quote! {
         ascent::internal::RelIndexCombined::new(& #total_var_name, #delta_var_name)
      }
   } else {
      let var = rel.var_name();
      quote! {
         #var
      }
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
   let mut vars_count = 0;
   for (ind_in_tuple, var) in vars {
      any_vars = true;
      vars_count += 1;
      match &rel.val_type {
         IndexValType::Reference => {
            let ind = syn::Index::from(ind_in_tuple);
            assignments.push(quote! {
               let #var = &__row.#ind;
            })
         },
         IndexValType::Direct(inds) => {
            let ind = inds.iter().enumerate().find(|(i, ind)| **ind == ind_in_tuple).unwrap().0;
            let ind = syn::Index::from(ind);

            assignments.push(quote! {
               let #var = &#val_ident.#ind;
            })
         },
      }  
   }
   
   match &rel.val_type {
      IndexValType::Reference if any_vars => {
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
      _ => ()
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
