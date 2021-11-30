use std::{collections::{BTreeSet, HashSet}, fmt::Display};

use itertools::{Iterate, Itertools};
use proc_macro2::{Ident, Span};
use syn::{Expr, Type, parse2, spanned::Spanned};

use crate::{infer_hir::IrRelation, infer_mir::{InferMir, MirBodyItem, MirRelation, MirRelationVersion, MirRule, MirScc, ir_relation_version_var_name, mir_rule_summary, mir_summary}, infer_syntax::CondClause, utils::{exp_cloned, expr_to_ident, pat_to_ident, tuple, tuple_spanned, tuple_type}};
use crate::infer_mir::MirRelationVersion::*;
pub(crate) fn compile_mir(mir: &InferMir, is_infer_run: bool) -> proc_macro2::TokenStream {
   
   let mut relation_fields = vec![];
   let mut field_defaults = vec![];

   for (rel, rel_indices) in mir.relations_ir_relations.iter(){
      let name = &rel.name;
      let field_types = tuple_type(&rel.field_types);
      relation_fields.push(quote! {
         pub #name : Vec<#field_types>,
      });
      field_defaults.push(quote! {#name : Default::default(),});
      for ind in rel_indices.iter(){
         let name = &ind.ir_name();
         let index_type: Vec<Type> = ind.indices.iter().map(|&i| rel.field_types[i].clone()).collect();
         let index_type = tuple_type(&index_type);
         let rel_index_type = rel_index_type(ind);
         relation_fields.push(quote!{
            #[allow(non_snake_case)]
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
         infer::internal::comment(#msg);
         {
            let _scc_start_time = ::std::time::Instant::now();
            #scc_compiled
            _self.#scc_time_field_name += _scc_start_time.elapsed();
         }
         // TODO remove this:
         // eprintln!("scc {} done.", #i);
      });
   }

   let update_indices_body = compile_update_indices_function_body(mir);
   let relation_sizes_body = compile_relation_sizes_body(mir);
   let scc_times_summary_body = compile_scc_times_summary_body(mir);

   let mut type_constaints = vec![];
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
            let type_constaints_type = 
               if is_lat {quote_spanned!(field_type.span()=>LatTypeConstraints)} 
               else {quote_spanned!(field_type.span()=>TypeConstraints)}; 
            type_constaints.push(quote_spanned!{field_type.span()=>
               let _type_constraints : infer::internal::#type_constaints_type<#field_type>;
            });
         }
      }
   }

   let mut relation_initializations = vec![];
   for (rel, init) in mir.relations_initializations.iter() {
      let rel_name = &rel.name;
      relation_initializations.push(quote! {
         _self.#rel_name = #init;
      });
   }
   if mir.relations_initializations.len() > 0 {
      relation_initializations.push(quote! {
         _self.update_indices();
      })
   }


   let run_func = if is_infer_run {quote!{}} else {
      quote! {
         #[allow(unused_imports)]
         pub fn run(&mut self) {
            use core::cmp::PartialEq;
            let _self = self;
            #(#sccs_compiled)*
         }
      }
   };
   let run_code = if !is_infer_run {quote!{}} else {
      quote! {
         use core::cmp::PartialEq;
         let _self = &mut __run_res;
         #(#relation_initializations)*
         #(#sccs_compiled)*
      }
   };
   let relation_initializations_for_default_impl = 
      if is_infer_run {vec![]} else {relation_initializations};
   let summary = mir_summary(mir);
   let (impl_generics, ty_generics, where_clause) = mir.declaration.generics.split_for_impl();
   let vis = &mir.declaration.visibility;
   let struct_name = &mir.declaration.ident;
   let generics = &mir.declaration.generics;
   let summary_fn = if is_infer_run {
      quote! {
         pub fn summary(&self) -> &'static str {#summary}
      }
   } else { quote! {
      pub fn summary() -> &'static str {#summary}
   }};
   let rule_time_fields = if mir.config.include_rule_times {rule_time_fields} else {vec![]};
   let rule_time_fields_defaults = if mir.config.include_rule_times {rule_time_fields_defaults} else {vec![]};
   let res = quote! {
      // use std::collections::{HashMap, HashSet};
      // use ::infer::internal::comment;
      #vis struct #struct_name #generics {
         #(#relation_fields)*
         #(#scc_time_fields)*
         #(#rule_time_fields)*
      }
      impl #impl_generics #struct_name #ty_generics #where_clause {
         #run_func
         pub fn update_indices(&mut self) {
            #update_indices_body
         }
         #[allow(unused_imports)]
         fn type_constaints() {
            #(#type_constaints)*
         }
         #summary_fn
         
         pub fn relation_sizes_summary(&self) -> String {
            #relation_sizes_body
         }
         pub fn scc_times_summary(&self) -> String {
            #scc_times_summary_body
         }
      }
      impl #impl_generics Default for #struct_name #ty_generics #where_clause {
         fn default() -> Self {
            let mut _self = #struct_name {
               #(#field_defaults)*
               #(#scc_time_field_defaults)*
               #(#rule_time_fields_defaults)*
            };
            #(#relation_initializations_for_default_impl)*
            _self
         }
      }
   };
   if !is_infer_run {res} else {
      quote! {
         {
            #res
            let mut __run_res: #struct_name #ty_generics = #struct_name::default();
            #[allow(unused_imports)]
            {
               infer::internal::comment("running...");
               #run_code
            }
            __run_res
         }
      }
   }
}

fn rel_index_type(rel: &IrRelation) -> Type {
   let key_type = rel.key_type();
   let ty = if rel.is_full_index() {quote!{
      infer::internal::RelFullIndexType<#key_type>
   }} else if rel.relation.is_lattice {quote! {
      infer::internal::LatticeIndexType<#key_type>
   }} else { quote! {
      infer::internal::RelIndexType<#key_type>
   }};
   syn::parse2(ty).unwrap()
}

fn scc_time_field_name(i: usize) -> Ident {
   Ident::new(&format!("scc{}_duration", i), Span::call_site())
}
fn rule_time_field_name(scc_ind: usize, rule_ind: usize) ->Ident {
   Ident::new(&format!("rule{}_{}_duration", scc_ind, rule_ind), Span::call_site())
}

fn compile_mir_scc(mir: &InferMir, scc_ind: usize) -> proc_macro2::TokenStream {

   let scc = &mir.sccs[scc_ind];
   let mut move_total_to_delta = vec![];
   let mut shift_delta_to_total_new_to_delta = vec![];
   let mut move_total_to_field = vec![];

   for rel in scc.dynamic_relations.iter().flat_map(|(rel, indices)| indices.iter()) {
      let ir_name = rel.ir_name();
      let delta_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Delta);
      let total_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::Total);
      let new_var_name = ir_relation_version_var_name(&ir_name, MirRelationVersion::New);
      let total_field = &rel.ir_name();
      let ty = rel_index_type(&rel);
      move_total_to_delta.push(quote! {
         #[allow(non_snake_case)]
         let #delta_var_name : &mut #ty = &mut _self.#total_field;
         #[allow(non_snake_case)]
         let mut #total_var_name : #ty = Default::default();
         #[allow(non_snake_case)]
         let mut #new_var_name : #ty = Default::default();
      });

      shift_delta_to_total_new_to_delta.push(quote_spanned!{rel.relation.name.span()=>
         infer::internal::RelIndexTrait::move_index_contents(#delta_var_name, &mut #total_var_name);
         // #delta_var_name.clear();
         std::mem::swap(&mut #new_var_name, #delta_var_name);
      });

      move_total_to_field.push(quote_spanned!{rel.relation.name.span()=>
         _self.#total_field = #total_var_name;
      });
   }
   for rel in scc.body_only_relations.iter().flat_map(|(rel, indices)| indices.iter()) {
      let total_var_name = ir_relation_version_var_name(&rel.ir_name(), MirRelationVersion::Total);
      let ty = rel_index_type(&rel);
      let total_field = &rel.ir_name();

      move_total_to_delta.push(quote! {
         #[allow(non_snake_case)]
         let #total_var_name : &mut #ty = &mut _self.#total_field;
      });
   }
   
   let mut evaluate_rules = vec![];

   for (i, rule) in scc.rules.iter().enumerate() {
      let msg = mir_rule_summary(rule);
      let rule_compiled = compile_mir_rule(rule, scc, mir, 0);
      let rule_time_field = rule_time_field_name(scc_ind, i);
      let (before_rule_var, update_rule_time_field) = if mir.config.include_rule_times {
         (quote! {let before_rule = ::std::time::Instant::now();}, 
          quote!{_self.#rule_time_field += before_rule.elapsed();})
      } else {(quote!{}, quote!{})};
      evaluate_rules.push(quote! {
         #before_rule_var
         infer::internal::comment(#msg);
         #rule_compiled
         #update_rule_time_field
      });
   }

   let evaluate_rules_loop = if scc.is_looping { quote! {
      #[allow(unused_assignments, unused_variables)]
      loop {
         let mut __changed = false;
         // evaluate rules
         #(#evaluate_rules)*

         // for dynamic indices in the scc:
         //    append delta to total.
         //    move new to delta
         #(#shift_delta_to_total_new_to_delta)*
         if !__changed {break;}
      }
   }} else {quote! {
      #[allow(unused_assignments, unused_variables)]
      {
         let mut __changed = false;
         #(#evaluate_rules)*
         #(#shift_delta_to_total_new_to_delta)*
         #(#shift_delta_to_total_new_to_delta)*
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


fn compile_relation_sizes_body(mir: &InferMir) -> proc_macro2::TokenStream {
   let mut write_sizes = vec![];
   for r in mir.relations_ir_relations.keys() {
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

fn compile_scc_times_summary_body(mir: &InferMir) -> proc_macro2::TokenStream {
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

fn compile_update_indices_function_body(mir: &InferMir) -> proc_macro2::TokenStream {
   
   let mut res = vec![];
   for (r,indices_set) in mir.relations_ir_relations.iter(){

      let mut update_indices = vec![];
      for ind in indices_set.iter(){
         let ind_name = &ind.ir_name();
         let selection_tuple : Vec<Expr> = ind.indices.iter().map(|&i| {
            let ind = syn::Index::from(i); 
            parse2(quote_spanned! {r.name.span()=> tuple.#ind.clone()}).unwrap()
         }).collect_vec();
         let selection_tuple = tuple_spanned(&selection_tuple, r.name.span());
         update_indices.push(quote_spanned! {r.name.span()=>
            let selection_tuple = #selection_tuple;
            infer::internal::RelIndexTrait::index_insert(&mut self.#ind_name, selection_tuple, i);
         });

      }
      let rel_name = &r.name;
      res.push(quote! {
         for (i, tuple) in self.#rel_name.iter().enumerate() {
            #(#update_indices)*
         }
      });
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

fn compile_mir_rule(rule: &MirRule, scc: &MirScc, mir: &InferMir, clause_ind: usize) -> proc_macro2::TokenStream {

   if clause_ind == 0 && rule.reorderable {
      let mut rule_cp1 = rule.clone();
      let mut rule_cp2 = rule.clone();
      rule_cp1.reorderable = false;
      rule_cp2.reorderable = false;
      rule_cp2.body_items.swap(0, 1);
      let rule_cp1_compiled = compile_mir_rule(&rule_cp1, scc, mir, 0);
      let rule_cp2_compiled = compile_mir_rule(&rule_cp2, scc, mir, 0);

      if let (MirBodyItem::Clause(bcl1), MirBodyItem::Clause(bcl2)) = (&rule.body_items[0], &rule.body_items[1]){
         // let rel1_full_index = &mir.relations_full_indices[&bcl1.rel.relation];
         let rel1_var_name = bcl1.rel.var_name();
         
         // let rel2_full_index = &mir.relations_full_indices[&bcl2.rel.relation];
         let rel2_var_name = bcl2.rel.var_name();

         
         // TODO testting:
         // return if bcl1.rel.version == Total && bcl2.rel.version == Delta {
         //    rule_cp2_compiled
         // } else {
         //    rule_cp1_compiled
         // };

         return if (&bcl1.rel.relation, bcl1.rel.version) == (&bcl2.rel.relation, bcl2.rel.version) {
            rule_cp1_compiled
         } else {
            quote_spanned!{bcl1.rel_args_span=>
               if #rel1_var_name.len() <= #rel2_var_name.len() {
                  #rule_cp1_compiled
               } else {
                  #rule_cp2_compiled
               }
            }
         };
      } else {
         panic!("unexpected body items in reorderable rule")
      }
   }
   if clause_ind < rule.body_items.len(){
      let bitem = &rule.body_items[clause_ind];
      let doing_simple_join = rule.has_simple_join && clause_ind == 0;
      let next_loop = if doing_simple_join {
         compile_mir_rule(rule, scc, mir, clause_ind + 2)
      } else {
         compile_mir_rule(rule, scc, mir, clause_ind + 1)
      };

      match bitem {
         MirBodyItem::Clause(bclause) => {
            if doing_simple_join {
               // println!("doing simple join for {}", mir_rule_summary(rule));
            }
            let (clause_ind, bclause) = 
               if doing_simple_join {(1, rule.body_items[1].unwrap_clause())} 
               else {(clause_ind, bclause)};

            fn bitem_vars(bitem : &MirBodyItem) -> Vec<Ident> {
               match bitem {
                  MirBodyItem::Clause(cl) => cl.args.iter().filter_map(expr_to_ident).collect(),

                  // TODO these look wrong ...
                  MirBodyItem::Generator(gen) => pat_to_ident(&gen.pattern).into_iter().collect(),
                  MirBodyItem::Cond(CondClause::IfLet(if_let)) => pat_to_ident(&if_let.pattern).into_iter().collect(),
                  MirBodyItem::Cond(CondClause::Let(let_cl)) => pat_to_ident(&let_cl.pattern).into_iter().collect(),
                  MirBodyItem::Cond(CondClause::If(_)) => vec![],
                  MirBodyItem::Agg(agg) => pat_to_ident(&agg.pat).into_iter().collect(),
               }
            }

            let bclause_rel_name = &bclause.rel.relation.name;
            let selected_args = &bclause.selected_args();
            let pre_clause_vars = rule.body_items.iter().take(clause_ind)
                                       .flat_map(bitem_vars)
                                       .collect::<Vec<_>>();

            let clause_vars = bclause.vars();
            let common_vars = clause_vars.iter().filter(|(_i,v)| pre_clause_vars.contains(v)).collect::<Vec<_>>();
            let common_vars_no_indices = common_vars.iter().map(|(_i,v)| v.clone()).collect::<Vec<_>>();

            let mut new_vars_assignmnets = vec![];
            for (i,var) in clause_vars.iter(){
               if common_vars_no_indices.contains(var) {continue;}
               let i_ind = syn::Index{index: *i as u32, span: var.span()};
               new_vars_assignmnets.push(quote_spanned! {var.span()=> let #var = &__row.#i_ind;});
            }

            let selected_args_cloned = selected_args.iter().map(exp_cloned).collect_vec();
            let selected_args_tuple = tuple_spanned(&selected_args_cloned, bclause.args_span);
            let rel_version_var_name = bclause.rel.var_name();
            
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
            let cloning_needed = true;
            let row_maybe_clone = maybe_clone(cloning_needed, bclause.rel_args_span);
            
            let matching_dot_iter = dot_iter_for_rel_index(&bclause.rel, quote_spanned!{bclause.rel_args_span=> __matching});

            // TODO cleanup
            // The special case where the first clause has indices, but there are no expressions
            // in the args of the first cluase
            if doing_simple_join {
               let cl1 = rule.body_items[0].unwrap_clause();
               let cl2 = bclause;
               let cl1_var_name = cl1.rel.var_name();
               let cl2_var_name = cl2.rel.var_name();
               let cl1_vars = cl1.vars();
               let mut cl1_vars_assignments = vec![];
               let mut cl1_join_vars_assignments = vec![];
               for (tuple_ind, &i) in cl1.rel.indices.iter().enumerate() {
                  let var = expr_to_ident(&cl1.args[i]).unwrap();
                  let tuple_ind = syn::Index{index: tuple_ind as u32, span: var.span()};
                  cl1_join_vars_assignments.push(quote_spanned! {var.span()=> let #var = &__cl1_joined_columns.#tuple_ind;});
               }
               let cl2_vars = cl2.args.iter().filter_map(expr_to_ident).collect_vec();
               for (i,var) in cl1_vars.iter(){
                  let i_ind = syn::Index{index: *i as u32, span: var.span()};
                  if cl1.rel.indices.contains(i) {continue;}
                  cl1_vars_assignments.push(quote_spanned! {var.span()=> let #var = &__row.#i_ind;});
               }
               let joined_args_for_cl2_cloned = cl2.selected_args().iter().map(exp_cloned).collect_vec();
               let joined_args_tuple_for_cl2 = tuple_spanned(&joined_args_for_cl2_cloned, cl2.args_span);
               
               let cl1_rel_name = &cl1.rel.relation.name;
               let cl2_rel_name = &cl2.rel.relation.name;
               let cl1_tuple_indices_dot_iter = dot_iter_for_rel_index(&cl1.rel, quote_spanned!(cl1.rel_args_span=> __cl1_tuple_indices));

               let cl1_cloning_needed = true;
               let cl1_row_maybe_clone = maybe_clone(cl1_cloning_needed, cl1.rel_args_span);

               let mut cl1_conds_then_rest = quote! {
                  for &__ind in #matching_dot_iter {
                     // TODO we may be doing excessive cloning
                     let __row = &_self.#cl2_rel_name[__ind] #row_maybe_clone;
                     #(#new_vars_assignmnets)*
                     #conds_then_next_loop
                  }
               };
               for cond in cl1.cond_clauses.iter().rev() {
                  cl1_conds_then_rest = compile_cond_clause(cond, cl1_conds_then_rest);
               }
               quote_spanned! {cl1.rel_args_span=>
                  for (__cl1_joined_columns, __cl1_tuple_indices) in #cl1_var_name.iter() {
                     #(#cl1_join_vars_assignments)*
                     if let Some(__matching) = #cl2_var_name.get(&#joined_args_tuple_for_cl2) {
                        for &cl1_ind in #cl1_tuple_indices_dot_iter{
                           let __row = &_self.#cl1_rel_name[cl1_ind] #cl1_row_maybe_clone;
                           #(#cl1_vars_assignments)*
                           #cl1_conds_then_rest
                        }
                     }
                  }
               }
            } else {  
               if false && clause_ind == 0 && !bclause.rel.indices.is_empty() && 
               bclause.args.iter().filter_map(expr_to_ident).count() == bclause.args.len() {
               // TODO clean up:
               // let bclause_rel_no_ind = &mir.relations_no_indices[&bclause.rel.relation];
               // let rel_no_ind_version_var_name = ir_relation_version_var_name(&bclause_rel_no_ind.ir_name(), bclause.rel.version);
               // quote_spanned! {bclause.rel_args_span=>
               //    if let Some(__matching) = #rel_no_ind_version_var_name.get(&()) {
               //       for &__ind in __matching.iter() {
               //          // TODO we may be doing excessive cloning
               //          let __row = &_self.#bclause_rel_name[__ind] #row_maybe_clone;
               //          #(#new_vars_assignmnets)*
               //          #conds_then_next_loop
               //       }
               //    }
               // }
               unreachable!()
            } else {
               quote_spanned! {bclause.rel_args_span=>
                  if let Some(__matching) = #rel_version_var_name.get( &#selected_args_tuple) {
                     for &__ind in #matching_dot_iter {
                        // TODO we may be doing excessive cloning
                        let __row = &_self.#bclause_rel_name[__ind] #row_maybe_clone;
                        #(#new_vars_assignmnets)*
                        #conds_then_next_loop
                     }
                  }
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
                        .find_position(|rel_arg| expr_to_ident(rel_arg) == Some(arg.clone())).unwrap().0, arg));
            let agg_args_tuple = agg_args_tuple_indices.map(|(ind, arg)| {
               let tuple_ind = syn::Index::from(ind);
               parse2(quote_spanned! {arg.span()=> &__row.#tuple_ind}).unwrap()
            }).collect_vec();
            let agg_args_tuple = tuple_spanned(&agg_args_tuple, agg.span);
            let agg_func = &agg.aggregator;
            let matching_dot_iter = dot_iter_for_rel_index(&mir_relation, quote_spanned!{agg.span => __matching});
            let to_iter_func = ind_val_option_to_iter_func_name_for_rel(&mir_relation);
            quote_spanned! {agg.span=>
               let __matching = #rel_version_var_name.get( &#selected_args_tuple);
               let __agregated_rel = &_self.#rel_name;
               let __agg_args = #to_iter_func(__matching)
                     .map(|&__ind| {
                              let __row = &__agregated_rel[__ind];
                              #agg_args_tuple
                     });
               for #pat in #agg_func(__agg_args) {
                  #next_loop
               }
               
            }
         }
      }
   } else {
      let mut add_rows = vec![];

      for hcl in rule.head_clause.iter() {

         let head_rel_name = &hcl.rel.name;
         let hcl_args_converted = hcl.args.iter().cloned().map(convert_head_arg).collect_vec();
         let new_row_tuple = tuple_spanned(&hcl_args_converted, hcl.args_span);
         
         let head_relation = &hcl.rel;
         let row_type = tuple_type(&head_relation.field_types);

         let mut update_indices = vec![];
         let rel_indices = scc.dynamic_relations.get(head_relation);
         if let Some(rel_indices) = rel_indices {
            for rel_ind in rel_indices.iter(){
               if rel_ind.is_full_index() {continue};
               let var_name = ir_relation_version_var_name(&rel_ind.ir_name(), New);
               let args_tuple : Vec<Expr> = rel_ind.indices.iter().map(|&i| {
                  let i_ind = syn::Index::from(i);
                  syn::parse2(quote_spanned!{head_rel_name.span()=> __new_row.#i_ind.clone()}).unwrap()
               }).collect();
               let args_tuple = tuple(&args_tuple);
               update_indices.push(quote_spanned! {hcl.span=>
                  infer::internal::RelIndexTrait::index_insert(&mut #var_name, #args_tuple, __new_row_ind);
               });
            }
         }

         let head_rel_full_index = &mir.relations_full_indices[head_relation];
         let head_rel_full_index_var_name_new = ir_relation_version_var_name(&head_rel_full_index.ir_name(), New);
         let head_rel_full_index_var_name_delta = ir_relation_version_var_name(&head_rel_full_index.ir_name(), Delta);
         let head_rel_full_index_var_name_total = ir_relation_version_var_name(&head_rel_full_index.ir_name(), Total);

         if !hcl.rel.is_lattice { 
            let head_rel_name_string = head_rel_name.to_string();
            let add_row = quote_spanned!{hcl.span=>
               // comment("check if the tuple already exists");
               let __new_row: #row_type = #new_row_tuple;
               // if !(#head_rel_full_index_var_name_new.contains_key(&__new_row) ||
               //    #head_rel_full_index_var_name_delta.contains_key(&__new_row) ||
               //    #head_rel_full_index_var_name_total.contains_key(&__new_row))
               // {
               //    let __new_row_ind = _self.#head_rel_name.len();
               //    #(#update_indices)*
               //    _self.#head_rel_name.push(__new_row);
               //    __changed = true;
               // }

               let __new_row_ind = _self.#head_rel_name.len();
               if !(#head_rel_full_index_var_name_total.contains_key(&__new_row) ||
                    #head_rel_full_index_var_name_delta.contains_key(&__new_row)){
                  if infer::internal::full_index_insert_if_not_present(&mut #head_rel_full_index_var_name_new, 
                                                                       &__new_row, __new_row_ind){
                     #(#update_indices)*
                     _self.#head_rel_name.push(__new_row);
                     __changed = true;
                  }
               }
            };
            add_rows.push(add_row);
         } else { 
            let head_lat_full_index = &mir.lattices_full_indices[head_relation];
            let head_lat_full_index_var_name_new = ir_relation_version_var_name(&head_lat_full_index.ir_name(), New);
            let head_lat_full_index_var_name_delta = ir_relation_version_var_name(&head_lat_full_index.ir_name(), Delta);
            let head_lat_full_index_var_name_full = ir_relation_version_var_name(&head_lat_full_index.ir_name(), Total);
            let tuple_lat_index = syn::Index::from(hcl.rel.field_types.len() - 1);
            let lattice_key_args = hcl.args.iter().take(hcl.args.len() - 1).map(exp_cloned).collect_vec();
            let lattice_key_tuple = tuple(&lattice_key_args);

            let add_row = quote! {
               let __new_row: #row_type = #new_row_tuple;
               let __lattice_key = #lattice_key_tuple;
               if let Some(__existing_ind) = #head_lat_full_index_var_name_new.get(&__lattice_key)
                  .or_else(|| #head_lat_full_index_var_name_delta.get(&__lattice_key))
                  .or_else(|| #head_lat_full_index_var_name_full.get(&__lattice_key)) 
               {
                  let __existing_ind = *__existing_ind.iter().next().unwrap();
                  // TODO possible excessive cloning here?
                  let __lat_changed = ::infer::Lattice::join_mut(&mut _self.#head_rel_name[__existing_ind].#tuple_lat_index, __new_row.#tuple_lat_index.clone());
                  if __lat_changed {
                     let __new_row_ind = __existing_ind;
                     #(#update_indices)*
                     __changed = true;
                  }
               } else {
                  let __new_row_ind = _self.#head_rel_name.len();
                  #(#update_indices)*
                  _self.#head_rel_name.push(__new_row);
                  __changed = true;
               }
            };
            add_rows.push(add_row);
         }
      }
      quote!{
         #(#add_rows)*
      }
   }
}

fn convert_head_arg(arg: Expr) -> Expr {
   if let Some(var) = expr_to_ident(&arg){
      parse2(quote_spanned!{arg.span()=> infer::internal::Convert::convert(#var)}).unwrap()
   } else {
      arg
   }
}

fn dot_iter_for_rel_index(rel: &MirRelation, var_name: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
   if rel.is_full_index {
      quote_spanned! {var_name.span()=> [#var_name]}
   } else {
      quote_spanned!{var_name.span()=> #var_name.iter()}
   }
}

fn ind_val_option_to_iter_func_name_for_rel(rel: &MirRelation) -> proc_macro2::TokenStream {
   if rel.is_full_index {
      quote! {infer::internal::rel_full_ind_val_option_to_iter}
   } else if !rel.relation.is_lattice {
      quote! {infer::internal::rel_ind_val_option_to_iter}
   } else {
      quote! {infer::internal::lat_ind_val_option_to_iter}
   }
}

