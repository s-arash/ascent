#![deny(warnings)]
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream};
use syn::spanned::Spanned;
use syn::{Attribute, Error, Expr, Pat, Type, parse_quote, parse2};

use crate::AscentProgram;
use crate::ascent_syntax::{
   BodyClauseArg, BodyItemNode, CondClause, DsAttributeContents, GeneratorNode, RelationIdentity, RelationNode,
   RuleNode, Signatures,
};
use crate::syn_utils::{expr_get_vars, pattern_get_vars};
use crate::utils::{expr_to_ident, is_wild_card, tuple_type};

#[derive(Clone)]
pub(crate) struct AscentConfig {
   #[allow(dead_code)]
   pub attrs: Vec<Attribute>,
   pub include_rule_times: bool,
   pub generate_run_partial: bool,
   pub inter_rule_parallelism: bool,
   pub default_ds: DsAttributeContents,
}

impl AscentConfig {
   const MEASURE_RULE_TIMES_ATTR: &'static str = "measure_rule_times";
   const GENERATE_RUN_TIMEOUT_ATTR: &'static str = "generate_run_timeout";
   const INTER_RULE_PARALLELISM_ATTR: &'static str = "inter_rule_parallelism";

   pub fn new(attrs: Vec<Attribute>, is_parallel: bool) -> syn::Result<AscentConfig> {
      let include_rule_times = attrs
         .iter()
         .find(|attr| attr.meta.path().is_ident(Self::MEASURE_RULE_TIMES_ATTR))
         .map(|attr| attr.meta.require_path_only())
         .transpose()?
         .is_some();
      let generate_run_partial = attrs
         .iter()
         .find(|attr| attr.meta.path().is_ident(Self::GENERATE_RUN_TIMEOUT_ATTR))
         .map(|attr| attr.meta.require_path_only())
         .transpose()?
         .is_some();
      let inter_rule_parallelism = attrs
         .iter()
         .find(|attr| attr.meta.path().is_ident(Self::INTER_RULE_PARALLELISM_ATTR))
         .map(|attr| attr.meta.require_path_only())
         .transpose()?;

      let recognized_attrs = [
         Self::MEASURE_RULE_TIMES_ATTR,
         Self::GENERATE_RUN_TIMEOUT_ATTR,
         Self::INTER_RULE_PARALLELISM_ATTR,
         REL_DS_ATTR,
      ];
      for attr in attrs.iter() {
         if !recognized_attrs.iter().any(|recognized_attr| attr.meta.path().is_ident(recognized_attr)) {
            let recognized_attrs = recognized_attrs.iter().map(|attr| format!("`{attr}`")).join(", ");
            return Err(Error::new_spanned(
               attr,
               format!("unrecognized attribute. recognized attributes are: {recognized_attrs}"),
            ));
         }
      }
      if inter_rule_parallelism.is_some() && !is_parallel {
         return Err(Error::new_spanned(inter_rule_parallelism, "attribute only allowed in parallel Ascent"));
      }
      let default_ds = get_ds_attr(&attrs)?
         .unwrap_or_else(|| DsAttributeContents { path: parse_quote! {::ascent::rel}, args: TokenStream::default() });
      Ok(AscentConfig {
         inter_rule_parallelism: inter_rule_parallelism.is_some(),
         attrs,
         include_rule_times,
         generate_run_partial,
         default_ds,
      })
   }
}

pub(crate) struct AscentIr {
   pub relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>>,
   pub relations_full_indices: HashMap<RelationIdentity, IrRelation>,
   pub lattices_full_indices: HashMap<RelationIdentity, IrRelation>,
   // pub relations_no_indices: HashMap<RelationIdentity, IrRelation>,
   pub relations_metadata: HashMap<RelationIdentity, RelationMetadata>,
   pub rules: Vec<IrRule>,
   pub signatures: Signatures,
   pub config: AscentConfig,
   pub is_parallel: bool,
}

#[derive(Clone)]
pub(crate) struct RelationMetadata {
   pub initialization: Option<Rc<Expr>>,
   pub attributes: Rc<Vec<Attribute>>,
   /// Will be `Some()` iff the relation is not a lattice
   pub ds_attr: Option<DsAttributeContents>,
}

pub(crate) struct IrRule {
   pub head_clauses: Vec<IrHeadClause>,
   pub body_items: Vec<IrBodyItem>,
   pub simple_join_start_index: Option<usize>,
}

#[allow(unused)]
pub(crate) fn ir_rule_summary(rule: &IrRule) -> String {
   fn bitem_to_str(bi: &IrBodyItem) -> String {
      match bi {
         IrBodyItem::Clause(cl) => cl.rel.ir_name().to_string(),
         IrBodyItem::Generator(_) => "for ⋯".into(),
         IrBodyItem::Cond(CondClause::If(..)) => format!("if ⋯"),
         IrBodyItem::Cond(CondClause::IfLet(..)) => format!("if let ⋯"),
         IrBodyItem::Cond(CondClause::Let(..)) => format!("let ⋯"),
         IrBodyItem::Agg(agg) => format!("agg {}", agg.rel.ir_name()),
      }
   }
   format!(
      "{} <-- {}",
      rule.head_clauses.iter().map(|hcl| hcl.rel.name.to_string()).join(", "),
      rule.body_items.iter().map(bitem_to_str).join(", ")
   )
}

#[derive(Clone)]
pub(crate) struct IrHeadClause {
   pub rel: RelationIdentity,
   pub args: Vec<Expr>,
   pub span: Span,
   pub args_span: Span,
}

pub(crate) enum IrBodyItem {
   Clause(IrBodyClause),
   Generator(GeneratorNode),
   Cond(CondClause),
   Agg(IrAggClause),
}

impl IrBodyItem {
   pub(crate) fn rel(&self) -> Option<&IrRelation> {
      match self {
         IrBodyItem::Clause(bcl) => Some(&bcl.rel),
         IrBodyItem::Agg(agg) => Some(&agg.rel),
         IrBodyItem::Generator(_) | IrBodyItem::Cond(_) => None,
      }
   }
}

#[derive(Clone)]
pub(crate) struct IrBodyClause {
   pub rel: IrRelation,
   pub args: Vec<Expr>,
   pub rel_args_span: Span,
   pub args_span: Span,
   pub cond_clauses: Vec<CondClause>,
}

impl IrBodyClause {
   #[allow(dead_code)]
   pub fn selected_args(&self) -> Vec<Expr> { self.rel.indices.iter().map(|&i| self.args[i].clone()).collect() }
}

#[derive(Clone)]
pub(crate) struct IrAggClause {
   pub span: Span,
   pub pat: Pat,
   pub aggregator: Expr,
   pub bound_args: Vec<Ident>,
   pub rel: IrRelation,
   pub rel_args: Vec<Expr>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub(crate) struct IrRelation {
   pub relation: RelationIdentity,
   pub indices: Vec<usize>,
   pub val_type: IndexValType,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum IndexValType {
   Reference,
   Direct(Vec<usize>),
}

impl IrRelation {
   pub fn new(relation: RelationIdentity, indices: Vec<usize>) -> Self {
      // TODO this is not the right place for this
      let val_type = if relation.is_lattice
      //|| indices.len() == relation.field_types.len()
      {
         IndexValType::Reference
      } else {
         IndexValType::Direct((0..relation.field_types.len()).filter(|i| !indices.contains(i)).collect_vec())
      };
      IrRelation { relation, indices, val_type }
   }

   pub fn key_type(&self) -> Type {
      let index_types: Vec<_> = self.indices.iter().map(|&i| self.relation.field_types[i].clone()).collect();
      tuple_type(&index_types)
   }
   pub fn ir_name(&self) -> Ident { ir_name_for_rel_indices(&self.relation.name, &self.indices) }
   pub fn is_full_index(&self) -> bool { self.relation.field_types.len() == self.indices.len() }
   pub fn is_no_index(&self) -> bool { self.indices.is_empty() }

   pub fn value_type(&self) -> Type {
      match &self.val_type {
         IndexValType::Reference => parse_quote! {usize},
         IndexValType::Direct(cols) => {
            let index_types: Vec<_> = cols.iter().map(|&i| self.relation.field_types[i].clone()).collect();
            tuple_type(&index_types)
         },
      }
   }
}

const REL_DS_ATTR: &str = "ds";
const RECOGNIIZED_REL_ATTRS: [&str; 1] = [REL_DS_ATTR];

pub(crate) fn compile_ascent_program_to_hir(prog: &AscentProgram, is_parallel: bool) -> syn::Result<AscentIr> {
   let ir_rules: Vec<(IrRule, Vec<IrRelation>)> =
      prog.rules.iter().map(|r| compile_rule_to_ir_rule(r, prog)).try_collect()?;
   let config = AscentConfig::new(prog.attributes.clone(), is_parallel)?;
   let num_relations = prog.relations.len();
   let mut relations_ir_relations: HashMap<RelationIdentity, HashSet<IrRelation>> =
      HashMap::with_capacity(num_relations);
   let mut relations_full_indices = HashMap::with_capacity(num_relations);
   let mut relations_initializations = HashMap::new();
   let mut relations_metadata = HashMap::with_capacity(num_relations);
   // let mut relations_no_indices = HashMap::new();
   let mut lattices_full_indices = HashMap::new();
   for rel in prog.relations.iter() {
      let rel_identity = RelationIdentity::from(rel);

      if rel.is_lattice {
         let indices = (0..rel_identity.field_types.len() - 1).collect_vec();
         let lat_full_index = IrRelation::new(rel_identity.clone(), indices);
         relations_ir_relations.entry(rel_identity.clone()).or_default().insert(lat_full_index.clone());
         lattices_full_indices.insert(rel_identity.clone(), lat_full_index);
      }

      let full_indices = (0..rel_identity.field_types.len()).collect_vec();
      let rel_full_index = IrRelation::new(rel_identity.clone(), full_indices);

      relations_ir_relations.entry(rel_identity.clone()).or_default().insert(rel_full_index.clone());
      // relations_ir_relations.entry(rel_identity.clone()).or_default().insert(rel_no_index.clone());
      relations_full_indices.insert(rel_identity.clone(), rel_full_index);
      if let Some(init_expr) = &rel.initialization {
         relations_initializations.insert(rel_identity.clone(), Rc::new(init_expr.clone()));
      }
      let ds_attribute = get_ds_attr(&rel.attrs)?;

      let ds_attribute = match (ds_attribute, rel.is_lattice) {
         (None, true) => None,
         (None, false) => Some(config.default_ds.clone()),
         (Some(attr), true) =>
            return Err(Error::new(attr.path.span(), "`lattice`s cannot have custom data structure providers")),
         (Some(attr), false) => Some(attr),
      };

      relations_metadata.insert(rel_identity.clone(), RelationMetadata {
         initialization: rel.initialization.clone().map(Rc::new),
         attributes: Rc::new(
            rel.attrs
               .iter()
               .filter(|attr| {
                  attr.meta.path().get_ident().map_or(true, |ident| !RECOGNIIZED_REL_ATTRS.iter().any(|ra| ident == ra))
               })
               .cloned()
               .collect_vec(),
         ),
         ds_attr: ds_attribute,
      });
      // relations_no_indices.insert(rel_identity, rel_no_index);
   }
   for (ir_rule, extra_relations) in ir_rules.iter() {
      for bitem in ir_rule.body_items.iter() {
         let rel = match bitem {
            IrBodyItem::Clause(bcl) => Some(&bcl.rel),
            IrBodyItem::Agg(agg) => Some(&agg.rel),
            _ => None,
         };
         if let Some(rel) = rel {
            let relation = &rel.relation;
            relations_ir_relations.entry(relation.clone()).or_default().insert(rel.clone());
         }
      }
      for extra_rel in extra_relations.iter() {
         relations_ir_relations.entry(extra_rel.relation.clone()).or_default().insert(extra_rel.clone());
      }
   }
   let signatures = prog.signatures.clone().unwrap_or_else(|| parse2(quote! {pub struct AscentProgram;}).unwrap());
   Ok(AscentIr {
      rules: ir_rules.into_iter().map(|(rule, _extra_rels)| rule).collect_vec(),
      relations_ir_relations,
      relations_full_indices,
      lattices_full_indices,
      relations_metadata,
      // relations_no_indices,
      signatures,
      config,
      is_parallel,
   })
}

fn get_ds_attr(attrs: &[Attribute]) -> syn::Result<Option<DsAttributeContents>> {
   let ds_attrs =
      attrs.iter().filter(|attr| attr.meta.path().get_ident().is_some_and(|ident| ident == REL_DS_ATTR)).collect_vec();
   match &ds_attrs[..] {
      [] => Ok(None),
      [attr] => {
         let res = syn::parse2::<DsAttributeContents>(attr.meta.require_list()?.tokens.clone())?;
         Ok(Some(res))
      },
      [_attr1, attr2, ..] => Err(Error::new(attr2.bracket_token.span.join(), "multiple `ds` attributes specified")),
   }
}

fn compile_rule_to_ir_rule(rule: &RuleNode, prog: &AscentProgram) -> syn::Result<(IrRule, Vec<IrRelation>)> {
   let mut body_items = vec![];
   let mut grounded_vars = vec![];
   fn extend_grounded_vars(
      grounded_vars: &mut Vec<Ident>, new_vars: impl IntoIterator<Item = Ident>,
   ) -> syn::Result<()> {
      for v in new_vars.into_iter() {
         if grounded_vars.contains(&v) {
            // TODO may someday this will work
            let other_var = grounded_vars.iter().find(|&x| x == &v).unwrap();
            let other_err = Error::new(other_var.span(), "variable being shadowed");
            let mut err = Error::new(v.span(), format!("`{v}` shadows another variable with the same name"));
            err.combine(other_err);
            return Err(err);
         }
         grounded_vars.push(v);
      }
      Ok(())
   }

   let first_clause_ind =
      rule.body_items.iter().enumerate().find(|(_, bi)| matches!(bi, BodyItemNode::Clause(..))).map(|(i, _)| i);
   let mut first_two_clauses_simple = first_clause_ind.is_some()
      && matches!(rule.body_items.get(first_clause_ind.unwrap() + 1), Some(BodyItemNode::Clause(..)));
   for (bitem_ind, bitem) in rule.body_items.iter().enumerate() {
      match bitem {
         BodyItemNode::Clause(ref bcl) => {
            if first_clause_ind == Some(bitem_ind)
               && bcl.cond_clauses.iter().any(|c| matches!(c, &CondClause::IfLet(_)))
            {
               first_two_clauses_simple = false;
            }

            if first_clause_ind.map(|x| x + 1) == Some(bitem_ind) && first_two_clauses_simple {
               let mut self_vars = HashSet::new();
               for var in bcl.args.iter().filter_map(|arg| expr_to_ident(arg.unwrap_expr_ref())) {
                  if !self_vars.insert(var) {
                     first_two_clauses_simple = false;
                  }
               }
               for cond_cl in bcl.cond_clauses.iter() {
                  let cond_expr = cond_cl.expr();
                  let expr_idents = expr_get_vars(cond_expr);
                  if !expr_idents.iter().all(|v| self_vars.contains(v)) {
                     first_two_clauses_simple = false;
                     break;
                  }
                  self_vars.extend(cond_cl.bound_vars());
               }
            }
            let mut indices = vec![];
            for (i, arg) in bcl.args.iter().enumerate() {
               if let Some(var) = expr_to_ident(arg.unwrap_expr_ref()) {
                  if grounded_vars.contains(&var) {
                     indices.push(i);
                     if first_clause_ind == Some(bitem_ind) {
                        first_two_clauses_simple = false;
                     }
                  } else {
                     grounded_vars.push(var);
                  }
               } else {
                  indices.push(i);
                  if bitem_ind < 2 + first_clause_ind.unwrap_or(0) {
                     first_two_clauses_simple = false;
                  }
               }
            }
            let relation = prog_get_relation(prog, &bcl.rel, bcl.args.len())?;

            for cond_clause in bcl.cond_clauses.iter() {
               extend_grounded_vars(&mut grounded_vars, cond_clause.bound_vars())?;
            }

            let ir_rel = IrRelation::new(relation.into(), indices);
            let ir_bcl = IrBodyClause {
               rel: ir_rel,
               args: bcl.args.iter().cloned().map(BodyClauseArg::unwrap_expr).collect(),
               rel_args_span: bcl.rel.span().join(bcl.args.span()).unwrap_or_else(|| bcl.rel.span()),
               args_span: bcl.args.span(),
               cond_clauses: bcl.cond_clauses.clone(),
            };
            body_items.push(IrBodyItem::Clause(ir_bcl));
         },
         BodyItemNode::Generator(ref gen) => {
            extend_grounded_vars(&mut grounded_vars, pattern_get_vars(&gen.pattern))?;
            body_items.push(IrBodyItem::Generator(gen.clone()));
         },
         BodyItemNode::Cond(ref cl) => {
            body_items.push(IrBodyItem::Cond(cl.clone()));
            extend_grounded_vars(&mut grounded_vars, cl.bound_vars())?;
         },
         BodyItemNode::Agg(ref agg) => {
            extend_grounded_vars(&mut grounded_vars, pattern_get_vars(&agg.pat))?;
            let indices = agg
               .rel_args
               .iter()
               .enumerate()
               .filter(|(_i, expr)| {
                  if is_wild_card(expr) {
                     return false;
                  } else if let Some(ident) = expr_to_ident(expr) {
                     if agg.bound_args.iter().contains(&ident) {
                        return false;
                     }
                  }
                  true
               })
               .map(|(i, _expr)| i)
               .collect_vec();
            let relation = prog_get_relation(prog, &agg.rel, agg.rel_args.len())?;

            let ir_rel = IrRelation::new(relation.into(), indices);
            let ir_agg_clause = IrAggClause {
               span: agg.agg_kw.span,
               pat: agg.pat.clone(),
               aggregator: agg.aggregator.get_expr(),
               bound_args: agg.bound_args.iter().cloned().collect_vec(),
               rel: ir_rel,
               rel_args: agg.rel_args.iter().cloned().collect_vec(),
            };
            body_items.push(IrBodyItem::Agg(ir_agg_clause));
         },
         _ => panic!("unrecognized body item"),
      }
   }
   let mut head_clauses = vec![];
   for hcl_node in rule.head_clauses.iter() {
      let hcl_node = hcl_node.clause();
      let rel = prog.relations.iter().find(|r| hcl_node.rel == r.name);
      let rel = match rel {
         Some(rel) => rel,
         None => return Err(Error::new(hcl_node.rel.span(), format!("relation `{}` is not defined", hcl_node.rel))),
      };

      let rel = RelationIdentity::from(rel);
      let head_clause = IrHeadClause {
         rel,
         args: hcl_node.args.iter().cloned().collect(),
         span: hcl_node.span(),
         args_span: hcl_node.args.span(),
      };
      head_clauses.push(head_clause);
   }

   let is_simple_join = first_two_clauses_simple && body_items.len() >= 2;
   let simple_join_start_index = if is_simple_join { first_clause_ind } else { None };

   let simple_join_ir_relations = if let Some(start_ind) = simple_join_start_index {
      let (bcl1, bcl2) = match &body_items[start_ind..start_ind + 2] {
         [IrBodyItem::Clause(bcl1), IrBodyItem::Clause(bcl2)] => (bcl1, bcl2),
         _ => panic!("incorrect simple join handling in ascent_hir"),
      };
      let bcl2_vars = bcl2.args.iter().filter_map(expr_to_ident).collect_vec();
      let indices = get_indices_given_grounded_variables(&bcl1.args, &bcl2_vars);
      let new_cl1_ir_relation = IrRelation::new(bcl1.rel.relation.clone(), indices);
      vec![new_cl1_ir_relation]
   } else {
      vec![]
   };

   if let Some(start_ind) = simple_join_start_index {
      if let IrBodyItem::Clause(cl1) = &mut body_items[start_ind] {
         cl1.rel = simple_join_ir_relations[0].clone();
      }
   }

   Ok((IrRule { simple_join_start_index, head_clauses, body_items }, vec![]))
}

pub fn ir_name_for_rel_indices(rel: &Ident, indices: &[usize]) -> Ident {
   let indices_str = if indices.is_empty() { format!("none") } else { indices.iter().join("_") };
   let name = format!("{}_indices_{}", rel, indices_str);
   Ident::new(&name, rel.span())
}

/// for a clause with args, returns the indices assuming vars are grounded.
pub fn get_indices_given_grounded_variables(args: &[Expr], vars: &[Ident]) -> Vec<usize> {
   let mut res = vec![];
   for (i, arg) in args.iter().enumerate() {
      if let Some(arg_var) = expr_to_ident(arg) {
         if vars.contains(&arg_var) {
            res.push(i);
         }
      } else {
         res.push(i);
      }
   }
   res
}

pub(crate) fn prog_get_relation<'a>(
   prog: &'a AscentProgram, name: &Ident, arity: usize,
) -> syn::Result<&'a RelationNode> {
   let relation = prog.relations.iter().find(|r| name == &r.name);
   match relation {
      Some(rel) =>
         if rel.field_types.len() != arity {
            Err(Error::new(
               name.span(),
               format!(
                  "wrong arity for relation `{name}` (expected {expected}, found {actual})",
                  expected = arity,
                  actual = rel.field_types.len()
               ),
            ))
         } else {
            Ok(rel)
         },
      None => Err(Error::new(name.span(), format!("relation `{}` is not defined", name))),
   }
}
