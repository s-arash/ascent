use std::rc::Rc;

use infer::dl;

type SrcLine = u32;

type Register = &'static str;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Val {
   Ref(Register),
   Lit(i32)
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Trace {
   Null,
   Cons(SrcLine, Rc<Trace>)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instr {
   Mov(Register, Val),
   Cmp(Register, Val, Val),
   Brz(Register, SrcLine),
   Add(Register, Val, Val),
   Br(SrcLine),
}
fn instr_vals(instr: &Instr) -> Vec<&Val>{
   todo!()
}

use Instr::*;
dl! {
   relation trace(SrcLine, Trace); //scrline duplicates the head of trace for fast lookup
   relation source(SrcLine, Instr);
   relation store(Trace, Register, i32);
   relation aeval(Trace, Val, i32);
   
   aeval(time.clone(), val, eval) <--
      source(pc, instr),
      trace(pc, time),
      for (&val, &eval) in instr_vals(instr).into_iter().filter_map(|v| match v {Val::Lit(l) => Some((v,l)), Val::Ref(r) => None});
   
   aeval(time.clone(), val, *eval) <--
      source(pc, instr),
      trace(pc, time),
      for (&val, &reg) in instr_vals(instr).into_iter().filter_map(|v| match v {Val::Lit(x) => None, Val::Ref(r) => Some((v, r))}),
      store(time, reg, eval);

   trace(pc + 1, Trace::Cons(pc + 1, Rc::new(time.clone()) )) <--
      source(pc, ?Mov(target, val)),
      trace(pc, time),
      aeval(time, val, eval);

   store(Trace::Cons(pc + 1, Rc::new(time.clone())), target, *target_eval) <--
      source(pc, ?Mov(target, val)),
      trace(pc, time),
      aeval(time, val, target_eval);
}
