//! DefUse Chains with Composed Types
//!
//! The following example utilizes a composed type to model a type hierarchy for instructions.
//!
//! In this example an instruction is either a `Read`` operation, a `Write` operation or a `Jump` instruction.
//!
//! However, to model the control flow through instructions, the flow relation needs to be able to
//! cover any of those.
//!
//! To model this situation, the union type Instr is introduced and utilized as shown above.
//!
//! (Adapted from https://souffle-lang.github.io/examples#defuse-chains-with-composed-types)

use ascent::ascent;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Var(&'static str);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Read(&'static str);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Write(&'static str);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Jump(&'static str);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Instr {
   Read(Read),
   Write(Write),
   Jump(Jump),
}

ascent! {
    // Facts:

    relation read(Read, Var);
    relation write(Write, Var);
    relation succ(Instr, Instr);

    // Rules:

    relation flow(Instr, Instr);

    flow(x, y) <-- succ(x, y);
    flow(x, z) <-- flow(x, y), flow(y, z);

    relation def_use(Write, Read);

    def_use(w, r) <-- write(w, x), read(r, x), let w2 = Instr::Write(w.clone()), let r2 = Instr::Read(r.clone()), flow(w2, r2);
}

fn main() {
   let mut prog = AscentProgram::default();

   prog.read = vec![(Read("r1"), Var("v1")), (Read("r2"), Var("v1")), (Read("r3"), Var("v2"))];

   prog.write = vec![(Write("w1"), Var("v1")), (Write("w2"), Var("v2")), (Write("w3"), Var("v2"))];

   prog.succ = vec![
      (Instr::Write(Write("w1")), Instr::Jump(Jump("o1"))),
      (Instr::Jump(Jump("o1")), Instr::Read(Read("r1"))),
      (Instr::Jump(Jump("o1")), Instr::Read(Read("r2"))),
      (Instr::Read(Read("r2")), Instr::Read(Read("r3"))),
      (Instr::Read(Read("r3")), Instr::Write(Write("w2"))),
   ];

   prog.run();

   let AscentProgram { mut def_use, .. } = prog;

   def_use.sort_by_key(|(key, _)| key.0);
   def_use.sort_by_key(|(_, key)| key.0);

   assert_eq!(def_use, vec![(Write("w1"), Read("r1")), (Write("w1"), Read("r2")),]);
}
