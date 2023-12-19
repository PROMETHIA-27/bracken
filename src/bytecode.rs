use std::collections::HashMap;
use std::hash::Hash;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Module {
    pub funcs: Vec<Func>,
}

#[derive(Clone, Debug, Hash, Serialize, Deserialize, PartialEq)]
pub struct Func {
    pub name: String,
    pub opcodes: Vec<Opcode>,
}

impl Func {
    pub fn op(&self, id: BeltOffset) -> Opcode {
        self.opcodes[id.0 as usize]
    }

    pub fn ops(&self) -> &[Opcode] {
        &self.opcodes
    }

    pub fn push_op(&mut self, op: Opcode) -> BeltOffset {
        self.opcodes.push(op);
        BeltOffset((self.opcodes.len() - 1) as u32)
    }

    pub fn remove_op(&mut self, id: BeltOffset) {
        self.opcodes[id.0 as usize] = Opcode::Nop;
    }

    /// Invalidates all [`OpcodeId`]s.
    pub fn remove_nops(&mut self) {
        let mut mapping = HashMap::new();
        let mut new = vec![];

        for (i, op) in self.opcodes.drain(..).enumerate() {
            match op {
                Opcode::Nop => continue,
                op => {
                    new.push(op);
                    mapping.insert(BeltOffset(i as u32), BeltOffset((new.len() - 1) as u32));
                }
            }
        }

        self.opcodes = new;

        macro_rules! map {
            ($($id:ident),+) => {
                {$(
                    *$id = mapping[&$id];
                )+}
            };
        }

        for op in &mut self.opcodes {
            match op {
                Opcode::LiteralS1(_)
                | Opcode::LiteralS2(_)
                | Opcode::LiteralS4(_)
                | Opcode::LiteralS8(_)
                | Opcode::LiteralU1(_)
                | Opcode::LiteralU2(_)
                | Opcode::LiteralU4(_)
                | Opcode::LiteralU8(_)
                | Opcode::LiteralF4(_)
                | Opcode::LiteralF8(_) => (),
                Opcode::Ret(i) => map!(i),
                Opcode::Add(i, j) => map!(i, j),
                Opcode::Nop => unreachable!(),
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub struct BeltOffset(pub u32);

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct BinaryF32(pub f32);

impl Hash for BinaryF32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as u32).hash(state);
    }
}

impl PartialEq for BinaryF32 {
    fn eq(&self, other: &Self) -> bool {
        self.0 as u32 == other.0 as u32
    }
}

impl Eq for BinaryF32 {}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct BinaryF64(pub f64);

impl Hash for BinaryF64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as u64).hash(state);
    }
}

impl PartialEq for BinaryF64 {
    fn eq(&self, other: &Self) -> bool {
        self.0 as u64 == other.0 as u64
    }
}

impl Eq for BinaryF64 {}

#[derive(Clone, Copy, Debug, Hash, Serialize, Deserialize, PartialEq)]
pub enum Opcode {
    LiteralS1(i8),
    LiteralS2(i16),
    LiteralS4(i32),
    LiteralS8(i64),
    LiteralU1(u8),
    LiteralU2(u16),
    LiteralU4(u32),
    LiteralU8(u64),
    LiteralF4(BinaryF32),
    LiteralF8(BinaryF64),
    Add(BeltOffset, BeltOffset),
    Ret(BeltOffset),
    Nop,
}
