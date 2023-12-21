use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Range;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Module {
    pub funcs: Vec<Func>,
}

#[derive(Clone, Debug, Hash, Serialize, Deserialize, PartialEq)]
pub struct Func {
    pub name: String,
    pub opcodes: Vec<Opcode>,
    pub labels: Vec<OpcodeIndex>,
    pub label_pool: Vec<LabelIndex>,
}

impl Func {
    pub fn op(&self, id: OpcodeIndex) -> Opcode {
        self.opcodes[id.0 as usize]
    }

    pub fn ops(&self) -> &[Opcode] {
        &self.opcodes
    }
}

impl Func {
    pub fn push_op(&mut self, op: Opcode) -> OpcodeIndex {
        self.opcodes.push(op);
        OpcodeIndex((self.opcodes.len() - 1) as u32)
    }

    pub fn remove_op(&mut self, id: OpcodeIndex) {
        self.opcodes[id.0 as usize] = Opcode::Nop;
    }

    pub fn add_label(&mut self) -> LabelIndex {
        self.labels.push(OpcodeIndex::UNSET);
        LabelIndex((self.labels.len() - 1) as u32)
    }

    pub fn set_label(&mut self, index: LabelIndex, value: OpcodeIndex) {
        self.labels[index.0 as usize] = value;
    }

    pub fn push_label_slice(&mut self, ids: &[LabelIndex]) -> LabelPoolSlice {
        let start = self.label_pool.len();
        self.label_pool.extend_from_slice(ids);
        // TODO: Replace all casts with into conversions and errors
        LabelPoolSlice(start as u32, self.label_pool.len() as u32)
    }

    pub fn drop_unused_labels(&mut self) {
        let mut new = vec![];
        let mut map = HashMap::new();

        let mut shift = |slice: &mut LabelPoolSlice| match map.get(slice).copied() {
            Some(new_targets) => *slice = new_targets,
            None => {
                let start = new.len();
                new.extend_from_slice(slice.slice(&self.label_pool));
                let new_targets = LabelPoolSlice(start as u32, new.len() as u32);
                map.insert(*slice, new_targets);
                *slice = new_targets;
            }
        };

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
                | Opcode::LiteralF8(_)
                | Opcode::Add
                | Opcode::Mult
                | Opcode::Return
                | Opcode::Jump(_)
                | Opcode::Nop => (),
                Opcode::Branch { targets, .. } => shift(targets),
            }
        }
    }

    pub fn remove_nops(&mut self) {
        self.opcodes = self
            .opcodes
            .drain(..)
            .filter(|&op| op != Opcode::Nop)
            .collect();
    }
}

#[derive(Clone, Copy, Debug, Hash, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpcodeIndex(u32);

impl OpcodeIndex {
    pub const UNSET: OpcodeIndex = OpcodeIndex(u32::MAX);

    pub fn check(self, bounds: Range<usize>, args: &mut Vec<OpcodeIndex>) {
        if self.0 as usize >= bounds.end {
            // TODO: Nice error for time travel
            panic!()
        }

        if (self.0 as usize) < bounds.start {
            args.push(self);
        }
    }

    pub fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    pub fn index(&self) -> usize {
        self.0.try_into().unwrap()
    }
}

#[derive(Clone, Copy, Debug, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub struct LabelIndex(u32);

impl LabelIndex {
    pub const UNSET: LabelIndex = LabelIndex(u32::MAX);

    pub fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    pub fn index(&self) -> usize {
        self.0.try_into().unwrap()
    }
}

#[derive(Clone, Copy, Debug, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub struct LabelPoolSlice(pub u32, pub u32);

impl LabelPoolSlice {
    pub fn slice(self, pool: &[LabelIndex]) -> &[LabelIndex] {
        &pool[self.0 as usize..self.1 as usize]
    }
}

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
    Add,
    Mult,
    Return,
    Jump(LabelIndex),
    Branch {
        default: LabelIndex,
        targets: LabelPoolSlice,
    },
    Nop,
}
