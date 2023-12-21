use std::collections::hash_map::RandomState;
use std::collections::{HashMap, HashSet};

use bincode::{DefaultOptions, Options};
use bracken::bytecode::{self, Func, LabelIndex, Opcode, SSAIndex};
use cm::Module;
use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::{
    types, AbiParam, Block, BlockCall, Function, InstBuilder, JumpTableData, Type, Value,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{self as cm, default_libcall_names};

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let module = DefaultOptions::new()
        .with_varint_encoding()
        .allow_trailing_bytes()
        .deserialize::<bytecode::Module>(&std::fs::read(file).unwrap())
        .unwrap();
    println!("{module:?}");
    run(&module);
}

fn run(module: &bytecode::Module) {
    let mut jit = Jit {
        module: JITModule::new(JITBuilder::new(default_libcall_names()).unwrap()),
    };
    compile_func(&module.funcs[0], &mut jit);
}

struct Jit {
    module: JITModule,
}

fn compile_func(func: &Func, jit: &mut Jit) {
    let mut sig = jit.module.make_signature();
    sig.returns = vec![AbiParam::new(types::I32)];
    let func_id = jit
        .module
        .declare_function(&func.name, cranelift_module::Linkage::Export, &sig)
        .unwrap();
    let mut cfunc = Function::new();
    cfunc.signature = sig;

    let mut fnctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut cfunc, &mut fnctx);

    let mut values = SSAValues::default();
    for (i, _) in func.ops().iter().enumerate() {
        if let Some(ty) = ssa_index_type(func.ops(), SSAIndex(i as u32)) {
            values.add(&mut b, i, ty);
        }
    }

    let blocks = BasicBlocks::new(func.ops(), &func.labels, &func.label_pool);
    println!("{blocks:?}");
    let block_map = blocks.block_map(&mut b);
    let blocks = blocks.0.into_iter().map(
        |BasicBlock {
             bounds: (start, end),
         }| (start, &func.ops()[start..end]),
    );
    for (start, block) in blocks {
        let cblock = *block_map.get(&start).unwrap();
        b.switch_to_block(cblock);
        if start == 0 {
            b.append_block_params_for_function_params(cblock);
        }
        // for arg in args {
        //     let param = b.append_block_param(cblock, ssa_index_type(func.ops(), arg).unwrap());
        //     values.add(arg, param);
        // }
        compile_block(
            block,
            start,
            &func.labels,
            &func.label_pool,
            cblock,
            &block_map,
            &mut b,
            &mut values,
        );
        // values.clear();
    }

    // TODO: A counter for jumps to a given block would let me seal sooner
    b.seal_all_blocks();
    b.finalize();

    let mut ctx = jit.module.make_context();
    ctx.func = cfunc;
    jit.module.define_function(func_id, &mut ctx).unwrap();

    jit.module.finalize_definitions().unwrap();

    let func = jit.module.get_finalized_function(func_id);
    let x = unsafe { std::mem::transmute::<_, extern "C" fn() -> i32>(func) }();
    println!("{x}");
}

#[allow(clippy::too_many_arguments)]
fn compile_block(
    ops: &[Opcode],
    offset: usize,
    labels: &[SSAIndex],
    label_pool: &[LabelIndex],
    _cblock: Block,
    block_map: &HashMap<usize, Block>,
    b: &mut FunctionBuilder,
    values: &mut SSAValues,
) {
    for (i, &op) in ops.iter().enumerate() {
        let i = SSAIndex((i + offset) as u32);
        match op {
            Opcode::LiteralS1(_) => todo!(),
            Opcode::LiteralS2(_) => todo!(),
            Opcode::LiteralS4(c) => {
                let value = b.ins().iconst(types::I32, c as i64);
                // values.add(i, value);
                values.set(i, b, value);
            }
            Opcode::LiteralS8(_) => todo!(),
            Opcode::LiteralU1(_) => todo!(),
            Opcode::LiteralU2(_) => todo!(),
            Opcode::LiteralU4(_) => todo!(),
            Opcode::LiteralU8(_) => todo!(),
            Opcode::LiteralF4(c) => {
                let value = b.ins().f32const(c.0);
                // values.add(i, value);
                values.set(i, b, value);
            }
            Opcode::LiteralF8(_) => todo!(),
            Opcode::Add(lhs, rhs) => {
                // let lhs = values.get(lhs).unwrap();
                // let rhs = values.get(rhs).unwrap();
                let lhs = values.get(lhs, b).unwrap();
                let rhs = values.get(rhs, b).unwrap();
                let lhs_ty = b.func.dfg.value_type(lhs);
                let rhs_ty = b.func.dfg.value_type(rhs);
                if lhs_ty.is_int() && rhs_ty.is_int() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = b.ins().iadd(lhs, rhs);
                    // values.add(i, value);
                    values.set(i, b, value);
                } else if lhs_ty.is_float() && rhs_ty.is_float() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = b.ins().fadd(lhs, rhs);
                    // values.add(i, value);
                    values.set(i, b, value);
                } else {
                    panic!("attempted to add a float to an int and/or differently sized numerics")
                }
            }
            Opcode::Mult(lhs, rhs) => {
                // let lhs = values.get(lhs).unwrap();
                // let rhs = values.get(rhs).unwrap();
                let lhs = values.get(lhs, b).unwrap();
                let rhs = values.get(rhs, b).unwrap();
                let lhs_ty = b.func.dfg.value_type(lhs);
                let rhs_ty = b.func.dfg.value_type(rhs);
                if lhs_ty.is_int() && rhs_ty.is_int() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = b.ins().imul(lhs, rhs);
                    // values.add(i, value);
                    values.set(i, b, value);
                } else if lhs_ty.is_float() && rhs_ty.is_float() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = b.ins().fmul(lhs, rhs);
                    // values.add(i, value);
                    values.set(i, b, value);
                } else {
                    panic!("attempted to add a float to an int and/or differently sized numerics")
                }
            }
            Opcode::Return(offset) => {
                let value = values.get(offset, b).unwrap();
                b.ins().return_(&[value]);
            }
            Opcode::Jump(target) => {
                let target_opcode = labels[target.0 as usize];
                let target_block = block_map[&(target_opcode.0 as usize)];
                b.ins().jump(target_block, &[]);
            }
            Opcode::Branch {
                pred,
                default,
                targets,
            } => {
                let pred = values.get(pred, b).unwrap();
                let default_opcode = labels[default.0 as usize];
                let default = b
                    .func
                    .dfg
                    .block_call(block_map[&(default_opcode.0 as usize)], &[]);
                let jumps: Vec<BlockCall> = targets
                    .slice(label_pool)
                    .iter()
                    .map(|&label| {
                        let target_opcode = labels[label.0 as usize];
                        let target_block = block_map[&(target_opcode.0 as usize)];
                        b.func.dfg.block_call(target_block, &[])
                    })
                    .collect();
                let table_data = JumpTableData::new(default, &jumps[..]);
                let table = b.create_jump_table(table_data);
                b.ins().br_table(pred, table);
            }
            Opcode::Nop => (),
        }
    }
}

// #[derive(Default)]
// struct SSAValues {
//     values: HashMap<SSAIndex, Value>,
// }

// impl SSAValues {
//     pub fn get(&self, index: SSAIndex) -> Option<Value> {
//         self.values.get(&index).copied()
//     }

//     pub fn add(&mut self, index: SSAIndex, value: Value) {
//         assert_eq!(self.values.insert(index, value), None);
//     }

//     pub fn clear(&mut self) {
//         self.values.clear();
//     }
// }

#[derive(Default)]
struct SSAValues {
    values: HashMap<usize, Variable>,
    counter: usize,
}

impl SSAValues {
    pub fn get(&self, index: SSAIndex, b: &mut FunctionBuilder) -> Option<Value> {
        self.values
            .get(&(index.0 as usize))
            .map(|&var| b.use_var(var))
    }

    pub fn set(&self, index: SSAIndex, b: &mut FunctionBuilder, value: Value) {
        if let Some(&var) = self.values.get(&(index.0 as usize)) {
            b.def_var(var, value)
        }
    }

    pub fn add(&mut self, b: &mut FunctionBuilder, index: usize, ty: Type) -> Variable {
        let next = self.next_var();
        assert_eq!(self.values.insert(index, next), None);
        b.declare_var(next, ty);
        next
    }

    fn next_var(&mut self) -> Variable {
        let var = Variable::new(self.counter);
        self.counter += 1;
        var
    }
}

#[derive(Clone, Debug)]
struct BasicBlock {
    bounds: (usize, usize),
}

#[derive(Clone, Debug, Default)]
pub struct BasicBlocks(Vec<BasicBlock>);

impl BasicBlocks {
    fn new(ops: &[Opcode], labels: &[SSAIndex], label_pool: &[LabelIndex]) -> Self {
        let starts = Self::starts(ops, labels, label_pool);
        let mut blocks = BasicBlocks::default();
        for start in starts.iter().map(|start| start.0 as usize) {
            let mut cursor = start + 1;
            loop {
                match ops.get(cursor - 1) {
                    Some(op) => match op {
                        Opcode::Jump(_) => {
                            blocks.0.push(BasicBlock {
                                bounds: (start, cursor),
                            });
                            break;
                        }
                        Opcode::Branch { .. } => {
                            blocks.0.push(BasicBlock {
                                bounds: (start, cursor),
                            });
                            break;
                        }
                        &Opcode::Return(i) => {
                            i.check(start..cursor, &mut args);
                            blocks.0.push(BasicBlock {
                                bounds: (start, cursor),
                            });
                            break;
                        }
                        &Opcode::Add(l, r) | &Opcode::Mult(l, r) => {
                            l.check(start..cursor, &mut args);
                            r.check(start..cursor, &mut args);
                        }
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
                        | Opcode::Nop => (),
                    },
                    None => {
                        // TODO: Return an error for not ending function properly
                        blocks.0.push(BasicBlock {
                            bounds: (start, cursor - 1),
                        });
                        break;
                    }
                }
                cursor += 1;
            }
        }
        blocks
    }

    fn starts(ops: &[Opcode], labels: &[SSAIndex], label_pool: &[LabelIndex]) -> HashSet<SSAIndex> {
        let mut starts = HashSet::from_iter([SSAIndex(0)]);
        for op in ops {
            match op {
                Opcode::Jump(target) => {
                    let target = labels[target.0 as usize];
                    starts.insert(target);
                }
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
                | Opcode::Add(_, _)
                | Opcode::Mult(_, _)
                | Opcode::Return(_)
                | Opcode::Nop => continue,
                Opcode::Branch {
                    default, targets, ..
                } => {
                    starts.insert(labels[default.0 as usize]);
                    for &label in targets.slice(label_pool) {
                        starts.insert(labels[label.0 as usize]);
                    }
                }
            }
        }
        starts
    }

    fn block_map(&self, b: &mut FunctionBuilder) -> HashMap<usize, Block> {
        self.0
            .iter()
            .map(
                |BasicBlock {
                     bounds: (start, _), ..
                 }| (*start, b.create_block()),
            )
            .collect()
    }
}

fn ssa_index_type(ops: &[Opcode], value: SSAIndex) -> Option<Type> {
    Some(match ops[value.0 as usize] {
        Opcode::LiteralS1(_) | Opcode::LiteralU1(_) => types::I8,
        Opcode::LiteralS2(_) | Opcode::LiteralU2(_) => types::I16,
        Opcode::LiteralS4(_) | Opcode::LiteralU4(_) => types::I32,
        Opcode::LiteralS8(_) | Opcode::LiteralU8(_) => types::I64,
        Opcode::LiteralF4(_) => types::F32,
        Opcode::LiteralF8(_) => types::F64,
        Opcode::Add(l, r) | Opcode::Mult(l, r) => {
            // TODO: Good errors
            let l = ssa_index_type(ops, l).unwrap();
            let r = ssa_index_type(ops, r).unwrap();
            arithmetic_result_type(l, r)
        }
        Opcode::Return(_) | Opcode::Jump(_) | Opcode::Branch { .. } | Opcode::Nop => return None,
    })
}

fn arithmetic_result_type(l: Type, r: Type) -> Type {
    if ((l.is_int() && r.is_int()) | (l.is_float() && r.is_float())) && l.bits() == r.bits() {
        l
    } else {
        // TODO: Better errors
        panic!("attempted to add a float to an int and/or differently sized numerics")
    }
}
