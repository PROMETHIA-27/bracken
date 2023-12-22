use std::collections::{HashMap, HashSet};

use bincode::{DefaultOptions, Options};
use bracken::bytecode::{self, Function, LabelIndex, Opcode, OpcodeIndex};
use cm::Module;
use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::{
    types, AbiParam, Block, BlockCall, Function as CFunction, InstBuilder, JumpTableData, Type,
    Value,
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

fn compile_func(func: &Function, jit: &mut Jit) {
    let mut sig = jit.module.make_signature();
    sig.returns = vec![AbiParam::new(types::I32)];
    let func_id = jit
        .module
        .declare_function(&func.name, cranelift_module::Linkage::Export, &sig)
        .unwrap();
    let mut cfunc = CFunction::new();
    cfunc.signature = sig;

    let mut fnctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut cfunc, &mut fnctx);

    for (i, &local) in func.locals.iter().enumerate() {
        b.declare_var(Variable::new(i), local.clif_type().unwrap());
    }

    let blocks = BasicBlocks::new(func.ops(), func);
    println!("{blocks:?}");
    let block_map = blocks.block_map(&mut b);
    let blocks = blocks.0.clone().into_iter().map(
        |BasicBlock {
             bounds: (start, end),
             params,
         }| (start, params, &func.ops()[start..end]),
    );
    let mut stack = vec![];
    for (start, params, block) in blocks {
        let (cblock, _) = *block_map.get(&OpcodeIndex::new(start)).unwrap();
        b.switch_to_block(cblock);
        if start == 0 {
            b.append_block_params_for_function_params(cblock);
            if let Some(false_entry) = b.func.layout.entry_block() {
                b.func.layout.insert_block(cblock, false_entry);
            }
        }
        for param in &params {
            let value = b.append_block_param(cblock, *param);
            stack.push(value);
        }
        compile_block(
            block,
            &func.labels,
            &func.label_pool,
            &block_map,
            &mut b,
            &mut stack,
        );
        stack.clear();
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
    labels: &[OpcodeIndex],
    label_pool: &[LabelIndex],
    block_map: &HashMap<OpcodeIndex, (Block, &[Type])>,
    b: &mut FunctionBuilder,
    stack: &mut Vec<Value>,
) {
    for &op in ops.iter() {
        match op {
            Opcode::LiteralS1(_) => todo!(),
            Opcode::LiteralS2(_) => todo!(),
            Opcode::LiteralS4(c) => {
                let value = b.ins().iconst(types::I32, c as i64);
                stack.push(value);
            }
            Opcode::LiteralS8(_) => todo!(),
            Opcode::LiteralU1(_) => todo!(),
            Opcode::LiteralU2(_) => todo!(),
            Opcode::LiteralU4(_) => todo!(),
            Opcode::LiteralU8(_) => todo!(),
            Opcode::LiteralF4(c) => {
                let value = b.ins().f32const(c.0);
                stack.push(value);
            }
            Opcode::LiteralF8(_) => todo!(),
            Opcode::Add | Opcode::Sub | Opcode::Mult => {
                let rhs = stack.pop().unwrap();
                let lhs = stack.pop().unwrap();
                let lhs_ty = b.func.dfg.value_type(lhs);
                let rhs_ty = b.func.dfg.value_type(rhs);
                if lhs_ty.is_int() && rhs_ty.is_int() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = match op {
                        Opcode::Add => b.ins().iadd(lhs, rhs),
                        Opcode::Sub => b.ins().isub(lhs, rhs),
                        Opcode::Mult => b.ins().imul(lhs, rhs),
                        _ => unreachable!(),
                    };
                    stack.push(value);
                } else if lhs_ty.is_float() && rhs_ty.is_float() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = match op {
                        Opcode::Add => b.ins().fadd(lhs, rhs),
                        Opcode::Sub => b.ins().fsub(lhs, rhs),
                        Opcode::Mult => b.ins().fmul(lhs, rhs),
                        _ => unreachable!(),
                    };
                    stack.push(value);
                } else {
                    panic!("attempted to add a float to an int and/or differently sized numerics")
                }
            }
            Opcode::Return => {
                let value = stack.pop().unwrap();
                b.ins().return_(&[value]);
            }
            Opcode::Jump(target) => {
                let target_opcode = labels[target.index()];
                let (target_block, block_args) = block_map[&target_opcode];
                let offset = stack.len() - block_args.len();
                b.ins().jump(target_block, &stack[offset..]);
            }
            Opcode::Branch { default, targets } => {
                let pred = stack.pop().unwrap();
                let default_opcode = labels[default.index()];
                let (default_block, default_block_args) = block_map[&default_opcode];
                let offset = stack.len() - default_block_args.len();
                let default = b.func.dfg.block_call(default_block, &stack[offset..]);
                let jumps: Vec<BlockCall> = targets
                    .slice(label_pool)
                    .iter()
                    .map(|&label| {
                        let target_opcode = labels[label.index()];
                        let (target_block, target_block_args) = block_map[&target_opcode];
                        let offset = stack.len() - target_block_args.len();
                        b.func.dfg.block_call(target_block, &stack[offset..])
                    })
                    .collect();
                let table_data = JumpTableData::new(default, &jumps[..]);
                let table = b.create_jump_table(table_data);
                b.ins().br_table(pred, table);
            }
            Opcode::StoreLocal(local) => {
                let value = stack.pop().unwrap();
                b.def_var(Variable::from_u32(local), value);
            }
            Opcode::LoadLocal(local) => {
                stack.push(b.use_var(Variable::from_u32(local)));
            }
            Opcode::Nop => (),
        }
    }
}

#[derive(Clone, Debug)]
struct BasicBlock {
    bounds: (usize, usize),
    params: Vec<Type>,
}

#[derive(Clone, Debug, Default)]
pub struct BasicBlocks(Vec<BasicBlock>);

impl BasicBlocks {
    fn new(ops: &[Opcode], func: &Function) -> Self {
        let starts = Self::starts(ops, &func.labels, &func.label_pool);
        let mut blocks = BasicBlocks::default();
        for start in starts.iter().map(|start| start.index()) {
            let mut cursor = start + 1;
            loop {
                match ops.get(cursor - 1) {
                    Some(op) => match op {
                        Opcode::Jump(_) | Opcode::Branch { .. } | Opcode::Return => {
                            blocks.0.push(BasicBlock {
                                bounds: (start, cursor),
                                params: compute_block_params(&ops[start..cursor], func),
                            });
                            break;
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
                        | Opcode::Add
                        | Opcode::Sub
                        | Opcode::Mult
                        | Opcode::StoreLocal(_)
                        | Opcode::LoadLocal(_)
                        | Opcode::Nop => (),
                    },
                    None => {
                        // TODO: Return an error for not ending function properly
                        panic!("ended function without diverging opcode");
                    }
                }
                cursor += 1;
            }
        }
        blocks
    }

    fn starts(
        ops: &[Opcode],
        labels: &[OpcodeIndex],
        label_pool: &[LabelIndex],
    ) -> HashSet<OpcodeIndex> {
        let mut starts = HashSet::from_iter([OpcodeIndex::new(0)]);
        for op in ops {
            match op {
                Opcode::Jump(target) => {
                    let target = labels[target.index()];
                    starts.insert(target);
                }
                Opcode::Branch {
                    default, targets, ..
                } => {
                    starts.insert(labels[default.index()]);
                    for &label in targets.slice(label_pool) {
                        starts.insert(labels[label.index()]);
                    }
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
                | Opcode::Add
                | Opcode::Sub
                | Opcode::Mult
                | Opcode::Return
                | Opcode::StoreLocal(_)
                | Opcode::LoadLocal(_)
                | Opcode::Nop => continue,
            }
        }
        starts
    }

    fn block_map(&self, b: &mut FunctionBuilder) -> HashMap<OpcodeIndex, (Block, &[Type])> {
        self.0
            .iter()
            .map(
                |BasicBlock {
                     bounds: (start, _),
                     params,
                 }| (OpcodeIndex::new(*start), (b.create_block(), &params[..])),
            )
            .collect()
    }
}

fn compute_block_params(block: &[Opcode], func: &Function) -> Vec<Type> {
    let mut inputs = vec![];
    let mut stack = vec![];

    for op in block {
        match op {
            Opcode::LiteralS1(_) | Opcode::LiteralU1(_) => stack.push(types::I8),
            Opcode::LiteralS2(_) | Opcode::LiteralU2(_) => stack.push(types::I16),
            Opcode::LiteralS4(_) | Opcode::LiteralU4(_) => stack.push(types::I32),
            Opcode::LiteralS8(_) | Opcode::LiteralU8(_) => stack.push(types::I64),
            Opcode::LiteralF4(_) => stack.push(types::F32),
            Opcode::LiteralF8(_) => stack.push(types::F64),
            Opcode::Add | Opcode::Sub | Opcode::Mult => {
                let rhs = stack.pop().unwrap_or_else(|| {
                    // TODO: Remove hardcoded generic instruction types
                    inputs.push(types::I32);
                    types::I32
                });
                let lhs = stack.pop().unwrap_or_else(|| {
                    inputs.push(rhs);
                    rhs
                });
                if lhs != rhs {
                    panic!();
                }
                stack.push(lhs);
            }
            Opcode::Return => {
                stack.pop().unwrap_or_else(|| {
                    inputs.push(types::I32);
                    types::I32
                });
            }
            &Opcode::StoreLocal(local) => {
                let ty = func.local(local).clif_type().unwrap();
                assert_eq!(
                    stack.pop().unwrap_or_else(|| {
                        // TODO: Type checking
                        inputs.push(ty);
                        ty
                    }),
                    ty
                );
            }
            &Opcode::LoadLocal(local) => {
                let ty = func.local(local);
                stack.push(ty.clif_type().unwrap());
            }
            Opcode::Jump(_) | Opcode::Branch { .. } | Opcode::Nop => (),
        }
    }

    inputs.reverse();
    inputs
}
