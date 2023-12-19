use bincode::{DefaultOptions, Options};
use bracken::bytecode::{self, BeltOffset, Func, Opcode};
use cm::Module;
use cranelift_codegen::ir::{types, AbiParam, Function, InstBuilder, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
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
    let main_id = jit
        .module
        .declare_function(&func.name, cranelift_module::Linkage::Export, &sig)
        .unwrap();
    let mut main = Function::new();
    main.signature = sig;

    let mut fnctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut main, &mut fnctx);

    let block = b.create_block();
    b.seal_block(block);
    b.append_block_params_for_function_params(block);
    b.switch_to_block(block);

    compile_bytecode(&func.opcodes, &mut b, &mut Belt::default());

    b.finalize();

    let mut ctx = jit.module.make_context();
    ctx.func = main;
    jit.module.define_function(main_id, &mut ctx).unwrap();

    jit.module.finalize_definitions().unwrap();

    let func = jit.module.get_finalized_function(main_id);
    let x = unsafe { std::mem::transmute::<_, extern "C" fn() -> i32>(func) }();
    println!("{x}");
}

fn compile_bytecode(ops: &[Opcode], b: &mut FunctionBuilder, belt: &mut Belt) {
    for &op in ops {
        match op {
            Opcode::LiteralS1(_) => todo!(),
            Opcode::LiteralS2(_) => todo!(),
            Opcode::LiteralS4(c) => {
                let value = b.ins().iconst(types::I32, c as i64);
                belt.push(value);
            }
            Opcode::LiteralS8(_) => todo!(),
            Opcode::LiteralU1(_) => todo!(),
            Opcode::LiteralU2(_) => todo!(),
            Opcode::LiteralU4(_) => todo!(),
            Opcode::LiteralU8(_) => todo!(),
            Opcode::LiteralF4(c) => {
                let value = b.ins().f32const(c.0);
                belt.push(value);
            }
            Opcode::LiteralF8(_) => todo!(),
            Opcode::Add(lhs, rhs) => {
                let lhs = belt.get(lhs).unwrap();
                let rhs = belt.get(rhs).unwrap();
                let lhs_ty = b.func.dfg.value_type(lhs);
                let rhs_ty = b.func.dfg.value_type(rhs);
                if lhs_ty.is_int() && rhs_ty.is_int() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = b.ins().iadd(lhs, rhs);
                    belt.push(value);
                } else if lhs_ty.is_float() && rhs_ty.is_float() && lhs_ty.bits() == rhs_ty.bits() {
                    let value = b.ins().fadd(lhs, rhs);
                    belt.push(value);
                } else {
                    panic!("attempted to add a float to an int and/or differently sized numerics")
                }
            }
            Opcode::Ret(offset) => {
                let value = belt.get(offset).unwrap();
                b.ins().return_(&[value]);
            }
            Opcode::Nop => (),
        }
    }
}

#[derive(Default)]
struct Belt {
    values: Vec<Value>,
}

impl Belt {
    /// Get the value at the given offset from the end of the belt
    pub fn get(&self, offset: BeltOffset) -> Option<Value> {
        let index = self.values.len().checked_sub(1 + offset.0 as usize)?;
        Some(*self.values.get(index).unwrap())
    }

    pub fn push(&mut self, value: Value) {
        self.values.push(value);
    }
}
