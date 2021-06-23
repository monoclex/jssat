use rustc_hash::FxHashMap;

use crate::frontend::ir::*;
use crate::id::*;
use crate::name::DebugName;

pub fn annotate(ir: &IR) -> TypeAnnotations {
    // TODO: i cannot build rome in a day. type inference of control flow
    // is very complicatetd.
    let entrypoint = ir
        .functions
        .get(&ir.entrypoint)
        .expect("expected entrypoint");

    debug_assert_eq!(
        entrypoint.parameters.len(),
        0,
        "entrypoint shall have no parameters"
    );

    if entrypoint.blocks.len() > 1 {
        todo!("control flow isn't supported atm");
    }

    let (_, block) = entrypoint.blocks.iter().next().unwrap();

    let entrypoint = FunctionId::new();
    let mut type_mapping = FxHashMap::<FunctionId, TypedFunction>::default();

    for instruction in block.instructions.iter() {
        match instruction {
            Instruction::RecordGet(_, _, _) => todo!("RecordGet"),
            Instruction::RecordSet(_, _, _) => todo!("RecordSet"),
            Instruction::RefIsEmpty(_, _) => todo!("RefIsEmpty"),
            Instruction::RefDeref(_, _) => todo!("RefDeref"),
            Instruction::MakePrimitive(_, _) => todo!("MakePrimitive"),
            Instruction::Call(_, _, _) => todo!("Call"),
            Instruction::Phi(_, _) => todo!("Phi"),
        }
    }

    // for (fn_id, function) in ir.functions.iter() {
    //     if function.blocks.len() > 1 {
    //         todo!("control flow isn't supported atm");
    //     }

    //     for (block_id, block) in function.blocks.iter() {
    //         for instruction in block.instructions.iter() {
    //             match instruction {}
    //         }
    //     }
    // }

    todo!()
}

#[derive(Debug)]
pub struct TypeAnnotations {
    pub entrypoint: FunctionId,
    pub functions: FxHashMap<FunctionId, TypedFunction>,
}

#[derive(Debug)]
pub struct TypedFunction {
    pub name: DebugName,
    pub parameters: Vec<Parameter>,
    pub return_type: ReturnType,
    // pub control_flow: ControlFlowGraph,
    // pub register_flow: ValueFlowGraph,
    pub entry_block: BlockId,
    pub blocks: FxHashMap<BlockId, FunctionBlock>,
    pub register_types: FxHashMap<RegisterId, ValueType>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: DebugName,
    pub register: RegisterId,
    pub r#type: ValueType,
}

#[derive(Debug)]
pub enum ReturnType {
    Void,
    Value(ValueType),
}

#[derive(Debug)]
pub enum ValueType {
    Any,
    Runtime,
}

impl TypeAnnotations {
    // pub fn
}
