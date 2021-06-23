use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, GlobalValue},
};

pub use super::ir as s;
pub use crate::backend::llvm::ir as l;
use crate::id::*;

pub struct GlueState<'ctx> {
    constants: HashMap<TopLevelId, GlobalValue<'ctx>>,
    functions: HashMap<TopLevelId, FunctionState<'ctx>>,
}

pub struct FunctionState<'ctx> {
    llvm: FunctionValue<'ctx>,
    blocks: HashMap<BlockId, BasicBlock<'ctx>>,
}

// TODO: cleanup this bad glue code. it didn't take any time at all to write,
// and the skeleton and llvm structures are very similar so i figured that
// this would be a really easy piece of code to rewrite in isolation.
pub fn glue<'c>(ir: s::IR, mut glue: GlueState<'c>) -> l::IR<'c> {
    l::IR {
        constants: ir
            .constants
            .into_iter()
            .map(|(k, c)| {
                (
                    k,
                    l::Constant {
                        llvm: glue.constants.remove(&k).unwrap(),
                        payload: c.payload,
                        name: c.name,
                    },
                )
            })
            .collect::<HashMap<_, _>>(),
        functions: ir
            .functions
            .into_iter()
            .map(|(k, f)| {
                (k, {
                    let mut glue = glue.functions.remove(&k).unwrap();

                    l::Function {
                        llvm: glue.llvm,
                        name: f.name,
                        parameter_types: f.parameter_types,
                        return_type: match f.return_type {
                            s::PossibleType::Void => l::PossibleType::Void,
                            s::PossibleType::Value(v) => l::PossibleType::Value(v),
                        },
                        body: f.body.map(|b| l::FunctionBody {
                            parameter_registers: b.parameter_registers,
                            entry_block: b.entry_block,
                            register_types: b.register_types,
                            body: b
                                .body
                                .into_iter()
                                .map(|(k, b)| {
                                    (
                                        k,
                                        l::Block {
                                            llvm_block: glue.blocks.remove(&k).unwrap(),
                                            end_flow: match b.end_flow {
                                                s::InstructionFlow::Phi(a, b) => {
                                                    l::InstructionFlow::Phi(
                                                        a,
                                                        b.into_iter()
                                                            .map(|implication| {
                                                                l::BlockImpliesRegister {
                                                                    block: implication.block,
                                                                    implies: implication.implies,
                                                                }
                                                            })
                                                            .collect::<Vec<_>>(),
                                                    )
                                                }
                                                s::InstructionFlow::Jmp(a) => {
                                                    l::InstructionFlow::Jmp(a)
                                                }
                                                s::InstructionFlow::JmpIf(a, b) => {
                                                    l::InstructionFlow::JmpIf(
                                                        l::BlockImpliesRegister {
                                                            block: a.block,
                                                            implies: a.implies,
                                                        },
                                                        b,
                                                    )
                                                }
                                                s::InstructionFlow::Ret(a) => {
                                                    l::InstructionFlow::Ret(a)
                                                }
                                            },
                                            instructions: b
                                                .instructions
                                                .into_iter()
                                                .map(|v| match v {
                                                    s::Instruction::LoadGlobal(a, b) => {
                                                        l::Instruction::LoadGlobal(a, b)
                                                    }
                                                    s::Instruction::SaveGlobal(a, b) => {
                                                        l::Instruction::SaveGlobal(a, b)
                                                    }
                                                    s::Instruction::RecordGet(a, b, c) => {
                                                        l::Instruction::RecordGet(
                                                            a,
                                                            b,
                                                            match c {
                                                                s::RecordKey::InternalSlot(a) => {
                                                                    l::RecordKey::InternalSlot(a)
                                                                }
                                                                s::RecordKey::Register(a) => {
                                                                    l::RecordKey::Register(a)
                                                                }
                                                            },
                                                        )
                                                    }
                                                    s::Instruction::RecordSet(a, b, c) => {
                                                        l::Instruction::RecordSet(
                                                            a,
                                                            match b {
                                                                s::RecordKey::InternalSlot(a) => {
                                                                    l::RecordKey::InternalSlot(a)
                                                                }
                                                                s::RecordKey::Register(a) => {
                                                                    l::RecordKey::Register(a)
                                                                }
                                                            },
                                                            match c {
                                                                s::Value::Runtime => l::Value::Runtime,
                                                                s::Value::Register(a) => {
                                                                    l::Value::Register(a)
                                                                }
                                                                s::Value::Constant(a) => {
                                                                    l::Value::Constant(a)
                                                                }
                                                                s::Value::Number(a) => {
                                                                    l::Value::Number(a)
                                                                }
                                                            },
                                                        )
                                                    }
                                                    s::Instruction::RefIsEmpty(a, b) => {
                                                        l::Instruction::RefIsEmpty(a, b)
                                                    }
                                                    s::Instruction::RefDeref(a, b) => {
                                                        l::Instruction::RefDeref(a, b)
                                                    }
                                                    s::Instruction::MakePrimitive {
                                                        result,
                                                        strategy,
                                                        primitive_kind,
                                                    } => l::Instruction::MakePrimitive {
                                                        result,
                                                        strategy: match strategy {
                                                            s::GarbageCollectionStrategy::Tracing => l::GarbageCollectionStrategy::Tracing,
                                                        },
                                                        primitive_kind: match primitive_kind {
                                                            s::PrimitiveKind::Record => l::PrimitiveKind::Record,
                                                            s::PrimitiveKind::List => l::PrimitiveKind::List,
                                                        },
                                                    },
                                                    s::Instruction::GcTracingUnmarkRoot(a) => {
                                                        l::Instruction::GcTracingUnmarkRoot(a)
                                                    }
                                                    s::Instruction::Call(a, b, c) => {
                                                        l::Instruction::Call(a, match b {
                                                            s::Callable::GlobalFunction(a) => l::Callable::GlobalFunction(a),
                                                            s::Callable::LocalFunction(a) => l::Callable::LocalFunction(a),
                                                        }, c.into_iter().map(|v| match v {
                                                            s::Value::Runtime => l::Value::Runtime,
                                                            s::Value::Register(a) => {
                                                                l::Value::Register(a)
                                                            }
                                                            s::Value::Constant(a) => {
                                                                l::Value::Constant(a)
                                                            }
                                                            s::Value::Number(a) => {
                                                                l::Value::Number(a)
                                                            }
                                                        }).collect::<Vec<_>>())
                                                    }
                                                })
                                                .collect::<Vec<_>>(),
                                        },
                                    )
                                })
                                .collect::<HashMap<_, _>>(),
                        }),
                    }
                })
            })
            .collect::<HashMap<_, _>>(),
    }
}
