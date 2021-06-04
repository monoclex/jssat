use std::collections::{HashMap, HashSet};

use super::ir::{Type, TypeAnnotations};
use crate::backend::skeleton::ir::{self as s, TypeManager, ValueType};
use crate::frontend::js::ir as j;
use crate::id::*;

// TODO: actually generate monomorphized versions of methods and such. this
// would involve generating code too, so we might have to output skeleton IR
// during this phase.
//
// monomorphizing would be decently simple:
// 1. for every function (very parallel), store:
//    - list of functions called
//    - along with type of each parameter (with full ctx info)
// 2. schedule `main` to be monomorphed.
//    while there are functions to be monomorphed, iterate over them:
//    - if the current function has already been monomorphed, skip
//    - for every function call, scheudule that function to be monomorphed with
//      the param types being called
//    - replace the caller's `call` with a call to the specific monomorphed function
//
// or something like that.
pub fn monomorphize(ir: &j::IR, type_annotations: &TypeAnnotations) -> (s::IR, s::TypeManager) {
    let mut free_id = TopLevelId::new();
    let mut constants = HashMap::new();
    let mut functions = HashMap::new();
    let mut entry_function = None;

    let mut monomorphizer = TypeManagerBuilder::new();

    for (id, constant) in ir.constants.iter() {
        constants.insert(
            *id,
            s::Constant {
                name: ir.debug_info.top_level_names.get(id).map(|b| b.clone()),
                // TODO: don't clone, definitely expensive here
                payload: constant.payload.clone(),
            },
        );
    }

    // TODO: consolidate external functions and IR functions somehow?
    for (id, ext_function) in ir.external_functions.iter() {
        let context = type_annotations.functions.get(id).expect("fn context");

        let name = ir.debug_info.top_level_names.get(id).map(|b| b.clone());

        let parameter_types = (ext_function.parameters.iter())
            .map(|p| &p.kind)
            .map(|t| monomorphizer.cache(map_type(t)))
            .collect::<Vec<_>>();

        let return_type = match context.types.get(&context.return_type) {
            Type::Void => s::PossibleType::Void,
            // TODO: there's got to be a better, more well thought out way to do this
            other => s::PossibleType::Value(monomorphizer.cache(monomorph(other))),
        };

        functions.insert(
            free_id,
            s::Function {
                name,
                parameter_types,
                return_type,
                body: None,
            },
        );
    }

    for (id, function) in ir.functions.iter() {
        if function.is_main {
            if entry_function.is_some() {
                panic!("main already exists");
            }

            entry_function = Some(*id);
        }

        let context = type_annotations.functions.get(id).expect("fn context");

        let name = ir.debug_info.top_level_names.get(id).map(|b| b.clone());

        let parameter_types = (function.parameters.iter())
            .map(|p| p.register)
            .map(|r| context.registers.get(&r).unwrap())
            .map(|t| context.types.get(t))
            .map(|t| monomorphizer.cache(monomorph(t)))
            .collect::<Vec<_>>();

        let return_type = match context.types.get(&context.return_type) {
            Type::Void => s::PossibleType::Void,
            // TODO: there's got to be a better, more well thought out way to do this
            other => s::PossibleType::Value(monomorphizer.cache(monomorph(other))),
        };

        let body = Some({
            let mut register_types = HashMap::new();
            let mut parameter_registers = Vec::new();
            let mut body = HashMap::new();
            let entry_block = function.body.blocks.iter().next().expect("a block").id;

            for (register_id, type_id) in context.registers.iter() {
                register_types.insert(
                    *register_id,
                    monomorphizer.cache(monomorph(context.types.get(type_id))),
                );
            }

            for p in function.parameters.iter() {
                parameter_registers.push(p.register);
            }

            // TODO: the high level JS AST should emit a similar structure rather than us manhandling it here
            for block in function.body.blocks.iter() {
                let mut block_inst = Vec::new();

                let (last, instructions) = block.instructions.split_last().expect("instructions");
                for inst in instructions.iter() {
                    match inst {
                        j::Instruction::LoadGlobal(_, _) => todo!("LoadGlobal"),
                        j::Instruction::SaveGlobal(_, _) => todo!("SaveGlobal"),
                        j::Instruction::RecordGet(_, _, _) => todo!("RecordGet"),
                        j::Instruction::RecordSet(_, _, _) => todo!("RecordSet"),
                        j::Instruction::RefIsEmpty(_, _) => todo!("RefIsEmpty"),
                        j::Instruction::RefDeref(_, _) => todo!("RefDeref"),
                        j::Instruction::MakePrimitive(_, _) => todo!("MakePrimitive"),
                        j::Instruction::GcMakeRegion(_) => todo!("GcMakeRegion"),
                        j::Instruction::GcEndRegion(_) => todo!("GcEndRegion"),
                        j::Instruction::GcTracingMarkRoot(_) => todo!("GcTracingMarkRoot"),
                        j::Instruction::GcTracingUnmarkRoot(_) => todo!("GcTracingUnmarkRoot"),
                        // TODO: don't duplicate `match` work so much
                        j::Instruction::Call(a, b, c) => block_inst.push(s::Instruction::Call(
                            *a,
                            match b {
                                j::Callable::GlobalFunction(a) => s::Callable::GlobalFunction(*a),
                                j::Callable::LocalFunction(a) => s::Callable::LocalFunction(*a),
                            },
                            c.iter()
                                .map(|v| match v {
                                    j::Value::Register(r) => s::Value::Register(*r),
                                    j::Value::Constant(c) => s::Value::Constant(*c),
                                    j::Value::Number(n) => s::Value::Number(*n),
                                })
                                .collect::<Vec<_>>(),
                        )),
                        j::Instruction::Phi(_, _)
                        | j::Instruction::Jmp(_)
                        | j::Instruction::JmpIf(_, _)
                        | j::Instruction::Ret(_) => {
                            panic!("control flow should be at end of block")
                        }
                    }
                }

                let end_flow = match last {
                    // TODO: put mapping from block implies into function OR make 'em the same thing
                    j::Instruction::Phi(a, b) => s::InstructionFlow::Phi(
                        *a,
                        b.iter()
                            .map(|implies| s::BlockImpliesRegister {
                                block: implies.block,
                                implies: implies.implies,
                            })
                            .collect::<Vec<_>>(),
                    ),
                    j::Instruction::Jmp(a) => s::InstructionFlow::Jmp(*a),
                    j::Instruction::JmpIf(a, b) => s::InstructionFlow::JmpIf(
                        s::BlockImpliesRegister {
                            block: a.block,
                            implies: a.implies,
                        },
                        *b,
                    ),
                    j::Instruction::Ret(a) => s::InstructionFlow::Ret(*a),
                    j::Instruction::LoadGlobal(_, _)
                    | j::Instruction::SaveGlobal(_, _)
                    | j::Instruction::RecordGet(_, _, _)
                    | j::Instruction::RecordSet(_, _, _)
                    | j::Instruction::RefIsEmpty(_, _)
                    | j::Instruction::RefDeref(_, _)
                    | j::Instruction::MakePrimitive(_, _)
                    | j::Instruction::GcMakeRegion(_)
                    | j::Instruction::GcEndRegion(_)
                    | j::Instruction::GcTracingMarkRoot(_)
                    | j::Instruction::GcTracingUnmarkRoot(_)
                    | j::Instruction::Call(_, _, _) => panic!("last instruction not control flow"),
                };

                body.insert(
                    block.id,
                    s::Block {
                        instructions: block_inst,
                        end_flow,
                    },
                );
            }

            s::FunctionBody {
                register_types,
                parameter_registers,
                entry_block,
                body,
            }
        });

        functions.insert(
            free_id,
            s::Function {
                name,
                parameter_types,
                return_type,
                body,
            },
        );
    }

    (
        s::IR {
            constants,
            functions,
            entry_function: entry_function.expect("generated IR must have a main"),
        },
        monomorphizer.build(),
    )
}

fn monomorph(t: &Type) -> ValueType {
    match t {
        Type::Any => ValueType::Any,
        Type::Void => panic!("cannot monomorphize void atm"),
    }
}

fn map_type(t: &crate::frontend::js::ir::Type) -> ValueType {
    match t {
        j::Type::Any => ValueType::Any,
        j::Type::Runtime => ValueType::Runtime,
        j::Type::Void => panic!("void not handled atm"),
        j::Type::Constant(_) => panic!("coinstant not handled atm"),
    }
}

struct TypeManagerBuilder {
    rev_mappings: HashMap<ValueType, TypeId>,
    mappings: HashMap<TypeId, ValueType>,
    free_id: TypeId,
    monomorphized_types: HashSet<ValueType>,
}

impl TypeManagerBuilder {
    fn new() -> Self {
        Self {
            rev_mappings: HashMap::new(),
            mappings: HashMap::new(),
            free_id: TypeId::new(),
            monomorphized_types: HashSet::new(),
        }
    }

    fn cache(&mut self, value: ValueType) -> TypeId {
        if self.monomorphized_types.insert(value.clone()) {
            let result = self.free_id;
            self.free_id = self.free_id.next();
            self.mappings.insert(result, value.clone());
            self.rev_mappings.insert(value, result);
            result
        } else {
            *self.rev_mappings.get(&value).unwrap()
        }
    }

    fn build(self) -> s::TypeManager {
        s::TypeManager {
            types: self.mappings,
        }
    }
}
