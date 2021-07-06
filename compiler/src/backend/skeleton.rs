use rustc_hash::{FxHashMap, FxHashSet};

use crate::backend::llvm::{
    self, BackendIR, Callable, Constant, ExternalFunction, Function, LLVMLinkage, NumberValue,
    OpaqueStruct, Parameter,
};

use crate::frontend::assembler::{self, Program, ReturnType, Type};
use crate::frontend::ir::{self};
use crate::frontend::type_annotater;
use crate::{id::*, UnwrapNone};

use super::llvm::ValueType;

struct ConstantMapper<'constants, 'name> {
    constants: &'constants FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
    constants_llvm: FxHashMap<ConstantId<LlvmCtx>, Constant<'name>>,
}

impl<'c, 'n> ConstantMapper<'c, 'n> {
    pub fn new(constants: &'c FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>) -> Self {
        Self {
            constants,
            constants_llvm: Default::default(),
        }
    }

    pub fn map_all(&mut self) {
        for (key, payload) in self.constants.iter() {
            self.map_constant(*key, payload.clone());
        }
    }

    fn map_constant(
        &mut self,
        id: ConstantId<AssemblerCtx>,
        payload: Vec<u8>,
    ) -> ConstantId<LlvmCtx> {
        let llvm_id = id.map_context();

        self.constants_llvm
            .insert(llvm_id, Constant { name: "", payload })
            .expect_free();

        llvm_id
    }

    pub fn map_existing(&self, id: ConstantId<AssemblerCtx>) -> ConstantId<LlvmCtx> {
        match self.try_map_existing(id) {
            Some(id) => id,
            None => panic!("contract error: `map_existing` called when fallible"),
        }
    }

    fn try_map_existing(&self, id: ConstantId<AssemblerCtx>) -> Option<ConstantId<LlvmCtx>> {
        let id = id.map_context();
        self.constants_llvm.contains_key(&id).then_some(id)
    }

    pub fn const_len(&self, id: ConstantId<LlvmCtx>) -> usize {
        self.constants_llvm
            .get(&id)
            .expect("should've gotten constant")
            .payload
            .len()
    }

    pub fn into_llvm_constants(self) -> FxHashMap<ConstantId<LlvmCtx>, Constant<'n>> {
        self.constants_llvm
    }
}

struct RuntimeTypes {
    opaque_structs: FxHashMap<OpaqueStructId<LlvmCtx>, OpaqueStruct<'static>>,
    counter: Counter<OpaqueStructId<LlvmCtx>>,
    pub runtime: OpaqueStructId<LlvmCtx>,
    // pub any: OpaqueStructId<LlvmCtx>, --> this is Value right
    pub value: OpaqueStructId<LlvmCtx>,
    pub string: OpaqueStructId<LlvmCtx>,
}

impl RuntimeTypes {
    pub fn new() -> Self {
        let mut me = Self {
            opaque_structs: Default::default(),
            counter: Default::default(),
            runtime: OpaqueStructId::new(),
            value: OpaqueStructId::new(),
            string: OpaqueStructId::new(),
        };
        me.init();
        me
    }

    fn init(&mut self) {
        self.runtime = self.insert("struct.Runtime");
        self.value = self.insert("struct.Value");
        self.string = self.insert("struct.String");
    }

    fn insert(&mut self, name: &'static str) -> OpaqueStructId<LlvmCtx> {
        let id = self.counter.next();
        self.opaque_structs.insert(id, OpaqueStruct { name });
        id
    }

    pub fn into_opaque_structs(self) -> FxHashMap<OpaqueStructId<LlvmCtx>, OpaqueStruct<'static>> {
        self.opaque_structs
    }
}

struct ExternalFunctionMapper<'rt_types> {
    rt_types: &'rt_types RuntimeTypes,
    external_functions: FxHashMap<ExternalFunctionId<LlvmCtx>, ExternalFunction>,
    assembler_id_map: FxHashMap<ExternalFunctionId<AssemblerCtx>, ExternalFunctionId<LlvmCtx>>,
    counter: Counter<ExternalFunctionId<LlvmCtx>>,
    jssatrt_runtime_new: ExternalFunctionId<LlvmCtx>,
    jssatrt_string_new_utf16: ExternalFunctionId<LlvmCtx>,
    jssatrt_any_new_string: ExternalFunctionId<LlvmCtx>,
}

impl<'r> ExternalFunctionMapper<'r> {
    pub fn new(rt_types: &'r RuntimeTypes) -> Self {
        let mut me = Self {
            rt_types,
            external_functions: Default::default(),
            assembler_id_map: Default::default(),
            counter: Default::default(),
            jssatrt_runtime_new: Default::default(),
            jssatrt_string_new_utf16: Default::default(),
            jssatrt_any_new_string: Default::default(),
        };
        me.init();
        me
    }

    fn init(&mut self) {
        let rt_types = self.rt_types;
        self.jssatrt_runtime_new = self.insert(
            "jssatrt_runtime_new".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.runtime).into_ptr()),
            vec![],
        );

        self.jssatrt_string_new_utf16 = self.insert(
            "jssatrt_string_new_utf16".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.string).into_ptr()),
            vec![
                ValueType::Opaque(rt_types.runtime).into_ptr(),
                ValueType::BitType(16).into_ptr(),
                ValueType::WordSizeBitType,
            ],
        );

        self.jssatrt_any_new_string = self.insert(
            "jssatrt_any_new_string".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.value).into_ptr()),
            vec![
                ValueType::Opaque(rt_types.runtime).into_ptr(),
                ValueType::Opaque(rt_types.string).into_ptr(),
            ],
        );
    }

    fn insert(
        &mut self,
        name: String,
        return_type: llvm::ReturnType,
        parameters: Vec<ValueType>,
    ) -> ExternalFunctionId<LlvmCtx> {
        let llvm_id = self.counter.next();

        let ext_fn = ExternalFunction {
            name,
            return_type,
            parameters,
        };

        self.external_functions
            .insert(llvm_id, ext_fn)
            .expect_free();

        llvm_id
    }

    pub fn extend(
        &mut self,
        assembler_ext_fns: FxHashMap<ExternalFunctionId<AssemblerCtx>, assembler::ExternalFunction>,
    ) {
        for (key, value) in assembler_ext_fns.into_iter() {
            let parameters = (value.parameters.into_iter())
                .map(|p| p.into_llvm(self.rt_types))
                .collect();

            let llvm_id = self.insert(
                value.name,
                value.returns.into_llvm(self.rt_types),
                parameters,
            );

            self.assembler_id_map.insert(key, llvm_id).expect_free();
        }
    }

    pub fn into_external_functions(
        self,
    ) -> FxHashMap<ExternalFunctionId<LlvmCtx>, ExternalFunction> {
        self.external_functions
    }
}

impl assembler::ReturnType {
    fn into_llvm(self, rt_types: &RuntimeTypes) -> llvm::ReturnType {
        match self {
            ReturnType::Void => llvm::ReturnType::Void,
            ReturnType::Value(v) => llvm::ReturnType::Value(v.into_llvm(rt_types)),
        }
    }
}

impl assembler::Type {
    fn into_llvm(self, rt_types: &RuntimeTypes) -> llvm::ValueType {
        match self {
            Type::FFI(ir::FFIValueType::Any) | Type::Val(type_annotater::ValueType::Any) => {
                llvm::ValueType::Opaque(rt_types.value).into_ptr()
            }
            Type::FFI(ir::FFIValueType::Runtime)
            | Type::Val(type_annotater::ValueType::Runtime) => {
                llvm::ValueType::Opaque(rt_types.runtime).into_ptr()
            }
            Type::Val(
                type_annotater::ValueType::Number | type_annotater::ValueType::ExactInteger(_),
            ) => llvm::ValueType::BitType(64),
            t => unimplemented!("for {:?}", t),
        }
    }
}

struct RegisterMapper {
    assembler_register_map: FxHashMap<RegisterId<AssemblerCtx>, RegisterId<LlvmCtx>>,
    counter: Counter<RegisterId<LlvmCtx>>,
}

impl RegisterMapper {
    pub fn new() -> Self {
        Self {
            assembler_register_map: Default::default(),
            counter: Default::default(),
        }
    }

    pub fn new_register(&self) -> RegisterId<LlvmCtx> {
        self.counter.next()
    }

    pub fn map(&mut self, id: RegisterId<AssemblerCtx>) -> RegisterId<LlvmCtx> {
        // TODO: use `new_register` when rust gets split borrows
        let counter = &self.counter;
        *(self.assembler_register_map)
            .entry(id)
            .or_insert_with(|| counter.next())
    }
}

pub fn translate(program: Program) -> BackendIR<'static> {
    //     //
    //     // let mut external_functions = FxHashMap::default();
    //     // let mut function = FxHashMap::default();

    let mut constants = ConstantMapper::new(&program.constants);
    constants.map_all();

    let rt_types = RuntimeTypes::new();

    let mut ext_fns = ExternalFunctionMapper::new(&rt_types);
    ext_fns.extend(program.external_functions.clone());

    let mut functions = FxHashMap::default();
    for (id, function) in program.functions.into_iter() {
        let id = id.map_context();

        let is_entrypoint = id == program.entrypoint.map_context();

        let name = is_entrypoint.then_some("main").unwrap_or("");
        let linkage = is_entrypoint
            .then_some(Some(LLVMLinkage::External))
            .unwrap_or(None);

        let return_type = function.return_type.clone().into_llvm(&rt_types);

        let mut reg_map = RegisterMapper::new();
        let mut runtime = None;

        let parameters = if is_entrypoint {
            // TODO: support `argc`/`argv`/etc.
            vec![]
        } else {
            let runtime_reg = reg_map.new_register();
            runtime = Some(runtime_reg);

            let mut params = vec![Parameter {
                r#type: llvm::ValueType::Opaque(rt_types.runtime).into_ptr(),
                register: runtime_reg,
            }];

            params.extend(
                (function.entry_block().parameters.iter()).map(|p| Parameter {
                    r#type: p.typ.clone().into_llvm(&rt_types),
                    register: reg_map.map(p.register),
                }),
            );

            params
        };

        let entry_block = function.entry_block.map_context();

        let mut blocks = FxHashMap::default();

        for (&id, block) in function.blocks.iter() {
            let llvm_id = id.map_context();
            let is_entryblock = llvm_id == entry_block;

            let mut instructions = Vec::new();

            // this isn't exactly a sound guarantee to make, since it's always
            // possible for consumers of jssat as a library (if that happens)
            // to emit a method which jumps back to the entry block, but we'll
            // assume that they don't do that
            let runtime = match runtime {
                Some(r) => r,
                None => {
                    let rt_reg = reg_map.new_register();

                    instructions.push(llvm::Instruction::Call(
                        Some(rt_reg),
                        Callable::External(ext_fns.jssatrt_runtime_new),
                        vec![],
                    ));

                    rt_reg
                }
            };

            let mut rt_regs = FxHashSet::default();
            for instruction in block.instructions.iter() {
                match instruction {
                    assembler::Instruction::Call(_, _, _) => {
                        // todo!()
                    }
                    &assembler::Instruction::GetRuntime(rt) => {
                        rt_regs.insert(rt);
                    }
                    &assembler::Instruction::MakeString(result, id) => {
                        let const_ptr = reg_map.new_register();
                        let id = constants.map_existing(id);
                        instructions.push(llvm::Instruction::LoadConstantPtr(const_ptr, id));

                        let const_len = reg_map.new_register();
                        instructions.push(llvm::Instruction::LoadNumber {
                            result: const_len,
                            value: NumberValue::UnsignedNative(constants.const_len(id)),
                        });

                        let result = reg_map.map(result);
                        instructions.push(llvm::Instruction::Call(
                            Some(result),
                            Callable::External(ext_fns.jssatrt_string_new_utf16),
                            vec![runtime, const_ptr, const_len],
                        ));
                    }
                    &assembler::Instruction::MakeNumber(result, value) => {
                        instructions.push(llvm::Instruction::LoadNumber {
                            result: reg_map.map(result),
                            value: NumberValue::SignedArbitrary(64, value),
                        });
                    }
                    &assembler::Instruction::MakeBoolean(result, value) => {
                        // match C convention, opts should be centered around this
                        let value = match value {
                            true => NumberValue::UnsignedArbitrary(1, 1),
                            false => NumberValue::UnsignedArbitrary(1, 0),
                        };

                        instructions.push(llvm::Instruction::LoadNumber {
                            result: reg_map.map(result),
                            value,
                        });
                    }
                    assembler::Instruction::Unreachable => {
                        instructions.push(llvm::Instruction::Unreachable);
                    }
                }
            }

            match &block.end {
                assembler::EndInstruction::Unreachable => {
                    instructions.push(llvm::Instruction::Unreachable);
                    // todo!()
                }
                assembler::EndInstruction::Jump(jump) => {
                    instructions.push(llvm::Instruction::Return(None));
                    // todo!()
                }
                assembler::EndInstruction::JumpIf {
                    condition,
                    true_path,
                    false_path,
                } => {
                    instructions.push(llvm::Instruction::Return(None));
                    // todo!()
                }
                assembler::EndInstruction::Return(register) => {
                    instructions.push(llvm::Instruction::Return(register.map(|r| reg_map.map(r))));
                }
            };

            blocks.insert(llvm_id, instructions);
        }

        let llvm_function = Function {
            name,
            linkage,
            return_type,
            parameters,
            entry_block,
            blocks,
        };

        functions.insert(id, llvm_function);
    }

    //         for (id, block) in function.blocks.iter() {
    //             let instructions: &mut Vec<llvm::Instruction> =
    //                 blocks.entry(*id).or_insert_with(|| vec![]);

    //             for instruction in block.instructions.iter() {
    //                 match instruction {
    //                     Instruction::Call(result, callable, args) => {
    //                         let raw_args = args;
    //                         let mut args = args
    //                             .iter()
    //                             .map(|r| {
    //                                 is_runtime_register
    //                                     .contains(r)
    //                                     .then_some(runtime_register)
    //                                     .unwrap_or(*r)
    //                             })
    //                             .collect::<Vec<_>>();

    //                         let arg_types = match callable {
    //                             frontend::ir::Callable::External(id) => ir
    //                                 .external_functions
    //                                 .get(id)
    //                                 .unwrap()
    //                                 .parameters
    //                                 .iter()
    //                                 .map(|p| type_annotater::ffi_value_type_to_value_type(p))
    //                                 .collect::<Vec<_>>(),
    //                             frontend::ir::Callable::Static(id) => annotations
    //                                 .functions
    //                                 .get(id)
    //                                 .unwrap()
    //                                 .parameters
    //                                 .iter()
    //                                 .map(|p| p.r#type.clone())
    //                                 .collect::<Vec<_>>(),
    //                         };

    //                         for (register, (original_register, actual_fn_arg_type)) in
    //                             args.iter_mut().zip(raw_args.iter().zip(arg_types.iter()))
    //                         {
    //                             let register_type =
    //                                 function.register_types.get(original_register).unwrap();
    //                             let target_type = actual_fn_arg_type;

    //                             make_arg_match(
    //                                 register,
    //                                 register_type,
    //                                 target_type,
    //                                 instructions,
    //                                 &mut free_register,
    //                                 runtime_register,
    //                                 jssatrt_any_new_string,
    //                                 &mut register_types,
    //                                 any_id,
    //                             );
    //                         }

    //                         for (idx, (r#type, register)) in
    //                             arg_types.iter().zip(args.iter()).enumerate()
    //                         {
    //                             debug_assert_eq!(
    //                                 register_types.get(register).unwrap(),
    //                                 &map_val_type(r#type, &opaque_map),
    //                                 "position {}",
    //                                 idx
    //                             );
    //                         }

    //                         let callable = match callable {
    //                             frontend::ir::Callable::External(id) => Callable::External(*id),
    //                             frontend::ir::Callable::Static(id) => {
    //                                 // for jssat <-> jssat function calls, the runtime register is
    //                                 // always the first parameter to the function
    //                                 args.insert(0, runtime_register);

    //                                 Callable::Static(*id)
    //                             } // frontend::ir::Callable::Virtual(_) => todo!(),
    //                         };

    //                         // insert type of register for type checking
    //                         if let Some(result) = result {
    //                             if let ReturnType::Value(return_type) = match &callable {
    //                                 Callable::External(id) => map_ffi_ret_type(
    //                                     &ir.external_functions.get(&id).unwrap().return_type,
    //                                     &opaque_map,
    //                                 ),
    //                                 Callable::Static(id) => map_ret_type(
    //                                     &annotations.functions.get(&id).unwrap().return_type,
    //                                     &opaque_map,
    //                                 ),
    //                             } {
    //                                 register_types.insert(*result, return_type);
    //                             } else {
    //                                 panic!("attempting to assign return type to something without return type");
    //                             }
    //                         }

    //                         instructions.push(llvm::Instruction::Call(*result, callable, args));
    //                     }
    //                 };
    //             }

    let constants = constants.into_llvm_constants();
    let external_functions = ext_fns.into_external_functions();
    let opaque_structs = rt_types.into_opaque_structs();

    BackendIR {
        constants,
        opaque_structs,
        external_functions,
        functions,
    }
}

// fn make_arg_match(
//     source_register: &mut RegisterId,
//     source_type: &type_annotater::ValueType,
//     target_type: &type_annotater::ValueType,
//     instructions: &mut Vec<llvm::Instruction>,
//     free_register: &mut RegisterId,
//     runtime: RegisterId,
//     jssatrt_any_new_string: ExternalFunctionId,
//     register_types: &mut FxHashMap<RegisterId, llvm::ValueType>,
//     any_id: OpaqueStructId,
// ) {
//     if source_type == target_type {
//         return;
//     }

//     match (source_type, target_type) {
//         (type_annotater::ValueType::ExactString(_), type_annotater::ValueType::Any)
//         | (type_annotater::ValueType::String, type_annotater::ValueType::Any) => {
//             // widen `String` to `Any` via `jssatrt_any_new_string`
//             let result = free_register.next_and_mut();

//             instructions.push(llvm::Instruction::Call(
//                 Some(result),
//                 Callable::External(jssatrt_any_new_string),
//                 vec![runtime, *source_register],
//             ));
//             register_types.insert(
//                 result,
//                 llvm::ValueType::Pointer(Box::new(llvm::ValueType::Opaque(any_id))),
//             );
//             *source_register = result;
//         }
//         (a, b) if a == b => {}
//         (a, b) => todo!("{:?} -> {:?}", a, b),
//     };
// }

// fn map_ffi_ret_type(ffi: &FFIReturnType, opaque: &OpaqueStructs) -> ReturnType {
//     match ffi {
//         FFIReturnType::Void => ReturnType::Void,
//         FFIReturnType::Value(v) => ReturnType::Value(map_ffi_val_type(v, opaque)),
//     }
// }

// fn map_ffi_val_type(ffi: &FFIValueType, opaque: &OpaqueStructs) -> ValueType {
//     match ffi {
//         FFIValueType::Any => ValueType::Pointer(Box::new(ValueType::Opaque(opaque.any))),
//         FFIValueType::Runtime => ValueType::Pointer(Box::new(ValueType::Opaque(opaque.runtime))),
//         FFIValueType::BytePointer => ValueType::Pointer(Box::new(ValueType::BitType(8))), // i8*
//         &FFIValueType::Pointer(size) => ValueType::Pointer(Box::new(ValueType::BitType(size))),
//         FFIValueType::Word => ValueType::WordSizeBitType,
//         FFIValueType::String => ValueType::Pointer(Box::new(ValueType::Opaque(opaque.string))),
//     }
// }

// fn map_ret_type(
//     r#type: &crate::frontend::type_annotater::ReturnType,
//     opaque: &OpaqueStructs,
// ) -> ReturnType {
//     match r#type {
//         crate::frontend::type_annotater::ReturnType::Never => ReturnType::Void,
//         crate::frontend::type_annotater::ReturnType::Void => ReturnType::Void,
//         crate::frontend::type_annotater::ReturnType::Value(v) => {
//             ReturnType::Value(map_val_type(v, opaque))
//         }
//     }
// }

// fn map_val_type(
//     r#type: &crate::frontend::type_annotater::ValueType,
//     opaque: &OpaqueStructs,
// ) -> ValueType {
//     match r#type {
//         frontend::type_annotater::ValueType::Any => map_ffi_val_type(&FFIValueType::Any, opaque),
//         frontend::type_annotater::ValueType::Runtime => {
//             map_ffi_val_type(&FFIValueType::Runtime, opaque)
//         }
//         // TODO: figure out a better type for a string and constant string
//         frontend::type_annotater::ValueType::String
//         | frontend::type_annotater::ValueType::ExactString(_) => {
//             map_ffi_val_type(&FFIValueType::String, opaque)
//         }
//         frontend::type_annotater::ValueType::BytePointer => {
//             map_ffi_val_type(&FFIValueType::BytePointer, opaque)
//         }
//         frontend::type_annotater::ValueType::Pointer(size) => {
//             map_ffi_val_type(&FFIValueType::Pointer(*size), opaque)
//         }
//         frontend::type_annotater::ValueType::Word => map_ffi_val_type(&FFIValueType::Word, opaque),
//         frontend::type_annotater::ValueType::Union(_) => todo!(),
//     }
// }
