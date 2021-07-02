use rustc_hash::FxHashMap;

use crate::frontend::ir::*;
use crate::id::*;
use crate::name::DebugName;

/// Type annotation mechanism in JSSAT.
///
/// This works by symbolically executing the JSSAT IR, and emitting equivalent functions.
pub fn annotate(ir: &IR) -> TypeAnnotations {
    let mut symb_exec_eng = SymbolicExecutionEngine::new(ir);

    let (main_fn_id, _) = symb_exec_eng.symbolically_execute(&ir.entrypoint, vec![]);

    TypeAnnotations {
        entrypoint: main_fn_id,
        functions: symb_exec_eng.typed_functions,
    }
}

#[derive(Debug)]
struct SymbolicExecutionEngine<'ir> {
    ir: &'ir IR,
    free_fn_id: FunctionId,
    executions: FxHashMap<(FunctionId, Vec<ValueType>), FnExecution>,
    typed_functions: FxHashMap<FunctionId, TypedFunction>,
}

#[derive(Debug)]
pub enum FnExecution {
    Finished(FunctionId),
    Executing(FunctionId),
}

impl SymbolicExecutionEngine<'_> {
    pub fn new<'ir>(ir: &'ir IR) -> SymbolicExecutionEngine<'ir> {
        SymbolicExecutionEngine {
            ir,
            free_fn_id: FunctionId::new(),
            executions: FxHashMap::default(),
            typed_functions: FxHashMap::default(),
        }
    }

    /// # symbolically_execute
    ///
    /// This function will symbolically execute a function until completion,
    /// taking all possible paths of execution until a return type is found for
    /// the function.
    fn symbolically_execute(
        &mut self,
        function_id: &FunctionId,
        parameters: Vec<ValueType>,
    ) -> (FunctionId, &ReturnType) {
        // TODO: figure out a plan of action to not over-allocate function ids
        // most times when executing `symbolically_execute` we won't ever use this
        let self_fn_id = self.free_fn_id.next_and_mut();

        let function = (self.ir.functions.get(function_id)).expect("valid function id");

        debug_assert_eq!(function.parameters.len(), parameters.len());
        debug_assert_eq!(function.blocks.len(), 1, "control flow not supported atm");

        // TODO: the current logic could deadlock or falsely claim for a function return `Never`
        //       this should be fixed by accounting for multiple paths of execution in the future.
        //       as of right now, that's future me's problem. sorry :hugging:
        //
        // check the current status of symbolic execution for a function like this one
        // this will handle edge cases like recursion
        match self.executions.get(&(*function_id, parameters.clone())) {
            // we've already symbolically executed this function before
            Some(FnExecution::Finished(_type_definition)) => {
                todo!("return type_definition?");
            }
            // nothing inserted
            None => {
                // insert a "currently executing" status
                self.executions.insert(
                    (*function_id, parameters.clone()),
                    FnExecution::Executing(self_fn_id),
                );
            }
            // a record that the function is currently being symbolically executed
            // reaching here would mean that we are most likely in some sort of recursion
            Some(FnExecution::Executing(_)) => {
                // generalize the `ValueType` parameters.
                // this is because we want to break out of the following scenarios:
                //
                // ```js
                // function recurse_forever(n) {
                //   return recurse_forever(n + 1);
                // }
                //
                // recurse_forever(0)
                // ```
                //
                // without generalization, the above function would recurse forever,
                // and be symbolically executed forever, as the types of the parameters
                // passed to it would be ExactNumber(0), ExactNumber(1), etc.
                //
                // with generalization, the types the function is symbolically executed for
                // become ExactNumber(0), Number, and seeing that it will only call itself,
                // it can get the return type Never.
                let generalized = parameters
                    .iter()
                    .map(|t| t.generalize())
                    .collect::<Vec<_>>();

                match self.executions.get(&(*function_id, generalized.clone())) {
                    // we've already executed it and we have a definition for it
                    Some(FnExecution::Finished(fn_id)) => {
                        return (
                            *fn_id,
                            &self.typed_functions.get(fn_id).unwrap().return_type,
                        );
                    }
                    // TODO: if we're executing multiple paths, we should wait for all paths
                    //       of execution to finish before making the call that the function
                    //       never returns.
                    //
                    // if we are currently working on symbolically executing the general function,
                    // it's probably a recursive function. conclude that we should return a Never.
                    Some(FnExecution::Executing(function_id)) => {
                        return (*function_id, &ReturnType::Never);
                    }
                    // haven't executed the generic function yet, execute it.
                    //
                    // if we end up going from a specific form of execution
                    // to a more generic one, that means that the definition
                    // of the specific function should really just be the generic
                    // definition.
                    None => {
                        // TODO: maybe it could cause problems to claim that the specific
                        // and generic function are exactly one and the same, when they have
                        // different IDs and stuff? oh well, /shrug
                        return self.symbolically_execute(function_id, generalized);
                    }
                };
            }
        };

        // at this point, we've confirmed that we're not in a recursive loop
        // (as recursiveness will be handled by the previous match)

        let mut typed_fn_blocks = FxHashMap::default();
        let mut register_types = FxHashMap::<RegisterId, ValueType>::default();

        // annotate the parameter registers with types
        for (register_id, parameter) in function
            .parameters
            .iter()
            .map(|p| p.register)
            .zip(parameters.iter())
        {
            register_types.insert(register_id, parameter.clone());
        }

        let (block_id, block) = function.blocks.iter().next().unwrap();
        let mut instructions = Vec::new();
        let mut is_prematurely_exiting_due_to_infinite_recursion = false;

        for instruction in block.instructions.iter() {
            match instruction {
                // external functions are simpler to deal with, because they have definite arguments.
                // moreover, there's no need to malleate typeless arguments into external function args
                // because the external function arguments **must** be correct, whereas with dynamic IR
                // we **must** produce a program rather than call out type errors.
                Instruction::Call(result, Callable::External(fn_id), args) => {
                    let parameter_types = args
                        .iter()
                        .map(|r| register_types.get(r).unwrap().clone())
                        .collect::<Vec<_>>();

                    let external_fn =
                        (self.ir.external_functions.get(&fn_id)).expect("expected external fn");

                    // type checking: assert that the registers are passable to the FFI function
                    for (idx, (ext_fn_type, arg_type)) in external_fn
                        .parameters
                        .iter()
                        .zip(parameter_types.iter())
                        .enumerate()
                    {
                        let ffi_as_value_type = ffi_value_type_to_value_type(ext_fn_type);
                        if !arg_type.assignable_to(&ffi_as_value_type) {
                            panic!(
                                "register {:?} type {:?} not assignable FFI type {:?} type {:?} to in instruction {:?}",
                                args[idx], arg_type, ext_fn_type, ffi_as_value_type, instruction
                            );
                        }
                    }

                    // aassign type to register, if one is wanted
                    match (result, &external_fn.return_type) {
                        (None, _) => {}
                        (Some(result_register), FFIReturnType::Value(v)) => {
                            register_types
                                .insert(*result_register, ffi_value_type_to_value_type(v));
                        }
                        (Some(_), FFIReturnType::Void) => {
                            panic!("cannot assign void to register at {:?}", instruction);
                        }
                    };

                    instructions.push(Instruction::Call(
                        *result,
                        Callable::External(*fn_id),
                        args.clone(),
                    ));
                }
                Instruction::Call(result, Callable::Static(fn_id), args) => {
                    let parameter_types = args
                        .iter()
                        .map(|r| register_types.get(r).unwrap().clone())
                        .collect::<Vec<_>>();

                    let (id, return_type) = self.symbolically_execute(&fn_id, parameter_types);

                    match (result, return_type) {
                        (_, ReturnType::Never) => {
                            instructions.push(Instruction::Call(
                                None,
                                Callable::Static(id),
                                args.clone(),
                            ));
                            instructions.push(Instruction::Unreachable);
                            is_prematurely_exiting_due_to_infinite_recursion = true;
                            break;
                        }
                        (None, _) => {}
                        (Some(register), ReturnType::Value(v)) => {
                            register_types.insert(*register, v.clone());
                        }
                        (Some(_), ReturnType::Void) => {
                            panic!("cannot assign void to register at {:?}", instruction);
                        }
                    };

                    instructions.push(Instruction::Call(
                        *result,
                        Callable::Static(id),
                        args.clone(),
                    ));
                }
                Instruction::GetRuntime(register) => {
                    register_types.insert(*register, ValueType::Runtime);

                    instructions.push(Instruction::GetRuntime(*register));
                }
                Instruction::MakeString(register, constant) => {
                    register_types.insert(*register, ValueType::ExactString(*constant));

                    instructions.push(Instruction::MakeString(*register, *constant));
                }
                Instruction::Unreachable => {
                    instructions.push(Instruction::Unreachable);
                }
                Instruction::MakeNumber(_, _) => todo!(),
                Instruction::CompareLessThan(_, _, _) => todo!(),
                Instruction::Add(_, _, _) => todo!(),
            }
        }

        let (end_instruction, return_type) = if is_prematurely_exiting_due_to_infinite_recursion {
            // if we're infinitely recursing,
            // that means the return type of this function is `Never`
            //
            // we can never return any data as we can never complete (Ret(none))
            // and signal to the caller that we never return (ReturnType::Never)
            (ControlFlowInstruction::Ret(None), ReturnType::Never)
        } else {
            (
                block.end.clone(),
                match &block.end {
                    ControlFlowInstruction::Ret(Some(register)) => {
                        ReturnType::Value(register_types.get(register).unwrap().clone())
                    }
                    ControlFlowInstruction::Ret(None) => ReturnType::Void,
                    ControlFlowInstruction::Jmp(_) => todo!(),
                    ControlFlowInstruction::JmpIf { .. } => todo!(),
                },
            )
        };

        typed_fn_blocks.insert(
            *block_id,
            FunctionBlock {
                parameters: todo!(),
                instructions,
                end: end_instruction,
            },
        );

        // now we've annotated the function totally. we can insert the information we know about the function
        let top_free_register = register_types
            .iter()
            .map(|(k, _)| *k)
            .max_by(|a, b| a.value().cmp(&b.value()))
            .map(|r| r.next())
            .unwrap_or(RegisterId::new());

        let typed_parameters = function
            .parameters
            .iter()
            .zip(parameters.iter())
            .map(|(p, t)| Parameter {
                name: p.name.clone(),
                register: p.register,
                r#type: t.clone(),
            })
            .collect::<Vec<_>>();

        self.typed_functions.insert(
            self_fn_id,
            TypedFunction {
                name: function.name.clone(),
                top_free_register,
                parameters: typed_parameters,
                return_type,
                entry_block: function.entry_block,
                blocks: typed_fn_blocks,
                register_types,
            },
        );

        self.executions.insert(
            (*function_id, parameters.clone()),
            FnExecution::Finished(self_fn_id),
        );

        (
            self_fn_id,
            &self.typed_functions.get(&self_fn_id).unwrap().return_type,
        )
    }
}

#[derive(Debug)]
pub struct TypeAnnotations {
    pub entrypoint: FunctionId,
    pub functions: FxHashMap<FunctionId, TypedFunction>,
}

#[derive(Debug)]
pub struct TypedFunction {
    pub name: DebugName,
    /// The highest register ID that is unavailable. All registers after this
    /// one must also be unclaimed. This is so that the skeleton phase can gen
    /// code for the LLVM phase
    pub top_free_register: RegisterId,
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

#[derive(Debug, Clone)]
pub enum ReturnType {
    Void,
    Value(ValueType),
    /// # [`ValueType::Never`]
    ///
    /// The type assigned to a function when it recurses to infinity, with no
    /// end in sight.
    Never,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValueType {
    /// # `Any`
    ///
    /// The `Any` type in JSSAT is used a a polymorphic "catch-all" for when
    /// the type system cannot figure something out.
    ///
    /// Narrowing an `Any` into a more specific type when it's not possible to
    /// do so results in runtime errors. This feature of the `Any` type allows
    /// us to compile all user provided code into an output, even if the code
    /// given should be considered a compiler error.
    ///
    /// The `Any` type is the most generic type possible for all values. Any
    /// JSSAT RT value can be cast into an `Any`, besides exotic primitives,
    // TODO: is `Reference`/`Pointer` the finalized name?
    /// such as a `Runtime` or `Reference`/`Pointer`.
    ///
    /// A hierarchy of JSSAT RT types is shown below:
    ///
    /// - [`ValueType::Any`]
    ///   - [`ValueType::String`]
    ///     - [`ValueType::ExactString`]
    Any,
    Runtime,
    String,
    // TODO: an "ExactString" should just be a String with some kind of
    // ExactnessGuarantee to be exactly a type of a constant
    ExactString(ConstantId),
    BytePointer,
    /// Pointer to data of the specified size. Pointer(16) -> `i16*`.
    Pointer(u16),
    Word,
    /// # [`ValueType::Union`]
    ///
    /// The type assigned to a value if it is determined to take two possible
    /// paths of execution.
    Union(Vec<ValueType>),
}

impl ValueType {
    fn generalize(&self) -> ValueType {
        match self {
            ValueType::Any => ValueType::Any,
            ValueType::Runtime => ValueType::Runtime,
            ValueType::String => ValueType::String,
            ValueType::ExactString(_) => ValueType::String,
            ValueType::BytePointer => ValueType::BytePointer,
            ValueType::Pointer(s) => ValueType::Pointer(*s),
            ValueType::Word => ValueType::Word,
            ValueType::Union(inner) => {
                // TODO: if there are inner `Union`s, flat_map 'em
                ValueType::Union(inner.iter().map(|v| v.generalize()).collect())
            }
        }
    }

    /// Panics if the input type isn't an inheritence type.
    fn generalize_up_one(&self) -> ValueType {
        match self {
            ValueType::Any => panic!("cannot generalize up one for an Any"),
            ValueType::String => ValueType::Any,
            ValueType::ExactString(_) => ValueType::String,
            ValueType::Runtime
            | ValueType::Union(_)
            | ValueType::BytePointer
            | ValueType::Pointer(_)
            | ValueType::Word => {
                panic!("expected inheritable")
            }
        }
    }

    fn assignable_to(&self, target: &ValueType) -> bool {
        // two types that are equivalent are always assignable to one another
        if self == target {
            return true;
        }

        {
            // these are native types that are only equal to their target{
            debug_assert!(
                self != target,
                "native types are only equal to their target. if self is never target, this guarantee is upheld"
            );

            if matches!(
                self,
                ValueType::Runtime | ValueType::BytePointer | ValueType::Word
            ) {
                return false;
            }

            if matches!(
                target,
                ValueType::Runtime | ValueType::BytePointer | ValueType::Word
            ) {
                return false;
            }
        }

        {
            // TODO: handle unions
            if let ValueType::Union(_) = self {
                todo!("unions");
            }

            if let ValueType::Union(_) = target {
                todo!("unions");
            }
        }

        {
            // now use inheritence logic

            debug_assert!(
                matches!(
                    self,
                    ValueType::Any | ValueType::String | ValueType::ExactString(_)
                ),
                "at this point, we should only be dealing with types with inheritence semantics"
            );

            debug_assert!(
                matches!(
                    target,
                    ValueType::Any | ValueType::String | ValueType::ExactString(_)
                ),
                "at this point, we should only be dealing with types with inheritence semantics"
            );

            debug_assert!(
                self != target,
                "we know self != target, so we must generalize self one level"
            );

            // if we are the most general type, we cannot store the most general
            // type into a more specific type.
            if let ValueType::Any = self {
                return false;
            }

            // TODO: instead of going through the entire `assignable_to`, just bring
            //       it back into this function. would just need self == target -> true
            //       and self == Any -> false in a loop
            return self.generalize_up_one().assignable_to(target);
        }
    }
}

pub fn ffi_value_type_to_value_type(ffi_value_type: &FFIValueType) -> ValueType {
    match ffi_value_type {
        FFIValueType::Any => ValueType::Any,
        FFIValueType::Runtime => ValueType::Runtime,
        FFIValueType::BytePointer => ValueType::BytePointer,
        FFIValueType::Pointer(size) => ValueType::Pointer(*size),
        FFIValueType::Word => ValueType::Word,
        FFIValueType::String => ValueType::String,
    }
}
