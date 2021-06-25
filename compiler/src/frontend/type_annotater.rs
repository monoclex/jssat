use rustc_hash::FxHashMap;

use crate::frontend::ir::*;
use crate::id::*;
use crate::name::DebugName;

/// Type annotation mechanism in JSSAT.
///
/// This works by symbolically executing the JSSAT IR, and emitting equivalent functions.
pub fn annotate(ir: &IR) -> TypeAnnotations {
    let mut symb_exec_eng = SymbolicExecutionEngine {
        ir,
        executions: FxHashMap::default(),
    };

    symb_exec_eng.symbolically_execute(&ir.entrypoint, vec![]);

    todo!()
}

struct SymbolicExecutionEngine<'ir> {
    ir: &'ir IR,
    executions: FxHashMap<(FunctionId, Vec<ValueType>), Option<TypedFunction>>,
}

impl SymbolicExecutionEngine<'_> {
    /// # symbolically_execute
    ///
    /// This function will symbolically execute a function until completion,
    /// taking all possible paths of execution until a return type is found for
    /// the function.
    fn symbolically_execute(&mut self, function_id: &FunctionId, parameters: Vec<ValueType>) {
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
            Some(Some(type_definition)) => {
                todo!("return type_definition?");
            }
            // nothing inserted
            None => {
                // insert a "currently executing" status
                self.executions
                    .insert((*function_id, parameters.clone()), None);
            }
            // a record that the function is currently being symbolically executed
            // reaching here would mean that we are most likely in some sort of recursion
            Some(None) => {
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

                let generic_definition =
                    match self.executions.get(&(*function_id, generalized.clone())) {
                        // we've already executed it and we have a definition for it
                        Some(Some(type_definition)) => type_definition,
                        // TODO: if we're executing multiple paths, we should wait for all paths
                        //       of execution to finish before making the call that the function
                        //       never returns.
                        //
                        // if we are currently working on symbolically executing the general function,
                        // it's probably a recursive function. conclude that we should return a Never.
                        Some(None) => {
                            todo!("function returns `Never`")
                        }
                        // haven't executed the generic function yet, execute it.
                        None => {
                            self.symbolically_execute(function_id, generalized);
                            todo!("what now");
                        }
                    };

                todo!("return info using the generic definition");
            }
        };

        // at this point, we've confirmed that we're not in a recursive loop
        // (as recursiveness will be handled by the previous match)

        let mut register_types = FxHashMap::<RegisterId, ValueType>::default();

        let (id, block) = function.blocks.iter().next().unwrap();

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
                        if !ffi_value_type_to_value_type(ext_fn_type).assignable_to(arg_type) {
                            panic!(
                                "register {:?} not assignable to FFI type {:?} in instruction {:?}",
                                args[idx], ext_fn_type, instruction
                            );
                        }
                    }

                    // aassign type to register, if one is wanted
                    let return_type = ffi_return_type_to_return_type(&external_fn.return_type);
                    match (result, return_type) {
                        (None, _) => {}
                        (Some(result_register), ReturnType::Value(v)) => {
                            register_types.insert(*result_register, v);
                        }
                        (Some(_), ReturnType::Void) => {
                            panic!("cannot assign void to register at {:?}", instruction);
                        }
                    };
                }
                Instruction::Call(result, Callable::Static(fn_id), args) => {
                    let parameter_types = args
                        .iter()
                        .map(|r| register_types.get(r).unwrap().clone())
                        .collect::<Vec<_>>();

                    self.symbolically_execute(&fn_id, parameter_types);
                }
                Instruction::GetRuntime(register) => {
                    register_types.insert(*register, ValueType::Runtime);
                }
                Instruction::MakeString(register, constant) => {
                    register_types.insert(*register, ValueType::ExactString(*constant));
                }
            }
        }

        let return_type = match block.end {
            ControlFlowInstruction::Ret(Some(register)) => {
                ReturnType::Value(register_types.get(&register).unwrap().clone())
            }
            ControlFlowInstruction::Ret(None) => ReturnType::Void,
        };

        // now we've annotated the function totally. we can insert the information we know about the function
        let top_free_register = register_types
            .iter()
            .map(|(k, v)| *k)
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

        self.executions.insert(
            (*function_id, parameters.clone()),
            Some(TypedFunction {
                name: function.name.clone(),
                top_free_register,
                parameters: typed_parameters,
                return_type,
                entry_block: function.entry_block,
                blocks: function.blocks.clone(),
                register_types,
            }),
        );
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

#[derive(Debug)]
pub enum ReturnType {
    Void,
    Value(ValueType),
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
    Word,
    /// # [`ValueType::Never`]
    ///
    /// The type assigned to a function when it recurses to infinity, with no
    /// end in sight.
    Never,
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
            ValueType::Word => ValueType::Word,
            ValueType::Never => ValueType::Never,
            ValueType::Union(inner) => {
                // TODO: if there are inner `Union`s, flat_map 'em
                ValueType::Union(inner.iter().map(|v| v.generalize()).collect())
            }
        }
    }

    /// Panics if the input type isn't an inheritence type.
    fn generalize_up_one(&self) -> ValueType {
        match self {
            ValueType::Any => todo!(),
            ValueType::String => todo!(),
            ValueType::ExactString(_) => todo!(),
            ValueType::Runtime => todo!(),
            ValueType::Union(_) | ValueType::BytePointer | ValueType::Word | ValueType::Never => {
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
            // anything is assignable to a `Never` value
            if let ValueType::Never = target {
                return true;
            }

            // we can assign a value of type `Never` to any other value, as once a
            // value is given a `Never` type it will never be computed, so any code
            // that wants to use that value is fine in doing so
            if let ValueType::Never = self {
                return true;
            }
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

fn ffi_return_type_to_return_type(ffi_return_type: &FFIReturnType) -> ReturnType {
    match ffi_return_type {
        FFIReturnType::Void => ReturnType::Void,
        FFIReturnType::Value(value) => ReturnType::Value(ffi_value_type_to_value_type(value)),
    }
}

fn ffi_value_type_to_value_type(ffi_value_type: &FFIValueType) -> ValueType {
    match ffi_value_type {
        FFIValueType::Any => ValueType::Any,
        FFIValueType::Runtime => ValueType::Runtime,
        FFIValueType::BytePointer => ValueType::BytePointer,
        FFIValueType::Word => ValueType::Word,
    }
}
