use rustc_hash::FxHashMap;

use crate::frontend::ir::*;
use crate::id::*;
use crate::name::DebugName;

pub fn annotate(ir: &IR) -> TypeAnnotations {
    // TODO: i cannot build rome in a day. type inference of control flow
    // is very complicatetd.
    let entrypoint_function = ir
        .functions
        .get(&ir.entrypoint)
        .expect("expected entrypoint");

    debug_assert_eq!(
        entrypoint_function.parameters.len(),
        0,
        "entrypoint shall have no parameters"
    );

    if entrypoint_function.blocks.len() > 1 {
        todo!("control flow isn't supported atm");
    }

    let (_, block) = entrypoint_function.blocks.iter().next().unwrap();

    let entrypoint = FunctionId::new();
    let mut type_mapping = FxHashMap::<FunctionId, TypedFunction>::default();
    let mut registers = FxHashMap::default();

    for instruction in block.instructions.iter() {
        match &instruction {
            // Instruction::RecordGet(_, _, _) => todo!("RecordGet"),
            // Instruction::RecordSet(_, _, _) => todo!("RecordSet"),
            // Instruction::RefIsEmpty(_, _) => todo!("RefIsEmpty"),
            // Instruction::RefDeref(_, _) => todo!("RefDeref"),
            // Instruction::MakePrimitive(_, _) => todo!("MakePrimitive"),
            &Instruction::Call(Some(result), Callable::External(external_fn), values) => {
                let external_fn = ir
                    .external_functions
                    .get(external_fn)
                    .expect("expected valid ext fn");

                let return_type = ffi_return_type_to_return_type(&external_fn.return_type);

                match return_type {
                    ReturnType::Void => panic!("cannot assign void to register"),
                    ReturnType::Value(value) => registers.insert(*result, value),
                };

                // TODO: ensure that every value can unify with the arguments of the external function
                // if they cannot, emit a compiler error
            }
            &Instruction::Call(None, Callable::External(external_fn), values) => {
                // TODO: ensure that every value can unify with the arguments of the external function
                // if they cannot, emit a compiler error

                let external_function = (ir.external_functions.get(&external_fn))
                    .expect("expected callable::external to pt to valid ext fn");

                for (register_id, ffi_value_type) in
                    values.iter().zip(external_function.parameters.iter())
                {
                    let register_type = registers.get(&register_id).expect("expected type");
                    let target_type = ffi_value_type_to_value_type(ffi_value_type);

                    // TODO: do something with the unification result?
                    target_type.unify(register_type);
                }
            }
            Instruction::GetRuntime(result) => {
                registers.insert(*result, ValueType::Runtime);
            }
            Instruction::MakeString(result, constant_id) => {
                registers.insert(*result, ValueType::ExactString(*constant_id));
            }
            _ => todo!("{:?}", instruction),
            // Instruction::Phi(_, _) => todo!("Phi"),
        }
    }

    let return_type = match block.end {
        ControlFlowInstruction::Ret(None) => ReturnType::Void,
        ControlFlowInstruction::Ret(Some(register)) => {
            ReturnType::Value(registers.get(&register).expect("type").clone())
        }
    };

    let top_free_register = registers
        .iter()
        .map(|(r, _)| *r)
        .max_by(|a, b| a.value().cmp(&b.value()))
        .map(|r| r.next())
        .unwrap_or(RegisterId::new());

    type_mapping.insert(
        entrypoint,
        TypedFunction {
            name: entrypoint_function.name.clone(),
            parameters: vec![],
            top_free_register,
            return_type,
            entry_block: entrypoint_function.entry_block,
            blocks: entrypoint_function.blocks.clone(),
            register_types: registers,
        },
    );

    TypeAnnotations {
        entrypoint,
        functions: type_mapping,
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

#[derive(Debug, Clone)]
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
}

impl ValueType {
    // TODO: do something with the current type, widening it to something else if necessary?
    // `self`: the type we are trying to ensure `target` is compatible with
    pub fn unify(&self, target: &ValueType) {
        match (self, target) {
            (ValueType::Any, ValueType::Any) => {}
            // (ValueType::Any, ValueType::Runtime) => todo!(),
            (ValueType::Any, ValueType::String) => {}
            (ValueType::Any, ValueType::ExactString(_)) => {}
            // (ValueType::Any, ValueType::BytePointer) => todo!(),
            // (ValueType::Any, ValueType::Word) => todo!(),
            // (ValueType::Runtime, ValueType::Any) => todo!(),
            (ValueType::Runtime, ValueType::Runtime) => {}
            // (ValueType::Runtime, ValueType::String) => todo!(),
            // (ValueType::Runtime, ValueType::ExactString(_)) => todo!(),
            (ValueType::Runtime, ValueType::BytePointer) => todo!(),
            // (ValueType::Runtime, ValueType::Word) => todo!(),
            // (ValueType::String, ValueType::Any) => todo!(),
            // (ValueType::String, ValueType::Runtime) => todo!(),
            (ValueType::String, ValueType::String) => {}
            (ValueType::String, ValueType::ExactString(_)) => {}
            // (ValueType::String, ValueType::BytePointer) => todo!(),
            // (ValueType::String, ValueType::Word) => todo!(),
            // (ValueType::ExactString(_), ValueType::Any) => todo!(),
            // (ValueType::ExactString(_), ValueType::Runtime) => todo!(),
            // (ValueType::ExactString(_), ValueType::String) => todo!(),
            (ValueType::ExactString(_), ValueType::ExactString(_)) => {}
            // (ValueType::ExactString(_), ValueType::BytePointer) => todo!(),
            // (ValueType::ExactString(_), ValueType::Word) => todo!(),
            (ValueType::BytePointer, ValueType::BytePointer) => {}
            // (ValueType::BytePointer, ValueType::Word) => todo!(),
            // (ValueType::Word, ValueType::BytePointer) => todo!(),
            (ValueType::Word, ValueType::Word) => {}
            (a, b) => panic!("cannot unify {:?} and {:?}", a, b),
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
