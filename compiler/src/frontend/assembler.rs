//! Assembles together the informatino from the original IR, and type_annotator
//! passes into a fully conherent IR, which will then be passed to `skeleton`.

use std::fmt::Debug;

use crate::isa::{ISAInstruction, LessThan, MakeTrivial, RecordKey};

use super::{
    ir::{self, FFIValueType},
    old_types::RegMap,
    type_annotater::ValueType,
};
use crate::id::*;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct Program {
    pub entrypoint: FunctionId<AssemblerCtx>,
    pub constants: FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
    pub external_functions: FxHashMap<ExternalFunctionId<AssemblerCtx>, ExternalFunction>,
    pub functions: FxHashMap<FunctionId<AssemblerCtx>, Function>,
}

#[derive(Clone, Debug)]
pub struct ExternalFunction {
    pub name: String,
    pub parameters: Vec<ValueType>,
    pub returns: ReturnType,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub entry_block: BlockId<AssemblerCtx>,
    pub blocks: FxHashMap<BlockId<AssemblerCtx>, Block>,
    pub return_type: ReturnType,
}

impl Function {
    pub fn entry_block(&self) -> &Block {
        self.blocks
            .get(&self.entry_block)
            .expect("must be entry block")
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub register_types: RegMap<AssemblerCtx>,
    pub parameters: Vec<Parameter>,
    pub instructions: Vec<Instruction>,
    pub end: EndInstruction,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub typ: ValueType,
    // TODO: maybe parameters should implicitly get the register accoridng to
    // their index? it makes sense not to do this in the other IRs because of
    // mangling parameters, but here we have pretty much all the information
    // necessary to craft a final product
    pub register: RegisterId<AssemblerCtx>,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Comment(&'static str, &'static std::panic::Location<'static>),
    RecordNew(RegisterId<AssemblerCtx>),
    RecordGet {
        result: RegisterId<AssemblerCtx>,
        record: RegisterId<AssemblerCtx>,
        key: RecordKey<AssemblerCtx>,
    },
    RecordSet {
        shape_id: ShapeId<AssemblerCtx>,
        record: RegisterId<AssemblerCtx>,
        key: RecordKey<AssemblerCtx>,
        value: RegisterId<AssemblerCtx>,
    },
    Call(
        Option<RegisterId<AssemblerCtx>>,
        Callable,
        Vec<RegisterId<AssemblerCtx>>,
    ),
    MakeTrivial(MakeTrivial<AssemblerCtx>),
    MakeFnPtr(RegisterId<AssemblerCtx>, BlockId<PureBbCtx>),
    MakeString(RegisterId<AssemblerCtx>, ConstantId<AssemblerCtx>),
    MakeNumber(RegisterId<AssemblerCtx>, i64),
    MakeBoolean(RegisterId<AssemblerCtx>, bool),
    /// "Widens" a given register to a type. The type must wider/bigger than the
    /// input register.
    Widen {
        result: RegisterId<AssemblerCtx>,
        input: RegisterId<AssemblerCtx>,
        from: ValueType,
        to: ValueType,
    },
    Unreachable,
    Noop,
    OpLessThan(LessThan<AssemblerCtx>),
}

#[derive(Clone, Debug)]
pub enum Callable {
    Extern(ExternalFunctionId<AssemblerCtx>),
    Static(FunctionId<AssemblerCtx>),
}

#[derive(Clone, Debug)]
pub enum EndInstruction {
    Unreachable,
    Jump(BlockJump),
    JumpIf {
        condition: RegisterId<AssemblerCtx>,
        true_path: BlockJump,
        false_path: BlockJump,
    },
    Return(Option<RegisterId<AssemblerCtx>>),
}

#[derive(Clone, Debug)]
pub struct BlockJump(pub BlockId<AssemblerCtx>, pub Vec<RegisterId<AssemblerCtx>>);

#[derive(Clone, Debug)]
pub enum ReturnType {
    Void,
    Value(ValueType),
}

impl ir::FFIReturnType {
    pub fn into_return_type(self) -> ReturnType {
        match self {
            ir::Returns::Value(v) => ReturnType::Value(v.into_value_type()),
            ir::Returns::Void => ReturnType::Void,
        }
    }
}

impl ir::FFIValueType {
    pub fn into_value_type(self) -> ValueType {
        match self {
            FFIValueType::Any => ValueType::Any,
            FFIValueType::Runtime => ValueType::Runtime,
            FFIValueType::String => ValueType::String,
        }
    }
}

impl Instruction {
    pub fn assigned_to(&self) -> Option<RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(result, _, _) => *result,
            Instruction::MakeString(result, _)
            | Instruction::MakeNumber(result, _)
            | Instruction::MakeBoolean(result, _)
            | Instruction::Widen { result, .. }
            | Instruction::RecordNew(result)
            | Instruction::RecordGet { result, .. }
            | Instruction::MakeFnPtr(result, _) => Some(*result),
            Instruction::Comment(_, _)
            | Instruction::Unreachable
            | Instruction::Noop
            | Instruction::RecordSet { .. } => None,
            Instruction::OpLessThan(inst) => inst.declared_register(),
            Instruction::MakeTrivial(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> Vec<RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(_, _, params) => params.clone(),
            // Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
            //     vec![*lhs, *rhs]
            // }
            Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _)
            | Instruction::MakeBoolean(_, _)
            | Instruction::MakeFnPtr(_, _) => Vec::new(),
            Instruction::Widen { input, .. } => vec![*input],
            Instruction::RecordGet {
                record,
                key: RecordKey::Prop(v),
                ..
            } => vec![*record, *v],
            Instruction::RecordGet {
                record,
                key: RecordKey::Slot(_),
                ..
            } => vec![*record],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Prop(v),
                value,
            } => vec![*record, *v, *value],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Slot(_),
                value,
            } => vec![*record, *value],
            Instruction::Comment(_, _)
            | Instruction::Unreachable
            | Instruction::Noop
            | Instruction::RecordNew(_) => vec![],
            Instruction::OpLessThan(inst) => inst.used_registers().to_vec(),
            Instruction::MakeTrivial(inst) => inst.used_registers().to_vec(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(_, _, params) => params.iter_mut().collect::<Vec<_>>(),
            // Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
            //     vec![*lhs, *rhs]
            // }
            Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _)
            | Instruction::MakeBoolean(_, _)
            | Instruction::MakeFnPtr(_, _) => Vec::new(),
            Instruction::Widen { input, .. } => vec![input],
            Instruction::RecordGet {
                record,
                key: RecordKey::Prop(v),
                ..
            } => vec![record, v],
            Instruction::RecordGet {
                record,
                key: RecordKey::Slot(_),
                ..
            } => vec![record],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Prop(v),
                value,
            } => vec![record, v, value],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Slot(_),
                value,
            } => vec![record, value],
            Instruction::Comment(_, _)
            | Instruction::Unreachable
            | Instruction::Noop
            | Instruction::RecordNew(_) => vec![],
            Instruction::OpLessThan(inst) => inst.used_registers_mut(),
            Instruction::MakeTrivial(inst) => inst.used_registers_mut(),
        }
    }
}

impl EndInstruction {
    pub fn used_registers(&self) -> Vec<RegisterId<AssemblerCtx>> {
        match self {
            EndInstruction::Jump(path) => path.1.clone(),
            EndInstruction::JumpIf {
                condition,
                true_path,
                false_path,
            } => {
                let mut r = vec![*condition];
                r.extend(true_path.1.clone());
                r.extend(false_path.1.clone());
                r
            }
            EndInstruction::Return(Some(r)) => vec![*r],
            EndInstruction::Return(None) => vec![],
            EndInstruction::Unreachable => vec![],
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<AssemblerCtx>> {
        match self {
            EndInstruction::Jump(path) => path.1.iter_mut().collect(),
            EndInstruction::JumpIf {
                condition,
                true_path,
                false_path,
            } => {
                let mut r = vec![condition];
                r.extend(true_path.1.iter_mut());
                r.extend(false_path.1.iter_mut());
                r
            }
            EndInstruction::Return(Some(r)) => vec![r],
            EndInstruction::Return(None) => vec![],
            EndInstruction::Unreachable => vec![],
        }
    }
}
