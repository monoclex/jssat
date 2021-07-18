//! This crate defines a contract that all instructions must provide. It is used
//! to allow easy composability of instructions for other IR passes with their
//! own in-house ISAs.

use tinyvec::{tiny_vec, TinyVec};

type RegisterId = crate::id::RegisterId<crate::id::NoContext>;
type ConstantId = crate::id::ConstantId<crate::id::NoContext>;
type BlockId = crate::id::BlockId<crate::id::NoContext>;
type FunctionId = crate::id::FunctionId<crate::id::NoContext>;
type ExternalFunctionId = crate::id::ExternalFunctionId<crate::id::NoContext>;

/// The contract provided by any single instruction. Provides methods to make
/// interfacing with all instructions easy.
pub trait ISAInstruction {
    /// An instruction is considered `pure` if its removal has no side effects
    /// for the execution of the program.
    ///
    /// Pure instructions are removed by optimization passes if the resultant
    /// type of the operation is a known constant, or if its result is unused.
    ///
    /// # Examples
    ///
    /// An example of a pure instruction is allocation of memory is considered.
    /// Despite it possibly having side effects regarding memory allocation,
    /// this type of side effect is un-observable to the actual behavior of the
    /// program.
    ///
    /// An example of a non-pure instruction would be calls to external
    /// functions, because removal of the instruction could cause a change in
    /// the behavior of the program.
    fn is_pure() -> bool {
        true
    }

    /// When an instruction introduces a register into the program, it
    /// "declares" it. This method is used to get what instructions declare
    /// which registers, so that optimization passes may examine the usages of
    /// these registers.
    fn declared_register(&self) -> Option<RegisterId>;

    /// An instruction is considered to use registers when those registers are
    /// used as operands of the current register. This means that declared
    /// registers are not considered used.
    fn used_registers(&self) -> TinyVec<[RegisterId; 3]>;

    /// Analogous to [`ISAInstruction::used_registers`], except that it
    /// provides mutable access to the registers being used to allow for
    /// changes to the registers.
    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId>;
}

pub struct Noop;

impl ISAInstruction for Noop {
    fn declared_register(&self) -> Option<RegisterId> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

pub struct Unreachable;

impl ISAInstruction for Unreachable {
    fn is_pure() -> bool {
        // removing this instruction affects program optimization
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

pub struct Return(pub Option<RegisterId>);

impl ISAInstruction for Return {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        self.0
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        match self.0 {
            Some(r) => TinyVec::from([r, Default::default(), Default::default()]),
            None => TinyVec::new(),
        }
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match &mut self.0 {
            Some(r) => vec![r],
            None => Vec::new(),
        }
    }
}

pub struct BlockJump(pub BlockId, pub Vec<RegisterId>);

pub struct Jump(pub BlockJump);

impl ISAInstruction for Jump {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&(self.0).1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        (self.0).1.iter_mut().collect()
    }
}

pub struct MakeRecord(pub RegisterId);

impl ISAInstruction for MakeRecord {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

pub struct MakeBoolean(pub RegisterId, pub bool);

impl ISAInstruction for MakeBoolean {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

pub struct MakeInteger(pub RegisterId, pub i64);

impl ISAInstruction for MakeInteger {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

/// [`MakeTrivial`] creates trivial items. Trivial items are elements with a
/// single possible value.
pub struct MakeTrivial(pub RegisterId, pub TrivialItem);

pub enum TrivialItem {
    Null,
    Undefined,
    Empty,
}

impl ISAInstruction for MakeTrivial {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

pub struct MakeString(pub RegisterId, pub ConstantId);

impl ISAInstruction for MakeString {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        Vec::new()
    }
}

pub struct OpNegate {
    pub result: RegisterId,
    pub operand: RegisterId,
}

impl ISAInstruction for OpNegate {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.operand]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.operand]
    }
}

pub struct OpAdd {
    pub result: RegisterId,
    pub lhs: RegisterId,
    pub rhs: RegisterId,
}

impl ISAInstruction for OpAdd {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

pub struct OpLessThan {
    pub result: RegisterId,
    pub lhs: RegisterId,
    pub rhs: RegisterId,
}

impl ISAInstruction for OpLessThan {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

pub struct OpEquals {
    pub result: RegisterId,
    pub lhs: RegisterId,
    pub rhs: RegisterId,
}

impl ISAInstruction for OpEquals {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

pub struct RecordGetProp {
    pub result: RegisterId,
    pub record: RegisterId,
    pub prop: RegisterId,
}

impl ISAInstruction for RecordGetProp {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.record, self.prop]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.record, &mut self.prop]
    }
}

pub struct RecordGetSlot {
    pub result: RegisterId,
    pub record: RegisterId,
    pub slot: Slot,
}

pub enum Slot {
    // TODO: expand this to all ecmascript internal slot types
    // (should this even be here?)
    Call,
}

impl ISAInstruction for RecordGetSlot {
    fn declared_register(&self) -> Option<RegisterId> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.record]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.record]
    }
}

pub struct RecordSetProp {
    pub record: RegisterId,
    pub prop: RegisterId,
    pub value: RegisterId,
}

impl ISAInstruction for RecordSetProp {
    fn declared_register(&self) -> Option<RegisterId> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        tiny_vec![self.record, self.prop, self.value]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        vec![&mut self.record, &mut self.prop, &mut self.value]
    }
}

pub struct JumpIf {
    pub condition: RegisterId,
    pub if_so: BlockJump,
    pub other: BlockJump,
}

impl ISAInstruction for JumpIf {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        (self.if_so.1.iter_mut())
            .chain(self.other.1.iter_mut())
            .collect()
    }
}

pub struct CallStatic {
    pub result: Option<RegisterId>,
    pub fn_id: FunctionId,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId>,
}

impl ISAInstruction for CallStatic {
    fn is_pure() -> bool {
        // inside of the function may be calls to external functions
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::from(self.args.as_slice())
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        self.args.iter_mut().collect()
    }
}

pub struct CallVirtual {
    pub result: Option<RegisterId>,
    pub fn_ptr: RegisterId,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId>,
}

impl ISAInstruction for CallVirtual {
    fn is_pure() -> bool {
        // calling a function that may call external functions is side-effectful
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        let mut used_registers = TinyVec::with_capacity(self.args.len() + 1);
        used_registers.extend(self.args.iter().copied());
        used_registers.push(self.fn_ptr);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        self.args
            .iter_mut()
            .chain(std::iter::once(&mut self.fn_ptr))
            .collect()
    }
}

pub struct CallExternal {
    pub result: Option<RegisterId>,
    pub fn_id: ExternalFunctionId,
    pub args: Vec<RegisterId>,
}

impl ISAInstruction for CallExternal {
    fn is_pure() -> bool {
        // calling external functions is inherently side-effectful
        false
    }

    fn declared_register(&self) -> Option<RegisterId> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        TinyVec::from(self.args.as_slice())
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        self.args.iter_mut().collect()
    }
}
