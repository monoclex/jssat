//! This crate defines a contract that all instructions must provide. It is used
//! to allow easy composability of instructions for other IR passes with their
//! own in-house ISAs.

use std::fmt::{Display, Write};

use derive_more::Display;
use tinyvec::TinyVec;

use crate::id::RegisterId;
use crate::id::Tag;

use crate::retag::RegRetagger;

/// The contract provided by any single instruction. Provides methods to make
/// interfacing with all instructions easy.
pub trait ISAInstruction<C: Tag> {
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
    fn declared_register(&self) -> Option<RegisterId<C>>;

    /// An instruction is considered to use registers when those registers are
    /// used as operands of the current register. This means that declared
    /// registers are not considered used.
    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]>;

    /// Analogous to [`ISAInstruction::used_registers`], except that it
    /// provides mutable access to the registers being used to allow for
    /// changes to the registers.
    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>>;

    fn display(&self, w: &mut impl Write) -> std::fmt::Result;
}

mod noop;
pub use noop::Noop;

mod unreachable;
pub use unreachable::Unreachable;

mod comment;
pub use comment::Comment;

mod ret;
pub use ret::Return;

mod blockjump;
pub use blockjump::BlockJump;

mod jump;
pub use jump::Jump;

mod newrecord;
pub use newrecord::NewRecord;

mod make;
pub use make::{Make, TrivialItem};

mod negate;
pub use negate::Negate;

mod binop;
pub use binop::{BinOp, BinaryOperator};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum RecordKey<C: Tag> {
    #[display(fmt = "%{}", _0)]
    Prop(RegisterId<C>),
    #[display(fmt = "[[{}]]", _0)]
    Slot(InternalSlot),
}

impl<C: Tag> RecordKey<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &impl RegRetagger<C, C2>) -> RecordKey<C2> {
        match self {
            RecordKey::Prop(r) => RecordKey::Prop(retagger.retag_old(r)),
            RecordKey::Slot(s) => RecordKey::Slot(s),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum InternalSlot {
    JSSATRandomDebugSlot,
    JSSATHasBinding,
    JSSATHasVarDeclaration,
    JSSATHasLexicalDeclaration,
    JSSATHasRestrictedGlobalProperty,
    JSSATCreateGlobalFunctionBinding,
    JSSATGetBindingValue,
    // TODO: expand this to all ecmascript internal slot types
    // (should this even be here?)
    Function,
    Realm,
    ScriptOrModule,
    GlobalObject,
    GlobalEnv,
    TemplateMap,
    Intrinsics,
    Call,
    ECMAScriptCode,
    HostDefined,
    VariableEnvironment,
    LexicalEnvironment,
    PrivateEnvironment,
    Base,
    ReferencedName,
    Strict,
    ThisValue,
    Value,
    Type,
    Target,
    BindingObject,
    DeclarativeRecord,
    ObjectRecord,
    OuterEnv,
    IsWithEnvironment,
    GlobalThisValue,
    VarNames,
    Writable,
    Enumerable,
    Configurable,
    DefineOwnProperty,
    Prototype,
    Extensible,
    GetOwnProperty,
    IsExtensible,
    Get,
    Set,
    GetPrototypeOf,
    HasProperty,
    /// `%Function.prototype%`
    FunctionPrototype,
    /// `%Object.prototype%`
    ObjectPrototype,
}

mod recordget;
pub use recordget::RecordGet;

mod recordset;
pub use recordset::RecordSet;

mod recordhaskey;
pub use recordhaskey::RecordHasKey;

mod jumpif;
pub use jumpif::JumpIf;

mod call;
pub use call::Call;

// TODO: widen/narrow instructions that operate based on a type
mod widen;
pub use widen::Widen;

mod narrow;
pub use narrow::Narrow;

mod generalize;
pub use generalize::Generalize;

pub struct Registers<'a, R: Tag>(pub &'a Vec<RegisterId<R>>);

impl<R: Tag> Display for Registers<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();

        match iter.next() {
            None => return Ok(()),
            Some(r) => write!(f, "%{}", r)?,
        };

        for r in iter {
            write!(f, ", %{}", r)?;
        }

        Ok(())
    }
}
