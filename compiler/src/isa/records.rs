use derive_more::Display;
use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

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

// TODO(refactor): internal slots should be able to be dynamically created by
//   some provider, and then get assigned a numeric number (so they're cheap
//   to copy around and such). for now, i'm just gonna keep sticking stuff here
//   (this will not come back to bite me as internal slots are pervasively used
//   throughout the codebase at all :DDD)
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
    // TODO: auto generate these from the ecmascript IR file
    JSSATCode,
    JSSATBoundNames,
    JSSATDeclarationPart,
    JSSATIsConstantDeclaration,
    JSSATLexicallyDeclaredNames,
    JSSATLexicallyScopedDeclarations,
    JSSATVarDeclaredNames,
    JSSATVarScopedDeclarations,
    JSSATTopLevelLexicallyDeclaredNames,
    JSSATTopLevelLexicallyScopedDeclarations,
    JSSATTopLevelVarDeclaredNames,
    JSSATTopLevelVarScopedDeclarations,
    JSSATContainsDuplicateLabels,
    JSSATContainsUndefinedBreakTarget,
    JSSATContainsUndefinedContinueTarget,
    JSSATHasName,
    JSSATIsFunctionDefinition,
    JSSATParseNodeSlot1,
    JSSATParseNodeSlot2,
    JSSATParseNodeSlot3,
    JSSATParseNodeSlot4,
    JSSATParseNodeSlot5,
    JSSATParseNodeSlot6,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct NewRecord<C: Tag> {
    pub result: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for NewRecord<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = NewRecord;", self.result)
    }
}

impl<C: Tag> NewRecord<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> NewRecord<C2> {
        NewRecord {
            result: retagger.retag_new(self.result),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordHasKey<C: Tag, S = ()> {
    pub result: RegisterId<C>,
    pub shape: S,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag, S> ISAInstruction<C> for RecordHasKey<C, S> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record];
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record];
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = RecordHasKey %{}.{};",
            self.result, self.record, self.key
        )
    }
}

impl<C: Tag, S> RecordHasKey<C, S> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordHasKey<C2, S> {
        RecordHasKey {
            shape: self.shape,
            result: retagger.retag_new(self.result),
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordGet<C: Tag, S = ()> {
    pub result: RegisterId<C>,
    pub shape: S,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag, S> ISAInstruction<C> for RecordGet<C, S> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record];
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record];
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = RecordGet %{}.{};",
            self.result, self.record, self.key
        )
    }
}

impl<C: Tag, S> RecordGet<C, S> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordGet<C2, S> {
        RecordGet {
            shape: self.shape,
            result: retagger.retag_new(self.result),
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordSet<C: Tag, S = ()> {
    pub shape: S,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
    pub value: Option<RegisterId<C>>,
}

impl<C: Tag, S> ISAInstruction<C> for RecordSet<C, S> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record];
        if let Some(register) = self.value {
            used_registers.push(register);
        }
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record];
        if let Some(register) = &mut self.value {
            used_registers.push(register);
        }
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "RecordSet %{}.{} = {}",
            self.record,
            self.key,
            match self.value {
                Some(r) => format!("%{}", r),
                None => format!("<remove>"),
            }
        )
    }
}

impl<C: Tag, S> RecordSet<C, S> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordSet<C2, S> {
        RecordSet {
            shape: self.shape,
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
            value: self.value.map(|value| retagger.retag_old(value)),
        }
    }
}
