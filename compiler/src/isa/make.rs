use derive_more::Display;
use std::fmt::{Display, Write};
use tinyvec::TinyVec;

use super::{atom::AtomDealer, Atom, ISAInstruction, InternalSlot};
use crate::{
    id::*,
    retag::{CnstRetagger, FnRetagger, RegRetagger},
};

/// [`MakeTrivial`] creates trivial items. Trivial items are elements with a
/// single possible value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Make<T: Tag, I> {
    pub result: RegisterId<T>,
    pub item: I,
}

impl<C: Tag, I: Display> ISAInstruction<C> for Make<C, I> {
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
        write!(w, "%{} = Make {};", self.result, self.item)
    }
}

#[allow(non_snake_case)]
#[derive(Clone, Copy)]
pub struct TrivialItemCompat {
    pub Runtime: Atom,
    pub Null: Atom,
    pub Undefined: Atom,
    pub Empty: Atom,
    pub Throw: Atom,
    pub Unresolvable: Atom,
    pub Lexical: Atom,
    pub Strict: Atom,
    pub Global: Atom,
    pub LexicalThis: Atom,
    pub Return: Atom,
    pub Initialized: Atom,
    pub Uninitialized: Atom,
    pub Sync: Atom,
}

impl TrivialItemCompat {
    pub fn new(dealer: &mut AtomDealer) -> Self {
        let [runtime, null, undefined, empty, throw, unresolvable, lexical, strict, global, lexical_this, r#return, initialized, uninitialized, sync] =
            [(); 14].map(|_| dealer.next());

        Self {
            Runtime: runtime,
            Null: null,
            Undefined: undefined,
            Empty: empty,
            Throw: throw,
            Unresolvable: unresolvable,
            Lexical: lexical,
            Strict: strict,
            Global: global,
            LexicalThis: lexical_this,
            Return: r#return,
            Initialized: initialized,
            Uninitialized: uninitialized,
            Sync: sync,
        }
    }
}

#[deprecated]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum TrivialItem {
    /// JSSAT Runtime
    Runtime,
    /// JS null
    Null,
    /// JS undefined
    Undefined,
    /// ECMAScript "empty"
    Empty,
    /// Completion record "throw"
    Throw,
    /// Reference Record "unresolvable"
    Unresolvable,
    /// Completion record "normal"
    Normal,
    /// The kind of a parse node
    ParseNodeKind(crate::frontend::js::ast::parse_nodes::ParseNodeKind),
    /// An internal slot (needed for OrdinaryObjectCreate)
    InternalSlot(InternalSlot),
    /// ECMAScript "lexical"
    Lexical,
    /// ECMAScript "strict"
    Strict,
    /// ECMAScript "global"
    Global,
    /// ECMAScript "lexical-this"
    LexicalThis,
    Return,
    Initialized,
    Uninitialized,
    Sync,
}

impl<T: Tag> Make<T, Atom> {
    #[track_caller]
    pub fn retag<T2: Tag>(self, retagger: &mut impl RegRetagger<T, T2>) -> Make<T2, Atom> {
        Make {
            result: retagger.retag_new(self.result),
            item: self.item,
        }
    }
}

impl<T: Tag> Make<T, bool> {
    #[track_caller]
    pub fn retag<T2: Tag>(self, retagger: &mut impl RegRetagger<T, T2>) -> Make<T2, bool> {
        Make {
            result: retagger.retag_new(self.result),
            item: self.item,
        }
    }
}

impl<T: Tag, C: Tag> Make<T, ConstantId<C>> {
    #[track_caller]
    pub fn retag<T2: Tag, C2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<T, T2>,
        const_retagger: &impl CnstRetagger<C, C2>,
    ) -> Make<T2, ConstantId<C2>> {
        Make {
            result: reg_retagger.retag_new(self.result),
            item: const_retagger.retag_old(self.item),
        }
    }
}

impl<T: Tag> Make<T, i64> {
    #[track_caller]
    pub fn retag<T2: Tag>(self, retagger: &mut impl RegRetagger<T, T2>) -> Make<T2, i64> {
        Make {
            result: retagger.retag_new(self.result),
            item: self.item,
        }
    }
}

impl<T: Tag, F: Tag> Make<T, FunctionId<F>> {
    #[track_caller]
    pub fn retag<T2: Tag, F2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<T, T2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> Make<T2, FunctionId<F2>> {
        Make {
            result: reg_retagger.retag_new(self.result),
            item: fn_retagger.retag_old(self.item),
        }
    }
}
