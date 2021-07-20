//! Used for implementing the `retag()` feature, which allows a piece of data
//! to be tagged for usage in only a specific context. Tags prevents errors
//! regarding mismatching the registers in one phase and another. To prevent
//! more errors, a specific [`CoreRetagger`] must be used to retag the tag in
//! the first place. Depending upon the retagger used, there will be additional
//! assertions placed to ensure that data from different phases are mixed up as
//! infrequently as possible.

use std::marker::PhantomData;
use std::panic::Location;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::id::IdCompat;
use crate::id::Tag;
use crate::id::{BlockId, ConstantId, ExternalFunctionId, FunctionId, RegisterId};

// Rust doesn't have HKTs, which makes this a lot of pain.
// Try to keep things in this layout:
//
// trait XRetagger
// trait XGenRetagger: XRetagger
// impl XPassRetagger: XRetagger, uses PassRetagger
// impl XGenPassRetagger: XRetagger, XGenRetagger, uses GenPassRetagger
// impl XMapRetagger: XRetagger, XGenRetagger, uses MapRetagger

// === REGISTERS ===

/// A retagger specifically for registers.
pub trait RegRetagger<C: Tag, C2: Tag> {
    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2>;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2>;

    #[track_caller]
    fn retag_olds(&self, ids: Vec<RegisterId<C>>) -> Vec<RegisterId<C2>> {
        ids.into_iter().map(|r| self.retag_old(r)).collect()
    }
}

pub trait RegGenRetagger<C: Tag, C2: Tag>: RegRetagger<C, C2> {
    /// Generates a new register that will not conflict with any other register.
    fn gen(&mut self) -> RegisterId<C2>;
}

#[derive(Debug)]
pub struct RegPassRetagger<C: Tag, C2: Tag>(PassRetagger<RegisterId<C>, RegisterId<C2>>);

impl<A: Tag, B: Tag> RegPassRetagger<A, B> {
    #[cfg(not(debug_assertions))]
    pub fn ignore_checks(&mut self) {}

    #[cfg(debug_assertions)]
    pub fn ignore_checks(&mut self) {
        self.0.ignore_check = true;
    }
}

impl<A: Tag, B: Tag> Default for RegPassRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> RegRetagger<C, C2> for RegPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_old(id)
    }
}

pub struct RegGenPassRetagger<C: Tag, C2: Tag>(GenPassRetagger<RegisterId<C>, RegisterId<C2>>);

impl<A: Tag, B: Tag> RegGenPassRetagger<A, B> {
    pub fn new(max: RegisterId<A>) -> Self {
        Self(GenPassRetagger::new(max))
    }
}

impl<C: Tag, C2: Tag> RegRetagger<C, C2> for RegGenPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> RegGenRetagger<C, C2> for RegGenPassRetagger<C, C2> {
    fn gen(&mut self) -> RegisterId<C2> {
        self.0.core_gen()
    }
}

#[derive(Debug)]
pub struct RegMapRetagger<C: Tag, C2: Tag>(MapRetagger<RegisterId<C>, RegisterId<C2>>);

impl<A: Tag, B: Tag> Default for RegMapRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> RegRetagger<C, C2> for RegMapRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> RegGenRetagger<C, C2> for RegMapRetagger<C, C2> {
    fn gen(&mut self) -> RegisterId<C2> {
        self.0.core_gen()
    }
}

// === BLOCKS ===

/// A retagger specifically for registers.
pub trait BlkRetagger<C: Tag, C2: Tag> {
    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn retag_new(&mut self, id: BlockId<C>) -> BlockId<C2>;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn retag_old(&self, id: BlockId<C>) -> BlockId<C2>;
}

pub trait BlkGenRetagger<C: Tag, C2: Tag>: BlkRetagger<C, C2> {
    /// Generates a new register that will not conflict with any other register.
    fn gen(&mut self) -> BlockId<C2>;
}

pub struct BlkPassRetagger<C: Tag, C2: Tag>(PassRetagger<BlockId<C>, BlockId<C2>>);

impl<A: Tag, B: Tag> Default for BlkPassRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> BlkRetagger<C, C2> for BlkPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: BlockId<C>) -> BlockId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: BlockId<C>) -> BlockId<C2> {
        self.0.core_retag_old(id)
    }
}

pub struct BlkGenPassRetagger<C: Tag, C2: Tag>(GenPassRetagger<BlockId<C>, BlockId<C2>>);

impl<A: Tag, B: Tag> BlkGenPassRetagger<A, B> {
    pub fn new(max: BlockId<A>) -> Self {
        Self(GenPassRetagger::new(max))
    }
}

impl<C: Tag, C2: Tag> BlkRetagger<C, C2> for BlkGenPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: BlockId<C>) -> BlockId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: BlockId<C>) -> BlockId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> BlkGenRetagger<C, C2> for BlkGenPassRetagger<C, C2> {
    fn gen(&mut self) -> BlockId<C2> {
        self.0.core_gen()
    }
}

pub struct BlkMapRetagger<C: Tag, C2: Tag>(MapRetagger<BlockId<C>, BlockId<C2>>);

impl<B: Tag, B2: Tag> BlkMapRetagger<B, B2> {
    pub fn counter(&self) -> usize {
        self.0.counter
    }

    pub fn new_with_counter(counter: usize) -> Self {
        let mut me = Self::default();
        me.0.counter = counter;
        me
    }
}

impl<A: Tag, B: Tag> Default for BlkMapRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> BlkRetagger<C, C2> for BlkMapRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: BlockId<C>) -> BlockId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: BlockId<C>) -> BlockId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> BlkGenRetagger<C, C2> for BlkMapRetagger<C, C2> {
    fn gen(&mut self) -> BlockId<C2> {
        self.0.core_gen()
    }
}

// === EXTERNAL FUNCTIONS ===

/// A retagger specifically for registers.
pub trait ExtFnRetagger<C: Tag, C2: Tag> {
    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn retag_new(&mut self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2>;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn retag_old(&self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2>;
}

pub trait ExtFnGenRetagger<C: Tag, C2: Tag>: ExtFnRetagger<C, C2> {
    /// Generates a new register that will not conflict with any other register.
    fn gen(&mut self) -> ExternalFunctionId<C2>;
}

#[derive(Debug)]
pub struct ExtFnPassRetagger<C: Tag, C2: Tag>(
    PassRetagger<ExternalFunctionId<C>, ExternalFunctionId<C2>>,
);

impl<A: Tag, B: Tag> Default for ExtFnPassRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> ExtFnRetagger<C, C2> for ExtFnPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2> {
        self.0.core_retag_old(id)
    }
}

pub struct ExtFnGenPassRetagger<C: Tag, C2: Tag>(
    GenPassRetagger<ExternalFunctionId<C>, ExternalFunctionId<C2>>,
);

impl<A: Tag, B: Tag> ExtFnGenPassRetagger<A, B> {
    pub fn new(max: ExternalFunctionId<A>) -> Self {
        Self(GenPassRetagger::new(max))
    }
}

impl<C: Tag, C2: Tag> ExtFnRetagger<C, C2> for ExtFnGenPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> ExtFnGenRetagger<C, C2> for ExtFnGenPassRetagger<C, C2> {
    fn gen(&mut self) -> ExternalFunctionId<C2> {
        self.0.core_gen()
    }
}

pub struct ExtFnMapRetagger<C: Tag, C2: Tag>(
    MapRetagger<ExternalFunctionId<C>, ExternalFunctionId<C2>>,
);

impl<A: Tag, B: Tag> Default for ExtFnMapRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> ExtFnRetagger<C, C2> for ExtFnMapRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: ExternalFunctionId<C>) -> ExternalFunctionId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> ExtFnGenRetagger<C, C2> for ExtFnMapRetagger<C, C2> {
    fn gen(&mut self) -> ExternalFunctionId<C2> {
        self.0.core_gen()
    }
}

// === FUNCTIONS ===

/// A retagger specifically for registers.
pub trait FnRetagger<C: Tag, C2: Tag> {
    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn retag_new(&mut self, id: FunctionId<C>) -> FunctionId<C2>;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn retag_old(&self, id: FunctionId<C>) -> FunctionId<C2>;
}

pub trait FnGenRetagger<C: Tag, C2: Tag>: FnRetagger<C, C2> {
    /// Generates a new register that will not conflict with any other register.
    fn gen(&mut self) -> FunctionId<C2>;
}

pub struct FnPassRetagger<C: Tag, C2: Tag>(PassRetagger<FunctionId<C>, FunctionId<C2>>);

impl<A: Tag, B: Tag> Default for FnPassRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> FnRetagger<C, C2> for FnPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: FunctionId<C>) -> FunctionId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: FunctionId<C>) -> FunctionId<C2> {
        self.0.core_retag_old(id)
    }
}

pub struct FnGenPassRetagger<C: Tag, C2: Tag>(GenPassRetagger<FunctionId<C>, FunctionId<C2>>);

impl<A: Tag, B: Tag> FnGenPassRetagger<A, B> {
    pub fn new(max: FunctionId<A>) -> Self {
        Self(GenPassRetagger::new(max))
    }
}

impl<C: Tag, C2: Tag> FnRetagger<C, C2> for FnGenPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: FunctionId<C>) -> FunctionId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: FunctionId<C>) -> FunctionId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> FnGenRetagger<C, C2> for FnGenPassRetagger<C, C2> {
    fn gen(&mut self) -> FunctionId<C2> {
        self.0.core_gen()
    }
}

pub struct FnMapRetagger<C: Tag, C2: Tag>(MapRetagger<FunctionId<C>, FunctionId<C2>>);

impl<A: Tag, B: Tag> Default for FnMapRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> FnRetagger<C, C2> for FnMapRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: FunctionId<C>) -> FunctionId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: FunctionId<C>) -> FunctionId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> FnGenRetagger<C, C2> for FnMapRetagger<C, C2> {
    fn gen(&mut self) -> FunctionId<C2> {
        self.0.core_gen()
    }
}

// === CONSTANTS ===

/// A retagger specifically for registers.
pub trait CnstRetagger<C: Tag, C2: Tag> {
    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn retag_new(&mut self, id: ConstantId<C>) -> ConstantId<C2>;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn retag_old(&self, id: ConstantId<C>) -> ConstantId<C2>;
}

pub trait CnstGenRetagger<C: Tag, C2: Tag>: CnstRetagger<C, C2> {
    /// Generates a new register that will not conflict with any other register.
    fn gen(&mut self) -> ConstantId<C2>;
}

pub struct CnstPassRetagger<C: Tag, C2: Tag>(PassRetagger<ConstantId<C>, ConstantId<C2>>);

impl<A: Tag, B: Tag> Default for CnstPassRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> CnstRetagger<C, C2> for CnstPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: ConstantId<C>) -> ConstantId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: ConstantId<C>) -> ConstantId<C2> {
        self.0.core_retag_old(id)
    }
}

pub struct CnstGenPassRetagger<C: Tag, C2: Tag>(GenPassRetagger<ConstantId<C>, ConstantId<C2>>);

impl<A: Tag, B: Tag> CnstGenPassRetagger<A, B> {
    pub fn new(max: ConstantId<A>) -> Self {
        Self(GenPassRetagger::new(max))
    }
}

impl<C: Tag, C2: Tag> CnstRetagger<C, C2> for CnstGenPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: ConstantId<C>) -> ConstantId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: ConstantId<C>) -> ConstantId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> CnstGenRetagger<C, C2> for CnstGenPassRetagger<C, C2> {
    fn gen(&mut self) -> ConstantId<C2> {
        self.0.core_gen()
    }
}

pub struct CnstMapRetagger<C: Tag, C2: Tag>(MapRetagger<ConstantId<C>, ConstantId<C2>>);

impl<A: Tag, B: Tag> Default for CnstMapRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> CnstRetagger<C, C2> for CnstMapRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: ConstantId<C>) -> ConstantId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: ConstantId<C>) -> ConstantId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> CnstGenRetagger<C, C2> for CnstMapRetagger<C, C2> {
    fn gen(&mut self) -> ConstantId<C2> {
        self.0.core_gen()
    }
}

// === IMPLEMENTATION ===

/// Underlying trait used to implement retagging. Additional traits are added
/// to convert between the various IDs defined. This is used to overcome rust
/// not having HKTs.
trait CoreRetagger {
    type I1: IdCompat;
    type I2: IdCompat;

    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn core_retag_new(&mut self, id: Self::I1) -> Self::I2;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn core_retag_old(&self, id: Self::I1) -> Self::I2;
}

/// Implements completely transparent retaggings. In release mode, this will
/// have no performance impact for retagging. However, it does not provide
/// a way to generate new IDs.
#[derive(Default, Debug)]
struct PassRetagger<I1, I2> {
    #[cfg(debug_assertions)]
    ignore_check: bool,
    #[cfg(debug_assertions)]
    tagged: FxHashSet<usize>,
    ids: PhantomData<(I1, I2)>,
}

impl<I1: IdCompat, I2: IdCompat> CoreRetagger for PassRetagger<I1, I2> {
    type I1 = I1;
    type I2 = I2;

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_old(&self, id: I1) -> I2 {
        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        if !self.ignore_check && !self.tagged.insert(id.raw_value()) {
            panic!("attempted to retag new value {}, value was old", id.value());
        }

        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_old(&self, id: I1) -> I2 {
        if !self.ignore_check && !self.tagged.contains(&id.raw_value()) {
            panic!("attempted to retag old value {}, value was new", id.value());
        }

        I2::raw_new_with_value(id.raw_value())
    }
}

/// Implements completely transparent retaggings, and allows for ID generation.
/// In release mode, this will have no performance impact for retagging. In
/// order to provide transparent retagging and ID generation, the maximum ID to
/// retag must be specified up-front.
struct GenPassRetagger<I1, I2> {
    max: usize,
    counter: usize,
    #[cfg(debug_assertions)]
    tagged: DebugHelper,
    ids: PhantomData<(I1, I2)>,
}

impl<I: IdCompat, I2: IdCompat> GenPassRetagger<I, I2> {
    pub fn new(max: I) -> Self {
        Self {
            max: max.raw_value(),
            counter: max.raw_value() + 1,
            #[cfg(debug_assertions)]
            tagged: Default::default(),
            ids: Default::default(),
        }
    }
}

impl<I1: IdCompat, I2: IdCompat> CoreRetagger for GenPassRetagger<I1, I2> {
    type I1 = I1;
    type I2 = I2;

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_old(&self, id: I1) -> I2 {
        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        if id.raw_value() > self.max {
            panic!(
                "attempted to retag value above maximum, {} > max {}",
                id.value(),
                self.max + 1
            );
        }

        self.tagged.retag_new(id.value());

        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_old(&self, id: I1) -> I2 {
        if id.raw_value() > self.max {
            panic!(
                "attempted to retag value above maximum, {} > max {}",
                id.value(),
                self.max + 1
            );
        }

        self.tagged.retag_old(id.value());

        I2::raw_new_with_value(id.raw_value())
    }
}

impl<I1, I2: IdCompat> GenPassRetagger<I1, I2> {
    fn core_gen(&mut self) -> I2 {
        let value = I2::raw_new_with_value(self.counter);
        self.counter += 1;
        value
    }
}

/// Implements retaggings by maintaining state about what it has mapped, and to
/// what values. This gives it the ability to generate new IDs that have not
/// been mapped while mapping old values, but is not as fast as
/// [`TransparentRetagger`].
#[derive(Default, Debug)]
struct MapRetagger<I1, I2> {
    counter: usize,
    #[cfg(debug_assertions)]
    map: FxHashMap<usize, (usize, &'static Location<'static>)>,
    #[cfg(not(debug_assertions))]
    map: FxHashMap<usize, usize>,
    ids: PhantomData<(I1, I2)>,
}

impl<I1: IdCompat, I2: IdCompat> CoreRetagger for MapRetagger<I1, I2> {
    type I1 = I1;
    type I2 = I2;

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        self.counter += 1;
        let old_value = self.map.insert(id.raw_value(), self.counter);

        if old_value.is_some() {
            panic!("attempted to retag new value {}, value was old", id.value());
        }

        I2::raw_new_with_value(self.counter)
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        self.counter += 1;
        let old_value = self
            .map
            .insert(id.raw_value(), (self.counter, Location::caller()));

        if let Some((_, location)) = old_value {
            panic!(
                "attempted to retag new value {}, value was old. initial retag: {}",
                id.value(),
                location
            );
        }

        I2::raw_new_with_value(self.counter)
    }

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_old(&self, id: I1) -> I2 {
        match self.map.get(&id.raw_value()) {
            Some(id) => I2::raw_new_with_value(*id),
            None => panic!("attempted to retag old value {}, value was new", id.value()),
        }
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_old(&self, id: I1) -> I2 {
        match self.map.get(&id.raw_value()) {
            Some((id, _)) => I2::raw_new_with_value(*id),
            None => panic!("attempted to retag old value {}, value was new", id.value()),
        }
    }
}

impl<I1, I2: IdCompat> MapRetagger<I1, I2> {
    fn core_gen(&mut self) -> I2 {
        loop {
            self.counter += 1;

            // TODO: i can't imagine this actually doing anything
            if self.map.get(&self.counter).is_none() {
                break;
            } else {
                println!("wtf we shouldnt run into this");
            }
        }

        I2::raw_new_with_value(self.counter)
    }
}

/// Provides debug information for retagging purposes
#[derive(Debug, Default)]
struct DebugHelper {
    locations: FxHashMap<usize, &'static Location<'static>>,
}

impl DebugHelper {
    #[track_caller]
    fn retag_new(&mut self, id: usize) {
        if let Some(old_location) = self.locations.insert(id, Location::caller()) {
            panic!(
                "attempted to retag new value {}, value was old. initial retag: {}",
                id, old_location
            )
        }
    }

    #[track_caller]
    fn retag_old(&self, id: usize) {
        if self.locations.get(&id).is_none() {
            panic!("attempted to retag old value {}, value was new", id);
        }
    }
}
