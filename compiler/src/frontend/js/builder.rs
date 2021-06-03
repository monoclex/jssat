use crate::frontend::js::ir::*;
use crate::id::*;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantId(TopLevelId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId(TopLevelId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternalFunctionId(TopLevelId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(TopLevelId);

pub struct ProgramBuilder {
    ir: IR,
    open_functions: Arc<AtomicUsize>,
    counter: TopLevelId,
    has_main: bool,
}

/// # Name
///
/// Parameters that accept a [`Name`] were considered to just accept a
/// [`Option<Box<str>>`] directly, so that users could succinctly type [`None`].
/// However, this was decided against as it could lead users into the pitfall
/// of typing out a lengthy `Some("asdf".to_string().into_boxed_str())`, which
/// is exactly the pitfall requiring a [`Name`] is designed to prevent.
///
/// Thus, we believe users will opt to type `Name::` and wait for intellisense.
/// This will guide them into the happy path of typing `Name::new("name")`, or
/// `Name::none()` for no names.
pub struct Name(Option<Box<str>>);

impl Name {
    pub fn new<S: ToString>(name: S) -> Name {
        Self(Some(name.to_string().into_boxed_str()))
    }

    pub fn none() -> Name {
        Self(None)
    }
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            ir: IR::new(),
            open_functions: Arc::new(AtomicUsize::new(0)),
            has_main: false,
            counter: TopLevelId::new(),
        }
    }

    pub fn constant(&mut self, name: Name, payload: Vec<u8>) -> ConstantId {
        let id = self.free_id();
        self.save_name(id, name);

        self.ir.constants.insert(id, Constant { payload });

        ConstantId(id)
    }

    pub fn global(&mut self, name: Name) -> GlobalId {
        let id = self.free_id();
        self.save_name(id, name);

        self.ir.global_variables.insert(id, GlobalVariable {});

        GlobalId(id)
    }

    pub fn external_function(
        &mut self,
        name: Name,
        return_type: Type,
        param_types: Vec<Type>,
    ) -> ExternalFunctionId {
        let id = self.free_id();
        self.save_name(id, name);

        self.ir.external_functions.insert(
            id,
            ExternalFunction {
                return_type,
                parameters: param_types
                    .into_iter()
                    .map(|kind| TypedParameter { kind })
                    .collect::<Vec<_>>(),
            },
        );

        ExternalFunctionId(id)
    }

    pub fn function(&mut self, name: Name, is_main: bool) -> FunctionBuilder {
        if is_main && self.has_main {
            panic!("Declared a duplicate `main`");
        }

        let id = self.free_id();
        self.save_name(id, name);

        if is_main {
            self.has_main = true;
        }

        FunctionBuilder::new(FunctionId(id), is_main, self.open_functions.clone())
    }

    pub fn build(self) -> IR {
        if self.open_functions.load(Ordering::Relaxed) != 0 {
            panic!("attempted to build IR - function builder left without calling 'finish'");
        }

        self.ir
    }

    fn free_id(&mut self) -> TopLevelId {
        let id = self.counter;
        self.counter = self.counter.next();
        id
    }

    fn save_name(&mut self, id: TopLevelId, name: Name) {
        if let Some(name) = name.0 {
            self.ir.debug_info.top_level_names.insert(id, name);
        }
    }
}

pub struct FunctionBuilder {
    id: FunctionId,
    is_main: bool,
    parameters: Vec<Parameter>,
    blocks: Vec<FunctionBlock>,
    // rather than use a RegisterId we use an Arc<AtomicUsize> so we can pass
    // it to BlockBuilders so they can get registers too
    counter: Arc<AtomicUsize>,
    debug_info: HashMap<RegisterId, Box<str>>,
    block_counter: BlockId,
    block_debug_info: HashMap<BlockId, Box<str>>,
    hold_open: Arc<AtomicUsize>,
}

impl FunctionBuilder {
    pub fn new(id: FunctionId, is_main: bool, hold_open: Arc<AtomicUsize>) -> Self {
        hold_open.fetch_add(1, Ordering::Relaxed);

        Self {
            id,
            is_main,
            parameters: vec![],
            blocks: vec![],
            counter: Arc::new(AtomicUsize::new(RegisterId::new().value())),
            debug_info: HashMap::new(),
            block_counter: BlockId::new(),
            block_debug_info: HashMap::new(),
            hold_open,
        }
    }

    pub fn parameter(&mut self, name: Name) -> RegisterId {
        let id = self.free_id();
        self.parameters.push(Parameter { register: id });

        if let Some(name) = name.0 {
            self.debug_info.insert(id, name);
        }

        id
    }

    pub fn block(&mut self, name: Name) -> BlockBuilder {
        let id = self.free_block_id();

        if let Some(name) = name.0 {
            self.block_debug_info.insert(id, name);
        }

        BlockBuilder::new(id, self.counter.clone())
    }

    pub fn finish(self, builder: &mut ProgramBuilder) -> FunctionId {
        self.hold_open.fetch_sub(1, Ordering::Relaxed);

        // builder.ir.functions.insert(index, element)
        builder.ir.functions.insert(
            self.id.0,
            Function {
                parameters: self.parameters,
                body: FunctionBody {
                    blocks: self.blocks,
                    debug_info: FunctionBodyDebugInfo {
                        block_names: self.block_debug_info,
                    },
                },
                debug_info: FunctionDebugInfo {
                    register_names: self.debug_info,
                },
                is_main: self.is_main,
            },
        );

        self.id
    }

    fn free_id(&mut self) -> RegisterId {
        let id = self.counter.fetch_add(1, Ordering::Relaxed);
        RegisterId::new_with_value(id)
    }

    fn free_block_id(&mut self) -> BlockId {
        let id = self.block_counter;
        self.block_counter = self.block_counter.next();
        id
    }
}

pub struct BlockBuilder {
    id: BlockId,
    register_counter: Arc<AtomicUsize>,
    instructions: Vec<Instruction>,
    debug_info: HashMap<RegisterId, Box<str>>,
}

impl BlockBuilder {
    pub fn new(id: BlockId, register_counter: Arc<AtomicUsize>) -> Self {
        Self {
            id,
            register_counter,
            instructions: vec![],
            debug_info: HashMap::new(),
        }
    }

    pub fn call(
        &mut self,
        function: FnRef,
        arguments: &[FnArg],
        store: bool,
    ) -> Option<RegisterId> {
        let result = if store { Some(self.free_id()) } else { None };

        let callable = match function {
            FnRef::Fn(fn_builder) => Callable::GlobalFunction(fn_builder.id.0),
            FnRef::ExtFn(id) => Callable::GlobalFunction(id.0),
        };

        let arguments = arguments
            .into_iter()
            .map(|a| match *a {
                FnArg::Cnst(constant_id) => Value::Constant(constant_id.0),
                FnArg::Number(number) => Value::Number(number),
                FnArg::Reg(register) => Value::Register(register),
            })
            .collect::<Vec<_>>();

        self.instructions
            .push(Instruction::Call(result, callable, arguments));

        result
    }

    pub fn ret(mut self, builder: &mut FunctionBuilder, argument: Option<RegisterId>) {
        self.instructions.push(Instruction::Ret(argument));
        self.finish(builder)
    }

    fn finish(self, builder: &mut FunctionBuilder) {
        if !Arc::ptr_eq(&builder.counter, &self.register_counter) {
            panic!("specified wrong function")
        }

        builder.debug_info.extend(self.debug_info);
        builder.blocks.push(FunctionBlock {
            id: self.id,
            instructions: self.instructions,
        });
    }

    fn free_id(&self) -> RegisterId {
        let id = self.register_counter.fetch_add(1, Ordering::Relaxed);
        RegisterId::new_with_value(id)
    }
}

pub enum FnRef<'function> {
    Fn(&'function FunctionBuilder),
    ExtFn(ExternalFunctionId),
}

pub enum FnArg {
    Reg(RegisterId),
    Cnst(ConstantId),
    Number(f64),
}
