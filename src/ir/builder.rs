use std::sync::atomic::{AtomicUsize, Ordering};

use super::*;

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
            counter: TopLevelId::first(),
        }
    }

    pub fn constant(&mut self, name: Name, payload: Vec<u8>) -> ConstantId {
        let id = self.free_id();

        self.ir.constants.push(Constant { id, payload });

        if let Some(name) = name.0 {
            self.ir.debug_info.top_level_names.insert(id, name);
        }

        ConstantId(id)
    }

    pub fn global(&mut self, name: Name) -> GlobalId {
        let id = self.free_id();

        self.ir.global_variables.push(GlobalVariable { id });

        if let Some(name) = name.0 {
            self.ir.debug_info.top_level_names.insert(id, name);
        }

        GlobalId(id)
    }

    pub fn external_function(&mut self, name: Name, param_types: Vec<Type>) -> ExternalFunctionId {
        let id = self.free_id();

        self.ir.external_functions.push(ExternalFunction {
            id,
            parameters: param_types
                .into_iter()
                .map(|kind| TypedParameter { kind })
                .collect::<Vec<_>>(),
        });

        ExternalFunctionId(id)
    }

    pub fn function(&mut self, name: Name, is_main: bool) -> FunctionBuilder {
        if is_main && self.has_main {
            panic!("Declared a duplicate `main`");
        }

        let id = self.free_id();

        if let Some(name) = name.0 {
            self.ir.debug_info.top_level_names.insert(id, name);
        }

        if is_main {
            self.has_main = true;
        }

        FunctionBuilder::new(FunctionId(id), is_main, self.open_functions.clone())
    }

    pub fn build(self) -> IR {
        if self.open_functions.load(Ordering::Relaxed) == 0 {
            panic!("attempted to build IR - function builder left without calling 'finish'");
        }

        self.ir
    }

    fn free_id(&mut self) -> TopLevelId {
        let id = self.counter;
        self.counter = self.counter.next();
        id
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
            counter: Arc::new(AtomicUsize::new(1)),
            debug_info: HashMap::new(),
            block_counter: BlockId::first(),
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
        builder.ir.functions.push(Function {
            id: self.id.0,
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
        });

        self.id
    }

    fn free_id(&mut self) -> RegisterId {
        let id = self.counter.fetch_add(1, Ordering::Relaxed);
        RegisterId(NonZeroUsize::new(id).unwrap())
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

    pub fn call(&mut self, function: FnRef, arguments: &[FnArg]) -> RegisterId {
        let result = self.free_id();

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
            .push(Instruction::Call(Some(result), callable, arguments));

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
        RegisterId(NonZeroUsize::new(id).unwrap())
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

pub fn ex() {
    let mut program = ProgramBuilder::new();
    let surrounding_agent = program.global(Name::new("surrounding_agent"));
    let print = program.external_function(Name::new("print"), vec![Type::Any]);

    let mut print_stub = program.function(Name::new("print_stub"), false);
    {
        let print_value = print_stub.parameter(Name::new("value"));

        let mut entry = print_stub.block(Name::new("entry"));
        entry.call(FnRef::ExtFn(print), &[FnArg::Reg(print_value)]);
        entry.ret(&mut print_stub, None);
    }

    let mut main = program.function(Name::new("main"), true);
    {
        let mut entry = main.block(Name::new("entry"));

        let hello_world = program.constant(Name::new("hello_world"), "Hello, World!".into());
        entry.call(FnRef::Fn(&print_stub), &[FnArg::Cnst(hello_world)]);

        entry.ret(&mut main, None);
    }

    print_stub.finish(&mut program);
    main.finish(&mut program);

    let ir = program.build();

    println!("{:?}", ir);
}
