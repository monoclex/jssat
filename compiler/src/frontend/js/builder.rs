use crate::frontend::js::ir::*;
use crate::id::*;
use crate::name::DebugName;
use crate::name::Name;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};

pub struct ProgramBuilder {
    ir: IR,
    open_functions: Arc<AtomicUsize>,
    counter: TopLevelId,
    has_main: bool,
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

    pub fn constant(&mut self, name: DebugName, payload: Vec<u8>) -> ConstantId {
        let id = self.free_id();

        self.ir.constants.insert(id, Constant { name, payload });

        id.convert()
    }

    pub fn global(&mut self, name: DebugName) -> GlobalId {
        let id = self.free_id();

        self.ir.global_variables.insert(id, GlobalVariable { name });

        id.convert()
    }

    pub fn external_function(
        &mut self,
        name: Name,
        return_type: Type,
        param_types: Vec<Type>,
    ) -> ExternalFunctionId {
        let id = self.free_id();

        self.ir.external_functions.insert(
            id,
            ExternalFunction {
                name,
                return_type,
                parameters: param_types
                    .into_iter()
                    .map(|kind| TypedParameter { kind })
                    .collect::<Vec<_>>(),
            },
        );

        id.convert()
    }

    pub fn function(&mut self, name: DebugName, is_main: bool) -> FunctionBuilder {
        if is_main && self.has_main {
            panic!("Declared a duplicate `main`");
        }

        let id = self.free_id();

        if is_main {
            self.has_main = true;
        }

        FunctionBuilder::new(name, id.convert(), is_main, self.open_functions.clone())
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
}

pub struct FunctionBuilder {
    id: FunctionId,
    is_main: bool,
    name: DebugName,
    parameters: Vec<Parameter>,
    blocks: Vec<FunctionBlock>,
    // rather than use a RegisterId we use an Arc<AtomicUsize> so we can pass
    // it to BlockBuilders so they can get registers too
    counter: Arc<AtomicUsize>,
    block_counter: BlockId,
    hold_open: Arc<AtomicUsize>,
}

impl FunctionBuilder {
    pub fn new(
        name: DebugName,
        id: FunctionId,
        is_main: bool,
        hold_open: Arc<AtomicUsize>,
    ) -> Self {
        hold_open.fetch_add(1, Ordering::Relaxed);

        Self {
            id,
            is_main,
            name,
            parameters: vec![],
            blocks: vec![],
            counter: Arc::new(AtomicUsize::new(RegisterId::new().value())),
            block_counter: BlockId::new(),
            hold_open,
        }
    }

    pub fn parameter(&mut self, name: DebugName) -> RegisterId {
        let register = self.free_id();

        self.parameters.push(Parameter { name, register });

        register
    }

    pub fn block(&mut self, name: DebugName) -> BlockBuilder {
        let id = self.free_block_id();

        BlockBuilder::new(name, id, self.counter.clone())
    }

    pub fn finish(self, builder: &mut ProgramBuilder) -> FunctionId {
        self.hold_open.fetch_sub(1, Ordering::Relaxed);

        // builder.ir.functions.insert(index, element)
        builder.ir.functions.insert(
            self.id.convert(),
            Function {
                name: self.name,
                parameters: self.parameters,
                body: FunctionBody {
                    blocks: self.blocks,
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
    name: DebugName,
    id: BlockId,
    register_counter: Arc<AtomicUsize>,
    instructions: Vec<Instruction>,
}

impl BlockBuilder {
    pub fn new(name: DebugName, id: BlockId, register_counter: Arc<AtomicUsize>) -> Self {
        Self {
            name,
            id,
            register_counter,
            instructions: vec![],
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
            FnRef::Fn(fn_builder) => Callable::GlobalFunction(fn_builder.id.convert()),
            FnRef::ExtFn(id) => Callable::GlobalFunction(id.convert()),
        };

        let arguments = arguments
            .into_iter()
            .map(|a| match *a {
                FnArg::Runtime => Value::Runtime,
                FnArg::Cnst(constant_id) => Value::Constant(constant_id.convert()),
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

        builder.blocks.push(FunctionBlock {
            name: self.name,
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
    Runtime,
    Reg(RegisterId),
    Cnst(ConstantId),
    Number(f64),
}
