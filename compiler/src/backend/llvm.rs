use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::TargetData,
};
use rustc_hash::FxHashMap;
use std::{hash::Hash, marker::PhantomData};

use super::BuildArtifact;

type BlockId = crate::id::BlockId<crate::id::LlvmCtx>;
type FunctionId = crate::id::FunctionId<crate::id::LlvmCtx>;
type ConstantId = crate::id::ConstantId<crate::id::LlvmCtx>;
type RegisterId = crate::id::RegisterId<crate::id::LlvmCtx>;
type OpaqueStructId = crate::id::OpaqueStructId<crate::id::LlvmCtx>;
type ExternalFunctionId = crate::id::ExternalFunctionId<crate::id::LlvmCtx>;

#[cfg(feature = "link-llvm")]
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, IntType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntValue},
    AddressSpace, OptimizationLevel,
};

#[cfg(not(feature = "link-llvm"))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FunctionValue<'a>(PhantomData<&'a ()>);

#[derive(Debug, Clone)]
pub struct BackendIR<'name> {
    pub constants: FxHashMap<ConstantId, Constant<'name>>,
    pub opaque_structs: FxHashMap<OpaqueStructId, OpaqueStruct<'name>>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>, // <'name>>,
    pub functions: FxHashMap<FunctionId, Function<'name>>,
}

#[derive(Debug, Clone)]
pub struct Constant<'name> {
    pub name: &'name str,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct OpaqueStruct<'name> {
    pub name: &'name str,
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    // <'name> {
    // pub name: &'name str,
    // TODO: fix the ugly hack (reason being we add external fns during the
    // translation of IR + TypeAnnotations -> BackendIr)
    pub name: String,
    pub return_type: ReturnType,
    pub parameters: Vec<ValueType>,
}

#[derive(Debug, Clone)]
pub struct Function<'name> {
    pub name: &'name str,
    pub linkage: Option<LLVMLinkage>,
    pub return_type: ReturnType,
    pub parameters: Vec<Parameter>,
    pub entry_block: BlockId,
    pub blocks: FxHashMap<BlockId, Vec<Instruction>>,
}

#[derive(Debug, Clone)]
pub struct PartialFunction<'ctx> {
    pub llvm: FunctionValue<'ctx>,
    pub parameters: Vec<RegisterId>,
    pub entry_block: BlockId,
    pub blocks: FxHashMap<BlockId, Vec<Instruction>>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub r#type: ValueType,
    pub register: RegisterId,
}

#[derive(Debug, Clone)]
pub enum NumberValue {
    UnsignedNative(usize),
    SignedNative(isize),
    /// bits, value
    UnsignedArbitrary(u16, u64),
    /// bits, value
    SignedArbitrary(u16, i64),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// # [`Instruction::LoadConstantPtr`]
    ///
    /// Loads the value of the constant as a `i8*` into the register specified.
    LoadConstantPtr(RegisterId, ConstantId),
    /// # [`Instruction::ChangePtr`]
    ///
    /// Given an input pointer, will bitcase it to the pointer of the desired size.
    ChangePtrSize {
        result: RegisterId,
        input: RegisterId,
        size: ValueType,
    },
    /// # [`Instruction::LoadConstantLen`]
    ///
    /// Loads the length of the payload of the constant as a word-sized valaue
    /// into the register specified.
    LoadConstantLen(RegisterId, ConstantId),
    /// # [`Instruction::LoadNumber`]
    ///
    /// Loads a number into a register.
    LoadNumber {
        result: RegisterId,
        value: NumberValue,
    },
    /// # [`Instruction::MathDivide`]
    ///
    /// Divides the register by the input.
    MathDivide {
        result: RegisterId,
        dividend: RegisterId,
        divisor: RegisterId,
    },
    /// # [`Instruction::Unreachable`]
    ///
    /// Indicates that it is impossible for the current path of execution to
    /// reach this instruction. It is undefined behavior if the code reaches
    /// this instruction.
    Unreachable,
    Call(Option<RegisterId>, Callable, Vec<RegisterId>),
    /// # [`Instruction::Return`]
    ///
    /// Returns the value in the register to the caller, or returns nothing if
    /// no register is given.
    Return(Option<RegisterId>),
    /// # [`Instruction::Jump`]
    ///
    /// Jumps to the given block register.
    Jump(BlockId),
    /// # [`Instruction::JumpIf`]
    ///
    /// Jumps to the given block register if the register specified is true,
    /// otherwise jumps to the false branch.
    JumpIf {
        condition: RegisterId,
        true_path: BlockId,
        false_path: BlockId,
    },
    /// # [`Instruction::Phi`]
    ///
    /// Will choose a value for the given register based on the value of
    /// another register.
    Phi(RegisterId, Vec<(BlockId, RegisterId)>),
}

#[derive(Debug, Clone)]
pub enum Callable {
    External(ExternalFunctionId),
    Static(FunctionId),
    // Virtual(RegisterId),
}

pub type ReturnType = crate::frontend::ir::Returns<ValueType>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    WordSizeBitType,
    BitType(u16),
    Opaque(OpaqueStructId),
    Pointer(Box<ValueType>),
}

enum SizeLevel {
    NoSizeOptimization = 0,
    Os = 1,
    Oz = 2,
}

impl ValueType {
    pub fn into_ptr(self) -> ValueType {
        ValueType::Pointer(box self)
    }
}

#[derive(Debug, Clone)]
pub enum LLVMLinkage {
    External,
}

impl LLVMLinkage {
    #[cfg(feature = "link-llvm")]
    pub fn to_llvm(&self) -> Linkage {
        match self {
            LLVMLinkage::External => Linkage::External,
        }
    }
}

#[cfg(not(feature = "link-llvm"))]
pub fn target_triplet() -> String {
    "not implemented".into()
}

#[cfg(feature = "link-llvm")]
pub fn target_triplet() -> String {
    TargetMachine::get_default_triple()
        .as_str()
        .to_str()
        .unwrap()
        .to_owned()
}

#[cfg(not(feature = "link-llvm"))]
pub fn compile(_ir: BackendIR) -> BuildArtifact {
    panic!("link-llvm not enabled");
}

#[cfg(feature = "link-llvm")]
pub fn compile(ir: BackendIR) -> BuildArtifact {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("jssat");

    #[cfg(target_pointer_width = "32")]
    let word_size = context.i32_type();
    #[cfg(target_pointer_width = "64")]
    let word_size = context.i64_type();

    let compiler = BackendCompiler {
        context: &context,
        builder,
        module: &module,
        word_size,
    };

    let mut constants = FxHashMap::default();
    for (id, constant) in ir.constants.into_iter() {
        constants.insert(
            id,
            (constant.payload.len(), compiler.llvm_constant(constant)),
        );
    }
    let constant_resolver = ConstantResolver { things: &constants };

    let mut opaque_structs = FxHashMap::default();
    for (id, opaque_struct) in ir.opaque_structs.into_iter() {
        opaque_structs.insert(id, compiler.llvm_opaque_struct(opaque_struct));
    }
    let opaque_struct_resolver = OpaqueStructResolver {
        things: &opaque_structs,
    };

    let mut external_functions = FxHashMap::default();
    for (id, external_function) in ir.external_functions.into_iter() {
        let external_function =
            compiler.llvm_external_function(external_function, &opaque_struct_resolver);
        external_functions.insert(id, external_function);
    }
    let external_function_resolver = ExternalFunctionResolver {
        things: &external_functions,
    };

    let mut functions = FxHashMap::default();
    for (id, function) in ir.functions.into_iter() {
        let function = compiler.llvm_function_start(function, &opaque_struct_resolver);
        functions.insert(id, function);
    }

    let function_types = functions
        .iter()
        .map(|(k, v)| (*k, v.llvm))
        .collect::<FxHashMap<_, _>>();
    let function_resolver = FunctionResolver {
        things: &function_types,
    };

    for (_, function) in functions {
        compiler.llvm_function_end(
            function,
            &constant_resolver,
            &opaque_struct_resolver,
            &external_function_resolver,
            &function_resolver,
        );
    }

    #[cfg(debug_assertions)]
    {
        // print llvm ir incase llvm segfaults while compiling
        let text_buff = module.print_to_string().to_string();
        println!(
            "EARLY LLVM IR:\n=== LLVM START\n{}\n=== LLVM END",
            text_buff
        );
    }

    // do some LLVM opts
    let pass_mgr = PassManagerBuilder::create();
    pass_mgr.set_optimization_level(OptimizationLevel::Aggressive);
    pass_mgr.set_size_level(SizeLevel::NoSizeOptimization as u32);

    let fpm = PassManager::create(&module);
    pass_mgr.populate_function_pass_manager(&fpm);

    let lpm = PassManager::create(());
    pass_mgr.populate_lto_pass_manager(&lpm, true, true);
    pass_mgr.populate_module_pass_manager(&lpm);

    while lpm.run_on(&module) {}

    #[cfg(debug_assertions)]
    {
        // print llvm ir incase llvm segfaults while compiling
        let text_buff = module.print_to_string().to_string();
        println!(
            "OPTIMIZED EARLY LLVM IR:\n=== LLVM START\n{}\n=== LLVM END",
            text_buff
        );
    }

    // do actual LLVM compilation

    Target::initialize_all(&Default::default());
    let target_triple = TargetMachine::get_default_triple();

    let target = Target::from_triple(&target_triple).unwrap();

    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("couldn't make target machine");

    let text_buff = module.print_to_string().to_string();

    let obj_buff = target_machine
        .write_to_memory_buffer(&module, FileType::Object)
        .expect("couldn't compile to assembly");

    BuildArtifact {
        llvm_ir: text_buff,
        obj: obj_buff.as_slice().to_vec(),
    }
}

#[cfg(feature = "link-llvm")]
type ConstantResolver<'structs, 'ctx> = Resolver<'structs, ConstantId, (usize, GlobalValue<'ctx>)>;

#[cfg(feature = "link-llvm")]
type OpaqueStructResolver<'structs, 'ctx> = Resolver<'structs, OpaqueStructId, StructType<'ctx>>;

#[cfg(feature = "link-llvm")]
type ExternalFunctionResolver<'structs, 'ctx> =
    Resolver<'structs, ExternalFunctionId, FunctionValue<'ctx>>;

#[cfg(feature = "link-llvm")]
type FunctionResolver<'structs, 'ctx> = Resolver<'structs, FunctionId, FunctionValue<'ctx>>;

#[cfg(feature = "link-llvm")]
struct Resolver<'map, K, V> {
    things: &'map FxHashMap<K, V>,
}

#[cfg(feature = "link-llvm")]
impl<K, V> Resolver<'_, K, V>
where
    K: Hash + Eq,
    V: Copy,
{
    pub fn resolve(&self, id: &K) -> V {
        *self.things.get(id).expect("expected thing at key")
    }
}

#[cfg(feature = "link-llvm")]
struct BackendCompiler<'ctx, 'module> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: &'module Module<'ctx>,
    word_size: IntType<'ctx>,
}

#[cfg(feature = "link-llvm")]
impl<'c> BackendCompiler<'c, '_> {
    pub fn llvm_constant(&self, constant: Constant) -> GlobalValue<'c> {
        let raw_const = self.constant_payload_to_basic_value(constant.payload);

        let global = self.module.add_global(
            raw_const.get_type(),
            Some(AddressSpace::Generic),
            constant.name,
        );

        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.set_constant(true);
        global.set_initializer(&raw_const);

        global
    }

    fn constant_payload_to_basic_value(&self, payload: Vec<u8>) -> BasicValueEnum<'c> {
        match std::str::from_utf8(payload.as_slice()) {
            Ok(str) => self
                .context
                .const_string(str.as_bytes(), false)
                .as_basic_value_enum(),
            Err(_) => {
                let byte_values = payload
                    .into_iter()
                    .map(|n| self.context.i8_type().const_int(n as u64, false))
                    .collect::<Vec<_>>();

                self.context
                    .i8_type()
                    .const_array(byte_values.as_slice())
                    .as_basic_value_enum()
            }
        }
    }

    pub fn llvm_opaque_struct(&self, opaque_struct: OpaqueStruct) -> StructType<'c> {
        self.context.opaque_struct_type(opaque_struct.name)
    }

    pub fn llvm_external_function(
        &self,
        external_function: ExternalFunction,
        opaque_struct_resolver: &OpaqueStructResolver<'_, 'c>,
    ) -> FunctionValue<'c> {
        let parameter_types = external_function
            .parameters
            .into_iter()
            .map(|v| self.llvm_typeify_value(v, &opaque_struct_resolver))
            .collect::<Vec<_>>();

        let parameter_types = parameter_types.as_slice();

        let function = match external_function.return_type {
            ReturnType::Void => self.context.void_type().fn_type(parameter_types, false),
            ReturnType::Value(v) => self
                .llvm_typeify_value(v, &opaque_struct_resolver)
                .fn_type(parameter_types, false),
        };

        self.module.add_function(
            external_function.name.as_str(),
            function,
            Some(Linkage::External),
        )
    }

    pub fn llvm_function_start(
        &self,
        function: Function,
        opaque_struct_resolver: &OpaqueStructResolver<'_, 'c>,
    ) -> PartialFunction<'c> {
        let mut parameter_registers = Vec::with_capacity(function.parameters.len());

        let parameter_types = function
            .parameters
            .into_iter()
            .map(|v| {
                parameter_registers.push(v.register);
                self.llvm_typeify_value(v.r#type, &opaque_struct_resolver)
            })
            .collect::<Vec<_>>();

        let parameter_types = parameter_types.as_slice();

        let llvm_function = match function.return_type {
            ReturnType::Void => self.context.void_type().fn_type(parameter_types, false),
            ReturnType::Value(v) => self
                .llvm_typeify_value(v, &opaque_struct_resolver)
                .fn_type(parameter_types, false),
        };

        PartialFunction {
            llvm: self.module.add_function(
                function.name,
                llvm_function,
                function.linkage.map(|l| l.to_llvm()),
            ),
            parameters: parameter_registers,
            entry_block: function.entry_block,
            blocks: function.blocks,
        }
    }

    pub fn llvm_function_end(
        &self,
        mut function: PartialFunction<'c>,
        constant_resolver: &ConstantResolver<'_, 'c>,
        opaque_struct_resolver: &OpaqueStructResolver<'_, 'c>,
        external_function_resolver: &ExternalFunctionResolver<'_, 'c>,
        function_resolver: &FunctionResolver<'_, 'c>,
    ) {
        println!("llvm_function_end: {:?} -> {:#?}", function, function);

        let entry_block_id = function.entry_block;
        let entry_block = (
            entry_block_id,
            function
                .blocks
                .remove(&entry_block_id)
                .expect("entry block"),
        );
        let non_entry_blocks = function.blocks.into_iter();

        let mut register_values = FxHashMap::default();

        for (idx, parameter) in function.parameters.iter().enumerate() {
            register_values.insert(*parameter, function.llvm.get_nth_param(idx as u32).unwrap());
        }

        let blocks = std::iter::once(entry_block)
            .chain(non_entry_blocks)
            .collect::<Vec<_>>();

        let mut block_map = FxHashMap::default();
        for (idx, block) in blocks.iter() {
            let basic_block = self.context.append_basic_block(function.llvm, "");
            block_map.insert(*idx, basic_block);
        }

        for (idx, block) in blocks.into_iter() {
            let basic_block = *block_map.get(&idx).unwrap();
            self.builder.position_at_end(basic_block);

            for instruction in block.into_iter() {
                match instruction {
                    Instruction::Call(result, function, args) => {
                        let llvm_callable = match function {
                            Callable::External(id) => external_function_resolver.resolve(&id),
                            Callable::Static(id) => function_resolver.resolve(&id),
                            // Callable::Virtual(_) => todo!(),
                        };

                        let o_args = args.clone();
                        let args = args
                            .clone()
                            .into_iter()
                            .map(|r| {
                                *register_values
                                    .get(&r)
                                    .expect(&format!("error calling {:?}, {:?}", r, args))
                            })
                            .collect::<Vec<_>>();

                        println!(
                            "calling {:?} with {:?} :: {:?}",
                            llvm_callable, args, o_args
                        );
                        let llvm_result =
                            self.builder.build_call(llvm_callable, args.as_slice(), "");

                        if let Some(result) = result {
                            register_values.insert(
                                result,
                                llvm_result
                                    .try_as_basic_value()
                                    .left()
                                    .expect("expected value"),
                            );
                        }
                    }
                    Instruction::LoadConstantPtr(result, constant) => {
                        let (_, global) = constant_resolver.resolve(&constant);

                        // if you think i know what i'm doing, you'd be absolutely incorrect

                        // BLACK MAGIC START
                        let ptr = global.as_pointer_value();
                        let get_element_ptr = unsafe {
                            ptr.const_in_bounds_gep(&[self.context.i64_type().const_int(0, false)])
                        };

                        let get_element_ptr = self.builder.build_bitcast(
                            get_element_ptr,
                            self.context.i8_type().ptr_type(AddressSpace::Generic),
                            "",
                        );
                        // BLACK MAGIC END

                        register_values.insert(result, get_element_ptr);
                    }
                    Instruction::ChangePtrSize {
                        result,
                        input,
                        size,
                    } => {
                        let source_ptr_type = register_values.get(&input).unwrap();
                        let target_ptr_type = self
                            .llvm_typeify_value(size, &opaque_struct_resolver)
                            .ptr_type(AddressSpace::Generic);

                        let casted =
                            self.builder
                                .build_bitcast(*source_ptr_type, target_ptr_type, "");

                        register_values.insert(result, casted);
                    }
                    Instruction::LoadNumber { result, value } => {
                        let number = self.llvm_manifest_number(value);
                        register_values.insert(result, number.as_basic_value_enum());
                    }
                    Instruction::MathDivide {
                        result,
                        dividend,
                        divisor,
                    } => {
                        let lhs = register_values.get(&dividend).unwrap();
                        let rhs = register_values.get(&divisor).unwrap();

                        debug_assert!(lhs.is_int_value());
                        debug_assert!(rhs.is_int_value());

                        let lhs = lhs.into_int_value();
                        let rhs = rhs.into_int_value();

                        let division = self.builder.build_int_unsigned_div(lhs, rhs, "");
                        register_values.insert(result, division.as_basic_value_enum());
                    }
                    Instruction::LoadConstantLen(result, constant) => {
                        let (len, _) = constant_resolver.resolve(&constant);

                        register_values
                            .insert(result, self.word_size.const_int(len as u64, false).into());
                    }
                    Instruction::Unreachable => {
                        self.builder.build_unreachable();
                    }
                    Instruction::Return(None) => {
                        self.builder.build_return(None);
                    }
                    Instruction::Return(Some(register)) => {
                        let value = *register_values.get(&register).unwrap();
                        self.builder.build_return(Some(&value));
                    }
                    Instruction::Jump(block) => {
                        let block = *block_map.get(&block).unwrap();
                        self.builder.build_unconditional_branch(block);
                    }
                    Instruction::JumpIf {
                        condition,
                        true_path,
                        false_path,
                    } => {
                        let condition = register_values.get(&condition).unwrap();
                        debug_assert!(condition.is_int_value());
                        let condition = condition.into_int_value();

                        let then_block = *block_map.get(&true_path).unwrap();
                        let else_block = *block_map.get(&false_path).unwrap();

                        self.builder
                            .build_conditional_branch(condition, then_block, else_block);
                    }
                    Instruction::Phi(result, implications) => {
                        let mut register_types = (implications.iter())
                            .map(|(_, r)| *register_values.get(r).unwrap())
                            .collect::<Vec<_>>();

                        for (prev_typ, next_typ) in
                            register_types.iter().zip(register_types.iter().skip(1))
                        {
                            if prev_typ.get_type() != next_typ.get_type() {
                                panic!("phi instruction has registers of conflicting types: {:?} <-> {:?} for {:?}", prev_typ, next_typ, Instruction::Phi(result, implications));
                            }
                        }

                        let typ = register_types.pop().unwrap();
                        register_values.insert(result, typ);

                        let phi = self.builder.build_phi(typ.get_type(), "");

                        let incoming = (implications.into_iter())
                            .map(|(block_id, register_id)| {
                                (
                                    block_map.get(&block_id).unwrap(),
                                    register_values.get(&register_id).unwrap(),
                                )
                            })
                            .map(|(block, register)| (register.as_basic_value_enum(), *block))
                            .collect::<Vec<_>>();

                        let incoming = incoming
                            .iter()
                            .map(|(reg, blk)| (reg as &dyn BasicValue, *blk))
                            .collect::<Vec<_>>();

                        phi.add_incoming(incoming.as_slice());
                    }
                }
            }
        }
    }

    fn llvm_manifest_number(&self, number_value: NumberValue) -> IntValue<'c> {
        match number_value {
            NumberValue::UnsignedNative(v) => self.word_size.const_int(v as u64, false),
            NumberValue::SignedNative(v) => self.word_size.const_int(v as u64, true),
            NumberValue::UnsignedArbitrary(bits, v) => self
                .context
                .custom_width_int_type(bits as u32)
                .const_int(v, false),
            NumberValue::SignedArbitrary(bits, v) => self
                .context
                .custom_width_int_type(bits as u32)
                .const_int(v as u64, true),
        }
    }

    fn llvm_typeify_value(
        &self,
        value_type: ValueType,
        opaque_struct_resolver: &OpaqueStructResolver<'_, 'c>,
    ) -> BasicTypeEnum<'c> {
        match value_type {
            ValueType::Pointer(inner) => self
                .llvm_typeify_value(*inner, &opaque_struct_resolver)
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
            ValueType::WordSizeBitType => self.word_size.as_basic_type_enum(),
            ValueType::Opaque(id) => opaque_struct_resolver.resolve(&id).as_basic_type_enum(),
            ValueType::BitType(bits) => self
                .context
                .custom_width_int_type(bits as u32)
                .as_basic_type_enum(),
        }
    }
}
