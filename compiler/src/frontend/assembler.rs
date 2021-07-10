//! Assembles together the informatino from the original IR, and type_annotator
//! passes into a fully conherent IR, which will then be passed to `skeleton`.

use std::{collections::VecDeque, sync::Mutex};

use rustc_hash::FxHashMap;

use super::{
    conv_only_bb::PureBlocks,
    ir,
    ir::{FFIValueType, IR},
    type_annotater::{self, AnnotatedBlock, PureAnnotatedBlocks, SymbolicEngine, ValueType},
};
use crate::{
    frontend::{
        ir::BasicBlockJump,
        type_annotater::{BlockExecutionKey, ExplorationBranch},
    },
    id::*,
    UnwrapNone,
};

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
    pub parameters: Vec<Type>,
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
    pub parameters: Vec<Parameter>,
    pub instructions: Vec<Instruction>,
    pub end: EndInstruction,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub typ: Type,
    // TODO: maybe parameters should implicitly get the register accoridng to
    // their index? it makes sense not to do this in the other IRs because of
    // mangling parameters, but here we have pretty much all the information
    // necessary to craft a final product
    pub register: RegisterId<AssemblerCtx>,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Call(
        Option<RegisterId<AssemblerCtx>>,
        Callable,
        Vec<RegisterId<AssemblerCtx>>,
    ),
    GetRuntime(RegisterId<AssemblerCtx>),
    MakeString(RegisterId<AssemblerCtx>, ConstantId<AssemblerCtx>),
    MakeNumber(RegisterId<AssemblerCtx>, i64),
    MakeBoolean(RegisterId<AssemblerCtx>, bool),
    /// "Widens" a given register to a type. The type must wider/bigger than the
    /// input register.
    Widen {
        result: RegisterId<AssemblerCtx>,
        input: RegisterId<AssemblerCtx>,
        from: Type,
        to: Type,
    },
    Unreachable,
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
    Value(Type),
}

#[derive(Clone, Debug)]
pub enum Type {
    FFI(FFIValueType),
    Val(ValueType),
}

pub fn assemble(ir: &IR, engine: SymbolicEngine) -> Program {
    Assembler::new(ir, engine).assemble()
}

fn map_ext_fn(exernal_function: ir::ExternalFunction) -> ExternalFunction {
    ExternalFunction {
        name: exernal_function.name,
        parameters: exernal_function
            .parameters
            .into_iter()
            .map(ir::FFIValueType::into_type)
            .collect(),
        returns: exernal_function.return_type.into_type(),
    }
}

impl type_annotater::ReturnType {
    fn into_type(self) -> ReturnType {
        match self {
            type_annotater::ReturnType::Void => ReturnType::Void,
            type_annotater::ReturnType::Value(v) => ReturnType::Value(v.into_type()),
            // when performing codegen for a function that never returns, we will have already
            // transformed the function into the appropriate code to quit after dealing with
            // a never type, and if we don't return anything might as well return void (nothing)
            type_annotater::ReturnType::Never => ReturnType::Void,
        }
    }
}

impl ValueType {
    fn into_type(self) -> Type {
        Type::Val(self)
    }
}

impl ir::FFIReturnType {
    fn into_type(self) -> ReturnType {
        match self {
            ir::FFIReturnType::Void => ReturnType::Void,
            ir::FFIReturnType::Value(v) => ReturnType::Value(v.into_type()),
        }
    }
}

impl ir::FFIValueType {
    fn into_type(self) -> Type {
        Type::FFI(self)
    }
}

pub struct UniqueFnIdGen {
    gen: Counter<FunctionId<AssemblerCtx>>,
    map: Mutex<FxHashMap<BlockId<AnnotatedCtx>, FunctionId<AssemblerCtx>>>,
}

impl Default for UniqueFnIdGen {
    fn default() -> Self {
        Self {
            gen: Default::default(),
            map: Default::default(),
        }
    }
}

impl UniqueFnIdGen {
    pub fn map(&self, block_id: BlockId<AnnotatedCtx>) -> FunctionId<AssemblerCtx> {
        let mut map = self.map.lock().unwrap();
        *map.entry(block_id).or_insert_with(|| self.gen.next())
    }
}

pub struct UniqueBlkIdGen {
    gen: Counter<BlockId<AssemblerCtx>>,
    map: Mutex<FxHashMap<BlockId<AnnotatedCtx>, BlockId<AssemblerCtx>>>,
}

impl Default for UniqueBlkIdGen {
    fn default() -> Self {
        Self {
            gen: Default::default(),
            map: Default::default(),
        }
    }
}

impl UniqueBlkIdGen {
    pub fn map(&self, block_id: BlockId<AnnotatedCtx>) -> BlockId<AssemblerCtx> {
        let mut map = self.map.lock().unwrap();
        *map.entry(block_id).or_insert_with(|| self.gen.next())
    }
}

pub struct UniqueConstantIdGen {
    gen: Counter<ConstantId<AssemblerCtx>>,
    map: Mutex<FxHashMap<ConstantId<IrCtx>, ConstantId<AssemblerCtx>>>,
    vals: Mutex<FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>>,
}

impl Default for UniqueConstantIdGen {
    fn default() -> Self {
        Self {
            gen: Default::default(),
            map: Default::default(),
            vals: Default::default(),
        }
    }
}

impl UniqueConstantIdGen {
    pub fn map(&self, block_id: ConstantId<IrCtx>, payload: &Vec<u8>) -> ConstantId<AssemblerCtx> {
        let mut map = self.map.lock().unwrap();
        let const_id = *map.entry(block_id).or_insert_with(|| self.gen.next());

        let mut vals = self.vals.lock().unwrap();
        vals.entry(const_id).or_insert_with(|| payload.clone());

        const_id
    }
}

struct Assembler<'ir> {
    ir: &'ir IR,
    annotated_blocks: PureAnnotatedBlocks,
    //
    constant_map: UniqueConstantIdGen,
    external_functions: FxHashMap<ExternalFunctionId<AssemblerCtx>, ExternalFunction>,
    fn_id_gen: UniqueFnIdGen,
    //
    function_ids:
        FxHashMap<(FunctionId<IrCtx>, BlockId<IrCtx>, Vec<ValueType>), FunctionId<AssemblerCtx>>,
}

impl<'ir> Assembler<'ir> {
    pub fn new(ir: &'ir IR, engine: SymbolicEngine) -> Self {
        let external_functions = (ir.external_functions.iter())
            .map(|(k, v)| (k.map_context::<AssemblerCtx>(), map_ext_fn(v.clone())))
            .collect::<FxHashMap<_, _>>();

        let annotated_blocks = engine.extract();

        Self {
            ir,
            annotated_blocks,
            constant_map: Default::default(),
            external_functions,
            fn_id_gen: Default::default(),
            function_ids: Default::default(),
        }
    }

    pub fn assemble(mut self) -> Program {
        let main_block = self
            .annotated_blocks
            .blocks
            .get_block_id_by_host(self.ir.entrypoint, self.ir.entry_block());

        let mut functions = FxHashMap::default();

        let program_entrypoint_id = self.annotated_blocks.get_block_id(main_block, &Vec::new());
        let mut fns_to_build = vec![program_entrypoint_id];

        while let Some(block_id) = fns_to_build.pop() {
            let fn_id = self.fn_id_gen.map(block_id);

            if functions.get(&fn_id).is_some() {
                // we've already assembled this function before
                continue;
            }

            let FnAssembleResult {
                function,
                assembly_requests,
            } = FnAssembler::new(&self, block_id).assemble();

            fns_to_build.extend(assembly_requests);
            functions.insert(fn_id, function);

            // TODO: be more efficient about which functions to add
            let lock = self.fn_id_gen.map.lock().unwrap();
            for (fn_id, id) in lock.iter() {
                fns_to_build.push(*fn_id);
            }
        }

        Program {
            entrypoint: self.fn_id_gen.map(program_entrypoint_id),
            constants: Mutex::into_inner(self.constant_map.vals).unwrap(),
            external_functions: self.external_functions,
            functions,
        }
    }
}

pub struct FnAssembleResult {
    function: Function,
    assembly_requests: Vec<BlockId<AnnotatedCtx>>,
}

struct FnAssembler<'duration, 'ir> {
    assembler: &'duration Assembler<'ir>,
    block_id: BlockId<AnnotatedCtx>,
    block_id_map: UniqueBlkIdGen,
}

impl<'d, 'ir> FnAssembler<'d, 'ir> {
    pub fn new(assembler: &'d Assembler<'ir>, block_id: BlockId<AnnotatedCtx>) -> Self {
        Self {
            assembler,
            block_id,
            block_id_map: Default::default(),
        }
    }

    pub fn assemble(mut self) -> FnAssembleResult {
        // v taken out of a field
        // we can ONLY use this because we know that the `conv_only_bb` step will
        // only generate registers abve all IR registers, which means that there
        // will never be a clash. this might have to be reconsidered later
        let mut register_map: RegIdMap<PureBbCtx, AssemblerCtx> = Default::default();

        let mut blocks = FxHashMap::default();

        let mut blocks_to_assemble = VecDeque::new();

        blocks_to_assemble.push_back(self.block_id);

        while let Some(block_id) = blocks_to_assemble.pop_front() {
            let assembler_blk_id = self.block_id_map.map(block_id);
            if blocks.contains_key(&assembler_blk_id) {
                // we've already assembled that block
                continue;
            }

            let annotated_block
            @
            AnnotatedBlock {
                block,
                args,
                registers,
            } = self.assembler.annotated_blocks.get_block(block_id);

            let inst_writer = InstWriter::new(&self, &mut register_map, annotated_block);

            // let (branch, register_types) = typed_fn.find(&block, &args);

            // LOGIC(param_dropping): this is the logic that determines which
            // parameters are dropped, it may need to be encapsulated somewhere
            // else for easier refactoring
            debug_assert_eq!(args.len(), block.parameters.len());
            let parameters = (args.iter())
                .zip(block.parameters.iter())
                // we can ignore simple parameters, as reconstructing those
                // params is cheaper than passing them along
                //
                // this comes at the cost of us needing to fill in the constant
                // values anytime we use these parameters
                .filter(|(typ, _)| !typ.is_simple())
                .map(|(typ, register)| Parameter {
                    typ: typ.to_type(),
                    register: inst_writer.register_map.map(*register),
                })
                .collect::<Vec<_>>();

            let (mut annotated_block, next) = inst_writer.write();
            annotated_block.parameters.extend(parameters);
            blocks
                .insert(assembler_blk_id, annotated_block)
                .expect_free();

            blocks_to_assemble.extend(next);
        }

        let return_type = self
            .assembler
            .annotated_blocks
            .get_return_type(self.block_id);

        FnAssembleResult {
            assembly_requests: vec![],
            function: Function {
                entry_block: self.block_id_map.map(self.block_id),
                blocks,
                return_type: return_type.clone().into_type(),
            },
        }
    }
}

struct InstWriter<'duration> {
    fn_assembler: &'duration FnAssembler<'duration, 'duration>,
    assembler: &'duration Assembler<'duration>,
    block: &'duration crate::frontend::conv_only_bb::Block,
    args: &'duration Vec<ValueType>,
    registers: &'duration FxHashMap<RegisterId<PureBbCtx>, ValueType>,
    register_map: &'duration mut RegIdMap<PureBbCtx, AssemblerCtx>,
    instructions: Vec<Instruction>,
    never_propagation: bool,
}

impl<'d> InstWriter<'d> {
    pub fn new(
        fn_assembler: &'d FnAssembler<'d, 'd>,
        register_map: &'d mut RegIdMap<PureBbCtx, AssemblerCtx>,
        block: AnnotatedBlock<'d>,
    ) -> Self {
        let assembler = fn_assembler.assembler;
        Self {
            fn_assembler,
            assembler,
            block: block.block,
            args: block.args,
            registers: block.registers,
            register_map,
            instructions: Vec::new(),
            never_propagation: false,
        }
    }

    fn is_simple(&self, register: RegisterId<PureBbCtx>) -> bool {
        InstWriter::_is_simple(self.registers, register)
    }

    fn _is_simple(
        register_types: &FxHashMap<RegisterId<PureBbCtx>, ValueType>,
        register: RegisterId<PureBbCtx>,
    ) -> bool {
        register_types.get(&register).unwrap().is_simple()
    }

    pub fn write(mut self) -> (Block, Vec<BlockId<AnnotatedCtx>>) {
        for inst in self.block.instructions.iter() {
            if !self.translate(inst) {
                break;
            }
        }

        let mut next = Vec::new();

        let end = match &self.block.end {
            ir::ControlFlowInstruction::Jmp(jmp) => {
                EndInstruction::Jump(self.map_bbjump(jmp, &mut next))
            }
            ir::ControlFlowInstruction::JmpIf {
                condition,
                true_path,
                false_path,
            } => {
                let condition = self.register_map.map(*condition);
                let true_path = self.map_bbjump(true_path, &mut next);
                let false_path = self.map_bbjump(false_path, &mut next);
                EndInstruction::JumpIf {
                    condition,
                    true_path,
                    false_path,
                }
            }
            ir::ControlFlowInstruction::Ret(reg) => {
                EndInstruction::Return(reg.map(|r| self.register_map.map(r)))
            }
        };

        let block = Block {
            parameters: vec![],
            instructions: self.instructions,
            end,
        };

        (block, next)
    }

    fn map_bbjump(
        &mut self,
        bbjump: &BasicBlockJump<PureBbCtx, PureBbCtx>,
        next: &mut Vec<BlockId<AnnotatedCtx>>,
    ) -> BlockJump {
        println!("mapping bbjump {:?} {:?}", bbjump, next);
        let BasicBlockJump(blk_id, args) = bbjump;

        let invoc_args = args
            .iter()
            .map(|r| self.registers.get(r).unwrap().clone())
            .collect();
        println!("invoc_args {:?}", invoc_args);
        let blk_id = self
            .assembler
            .annotated_blocks
            .get_block_id(*blk_id, &invoc_args);
        next.push(blk_id);
        let blk_id = self.fn_assembler.block_id_map.map(blk_id);
        let args = args.iter().map(|r| self.register_map.map(*r)).collect();

        BlockJump(blk_id, args)
    }

    pub fn translate(&mut self, inst: &ir::Instruction<PureBbCtx>) -> bool {
        match inst {
            // "simple" instructions should be ignored, as we can easily insert
            // these instructions exactly when they are required. with our opt
            // passes, we should be able to completely elide creation of simple
            // values too!

            // dead-simple instructions, always will be simple
            ir::Instruction::MakeString(_, _)
            | ir::Instruction::MakeInteger(_, _)
            | ir::Instruction::GetRuntime(_) => {}

            // calculatably simple instructions
            ir::Instruction::CompareLessThan(result, _, _) | ir::Instruction::Add(result, _, _)
                if self.is_simple(*result) => {}

            // the remaining instructions are decidedly complex

            // the simple cases are handled above
            ir::Instruction::CompareLessThan(_result, _lhs, _rhs) => {
                //
                todo!()
            }

            ir::Instruction::Add(_result, _lhs, _rhs) => {
                //
                todo!()
            }

            // the rest are complicated
            // a `call` function may invoke an external function, whose arguments
            // may not be elided whatsoever. because calling a JSSAT function
            // may invoke external functions as well, by induction, we may not
            // elide the arguments to other JSSAT functions.
            ir::Instruction::Call(result, function, args) => {
                let original_args = args;
                let arg_types = args
                    .iter()
                    .map(|r| self.registers.get(r).unwrap().clone())
                    .collect::<Vec<_>>();

                let args = match function {
                    ir::Callable::External(ext_id) => {
                        // in external functions, we can never omit any params
                        let ext_fn = self
                            .assembler
                            .external_functions
                            .get(&ext_id.map_context())
                            .unwrap();

                        let mut calling_args = Vec::new();

                        for (idx, simple_arg) in args.iter().enumerate() {
                            if self.is_simple(*simple_arg) {
                                // simple arguments are omitted from compilation entirely, so to call
                                // an external function with a simple value we must produce it
                                self.compute_simple(*simple_arg);
                            }

                            let mut id = self.register_map.map(*simple_arg);

                            match (
                                // from
                                self.registers.get(simple_arg).unwrap(),
                                // to
                                &ext_fn.parameters[idx],
                            ) {
                                (ValueType::Runtime, Type::FFI(FFIValueType::Runtime)) => {}
                                (ValueType::ExactString(s), Type::FFI(FFIValueType::Any)) => {
                                    id = self.widen_string_to_any(id, *s);
                                }
                                (ValueType::ExactInteger(v), Type::FFI(FFIValueType::Any)) => {
                                    id = self.widen_int_to_any(id, *v);
                                }
                                (lhs, rhs) => {
                                    unimplemented!("cannot handle {:?} -> {:?}", lhs, rhs)
                                }
                            };

                            calling_args.push(id);
                        }

                        calling_args
                    }
                    ir::Callable::Static(_) => {
                        // LOGIC(param_dropping): this is the logic that determines which
                        // parameters to pass to a block, as some parameters may be
                        // dropped. the complexity comes from having to ensure that this
                        // piece of logic aligns with the other piece of logic related
                        // to this one

                        // TODO: we know that we can always drop simple arguments
                        // because we never have code in the type_annotator phase that
                        // bails when a function gets too recursive, meaning we will
                        // never have a jump with simple args to a block that accepts
                        // generic/abstract args (i.e. Jmp A(0) -> A(Int) is impossible)
                        let register_types = self.registers;
                        args.iter()
                            .filter(|r| !InstWriter::_is_simple(register_types, **r))
                            .map(|r| self.register_map.map(*r))
                            .collect::<Vec<_>>()
                    }
                };

                let result = (*result).map(|r| self.register_map.map(r));
                self.instructions.push(Instruction::Call(
                    result,
                    match function {
                        ir::Callable::External(id) => Callable::Extern(id.map_context()),
                        ir::Callable::Static(id) => {
                            // find the block id that most matches the args passed to the function
                            let blk_id = {
                                let entry_block =
                                    (self.assembler.ir.functions.get(id).unwrap()).entry_block;

                                let arg_types = original_args
                                    .iter()
                                    .map(|a| self.registers.get(a).unwrap().clone())
                                    .collect::<Vec<_>>();

                                let bb_blk_id = self
                                    .assembler
                                    .annotated_blocks
                                    .blocks
                                    .get_block_id_by_host(*id, entry_block);

                                self.assembler
                                    .annotated_blocks
                                    .get_block_id(bb_blk_id, &arg_types)
                            };

                            Callable::Static(self.assembler.fn_id_gen.map(blk_id))
                        }
                    },
                    args,
                ));
            }
        }

        true
    }

    fn intern_constant(&mut self, id: ConstantId<IrCtx>) -> ConstantId<AssemblerCtx> {
        let payload = &self.assembler.ir.constants.get(&id).unwrap().payload;
        self.assembler.constant_map.map(id, payload)
    }

    fn compute_simple(&mut self, arg: RegisterId<PureBbCtx>) -> RegisterId<AssemblerCtx> {
        debug_assert!(self.is_simple(arg));

        let reg = self.register_map.map(arg);

        match self.registers.get(&arg).unwrap() {
            ValueType::Any
            | ValueType::String
            | ValueType::Number
            | ValueType::Pointer(_)
            | ValueType::Word
            | ValueType::Boolean => unreachable!("cannot be simple yet get here"),
            ValueType::Runtime => {
                self.instructions.push(Instruction::GetRuntime(reg));
            }
            ValueType::ExactString(id) => {
                let interned = self.intern_constant(*id);
                self.instructions
                    .push(Instruction::MakeString(reg, interned));
            }
            ValueType::ExactInteger(n) => {
                self.instructions.push(Instruction::MakeNumber(reg, *n));
            }
            ValueType::Bool(b) => {
                self.instructions.push(Instruction::MakeBoolean(reg, *b));
            }
        };

        reg
    }

    // fn map_basic_block_jump(&mut self, bbjump: &BasicBlockJump<PureBbCtx, IrCtx>) -> BlockJump {
    //     let BasicBlockJump(id, args) = bbjump;

    //     let invocation_arg_types = args
    //         .iter()
    //         .map(|r| self.registers.get(r).unwrap().clone())
    //         .collect();

    //     // LOGIC(param_dropping): this is the logic that determines which
    //     // parameters to pass to a block, as some parameters may be
    //     // dropped. the complexity comes from having to ensure that this
    //     // piece of logic aligns with the other piece of logic related
    //     // to this one

    //     // TODO: we know that we can always drop simple arguments
    //     // because we never have code in the type_annotator phase that
    //     // bails when a function gets too recursive, meaning we will
    //     // never have a jump with simple args to a block that accepts
    //     // generic/abstract args (i.e. Jmp A(0) -> A(Int) is impossible)
    //     let register_types = self.registers;
    //     let register_map = &mut self.register_map;

    //     let args = (args.iter())
    //         .filter(|arg| !InstWriter::_is_simple(register_types, **arg))
    //         .copied()
    //         .map(|r| register_map.map(r))
    //         .collect();

    //     // self.assembler.annotated_blocks.blocks.get_block_id_by_host(original_function, original_block)

    //     BlockJump((self.map_block)(*id, &invocation_arg_types), args)
    // }

    fn widen_string_to_any(
        &mut self,
        register: RegisterId<AssemblerCtx>,
        value: ConstantId<IrCtx>,
    ) -> RegisterId<AssemblerCtx> {
        let result = self.register_map.gen();
        self.instructions.push(Instruction::Widen {
            result,
            input: register,
            from: Type::Val(ValueType::ExactString(value)),
            to: Type::FFI(FFIValueType::Any),
        });
        result
    }

    fn widen_int_to_any(
        &mut self,
        register: RegisterId<AssemblerCtx>,
        value: i64,
    ) -> RegisterId<AssemblerCtx> {
        let result = self.register_map.gen();
        self.instructions.push(Instruction::Widen {
            result,
            input: register,
            from: Type::Val(ValueType::ExactInteger(value)),
            to: Type::FFI(FFIValueType::Any),
        });
        result
    }

    // fn complete(mut self, end: &ir::ControlFlowInstruction<PureBbCtx, IrCtx>) -> Block {
    //     let end = if self.never_propagation {
    //         EndInstruction::Unreachable
    //     } else {
    //         match end {
    //             ir::ControlFlowInstruction::Jmp(bbjump) => {
    //                 EndInstruction::Jump(self.map_basic_block_jump(bbjump))
    //             }
    //             ir::ControlFlowInstruction::JmpIf {
    //                 condition,
    //                 true_path,
    //                 false_path,
    //             } => {
    //                 // // if this assertion doesn't hold, we can just manually insert
    //                 // // the constant jump ourselves
    //                 // debug_assert!(
    //                 //     !matches!(
    //                 //         self.register_types.get(condition).unwrap(),
    //                 //         ValueType::Bool(_)
    //                 //     ),
    //                 //     "shouldn't be conditionalling jumping when path is known"
    //                 // );

    //                 match self.register_types.get(condition).unwrap() {
    //                     ValueType::Bool(true) => {
    //                         EndInstruction::Jump(self.map_basic_block_jump(true_path))
    //                     }
    //                     ValueType::Bool(false) => {
    //                         EndInstruction::Jump(self.map_basic_block_jump(false_path))
    //                     }
    //                     ValueType::Boolean => EndInstruction::JumpIf {
    //                         condition: self.register_map.map(*condition),
    //                         true_path: self.map_basic_block_jump(true_path),
    //                         false_path: self.map_basic_block_jump(false_path),
    //                     },
    //                     _ => unreachable!("condition register should be only bools"),
    //                 }
    //             }
    //             ir::ControlFlowInstruction::Ret(None) => EndInstruction::Return(None),
    //             ir::ControlFlowInstruction::Ret(Some(arg)) => {
    //                 if self.is_simple(*arg) {
    //                     self.compute_simple(*arg);
    //                 }

    //                 EndInstruction::Return(Some(self.register_map.map(*arg)))
    //             }
    //         }
    //     };

    //     Block {
    //         parameters: self.parameters,
    //         instructions: self.instructions,
    //         end,
    //     }
    // }
}

impl ValueType {
    pub fn to_type(&self) -> Type {
        Type::Val(self.clone())
    }

    pub fn is_const(&self) -> bool {
        match self {
            ValueType::Any
            | ValueType::Runtime
            | ValueType::String
            | ValueType::Number
            | ValueType::Pointer(_)
            | ValueType::Word
            | ValueType::Boolean => false,
            ValueType::ExactInteger(_) | ValueType::ExactString(_) | ValueType::Bool(_) => true,
        }
    }

    /// States whether the type is "simple" or not. Simple types are extremely
    /// cheap to rebuild at any moment in the IR, and are considered cheaper to
    /// build than to pass around. Passing them around is considered "expensive"
    /// because then we're using more registers than necessary, which cause
    /// performance deficits because the more registers we use the more likely
    /// we'll need to spill onto the stack to generate a function.
    pub fn is_simple(&self) -> bool {
        matches!(
            self,
            ValueType::Runtime
                | ValueType::ExactInteger(_)
                | ValueType::Bool(_)
                | ValueType::ExactString(_)
        )
    }
}
