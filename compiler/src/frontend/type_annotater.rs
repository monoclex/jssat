use ref_cast::RefCast;
use rustc_hash::FxHashMap;
use std::hash::Hash;

use crate::frontend::ir::*;
use crate::frontend::types::ShapeKey;
use crate::id::*;
use crate::poor_hashmap::PoorMap;

use super::conv_only_bb::{Block, PureBlocks};
use super::types::RecordShape;
use super::types::RegMap;

/// Type annotation mechanism in JSSAT.
///
/// This works by symbolically executing the JSSAT IR, and emitting equivalent functions.
// can't use references because we need them to live 'static for tokio::spawn to work
pub fn annotate(ir: &IR, pure_blocks: &PureBlocks) -> TypeAnnotations {
    let mut engine = SymbolicExecutionEngine::new(ir, pure_blocks);

    let main = pure_blocks.get_block_id_by_host(ir.entrypoint, ir.entry_block());
    engine.execute(main, InvocationArgs::default());

    engine.execution_map.into_type_annotations()
}

// output data structures

#[derive(Debug, Clone)]
pub struct TypeAnnotations {
    block_executions: FxHashMap<BlockId<PureBbCtx>, BlockExecutions>,
    id_to_invoc: FxHashMap<BlockId<AnnotatedCtx>, (BlockId<PureBbCtx>, InvocationArgs)>,
}

impl TypeAnnotations {
    pub fn get_invocations(&self, block: BlockId<PureBbCtx>) -> &BlockExecutions {
        self.block_executions.get(&block).expect("expected block")
    }

    pub fn get_type_info_by_id(&self, id: BlockId<AnnotatedCtx>) -> &TypeInformation {
        let (blk_id, invocation_args) = self.id_to_invoc.get(&id).unwrap();
        let blk_exec = self.block_executions.get(blk_id).unwrap();
        blk_exec.get_type_info_by_invocation_args(invocation_args)
    }
}

#[derive(Debug, Clone)]
pub struct BlockExecutions {
    runs: PoorMap<InvocationArgs, TypeInformation>,
}

impl BlockExecutions {
    pub fn get_type_info_by_invocation(
        &self,
        invocation_args: &Vec<ValueType>,
    ) -> &TypeInformation {
        self.get_type_info_by_invocation_args(InvocationArgs::ref_cast(invocation_args))
    }

    fn get_type_info_by_invocation_args(
        &self,
        invocation_args: &InvocationArgs,
    ) -> &TypeInformation {
        self.runs.get(invocation_args).unwrap()
    }
}

#[derive(RefCast, PartialEq, Eq, Debug, Clone)]
#[repr(transparent)]
pub struct InvocationArgs<C: ContextTag = PureBbCtx>(pub RegMap<C>);

impl<C: ContextTag> Default for InvocationArgs<C> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: ContextTag> InvocationArgs<C> {
    pub fn into_reg_map(self) -> RegMap<C> {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct TypeInformation {
    pub pure_id: BlockId<PureBbCtx>,
    pub annotated_id: BlockId<AnnotatedCtx>,
    pub register_types: RegMap<PureBbCtx>,
    pub return_type: ReturnType,
}

impl TypeInformation {
    pub fn invocation_args(&self, bblock: &Block) -> Vec<ValueType> {
        bblock
            .parameters
            .iter()
            .map(|r| self.register_types.get(*r))
            .cloned()
            .collect()
    }

    pub fn get_type(&self, register_id: RegisterId<PureBbCtx>) -> &ValueType {
        self.register_types.get(register_id)
    }
}

// symbolic execution engine

struct SymbolicExecutionEngine<'duration> {
    ir: &'duration IR,
    blocks: &'duration PureBlocks,
    execution_map: ExecutionMap,
}

#[derive(Clone)]
struct ExecutionResult {
    registers: RegMap<PureBbCtx>,
    returns: ReturnType,
}

impl ExecutionResult {
    pub fn return_type(&self) -> ReturnType {
        self.returns.clone()
    }
}

impl<'d> SymbolicExecutionEngine<'d> {
    pub fn new(ir: &'d IR, blocks: &'d PureBlocks) -> Self {
        Self {
            ir,
            blocks,
            execution_map: Default::default(),
        }
    }

    pub fn execute(
        &mut self,
        block_id: BlockId<PureBbCtx>,
        invocation_args: InvocationArgs,
    ) -> &ExecutionResult {
        let block = self.blocks.get_block(block_id);

        let mut execution = self
            .execution_map
            .get_execution_mut(block_id, invocation_args.clone());

        let mut wtf_borrow_checker = false;

        // check if we've symbolically executed this before
        match &execution.status {
            ExecutionStatus::Unbegun => {
                execution.status = ExecutionStatus::InProgress;
            }
            ExecutionStatus::InProgress => {
                // if we're attempting to symbolically execute something that's
                // already being executed, we're probably in some sort of
                // recursive loop. bail now
                todo!("return 'never' & include need for least fixed point");
            }
            ExecutionStatus::Completed(_) => {
                // already executed, we don't have to execute this
                wtf_borrow_checker = true; // because we can't usee the data inside of Completed
            }
        };

        if wtf_borrow_checker {
            let execution = self
                .execution_map
                .get_execution_mut(block_id, invocation_args);

            match &execution.status {
                ExecutionStatus::Completed(result) => return result,
                _ => unreachable!(),
            };
        }

        // symbolically execute the function

        let mut registers = invocation_args.into_reg_map();

        let mut never_infected = false;
        for instruction in block.instructions.iter() {
            match instruction {
                &Instruction::GetRuntime(r) => {
                    registers.insert(r, ValueType::Runtime);
                }
                &Instruction::MakeString(r, c) => {
                    let constant = self.ir.constants.get(&c).unwrap();
                    registers.insert(r, ValueType::ExactString(constant.payload.clone()));
                }
                &Instruction::MakeInteger(r, v) => {
                    registers.insert(r, ValueType::ExactInteger(v));
                }
                &Instruction::CompareLessThan(res, l, r) => {
                    let l = registers.get(l);
                    let r = registers.get(r);

                    let res_typ = match (l, r) {
                        (ValueType::Number, ValueType::Number)
                        | (ValueType::Number, ValueType::ExactInteger(_))
                        | (ValueType::ExactInteger(_), ValueType::Number) => ValueType::Boolean,
                        (&ValueType::ExactInteger(l), &ValueType::ExactInteger(r)) => {
                            ValueType::Bool(l < r)
                        }
                        (l, r) => panic!("unsupported `<` of types {:?} and {:?}", l, r),
                    };

                    registers.insert(res, res_typ);
                }
                &Instruction::Add(res, l, r) => {
                    let l = registers.get(l);
                    let r = registers.get(r);

                    let res_typ = match (l, r) {
                        (ValueType::Number, ValueType::Number)
                        | (ValueType::Number, ValueType::ExactInteger(_))
                        | (ValueType::ExactInteger(_), ValueType::Number) => ValueType::Number,
                        (&ValueType::ExactInteger(l), &ValueType::ExactInteger(r)) => {
                            ValueType::ExactInteger(l + r)
                        }
                        (l, r) => panic!("unsupported `+` of types {:?} and {:?}", l, r),
                    };

                    registers.insert(res, res_typ);
                }
                Instruction::Call(res, Callable::Static(fn_id), args) => {
                    let func = self.ir.functions.get(fn_id).unwrap();
                    let blk_id = self.blocks.get_block_id_by_host(*fn_id, func.entry_block);

                    // TODO: use zip_eq
                    let src_regs = args.iter().copied().collect::<Vec<_>>();
                    let dest_regs = self
                        .blocks
                        .get_block(blk_id)
                        .parameters
                        .iter()
                        .copied()
                        .collect::<Vec<_>>();
                    debug_assert_eq!(src_regs.len(), dest_regs.len());

                    let invocation_args = registers
                        .prepare_invocation(src_regs.into_iter().zip(dest_regs.into_iter()));

                    let result = self.execute(blk_id, invocation_args);

                    match (res, result.return_type()) {
                        (Some(_), ReturnType::Void) => panic!("cannot assign register to void"),
                        (None, _) => {}
                        (_, ReturnType::Never) => {
                            never_infected = true;
                            break;
                        }
                        (Some(reg), ReturnType::Value(v)) => {
                            registers.insert(*reg, v);
                        }
                    };
                }
                Instruction::Call(res, Callable::External(fn_id), args) => {
                    // TODO: ensure/make args are coercible into `fn_id`,
                    // although the `assembler` phase does this for us as of the time of writing
                    let ext_fn = self.ir.external_functions.get(fn_id).unwrap();

                    match (*res, &ext_fn.return_type) {
                        (Some(_), Returns::Void) => panic!("cannot assign `void` to register"),
                        (None, _) => {}
                        (Some(reg), Returns::Value(v)) => {
                            registers.insert(reg, v.clone().into_value_type());
                        }
                    };
                }
                &Instruction::RecordNew(r) => {
                    let allocation = registers.insert_alloc();
                    registers.insert(r, ValueType::Record(allocation));
                }
                Instruction::RecordGet {
                    result,
                    record,
                    key,
                } => {
                    let key = match key {
                        &RecordKey::Value(r) => match registers.get(r) {
                            ValueType::String => ShapeKey::String,
                            ValueType::ExactString(str) => ShapeKey::Str(str.clone()),
                            ValueType::Any
                            | ValueType::Number
                            | ValueType::ExactInteger(_)
                            | ValueType::Boolean
                            | ValueType::Bool(_)
                            | ValueType::Word => {
                                todo!("may be implemented at a later date, but dunno")
                            }
                            ValueType::Runtime | ValueType::Pointer(_) | ValueType::Record(_) => {
                                unimplemented!("unsupported record key type")
                            }
                        },
                        RecordKey::InternalSlot(slot) => ShapeKey::InternalSlot(slot),
                    };

                    if let ValueType::Record(alloc) = *registers.get(*record) {
                        let shape = registers.get_shape(alloc);
                        let prop_value_typ = shape.type_at_key(&key).clone();
                        registers.insert(*result, prop_value_typ);
                    } else {
                        panic!("cannot call RecordGet on non record");
                    }
                }
                Instruction::RecordSet { record, key, value } => {
                    // TODO: deduplicate ShapeKey code
                    let key = match *key {
                        RecordKey::Value(v) => match registers.get(v) {
                            ValueType::String => ShapeKey::String,
                            ValueType::ExactString(str) => ShapeKey::Str(str.clone()),
                            ValueType::Any
                            | ValueType::Number
                            | ValueType::ExactInteger(_)
                            | ValueType::Boolean
                            | ValueType::Bool(_)
                            | ValueType::Word => {
                                todo!("may be implemented at a later date, but dunno")
                            }
                            ValueType::Runtime | ValueType::Pointer(_) | ValueType::Record(_) => {
                                unimplemented!("unsupported record key type")
                            }
                        },
                        RecordKey::InternalSlot(slot) => ShapeKey::InternalSlot(slot),
                    };

                    if let ValueType::Record(alloc) = *registers.get(*record) {
                        let shape = registers.get_shape(alloc);
                        let value_typ = registers.get(*value).clone();
                        let shape = shape.add_prop(key, value_typ);
                        let shape_id = registers.insert_shape(shape);
                        registers.assign_new_shape(alloc, shape_id);
                    } else {
                        panic!("cannot call RecordSet on non record");
                    }
                }
            };
        }

        let ret_typ = if never_infected {
            ReturnType::Never
        } else {
            match &block.end {
                ControlFlowInstruction::Jmp(to) => {
                    let to = self.execute_jmp(to, &registers);
                    to.return_type()
                }
                ControlFlowInstruction::JmpIf {
                    condition,
                    true_path,
                    false_path,
                } => {
                    let condition = registers.get(*condition);

                    match condition {
                        ValueType::Boolean => {
                            let true_path = self.execute_jmp(true_path, &registers).clone();
                            let false_path = self.execute_jmp(false_path, &registers);

                            true_path.return_type().unify(&false_path.return_type())
                        }
                        ValueType::Bool(true) => {
                            let true_path = self.execute_jmp(true_path, &registers);
                            true_path.return_type()
                        }
                        ValueType::Bool(false) => {
                            let false_path = self.execute_jmp(false_path, &registers);
                            false_path.return_type()
                        }
                        _ => panic!("cannot conditionally jump based on non boolean"),
                    }
                }
                ControlFlowInstruction::Ret(Some(result)) => {
                    let ret_typ = registers.get(*result);
                    ReturnType::Value(ret_typ.clone())
                }
                ControlFlowInstruction::Ret(None) => ReturnType::Void,
            }
        };

        let execution_result = ExecutionResult {
            registers,
            returns: ret_typ,
        };

        let mut execution = self
            .execution_map
            .get_execution_mut(block_id, invocation_args);

        execution.status = ExecutionStatus::Completed(execution_result);

        if let ExecutionStatus::Completed(result) = &execution.status {
            result
        } else {
            unreachable!("let is irrefutable")
        }
    }

    fn execute_jmp(
        &mut self,
        jump: &BasicBlockJump<PureBbCtx, PureBbCtx>,
        registers: &RegMap<PureBbCtx>,
    ) -> &ExecutionResult {
        let BasicBlockJump(blk_id, args) = jump;

        // TODO: use zip_eq
        let src_regs = args.iter().copied().collect::<Vec<_>>();
        let dest_regs = self
            .blocks
            .get_block(*blk_id)
            .parameters
            .iter()
            .copied()
            .collect::<Vec<_>>();
        debug_assert_eq!(src_regs.len(), dest_regs.len());

        let invocation_args =
            registers.prepare_invocation(src_regs.into_iter().zip(dest_regs.into_iter()));

        self.execute(*blk_id, invocation_args)
    }
}

impl Unifyable for ReturnType {
    fn unify(&self, other: &Self) -> Self {
        match (self, other) {
            (ReturnType::Never, r) | (r, ReturnType::Never) => r.clone(),
            // copied from Returns<T>
            (ReturnType::Value(lhs), ReturnType::Value(rhs)) => ReturnType::Value(lhs.unify(rhs)),
            (ReturnType::Value(v), ReturnType::Void) | (ReturnType::Void, ReturnType::Value(v)) => {
                ReturnType::Value(v.clone())
            }
            (ReturnType::Void, ReturnType::Void) => ReturnType::Void,
        }
    }
}

struct ExecutionMap {
    map: FxHashMap<BlockId<PureBbCtx>, PoorMap<InvocationArgs, ExecutionInformation>>,
    counter: Counter<BlockId<AnnotatedCtx>>,
}

impl ExecutionMap {
    pub fn get_execution_mut(
        &mut self,
        block_id: BlockId<PureBbCtx>,
        args: InvocationArgs,
    ) -> &mut ExecutionInformation {
        let block_invocations = self.map.entry(block_id).or_insert_with(Default::default);

        let counter = &self.counter;
        let factory = || ExecutionInformation {
            id: counter.next(),
            status: ExecutionStatus::Unbegun,
        };

        block_invocations.entry(args).or_insert_with(factory)
    }

    pub fn into_type_annotations(self) -> TypeAnnotations {
        let mut block_executions = FxHashMap::default();
        let mut id_to_invoc = FxHashMap::default();

        for (bb_id, invocations) in self.map.into_iter() {
            for (a, b) in invocations.iter() {
                id_to_invoc.insert(b.id, (bb_id, a.clone()));
            }

            let mut runs = PoorMap::default();
            for (args, exec_info) in invocations.into_iter() {
                let res = if let ExecutionStatus::Completed(r) = exec_info.status {
                    r
                } else {
                    unreachable!("can only have completed entries after execution");
                };

                let return_type = res.return_type();
                let typ_info = TypeInformation {
                    pure_id: bb_id,
                    annotated_id: exec_info.id,
                    register_types: res.registers,
                    return_type,
                };

                runs.insert(args, typ_info);
            }

            block_executions.insert(bb_id, BlockExecutions { runs });
        }

        TypeAnnotations {
            block_executions,
            id_to_invoc,
        }
    }
}

impl Default for ExecutionMap {
    fn default() -> Self {
        Self {
            map: Default::default(),
            counter: Default::default(),
        }
    }
}

struct ExecutionInformation {
    id: BlockId<AnnotatedCtx>,
    status: ExecutionStatus,
}

enum ExecutionStatus {
    Unbegun,
    InProgress,
    Completed(ExecutionResult),
}

impl Unifyable for ValueType {
    fn unify(&self, other: &Self) -> Self {
        match (self, other) {
            (lhs, rhs) if lhs == rhs => lhs.clone(),
            (lhs, rhs) => panic!("unification for {:?} and {:?} not written atm", lhs, rhs),
        }
    }
}

/*
struct Factory<F> {
    factory: F
}

impl<F> Factory<F> where F: Fn() {
    pub fn new(factory: F) -> Self {
        Self { factory }
    }
}

struct UsesFactory {
    my: Factory<?>
}

impl UsesFactory {
    pub fn new() -> Self {
        Self {
            my: Factory::new(|| println!("hi"))
        }
    }
}
*/

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Void,
    Value(ValueType),
    /// # [`ValueType::Never`]
    ///
    /// The type assigned to a function when it recurses to infinity, with no
    /// end in sight.
    Never,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
    /// # `Any`
    ///
    /// The `Any` type in JSSAT is used a a polymorphic "catch-all" for when
    /// the type system cannot figure something out.
    ///
    /// Narrowing an `Any` into a more specific type when it's not possible to
    /// do so results in runtime errors. This feature of the `Any` type allows
    /// us to compile all user provided code into an output, even if the code
    /// given should be considered a compiler error.
    ///
    /// The `Any` type is the most generic type possible for all values. Any
    /// JSSAT RT value can be cast into an `Any`, besides exotic primitives,
    // TODO: is `Reference`/`Pointer` the finalized name?
    /// such as a `Runtime` or `Reference`/`Pointer`.
    ///
    /// A hierarchy of JSSAT RT types is shown below:
    ///
    /// - [`ValueType::Any`]
    ///   - [`ValueType::String`]
    ///     - [`ValueType::ExactString`]
    Any,
    Runtime,
    String,
    // TODO: an "ExactString" should just be a String with some kind of
    // ExactnessGuarantee to be exactly a type of a constant
    ExactString(Vec<u8>),
    Number,
    ExactInteger(i64),
    Boolean,
    Bool(bool),
    /// Pointer to data of the specified size. Pointer(16) -> `i16*`.
    Pointer(u16),
    Word,
    /// A record. The ID present inside of the object is the allocation id. The
    /// allocation id is then linked to a table of allocation IDs to the
    Record(AllocationId<NoContext>),
}

/// A snapshot of a [`ValueType`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SnapshotType {
    Any,
    Runtime,
    String,
    ExactString(Vec<u8>),
    Number,
    ExactInteger(i64),
    Boolean,
    Bool(bool),
    Pointer(u16),
    Word,
    Record(ShapeId<NoContext>),
}
