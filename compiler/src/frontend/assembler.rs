//! Assembles together the informatino from the original IR, and type_annotator
//! passes into a fully conherent IR, which will then be passed to `skeleton`.

use std::{collections::VecDeque, sync::Mutex};

use rustc_hash::FxHashMap;

use super::{
    conv_only_bb::{self, PureBlocks},
    ir,
    ir::{FFIValueType, RecordKey, IR},
    isa::{ISAInstruction, OpLessThan},
    old_types::{RegMap, ShapeKey},
    type_annotater::{self, InvocationArgs, TypeAnnotations, TypeInformation, ValueType},
};
use crate::{frontend::ir::BasicBlockJump, id::*};

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
    pub parameters: Vec<ValueType>,
    pub returns: ReturnType,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub register_types: RegMap<AssemblerCtx>,
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
    pub typ: ValueType,
    // TODO: maybe parameters should implicitly get the register accoridng to
    // their index? it makes sense not to do this in the other IRs because of
    // mangling parameters, but here we have pretty much all the information
    // necessary to craft a final product
    pub register: RegisterId<AssemblerCtx>,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    RecordNew(RegisterId<AssemblerCtx>),
    RecordGet {
        result: RegisterId<AssemblerCtx>,
        record: RegisterId<AssemblerCtx>,
        key: RecordKey<AssemblerCtx>,
    },
    RecordSet {
        shape_id: ShapeId<AssemblerCtx>,
        record: RegisterId<AssemblerCtx>,
        key: RecordKey<AssemblerCtx>,
        value: RegisterId<AssemblerCtx>,
    },
    Call(
        Option<RegisterId<AssemblerCtx>>,
        Callable,
        Vec<RegisterId<AssemblerCtx>>,
    ),
    GetRuntime(RegisterId<AssemblerCtx>),
    MakeFnPtr(RegisterId<AssemblerCtx>, BlockId<PureBbCtx>),
    MakeString(RegisterId<AssemblerCtx>, ConstantId<AssemblerCtx>),
    MakeNumber(RegisterId<AssemblerCtx>, i64),
    MakeBoolean(RegisterId<AssemblerCtx>, bool),
    MakeNull(RegisterId<AssemblerCtx>),
    MakeUndefined(RegisterId<AssemblerCtx>),
    /// "Widens" a given register to a type. The type must wider/bigger than the
    /// input register.
    Widen {
        result: RegisterId<AssemblerCtx>,
        input: RegisterId<AssemblerCtx>,
        from: ValueType,
        to: ValueType,
    },
    Unreachable,
    Noop,
    OpLessThan(OpLessThan<AssemblerCtx>),
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
    Value(ValueType),
}

pub fn assemble(ir: &IR, pure_blocks: &PureBlocks, type_annotations: TypeAnnotations) -> Program {
    Assembler::new(ir, pure_blocks, type_annotations).assemble()
}

fn map_ext_fn(exernal_function: ir::ExternalFunction) -> ExternalFunction {
    ExternalFunction {
        name: exernal_function.name,
        parameters: exernal_function
            .parameters
            .into_iter()
            .map(ir::FFIValueType::into_value_type)
            .collect(),
        returns: exernal_function.return_type.into_return_type(),
    }
}

impl ir::FFIReturnType {
    pub fn into_return_type(self) -> ReturnType {
        match self {
            ir::Returns::Value(v) => ReturnType::Value(v.into_value_type()),
            ir::Returns::Void => ReturnType::Void,
        }
    }
}

impl ir::FFIValueType {
    pub fn into_value_type(self) -> ValueType {
        match self {
            FFIValueType::Any => ValueType::Any,
            FFIValueType::Runtime => ValueType::Runtime,
            FFIValueType::String => ValueType::String,
        }
    }
}

impl type_annotater::ReturnType {
    fn into_type(self) -> ReturnType {
        match self {
            type_annotater::ReturnType::Void => ReturnType::Void,
            type_annotater::ReturnType::Value(v) => ReturnType::Value(v),
            // when performing codegen for a function that never returns, we will have already
            // transformed the function into the appropriate code to quit after dealing with
            // a never type, and if we don't return anything might as well return void (nothing)
            type_annotater::ReturnType::Never => ReturnType::Void,
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct UniqueBlkIdGen {
    gen: Counter<BlockId<AssemblerCtx>>,
    map: Mutex<FxHashMap<BlockId<AnnotatedCtx>, BlockId<AssemblerCtx>>>,
    map_rev: Mutex<FxHashMap<BlockId<AssemblerCtx>, BlockId<AnnotatedCtx>>>,
}

impl Default for UniqueBlkIdGen {
    fn default() -> Self {
        Self {
            gen: Default::default(),
            map: Default::default(),
            map_rev: Default::default(),
        }
    }
}

impl UniqueBlkIdGen {
    pub fn map(&self, block_id: BlockId<AnnotatedCtx>) -> BlockId<AssemblerCtx> {
        let mut map = self.map.lock().unwrap();
        *map.entry(block_id).or_insert_with(|| {
            let assembler_id = self.gen.next();
            let mut map_rev = self.map_rev.lock().unwrap();
            map_rev.insert(assembler_id, block_id);
            assembler_id
        })
    }

    pub fn get_rev(&self, block_id: BlockId<AssemblerCtx>) -> BlockId<AnnotatedCtx> {
        *self.map_rev.lock().unwrap().get(&block_id).unwrap()
    }
}

#[derive(Debug)]
pub struct UniqueConstantIdGen {
    gen: Counter<ConstantId<AssemblerCtx>>,
    map: Mutex<MutexGuardedUniqueConstantIdGen>,
}

#[derive(Debug)]
struct MutexGuardedUniqueConstantIdGen {
    vals: Vec<Vec<u8>>,
    map: FxHashMap<usize, ConstantId<AssemblerCtx>>,
}

impl MutexGuardedUniqueConstantIdGen {
    pub fn into_tuple(self) -> (Vec<Vec<u8>>, FxHashMap<usize, ConstantId<AssemblerCtx>>) {
        (self.vals, self.map)
    }
}

impl Default for UniqueConstantIdGen {
    fn default() -> Self {
        Self {
            gen: Default::default(),
            map: Mutex::new(MutexGuardedUniqueConstantIdGen {
                vals: Default::default(),
                map: Default::default(),
            }),
        }
    }
}

impl UniqueConstantIdGen {
    pub fn map(&self, payload: &[u8]) -> ConstantId<AssemblerCtx> {
        let mut lock = self.map.lock().unwrap();

        if let Some((idx, _)) = lock.vals.iter().enumerate().find(|&(_, v)| v == payload) {
            *lock.map.get(&idx).unwrap()
        } else {
            let id = self.gen.next();
            let idx = lock.vals.len();
            lock.vals.push(payload.to_vec());
            lock.map.insert(idx, id);
            id
        }
    }
}

#[derive(Debug)]
struct Assembler<'duration> {
    ir: &'duration IR,
    pure_bocks: &'duration PureBlocks,
    type_annotations: TypeAnnotations,
    //
    constant_map: UniqueConstantIdGen,
    fn_id_gen: UniqueFnIdGen,
}

impl<'d> Assembler<'d> {
    pub fn new(ir: &'d IR, pure_bocks: &'d PureBlocks, type_annotations: TypeAnnotations) -> Self {
        Self {
            ir,
            pure_bocks,
            type_annotations,
            constant_map: Default::default(),
            fn_id_gen: Default::default(),
        }
    }

    pub fn fn_id_annblk(&self, annblk: BlockId<AnnotatedCtx>) -> FunctionId<AssemblerCtx> {
        let type_info = self.type_annotations.get_type_info_by_id(annblk);
        self.fn_id_gen.map(type_info.annotated_id)
    }

    pub fn fn_id(
        &self,
        bb_id: BlockId<PureBbCtx>,
        args: &InvocationArgs,
    ) -> FunctionId<AssemblerCtx> {
        let type_info = self
            .type_annotations
            .get_invocations(bb_id)
            .get_type_info_by_invocation_args(args);
        self.fn_id_gen.map(type_info.annotated_id)
    }

    pub fn blk_id(
        &self,
        bb_id: BlockId<PureBbCtx>,
        args: &InvocationArgs,
    ) -> BlockId<AnnotatedCtx> {
        let type_info = self
            .type_annotations
            .get_invocations(bb_id)
            .get_type_info_by_invocation_args(args);
        type_info.annotated_id
    }

    pub fn intern_constant(&self, payload: &[u8]) -> ConstantId<AssemblerCtx> {
        self.constant_map.map(payload)
    }

    fn entry_record(&self) -> BlockId<AnnotatedCtx> {
        let entry_fn = self.ir.entrypoint;
        let entry_blk = self.ir.entry_block();
        let entry_bb_blk = self.pure_bocks.get_block_id_by_host(entry_fn, entry_blk);
        self.type_annotations
            .get_invocations(entry_bb_blk)
            .get_type_info_by_invocation_args(&Default::default())
            .annotated_id
    }

    pub fn assemble(self) -> Program {
        let mut functions = FxHashMap::default();

        let mut fns = VecDeque::new();
        fns.push_back(self.entry_record());

        while let Some(id) = fns.pop_front() {
            let type_info = self.type_annotations.get_type_info_by_id(id);

            let fn_id = self.fn_id_gen.map(type_info.annotated_id);

            // let assembled = FnAssembler::new(&self, fn_id, type_info, bb_id, args).assemble();
            let (assembled, to_assemble) = FnAssembler::new(&self, type_info).assemble();
            fns.extend(to_assemble);

            functions.insert(fn_id, assembled);
        }

        // `entrypoint`
        let entry_blk_id = self.entry_record();
        let entrypoint = self.fn_id_annblk(entry_blk_id);

        // `constants`
        let (const_payloads, const_ids) = Mutex::into_inner(self.constant_map.map)
            .unwrap()
            .into_tuple();
        let constants = (const_ids.into_iter())
            .map(|(idx, id)| (id, const_payloads[idx].clone()))
            .collect();

        // `external_functions`
        // external functions don't need any transformations, so we can directly map 'em
        let external_functions = (self.ir.external_functions.iter())
            .map(|(k, v)| (k.map_context::<AssemblerCtx>(), map_ext_fn(v.clone())))
            .collect::<FxHashMap<_, _>>();

        Program {
            entrypoint,
            constants,
            external_functions,
            functions,
        }
    }
}

// pub struct FnAssembleResult {
//     function: Function,
//     assembly_requests: Vec<BlockId<AnnotatedCtx>>,
// }

#[derive(Debug)]
struct FnAssembler<'duration> {
    assembler: &'duration Assembler<'duration>,
    // fn_id: FunctionId<AssemblerCtx>,
    type_info: &'duration TypeInformation,
    // bb_id: BlockId<PureBbCtx>,
    // args: Vec<ValueType>,
    block_id_map: UniqueBlkIdGen,
}

impl<'d> FnAssembler<'d> {
    pub fn new(
        assembler: &'d Assembler,
        // fn_id: FunctionId<AssemblerCtx>,
        type_info: &'d TypeInformation,
        // bb_id: BlockId<PureBbCtx>,
        // args: Vec<ValueType>,
    ) -> Self {
        Self {
            assembler,
            // fn_id,
            type_info,
            // bb_id,
            // args,
            block_id_map: Default::default(),
        }
    }

    pub fn assemble(self) -> (Function, Vec<BlockId<AnnotatedCtx>>) {
        let entry_block = self.block_id_map.map(self.type_info.annotated_id);

        let mut blocks = FxHashMap::default();
        let mut register_types = RegMap::default();
        let mut reg_map = RegIdMap::default();
        let mut to_assemble = Vec::new();

        let mut block_queue = VecDeque::new();
        block_queue.push_back(entry_block);

        while let Some(block_id) = block_queue.pop_front() {
            if blocks.contains_key(&block_id) {
                continue;
            }

            let reg_typs = self
                .type_info_for_assembler_block(block_id)
                .register_types
                .clone();

            let (block, to_visit, reg_types) =
                InstWriter::new(&self, block_id, &mut reg_map, &mut to_assemble, reg_typs).write();
            blocks.insert(block_id, block);

            register_types.extend(reg_types);

            block_queue.extend(to_visit);
        }

        let return_type = self.type_info.return_type.clone().into_type();

        (
            Function {
                register_types,
                entry_block,
                blocks,
                return_type,
            },
            to_assemble,
        )
    }

    fn type_info_for_assembler_block(&self, id: BlockId<AssemblerCtx>) -> &TypeInformation {
        let annotated_id = self.block_id_map.get_rev(id);
        self.assembler
            .type_annotations
            .get_type_info_by_id(annotated_id)
    }
}

#[derive(Debug)]
struct InstWriter<'duration> {
    fn_assembler: &'duration FnAssembler<'duration>,
    reg_map: &'duration mut RegIdMap<PureBbCtx, AssemblerCtx>,
    type_info: &'duration TypeInformation,
    to_assemble: &'duration mut Vec<BlockId<AnnotatedCtx>>,
    bb: &'duration conv_only_bb::Block,
    register_types: RegMap<AssemblerCtx>,
    instructions: Vec<Instruction>,
    to_visit: Vec<BlockId<AssemblerCtx>>,
}

enum HaltStatus {
    // Stop,
    Continue,
}

impl<'d> InstWriter<'d> {
    pub fn new(
        fn_assembler: &'d FnAssembler<'d>,
        block_id: BlockId<AssemblerCtx>,
        reg_map: &'d mut RegIdMap<PureBbCtx, AssemblerCtx>,
        to_assemble: &'d mut Vec<BlockId<AnnotatedCtx>>,
        bb_register_types: RegMap<PureBbCtx>,
    ) -> Self {
        let type_info = fn_assembler.type_info_for_assembler_block(block_id);
        let bb = (fn_assembler.assembler.pure_bocks).get_block(type_info.pure_id);

        let mut register_types = bb_register_types.duplicate_with_allocations();
        for r in bb_register_types.registers() {
            register_types.insert(r.map_context(), bb_register_types.get(r).clone());
        }

        Self {
            fn_assembler,
            reg_map,
            type_info,
            to_assemble,
            bb,
            register_types,
            instructions: Default::default(),
            to_visit: Default::default(),
        }
    }

    pub fn write(mut self) -> (Block, Vec<BlockId<AssemblerCtx>>, RegMap<AssemblerCtx>) {
        println!("writing: {:?}", &self);
        let parameters = self
            .bb
            .parameters
            .iter()
            .map(|r| {
                let parameter = Parameter {
                    typ: self.type_info.register_types.get(*r).clone(),
                    register: self.reg_map.map(*r),
                };
                self.register_types
                    .insert(parameter.register, parameter.typ.clone());
                parameter
            })
            .collect();

        for inst in self.bb.instructions.iter() {
            match self.write_inst(inst) {
                HaltStatus::Continue => continue,
                // _ => break,
            }
        }

        let end = match &self.bb.end {
            ir::ControlFlowInstruction::Jmp(bbjump) => {
                EndInstruction::Jump(self.map_bbjump(bbjump))
            }
            ir::ControlFlowInstruction::JmpIf {
                condition,
                true_path,
                false_path,
            } => {
                let condition = self.reg_map.map(*condition);
                match self.register_types.get(condition) {
                    ValueType::Bool(true) => EndInstruction::Jump(self.map_bbjump(true_path)),
                    ValueType::Bool(false) => EndInstruction::Jump(self.map_bbjump(false_path)),
                    ValueType::Boolean => EndInstruction::JumpIf {
                        condition,
                        true_path: self.map_bbjump(true_path),
                        false_path: self.map_bbjump(false_path),
                    },
                    _ => unreachable!(
                        "invalid conditional register, this should be caught by typ annotation"
                    ),
                }
            }
            ir::ControlFlowInstruction::Ret(reg) => {
                EndInstruction::Return(reg.map(|r| self.reg_map.map(r)))
            }
        };

        let block = Block {
            parameters,
            instructions: self.instructions,
            end,
        };

        (block, self.to_visit, self.register_types)
    }

    fn write_inst(&mut self, inst: &ir::Instruction<PureBbCtx>) -> HaltStatus {
        match inst {
            &ir::Instruction::ReferenceOfFunction(result, func) => {
                let fnptr = self.reg_map.map(result);
                let static_fn = self.fn_assembler.assembler.ir.functions.get(&func).unwrap();
                let entry_blk = static_fn.entry_block;
                let pure_bb_id =
                    (self.fn_assembler.assembler.pure_bocks).get_block_id_by_host(func, entry_blk);
                self.instructions
                    .push(Instruction::MakeFnPtr(fnptr, pure_bb_id));
            }
            &ir::Instruction::GetRuntime(rt) => {
                let rt = self.reg_map.map(rt);
                self.instructions.push(Instruction::GetRuntime(rt));
                self.register_types.insert(rt, ValueType::Runtime);
            }
            &ir::Instruction::MakeString(str, val) => {
                let constant = self.fn_assembler.assembler.ir.constants.get(&val).unwrap();

                let str_reg = self.reg_map.map(str);
                let const_id = self
                    .fn_assembler
                    .assembler
                    .intern_constant(constant.payload.as_slice());

                self.instructions
                    .push(Instruction::MakeString(str_reg, const_id));
                self.register_types
                    .insert(str_reg, ValueType::ExactString(constant.payload.clone()));
            }
            &ir::Instruction::MakeInteger(int, val) => {
                let int_reg = self.reg_map.map(int);
                self.instructions
                    .push(Instruction::MakeNumber(int_reg, val));
                self.register_types
                    .insert(int_reg, ValueType::ExactInteger(val));
            }
            &ir::Instruction::CompareLessThan(inst) => {
                let cmp = inst.result;
                let lhs = inst.lhs;
                let rhs = inst.rhs;
                let cmp_typ = self.type_info.get_type(cmp);

                let cmp = self.reg_map.map(cmp);
                let lhs = self.reg_map.map(lhs);
                let rhs = self.reg_map.map(rhs);

                // TODO: emit a `CompareLessThan` instruction properly
                // for now i'll just emit MakeBoolean
                // and i'm not introducing `CompareLessThan` rn because im busy rewriting the assembler
                // and annotator and that's a big enough task for now
                if let ValueType::Bool(b) = *cmp_typ {
                    self.instructions.push(Instruction::MakeBoolean(cmp, b));
                    self.register_types.insert(cmp, ValueType::Bool(b));
                } else {
                    todo!("support non const comparison via {:?} {:?}", lhs, rhs);
                }
            }
            &ir::Instruction::Add(res, lhs, rhs) => {
                let res_typ = self.type_info.get_type(res);

                let res = self.reg_map.map(res);
                let lhs = self.reg_map.map(lhs);
                let rhs = self.reg_map.map(rhs);

                // TODO: emit a `Add` instruction properly
                // same case with above
                if let ValueType::ExactInteger(i) = *res_typ {
                    self.instructions.push(Instruction::MakeNumber(res, i));
                    self.register_types.insert(res, ValueType::ExactInteger(i));
                } else {
                    todo!("suport non const math via {:?} {:?}", lhs, rhs);
                }
            }
            ir::Instruction::Call(original_res, ir::Callable::External(fn_id), args) => {
                let res = original_res.map(|r| self.reg_map.map(r));

                let ext_fn = (self.fn_assembler.assembler.ir.external_functions)
                    .get(fn_id)
                    .unwrap();

                let args = args
                    .iter()
                    .map(|r| self.reg_map.map(*r))
                    .collect::<Vec<_>>();

                let val_types = (ext_fn.parameters.iter())
                    .map(|t| t.clone().into_value_type())
                    .collect::<Vec<_>>();

                debug_assert_eq!(args.len(), val_types.len());
                let args = args
                    .into_iter()
                    .zip(val_types.iter())
                    .map(|(register, coercion_type)| {
                        // TODO(far future): don't clone when split/partial borrows come along
                        let register_typ = self.register_types.get(register).clone();
                        self.coerce(register, &register_typ, coercion_type)
                    })
                    .collect::<Vec<_>>();

                self.instructions.push(Instruction::Call(
                    res,
                    Callable::Extern(fn_id.map_context()),
                    args,
                ));

                if let Some(reg) = res {
                    if let ir::Returns::Value(v) = ext_fn.return_type.clone() {
                        self.register_types.insert(reg, v.into_value_type());
                    } else {
                        panic!("expected assignment to a register to be non void");
                    }
                }
            }
            ir::Instruction::Call(res, ir::Callable::Static(fn_id), args) => {
                let original_res = res;
                let res = res.map(|r| self.reg_map.map(r));

                let static_fn = self.fn_assembler.assembler.ir.functions.get(fn_id).unwrap();
                let entry_blk = static_fn.entry_block;

                let pure_bb_id = (self.fn_assembler.assembler.pure_bocks)
                    .get_block_id_by_host(*fn_id, entry_blk);

                // TODO: use `zip_eq`
                println!("!!!! preparing call!!!!!!1!!!!!!");
                let src_regs = args
                    .iter()
                    .map(|r| self.reg_map.map(*r))
                    .collect::<Vec<_>>();
                let dest_regs = self
                    .fn_assembler
                    .assembler
                    .pure_bocks
                    .get_block(pure_bb_id)
                    .parameters
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();
                debug_assert_eq!(src_regs.len(), dest_regs.len());
                let invocation_args = self
                    .register_types
                    .prepare_invocation(src_regs.into_iter().zip(dest_regs.into_iter()));
                println!(
                    "preparing to call function:::::: {:#?} into block {:#?}",
                    invocation_args,
                    self.fn_assembler.assembler.pure_bocks.get_block(pure_bb_id)
                );

                let annotated_fn_id = self
                    .fn_assembler
                    .assembler
                    .fn_id(pure_bb_id, &invocation_args);
                let annotated_blk_id = self
                    .fn_assembler
                    .assembler
                    .blk_id(pure_bb_id, &invocation_args);

                let args = args.iter().map(|r| self.reg_map.map(*r)).collect();

                self.instructions.push(Instruction::Call(
                    res,
                    Callable::Static(annotated_fn_id),
                    args,
                ));

                debug_assert!(
                    res.is_some() == original_res.is_some()
                        && res.is_none() == original_res.is_none()
                );
                if let (Some(reg), Some(orig_reg)) = (res, *original_res) {
                    // TODO: handle never type

                    self.register_types
                        .insert(reg, self.type_info.get_type(orig_reg).clone());
                }

                self.to_assemble.push(annotated_blk_id);
            }
            ir::Instruction::Call(res, ir::Callable::Virtual(fn_ptr_id), args) => {
                let fn_ptr_id = self.reg_map.map(*fn_ptr_id);
                let pure_bb_id = if let ValueType::FnPtr(id) = self.register_types.get(fn_ptr_id) {
                    *id
                } else {
                    panic!("cannot virtual call non fnptr")
                };

                // TODO: don't blatantly copy Call(Static())

                let original_res = res;
                let res = res.map(|r| self.reg_map.map(r));

                // TODO: use `zip_eq`
                println!("!!!! preparing call!!!!!!2!!!!!!");
                let src_regs = args
                    .iter()
                    .map(|r| self.reg_map.map(*r))
                    .collect::<Vec<_>>();
                let dest_regs = self
                    .fn_assembler
                    .assembler
                    .pure_bocks
                    .get_block(pure_bb_id)
                    .parameters
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();
                debug_assert_eq!(src_regs.len(), dest_regs.len());
                let invocation_args = self
                    .register_types
                    .prepare_invocation(src_regs.into_iter().zip(dest_regs.into_iter()));
                println!(
                    "preparing to call function:::::: {:#?} into block {:#?}",
                    invocation_args,
                    self.fn_assembler.assembler.pure_bocks.get_block(pure_bb_id)
                );

                let annotated_fn_id = self
                    .fn_assembler
                    .assembler
                    .fn_id(pure_bb_id, &invocation_args);
                let annotated_blk_id = self
                    .fn_assembler
                    .assembler
                    .blk_id(pure_bb_id, &invocation_args);

                let args = args.iter().map(|r| self.reg_map.map(*r)).collect();

                self.instructions.push(Instruction::Call(
                    res,
                    Callable::Static(annotated_fn_id),
                    args,
                ));

                debug_assert!(
                    res.is_some() == original_res.is_some()
                        && res.is_none() == original_res.is_none()
                );
                if let (Some(reg), Some(orig_reg)) = (res, *original_res) {
                    // TODO: handle never type

                    self.register_types
                        .insert(reg, self.type_info.get_type(orig_reg).clone());
                }

                self.to_assemble.push(annotated_blk_id);
            }
            // TODO: the fact that this is literally copied and pasted from type_annotater makes me rethink
            // if this entire `assembler` phase is even necessary
            //
            // i'll probably end up trashing this entire struct
            &ir::Instruction::RecordNew(r) => {
                let r = self.reg_map.map(r.result());
                self.instructions.push(Instruction::RecordNew(r));
                let alloc = self.register_types.insert_alloc();
                self.register_types.insert(r, ValueType::Record(alloc));
            }
            ir::Instruction::RecordGet {
                result,
                record,
                key,
            } => {
                let result = &self.reg_map.map(*result);
                let record = &self.reg_map.map(*record);

                let rec_key = match *key {
                    RecordKey::Value(v) => RecordKey::Value(self.reg_map.map(v)),
                    RecordKey::InternalSlot(s) => RecordKey::InternalSlot(s),
                };

                let key = match key {
                    &RecordKey::Value(r) => match self.register_types.get(self.reg_map.map(r)) {
                        ValueType::String => ShapeKey::String,
                        ValueType::ExactString(str) => ShapeKey::Str(str.clone()),
                        ValueType::Any
                        | ValueType::Number
                        | ValueType::ExactInteger(_)
                        | ValueType::Boolean
                        | ValueType::Bool(_)
                        | ValueType::Null
                        | ValueType::Undefined => {
                            todo!("may be implemented at a later date, but dunno")
                        }
                        ValueType::Runtime | ValueType::Record(_) | ValueType::FnPtr(_) => {
                            unimplemented!("unsupported record key type")
                        }
                    },
                    RecordKey::InternalSlot(slot) => ShapeKey::InternalSlot(slot),
                };

                if let ValueType::Record(alloc) = *self.register_types.get(*record) {
                    let shape = self.register_types.get_shape(alloc);
                    let prop_value_typ = shape.type_at_key(&key).clone();
                    self.register_types.insert(*result, prop_value_typ);

                    self.instructions.push(Instruction::RecordGet {
                        result: *result,
                        record: *record,
                        key: rec_key,
                    });
                } else {
                    panic!("cannot call RecordGet on non record");
                }
            }
            ir::Instruction::RecordSet { record, key, value } => {
                let record = &self.reg_map.map(*record);
                let value = &self.reg_map.map(*value);

                let rec_key = match *key {
                    RecordKey::Value(v) => RecordKey::Value(self.reg_map.map(v)),
                    RecordKey::InternalSlot(s) => RecordKey::InternalSlot(s),
                };

                let key = match *key {
                    RecordKey::Value(v) => match self.register_types.get(self.reg_map.map(v)) {
                        ValueType::String => ShapeKey::String,
                        ValueType::ExactString(str) => ShapeKey::Str(str.clone()),
                        ValueType::Any
                        | ValueType::Number
                        | ValueType::ExactInteger(_)
                        | ValueType::Boolean
                        | ValueType::Bool(_)
                        | ValueType::Null
                        | ValueType::Undefined => {
                            todo!("may be implemented at a later date, but dunno")
                        }
                        ValueType::Runtime | ValueType::Record(_) | ValueType::FnPtr(_) => {
                            unimplemented!("unsupported record key type")
                        }
                    },
                    RecordKey::InternalSlot(slot) => ShapeKey::InternalSlot(slot),
                };

                if let ValueType::Record(alloc) = *self.register_types.get(*record) {
                    let shape = self.register_types.get_shape(alloc);
                    let value_typ = self.register_types.get(*value).clone();
                    let shape = shape.add_prop(key, value_typ);
                    let shape_id = self.register_types.insert_shape(shape);
                    self.register_types.assign_new_shape(alloc, shape_id);

                    self.instructions.push(Instruction::RecordSet {
                        shape_id,
                        record: *record,
                        key: rec_key,
                        value: *value,
                    });
                } else {
                    panic!("cannot call RecordSet on non record");
                }
            }
            &ir::Instruction::MakeNull(r) => {
                self.instructions
                    .push(Instruction::MakeNull(self.reg_map.map(r)));
            }
            &ir::Instruction::MakeUndefined(r) => {
                self.instructions
                    .push(Instruction::MakeUndefined(self.reg_map.map(r)));
            }
            &ir::Instruction::CompareEqual(res, l, r) => {
                let res_typ = self.type_info.get_type(res);

                let res = self.reg_map.map(res);
                let l = self.reg_map.map(l);
                let r = self.reg_map.map(r);

                // TODO: emit a `CompareEqual` instruction properly
                // same case with above (i.e. Add)
                if let ValueType::Bool(b) = *res_typ {
                    self.instructions.push(Instruction::MakeBoolean(res, b));
                } else {
                    todo!("suport non const comparison via {:?} {:?}", l, r);
                }
            }
            &ir::Instruction::Negate(res, v) => {
                let res_typ = self.type_info.get_type(res);

                let res = self.reg_map.map(res);
                let r = self.reg_map.map(v);

                // TODO: emit a `Negate` instruction properly
                // same case with above
                if let ValueType::Bool(b) = *res_typ {
                    self.instructions.push(Instruction::MakeBoolean(res, b));
                } else {
                    todo!("suport non const comparison via {:?} {:?}", r, v);
                }
            }
        };
        HaltStatus::Continue
    }

    fn map_bbjump(&mut self, bbjump: &BasicBlockJump<PureBbCtx, PureBbCtx>) -> BlockJump {
        let BasicBlockJump(id, args) = bbjump;

        // TODO: use `zip_eq`
        println!("!!!! preparing cal2l!!!!!!!!!!!!");
        let src_regs = args
            .iter()
            .map(|r| self.reg_map.map(*r))
            .collect::<Vec<_>>();
        let dest_regs = self
            .fn_assembler
            .assembler
            .pure_bocks
            .get_block(*id)
            .parameters
            .iter()
            .copied()
            .collect::<Vec<_>>();
        debug_assert_eq!(src_regs.len(), dest_regs.len());
        let invocation_args = self
            .register_types
            .prepare_invocation(src_regs.into_iter().zip(dest_regs.into_iter()));

        let typ_info = self
            .fn_assembler
            .assembler
            .type_annotations
            .get_invocations(*id)
            .get_type_info_by_invocation_args(&invocation_args);

        let id = self.fn_assembler.block_id_map.map(typ_info.annotated_id);
        self.to_visit.push(id);

        let args = args
            .iter()
            .map(|r| self.reg_map.map(*r))
            .collect::<Vec<_>>();

        BlockJump(id, args)
    }

    fn coerce(
        &mut self,
        reg: RegisterId<AssemblerCtx>,
        current: &ValueType,
        into: &ValueType,
    ) -> RegisterId<AssemblerCtx> {
        if current == into {
            return reg;
        }

        let result = self.reg_map.gen();
        self.instructions.push(Instruction::Widen {
            result,
            input: reg,
            from: current.clone(),
            to: into.clone(),
        });

        result
    }
}

impl Instruction {
    pub fn assigned_to(&self) -> Option<RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(result, _, _) => *result,
            Instruction::GetRuntime(result)
            | Instruction::MakeString(result, _)
            | Instruction::MakeNumber(result, _)
            | Instruction::MakeBoolean(result, _)
            | Instruction::MakeNull(result)
            | Instruction::MakeUndefined(result)
            | Instruction::Widen { result, .. }
            | Instruction::RecordNew(result)
            | Instruction::RecordGet { result, .. }
            | Instruction::MakeFnPtr(result, _) => Some(*result),
            Instruction::Unreachable | Instruction::Noop | Instruction::RecordSet { .. } => None,
            Instruction::OpLessThan(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> Vec<RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(_, _, params) => params.clone(),
            // Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
            //     vec![*lhs, *rhs]
            // }
            Instruction::GetRuntime(_)
            | Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _)
            | Instruction::MakeBoolean(_, _)
            | Instruction::MakeFnPtr(_, _) => Vec::new(),
            Instruction::Widen { input, .. } => vec![*input],
            Instruction::RecordGet {
                record,
                key: RecordKey::Value(v),
                ..
            } => vec![*record, *v],
            Instruction::RecordGet {
                record,
                key: RecordKey::InternalSlot(_),
                ..
            } => vec![*record],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Value(v),
                value,
            } => vec![*record, *v, *value],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::InternalSlot(_),
                value,
            } => vec![*record, *value],
            Instruction::Unreachable
            | Instruction::Noop
            | Instruction::RecordNew(_)
            | Instruction::MakeNull(_)
            | Instruction::MakeUndefined(_) => vec![],
            Instruction::OpLessThan(inst) => inst.used_registers().to_vec(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(_, _, params) => params.iter_mut().collect::<Vec<_>>(),
            // Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
            //     vec![*lhs, *rhs]
            // }
            Instruction::GetRuntime(_)
            | Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _)
            | Instruction::MakeBoolean(_, _)
            | Instruction::MakeFnPtr(_, _) => Vec::new(),
            Instruction::Widen { input, .. } => vec![input],
            Instruction::RecordGet {
                record,
                key: RecordKey::Value(v),
                ..
            } => vec![record, v],
            Instruction::RecordGet {
                record,
                key: RecordKey::InternalSlot(_),
                ..
            } => vec![record],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Value(v),
                value,
            } => vec![record, v, value],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::InternalSlot(_),
                value,
            } => vec![record, value],
            Instruction::Unreachable
            | Instruction::Noop
            | Instruction::RecordNew(_)
            | Instruction::MakeNull(_)
            | Instruction::MakeUndefined(_) => vec![],
            Instruction::OpLessThan(inst) => inst.used_registers_mut(),
        }
    }
}

impl EndInstruction {
    pub fn used_registers(&self) -> Vec<RegisterId<AssemblerCtx>> {
        match self {
            EndInstruction::Jump(path) => path.1.clone(),
            EndInstruction::JumpIf {
                condition,
                true_path,
                false_path,
            } => {
                let mut r = vec![*condition];
                r.extend(true_path.1.clone());
                r.extend(false_path.1.clone());
                r
            }
            EndInstruction::Return(Some(r)) => vec![*r],
            EndInstruction::Return(None) => vec![],
            EndInstruction::Unreachable => vec![],
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<AssemblerCtx>> {
        match self {
            EndInstruction::Jump(path) => path.1.iter_mut().collect(),
            EndInstruction::JumpIf {
                condition,
                true_path,
                false_path,
            } => {
                let mut r = vec![condition];
                r.extend(true_path.1.iter_mut());
                r.extend(false_path.1.iter_mut());
                r
            }
            EndInstruction::Return(Some(r)) => vec![r],
            EndInstruction::Return(None) => vec![],
            EndInstruction::Unreachable => vec![],
        }
    }
}
