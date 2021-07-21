//! Assembles together the informatino from the original IR, and type_annotator
//! passes into a fully conherent IR, which will then be passed to `skeleton`.

use std::{collections::VecDeque, fmt::Debug, sync::Mutex};

use crate::isa::{
    CallExtern, CallStatic, CallVirt, ISAInstruction, MakeTrivial, OpLessThan, RecordKey,
};
use crate::retag::{ExtFnPassRetagger, RegGenRetagger, RegMapRetagger, RegRetagger};

use super::{
    conv_only_bb::{self, PureBlocks},
    ir,
    ir::{FFIValueType, IR},
    old_types::{RegMap, ShapeKey},
    type_annotater::{self, InvocationArgs, TypeAnnotations, TypeInformation, ValueType},
};
use crate::id::*;
use crate::retag::{BlkPassRetagger, FnRetagger};
use crate::retag::{BlkRetagger, RegPassRetagger};
use crate::retag::{ExtFnRetagger, FnPassRetagger};
use rustc_hash::FxHashMap;

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
    MakeTrivial(MakeTrivial<AssemblerCtx>),
    MakeFnPtr(RegisterId<AssemblerCtx>, BlockId<PureBbCtx>),
    MakeString(RegisterId<AssemblerCtx>, ConstantId<AssemblerCtx>),
    MakeNumber(RegisterId<AssemblerCtx>, i64),
    MakeBoolean(RegisterId<AssemblerCtx>, bool),
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
            println!("=== assembling new function ===");
            let (assembled, to_assemble) = FnAssembler::new(&self, type_info).assemble();
            println!("=== done assembling new function ===");
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
        let mut ext_retagger = ExtFnPassRetagger::default();
        let external_functions = (self.ir.external_functions.iter())
            .map(|(k, v)| (ext_retagger.retag_new(*k), map_ext_fn(v.clone())))
            .collect::<FxHashMap<_, _>>();

        Program {
            entrypoint,
            constants,
            external_functions,
            functions,
        }
    }
}

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
        let mut to_assemble = Vec::new();
        let mut reg_map = RegMapRetagger::default();

        let mut ext_fn_retagger = ExtFnPassRetagger::default();
        for (id, _) in self.assembler.ir.external_functions.iter() {
            ext_fn_retagger.retag_new(*id);
        }

        let mut block_queue = VecDeque::new();
        block_queue.push_back(entry_block);

        while let Some(block_id) = block_queue.pop_front() {
            eprintln!("=======================> blk {:?}", block_id);

            if blocks.contains_key(&block_id) {
                continue;
            }

            let reg_typs = self
                .type_info_for_assembler_block(block_id)
                .register_types
                .clone();

            let (block, to_visit, reg_types) = InstWriter::new(
                &self,
                block_id,
                &mut reg_map,
                &mut to_assemble,
                reg_typs,
                &ext_fn_retagger,
            )
            .write();
            println!("completed writing block, left: {:?}", block_queue);
            blocks.insert(block_id, block);

            register_types.extend(reg_types);

            block_queue.extend(to_visit);
            println!("completed writing block, left++: {:?}", block_queue);
        }

        let return_type = self.type_info.return_type.clone().into_type();

        println!("done");
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
struct InstWriter<'duration, R, RF> {
    assembler_block_id: BlockId<AssemblerCtx>,
    fn_assembler: &'duration FnAssembler<'duration>,
    retagger: &'duration mut R,
    ext_fn_retagger: &'duration RF,
    type_info: &'duration TypeInformation,
    to_assemble: &'duration mut Vec<BlockId<AnnotatedCtx>>,
    bb: &'duration conv_only_bb::Block,
    register_types: RegMap<AssemblerCtx>,
    bb_register_types: RegMap<PureBbCtx>,
    instructions: Vec<Instruction>,
    to_visit: Vec<BlockId<AssemblerCtx>>,
}

enum HaltStatus {
    // Stop,
    Continue,
}

impl<'d, R, RF> InstWriter<'d, R, RF>
where
    R: RegGenRetagger<PureBbCtx, AssemblerCtx> + Debug,
    RF: ExtFnRetagger<IrCtx, AssemblerCtx> + Debug,
{
    pub fn new(
        fn_assembler: &'d FnAssembler<'d>,
        block_id: BlockId<AssemblerCtx>,
        retagger: &'d mut R,
        to_assemble: &'d mut Vec<BlockId<AnnotatedCtx>>,
        bb_register_types: RegMap<PureBbCtx>,
        ext_fn_retagger: &'d RF,
    ) -> Self {
        let type_info = fn_assembler.type_info_for_assembler_block(block_id);
        let bb = (fn_assembler.assembler.pure_bocks).get_block(type_info.pure_id);

        println!(
            "we are clling this block wti hreg typs: {:#?}",
            bb_register_types
        );
        let register_types = bb_register_types.duplicate_with_allocations();

        Self {
            assembler_block_id: block_id,
            fn_assembler,
            retagger,
            type_info,
            to_assemble,
            bb,
            register_types,
            bb_register_types,
            ext_fn_retagger,
            instructions: Default::default(),
            to_visit: Default::default(),
        }
    }

    pub fn write(mut self) -> (Block, Vec<BlockId<AssemblerCtx>>, RegMap<AssemblerCtx>) {
        println!(
            "\n\n\n\n\n\n\n\n\n\n\nwriting: {:?} ({:?}) || {:?}",
            self.bb.id, self.type_info.annotated_id, self.assembler_block_id
        );

        println!("params: {:?}", self.bb.parameters);

        let parameters = self
            .bb
            .parameters
            .iter()
            .map(|r| {
                let parameter = Parameter {
                    typ: self.type_info.register_types.get(*r).clone(),
                    register: self.retagger.retag_new(*r),
                };
                println!(
                    "{:?} |-> {:?} : {:?}",
                    *r, parameter.register, parameter.typ
                );
                if let ValueType::Record(a) = &parameter.typ {
                    println!(":: {:?}", self.register_types.get_shape(*a));
                }
                self.register_types
                    .insert(parameter.register, parameter.typ.clone());
                parameter
            })
            .collect();

        for inst in self.bb.instructions.iter() {
            println!("writing inst: {:?}", inst);
            match self.write_inst(inst) {
                HaltStatus::Continue => continue,
                // _ => break,
            }
        }

        let end = match &self.bb.end {
            ir::ControlFlowInstruction::Jmp(bbjump) => {
                EndInstruction::Jump(self.map_bbjump(&bbjump.0))
            }
            ir::ControlFlowInstruction::JmpIf(inst) => {
                println!("the condition in {:?}", inst);
                let condition = self.retagger.retag_old(inst.condition);
                match self.register_types.get(condition) {
                    ValueType::Bool(true) => EndInstruction::Jump(self.map_bbjump(&inst.if_so)),
                    ValueType::Bool(false) => EndInstruction::Jump(self.map_bbjump(&inst.other)),
                    ValueType::Boolean => EndInstruction::JumpIf {
                        condition,
                        true_path: self.map_bbjump(&inst.if_so),
                        false_path: self.map_bbjump(&inst.other),
                    },
                    _ => unreachable!(
                        "invalid conditional register, this should be caught by typ annotation"
                    ),
                }
            }
            ir::ControlFlowInstruction::Ret(inst) => {
                EndInstruction::Return(inst.retag(self.retagger).0)
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
        println!("writing inst {:?}", inst);
        for u in inst.used_registers() {
            let r = self.retagger.retag_old(u);
            let t = self.register_types.get(r);
            println!("%{}: {:?}", r, t);
        }

        match inst {
            ir::Instruction::Comment(_, _) => {}
            &ir::Instruction::GetFnPtr(inst) => {
                // TODO: dont do this
                let mut hacks = FnPassRetagger::default();
                hacks.retag_new(inst.function);
                let inst = inst.retag(self.retagger, &hacks);

                let static_fn = self
                    .fn_assembler
                    .assembler
                    .ir
                    .functions
                    .get(&inst.function)
                    .unwrap();
                let entry_blk = static_fn.entry_block;
                let pure_bb_id = (self.fn_assembler.assembler.pure_bocks)
                    .get_block_id_by_host(inst.function, entry_blk);
                self.instructions
                    .push(Instruction::MakeFnPtr(inst.result, pure_bb_id));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types
                //     .insert(fnptr, ValueType::FnPtr(pure_bb_id));
            }
            &ir::Instruction::MakeTrivial(inst) => {
                let inst = inst.retag(self.retagger);
                self.instructions
                    .push(Instruction::MakeTrivial(MakeTrivial {
                        result: inst.result,
                        item: inst.item,
                    }));
                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types.insert(
                //     inst.result,
                //     match inst.item {
                //         super::isa::TrivialItem::Runtime => ValueType::Runtime,
                //         super::isa::TrivialItem::Null => ValueType::Null,
                //         super::isa::TrivialItem::Undefined => ValueType::Undefined,
                //         super::isa::TrivialItem::Empty => todo!(), // ValueType::Empty,
                //     },
                // );
            }
            &ir::Instruction::MakeBytes(inst) => {
                let constant = self
                    .fn_assembler
                    .assembler
                    .ir
                    .constants
                    .get(&inst.constant)
                    .unwrap();

                let str_reg = self.retagger.retag_new(inst.result);
                let const_id = self
                    .fn_assembler
                    .assembler
                    .intern_constant(constant.payload.as_slice());

                self.instructions
                    .push(Instruction::MakeString(str_reg, const_id));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types
                //     .insert(str_reg, ValueType::ExactString(constant.payload.clone()));
            }
            &ir::Instruction::MakeInteger(inst) => {
                let inst = inst.retag(self.retagger);
                self.instructions
                    .push(Instruction::MakeNumber(inst.result, inst.value));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types
                //     .insert(inst.result, ValueType::ExactInteger(inst.value));
            }
            &ir::Instruction::OpLessThan(inst) => {
                let cmp_typ = self.type_info.get_type(inst.result);
                let inst = inst.retag(self.retagger);

                // TODO: emit a `CompareLessThan` instruction properly
                // for now i'll just emit MakeBoolean
                // and i'm not introducing `CompareLessThan` rn because im busy rewriting the assembler
                // and annotator and that's a big enough task for now
                if let ValueType::Bool(b) = *cmp_typ {
                    self.instructions
                        .push(Instruction::MakeBoolean(inst.result, b));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types.insert(inst.result, ValueType::Bool(b));
                } else {
                    todo!(
                        "support non const comparison via {:?} {:?}",
                        inst.lhs,
                        inst.rhs
                    );
                }
            }
            &ir::Instruction::OpAdd(inst) => {
                let res_typ = self.type_info.get_type(inst.result);
                let inst = inst.retag(self.retagger);

                // TODO: emit a `Add` instruction properly
                // same case with above
                if let ValueType::ExactInteger(i) = *res_typ {
                    self.instructions
                        .push(Instruction::MakeNumber(inst.result, i));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types
                //     .insert(inst.result, ValueType::ExactInteger(i));
                } else {
                    todo!("suport non const math via {:?} {:?}", inst.lhs, inst.rhs);
                }
            }
            ir::Instruction::CallExtern(inst) => {
                let ext_fn = (self.fn_assembler.assembler.ir.external_functions)
                    .get(&inst.fn_id)
                    .unwrap();

                let inst = inst.clone().retag(self.retagger, self.ext_fn_retagger);

                let val_types = (ext_fn.parameters.iter())
                    .map(|t| t.clone().into_value_type())
                    .collect::<Vec<_>>();

                debug_assert_eq!(inst.args.len(), val_types.len());
                let args = inst
                    .args
                    .into_iter()
                    .zip(val_types.iter())
                    .map(|(register, coercion_type)| {
                        // TODO(far future): don't clone when split/partial borrows come along
                        let register_typ = self.register_types.get(register).clone();
                        self.coerce(register, &register_typ, coercion_type)
                    })
                    .collect::<Vec<_>>();

                self.instructions.push(Instruction::Call(
                    inst.result,
                    Callable::Extern(inst.fn_id),
                    args,
                ));

                if let Some(reg) = inst.result {
                    if let ir::Returns::Value(v) = ext_fn.return_type.clone() {
                        // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                        // self.register_types.insert(reg, v.into_value_type());
                    } else {
                        panic!("expected assignment to a register to be non void");
                    }
                }
            }
            ir::Instruction::CallStatic(inst) => {
                let original_res = inst.result;

                // MUSTDO
                // TODO: remove this
                let mut tmp_retagger = FnPassRetagger::default();
                tmp_retagger.retag_new(inst.fn_id);

                let inst = inst.clone().retag(self.retagger, &tmp_retagger);

                let static_fn = self
                    .fn_assembler
                    .assembler
                    .ir
                    .functions
                    .get(&inst.fn_id)
                    .unwrap();
                let entry_blk = static_fn.entry_block;

                let pure_bb_id = (self.fn_assembler.assembler.pure_bocks)
                    .get_block_id_by_host(inst.fn_id, entry_blk);

                // TODO: use `zip_eq`
                println!("!!!! preparing call!!!!!!1!!!!!!");
                let dest_regs = self
                    .fn_assembler
                    .assembler
                    .pure_bocks
                    .get_block(pure_bb_id)
                    .parameters
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();
                debug_assert_eq!(inst.args.len(), dest_regs.len());
                let invocation_args = self
                    .register_types
                    .prepare_invocation(inst.args.clone().into_iter().zip(dest_regs.into_iter()));
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

                self.instructions.push(Instruction::Call(
                    inst.result,
                    Callable::Static(annotated_fn_id),
                    inst.args,
                ));

                if let (Some(reg), Some(orig_reg)) = (inst.result, original_res) {
                    // TODO: handle never type

                    // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                    // self.register_types
                    //     .insert(reg, self.type_info.get_type(orig_reg).clone());
                } else if inst.result.is_some() {
                    panic!("didnt assign return type");
                }

                self.to_assemble.push(annotated_blk_id);
            }
            ir::Instruction::CallVirt(inst) => {
                let original_res = inst.result;
                let inst = inst.clone().retag(self.retagger);
                let pure_bb_id = if let ValueType::FnPtr(id) = self.register_types.get(inst.fn_ptr)
                {
                    *id
                } else {
                    panic!("cannot virtual call non fnptr")
                };

                // TODO: don't blatantly copy Call(Static())

                // TODO: use `zip_eq`
                println!("!!!! preparing call!!!!!!2!!!!!!");
                let dest_regs = self
                    .fn_assembler
                    .assembler
                    .pure_bocks
                    .get_block(pure_bb_id)
                    .parameters
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();
                debug_assert_eq!(inst.args.len(), dest_regs.len());
                let invocation_args = self
                    .register_types
                    .prepare_invocation(inst.args.clone().into_iter().zip(dest_regs.into_iter()));
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

                self.instructions.push(Instruction::Call(
                    inst.result,
                    Callable::Static(annotated_fn_id),
                    inst.args,
                ));

                if let (Some(reg), Some(orig_reg)) = (inst.result, original_res) {
                    // TODO: handle never type

                    self.register_types
                        .insert(reg, self.type_info.get_type(orig_reg).clone());
                } else if inst.result.is_some() {
                    panic!("didnt assign return type");
                }

                self.to_assemble.push(annotated_blk_id);
            }
            // TODO: the fact that this is literally copied and pasted from type_annotater makes me rethink
            // if this entire `assembler` phase is even necessary
            //
            // i'll probably end up trashing this entire struct
            &ir::Instruction::NewRecord(inst) => {
                let inst = inst.retag(self.retagger);
                self.instructions.push(Instruction::RecordNew(inst.result));
                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // let alloc = self.register_types.insert_alloc();
                // self.register_types
                //     .insert(inst.result, ValueType::Record(alloc));
            }
            &ir::Instruction::RecordGet(inst) => {
                let inst = inst.retag(self.retagger);

                let key = match inst.key {
                    RecordKey::Prop(r) => match self.register_types.get(r) {
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
                    RecordKey::Slot(slot) => ShapeKey::InternalSlot(slot),
                };

                if let ValueType::Record(alloc) = *self.register_types.get(inst.record) {
                    println!("record type is {:?}", alloc);
                    let shape = self.register_types.get_shape(alloc);
                    println!("record shape is {:?}", shape);
                    let prop_value_typ = shape.type_at_key(&key).clone();
                    println!("prop typ {:?}", prop_value_typ);

                    // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                    // self.register_types.insert(inst.result, prop_value_typ);

                    self.instructions.push(Instruction::RecordGet {
                        result: inst.result,
                        record: inst.record,
                        key: inst.key,
                    });
                } else {
                    panic!("cannot call RecordGet on non record");
                }
            }
            ir::Instruction::RecordSet(inst) => {
                let inst = inst.retag(self.retagger);

                let key = match inst.key {
                    RecordKey::Prop(v) => match self.register_types.get(v) {
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
                    RecordKey::Slot(slot) => ShapeKey::InternalSlot(slot),
                };

                if let ValueType::Record(alloc) = *self.register_types.get(inst.record) {
                    let shape = self.register_types.get_shape(alloc);
                    let value_typ = self.register_types.get(inst.value).clone();
                    let shape = shape.add_prop(key, value_typ);
                    let shape_id = self.register_types.insert_shape(shape);
                    self.register_types.assign_new_shape(alloc, shape_id);

                    self.instructions.push(Instruction::RecordSet {
                        shape_id,
                        record: inst.record,
                        key: inst.key,
                        value: inst.value,
                    });
                } else {
                    panic!("cannot call RecordSet on non record");
                }
            }
            &ir::Instruction::OpEquals(inst) => {
                let res_typ = self.type_info.get_type(inst.result);
                let inst = inst.retag(self.retagger);

                // TODO: emit a `CompareEqual` instruction properly
                // same case with above (i.e. Add)
                if let ValueType::Bool(b) = *res_typ {
                    self.instructions
                        .push(Instruction::MakeBoolean(inst.result, b));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types.insert(inst.result, ValueType::Bool(b));
                } else {
                    todo!(
                        "suport non const comparison via {:?} {:?}",
                        inst.lhs,
                        inst.rhs
                    );
                }
            }
            &ir::Instruction::OpNegate(inst) => {
                let res_typ = self.type_info.get_type(inst.result);
                let inst = inst.retag(self.retagger);

                // TODO: emit a `Negate` instruction properly
                // same case with above
                if let ValueType::Bool(b) = *res_typ {
                    self.instructions
                        .push(Instruction::MakeBoolean(inst.result, b));

                // TODO: aren't we suppose to insert types??????????????????/ wtf??????????????????????////////
                // self.register_types.insert(inst.result, ValueType::Bool(b));
                } else {
                    todo!(
                        "suport non const comparison via {:?} {:?}",
                        inst.result,
                        inst.operand
                    );
                }
            }
        };

        if let Some(u) = inst.assigned_to() {
            let r = self.retagger.retag_old(u);
            let t = self.register_types.get(r);
            println!("%{} = {:?}", r, t);
        }

        HaltStatus::Continue
    }

    #[track_caller]
    fn map_bbjump(&mut self, bbjump: &crate::isa::BlockJump<PureBbCtx, PureBbCtx>) -> BlockJump {
        let crate::isa::BlockJump(id, args) = bbjump;

        // TODO: use `zip_eq`
        println!("!!!! preparing cal2l!!!!!!!!!!!!");
        let src_regs = args
            .iter()
            .map(|r| self.retagger.retag_old(*r))
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

        println!("calling in {:?}", bbjump);
        println!("because {}", std::panic::Location::caller());

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
            .map(|r| self.retagger.retag_old(*r))
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

        let result = self.retagger.gen();
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
            Instruction::MakeString(result, _)
            | Instruction::MakeNumber(result, _)
            | Instruction::MakeBoolean(result, _)
            | Instruction::Widen { result, .. }
            | Instruction::RecordNew(result)
            | Instruction::RecordGet { result, .. }
            | Instruction::MakeFnPtr(result, _) => Some(*result),
            Instruction::Unreachable | Instruction::Noop | Instruction::RecordSet { .. } => None,
            Instruction::OpLessThan(inst) => inst.declared_register(),
            Instruction::MakeTrivial(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> Vec<RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(_, _, params) => params.clone(),
            // Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
            //     vec![*lhs, *rhs]
            // }
            Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _)
            | Instruction::MakeBoolean(_, _)
            | Instruction::MakeFnPtr(_, _) => Vec::new(),
            Instruction::Widen { input, .. } => vec![*input],
            Instruction::RecordGet {
                record,
                key: RecordKey::Prop(v),
                ..
            } => vec![*record, *v],
            Instruction::RecordGet {
                record,
                key: RecordKey::Slot(_),
                ..
            } => vec![*record],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Prop(v),
                value,
            } => vec![*record, *v, *value],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Slot(_),
                value,
            } => vec![*record, *value],
            Instruction::Unreachable | Instruction::Noop | Instruction::RecordNew(_) => vec![],
            Instruction::OpLessThan(inst) => inst.used_registers().to_vec(),
            Instruction::MakeTrivial(inst) => inst.used_registers().to_vec(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<AssemblerCtx>> {
        match self {
            Instruction::Call(_, _, params) => params.iter_mut().collect::<Vec<_>>(),
            // Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
            //     vec![*lhs, *rhs]
            // }
            Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _)
            | Instruction::MakeBoolean(_, _)
            | Instruction::MakeFnPtr(_, _) => Vec::new(),
            Instruction::Widen { input, .. } => vec![input],
            Instruction::RecordGet {
                record,
                key: RecordKey::Prop(v),
                ..
            } => vec![record, v],
            Instruction::RecordGet {
                record,
                key: RecordKey::Slot(_),
                ..
            } => vec![record],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Prop(v),
                value,
            } => vec![record, v, value],
            Instruction::RecordSet {
                shape_id: _,
                record,
                key: RecordKey::Slot(_),
                value,
            } => vec![record, value],
            Instruction::Unreachable | Instruction::Noop | Instruction::RecordNew(_) => vec![],
            Instruction::OpLessThan(inst) => inst.used_registers_mut(),
            Instruction::MakeTrivial(inst) => inst.used_registers_mut(),
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
