use rustc_hash::{FxHashMap, FxHashSet};

use crate::backend::llvm::{
    self, BackendIR, Callable, Constant, ExternalFunction, Function, LLVMLinkage, NumberValue,
    OpaqueStruct, Parameter,
};

use crate::frontend::assembler::{self, Program, ReturnType};
use crate::frontend::old_types::{RecordShape, RegMap, ShapeKey};
use crate::frontend::type_annotater;
use crate::isa::{LessThan, RecordKey, TrivialItem};
use crate::{id::*, UnwrapNone};

use super::llvm::{Struct, ValueType};

struct ConstantMapper<'constants, 'name> {
    constants: &'constants FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
    constants_llvm: FxHashMap<ConstantId<LlvmCtx>, Constant<'name>>,
}

impl<'c, 'n> ConstantMapper<'c, 'n> {
    pub fn new(constants: &'c FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>) -> Self {
        Self {
            constants,
            constants_llvm: Default::default(),
        }
    }

    pub fn map_all(&mut self) {
        for (key, payload) in self.constants.iter() {
            self.map_constant(*key, payload.clone());
        }
    }

    fn map_constant(
        &mut self,
        id: ConstantId<AssemblerCtx>,
        payload: Vec<u8>,
    ) -> ConstantId<LlvmCtx> {
        let llvm_id = id.map_context();

        self.constants_llvm
            .insert(llvm_id, Constant { name: "", payload })
            .expect_free();

        llvm_id
    }

    pub fn map_existing(&self, id: ConstantId<AssemblerCtx>) -> ConstantId<LlvmCtx> {
        match self.try_map_existing(id) {
            Some(id) => id,
            None => panic!("contract error: `map_existing` called when fallible"),
        }
    }

    fn try_map_existing(&self, id: ConstantId<AssemblerCtx>) -> Option<ConstantId<LlvmCtx>> {
        let id = id.map_context();
        self.constants_llvm.contains_key(&id).then_some(id)
    }

    pub fn const_len(&self, id: ConstantId<LlvmCtx>) -> usize {
        self.constants_llvm
            .get(&id)
            .expect("should've gotten constant")
            .payload
            .len()
    }

    pub fn into_llvm_constants(self) -> FxHashMap<ConstantId<LlvmCtx>, Constant<'n>> {
        self.constants_llvm
    }
}

struct RuntimeTypes {
    opaque_structs: FxHashMap<OpaqueStructId<LlvmCtx>, OpaqueStruct<'static>>,
    counter: Counter<OpaqueStructId<LlvmCtx>>,
    pub runtime: OpaqueStructId<LlvmCtx>,
    // pub any: OpaqueStructId<LlvmCtx>, --> this is Value right
    pub value: OpaqueStructId<LlvmCtx>,
    pub string: OpaqueStructId<LlvmCtx>,
}

impl RuntimeTypes {
    pub fn new() -> Self {
        let mut me = Self {
            opaque_structs: Default::default(),
            counter: Default::default(),
            runtime: OpaqueStructId::new(),
            value: OpaqueStructId::new(),
            string: OpaqueStructId::new(),
        };
        me.init();
        me
    }

    fn init(&mut self) {
        self.runtime = self.insert("struct.Runtime");
        self.value = self.insert("struct.Any");
        self.string = self.insert("struct.String");
    }

    fn insert(&mut self, name: &'static str) -> OpaqueStructId<LlvmCtx> {
        let id = self.counter.next();
        self.opaque_structs.insert(id, OpaqueStruct { name });
        id
    }

    pub fn into_opaque_structs(self) -> FxHashMap<OpaqueStructId<LlvmCtx>, OpaqueStruct<'static>> {
        self.opaque_structs
    }
}

struct ExternalFunctionMapper<'rt_types> {
    rt_types: &'rt_types RuntimeTypes,
    external_functions: FxHashMap<ExternalFunctionId<LlvmCtx>, ExternalFunction>,
    assembler_id_map: FxHashMap<ExternalFunctionId<AssemblerCtx>, ExternalFunctionId<LlvmCtx>>,
    counter: Counter<ExternalFunctionId<LlvmCtx>>,
    jssatrt_runtime_new: ExternalFunctionId<LlvmCtx>,
    jssatrt_string_new_utf16: ExternalFunctionId<LlvmCtx>,
    jssatrt_any_new_string: ExternalFunctionId<LlvmCtx>,
    jssatrt_any_new_int: ExternalFunctionId<LlvmCtx>,
}

impl<'r> ExternalFunctionMapper<'r> {
    pub fn new(rt_types: &'r RuntimeTypes) -> Self {
        let mut me = Self {
            rt_types,
            external_functions: Default::default(),
            assembler_id_map: Default::default(),
            counter: Default::default(),
            jssatrt_runtime_new: Default::default(),
            jssatrt_string_new_utf16: Default::default(),
            jssatrt_any_new_string: Default::default(),
            jssatrt_any_new_int: Default::default(),
        };
        me.init();
        me
    }

    fn init(&mut self) {
        let rt_types = self.rt_types;
        self.jssatrt_runtime_new = self.insert(
            "jssatrt_runtime_new".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.runtime).into_ptr()),
            vec![],
        );

        self.jssatrt_string_new_utf16 = self.insert(
            "jssatrt_string_new_utf16".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.string).into_ptr()),
            vec![
                ValueType::Opaque(rt_types.runtime).into_ptr(),
                ValueType::BitType(16).into_ptr(),
                ValueType::WordSizeBitType,
            ],
        );

        self.jssatrt_any_new_string = self.insert(
            "jssatrt_any_new_string".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.value).into_ptr()),
            vec![
                ValueType::Opaque(rt_types.runtime).into_ptr(),
                ValueType::Opaque(rt_types.string).into_ptr(),
            ],
        );

        self.jssatrt_any_new_int = self.insert(
            "jssatrt_any_new_int".into(),
            llvm::ReturnType::Value(ValueType::Opaque(rt_types.value).into_ptr()),
            vec![
                ValueType::Opaque(rt_types.runtime).into_ptr(),
                ValueType::BitType(64),
            ],
        );
    }

    fn insert(
        &mut self,
        name: String,
        return_type: llvm::ReturnType,
        parameters: Vec<ValueType>,
    ) -> ExternalFunctionId<LlvmCtx> {
        let llvm_id = self.counter.next();

        let ext_fn = ExternalFunction {
            name,
            return_type,
            parameters,
        };

        self.external_functions
            .insert(llvm_id, ext_fn)
            .expect_free();

        llvm_id
    }

    pub fn map_existing(
        &self,
        id: ExternalFunctionId<AssemblerCtx>,
    ) -> ExternalFunctionId<LlvmCtx> {
        match self.try_map_existing(id) {
            Some(id) => id,
            None => panic!("contract error: `map_existing` called when fallible"),
        }
    }

    fn try_map_existing(
        &self,
        id: ExternalFunctionId<AssemblerCtx>,
    ) -> Option<ExternalFunctionId<LlvmCtx>> {
        self.assembler_id_map.get(&id).copied()
    }

    pub fn extend(
        &mut self,
        assembler_ext_fns: FxHashMap<ExternalFunctionId<AssemblerCtx>, assembler::ExternalFunction>,
    ) {
        for (key, value) in assembler_ext_fns.into_iter() {
            let parameters = (value.parameters.into_iter())
                .map(|p| p.into_llvm_notrecord(self.rt_types, None))
                .collect();

            let llvm_id = self.insert(
                value.name,
                value.returns.into_llvm_notrecord(self.rt_types, None),
                parameters,
            );

            self.assembler_id_map.insert(key, llvm_id).expect_free();
        }
    }

    pub fn into_external_functions(
        self,
    ) -> FxHashMap<ExternalFunctionId<LlvmCtx>, ExternalFunction> {
        self.external_functions
    }
}

impl assembler::ReturnType {
    fn into_llvm(
        self,
        rt_types: &RuntimeTypes,
        struct_resolver: &RecordStructBuilder,
    ) -> llvm::ReturnType {
        match self {
            ReturnType::Void => llvm::ReturnType::Void,
            ReturnType::Value(v) => llvm::ReturnType::Value(v.into_llvm(rt_types, struct_resolver)),
        }
    }

    fn into_llvm_notrecord(
        self,
        rt_types: &RuntimeTypes,
        struct_resolver: Option<&RecordStructBuilder>,
    ) -> llvm::ReturnType {
        match self {
            ReturnType::Void => llvm::ReturnType::Void,
            ReturnType::Value(v) => {
                llvm::ReturnType::Value(v.into_llvm_notrecord(rt_types, struct_resolver))
            }
        }
    }
}

impl type_annotater::ValueType {
    fn into_llvm(
        self,
        rt_types: &RuntimeTypes,
        struct_resolver: &RecordStructBuilder,
    ) -> llvm::ValueType {
        match self {
            type_annotater::ValueType::Any => llvm::ValueType::Opaque(rt_types.value).into_ptr(),
            type_annotater::ValueType::Runtime => {
                llvm::ValueType::Opaque(rt_types.runtime).into_ptr()
            }
            type_annotater::ValueType::Number | type_annotater::ValueType::ExactInteger(_) => {
                llvm::ValueType::BitType(64)
            }
            type_annotater::ValueType::Record(r) => {
                llvm::ValueType::Defined(struct_resolver.get_type(r).id).into_ptr()
            }
            // function pointers aren't implemented as actual function pointers but a number that switches
            // where to go based on the value
            type_annotater::ValueType::FnPtr(_) => llvm::ValueType::WordSizeBitType,
            // TODO: i keep having to repeat the value types for LLVM things, maybe there's a trait i can implement
            // for value types or something
            // like go look at the code to for MakeNull, i have to push a number
            // and then here i have to add it here too
            // it's a mega todo to fix that
            type_annotater::ValueType::Null | type_annotater::ValueType::Undefined => {
                llvm::ValueType::BitType(1)
            }
            t => unimplemented!("for {:?}", t),
        }
    }

    fn into_llvm_notrecord(
        self,
        rt_types: &RuntimeTypes,
        struct_resolver: Option<&RecordStructBuilder>,
    ) -> llvm::ValueType {
        match self {
            type_annotater::ValueType::Any => llvm::ValueType::Opaque(rt_types.value).into_ptr(),
            type_annotater::ValueType::Runtime => {
                llvm::ValueType::Opaque(rt_types.runtime).into_ptr()
            }
            type_annotater::ValueType::Number | type_annotater::ValueType::ExactInteger(_) => {
                llvm::ValueType::BitType(64)
            }
            type_annotater::ValueType::Record(r) => {
                llvm::ValueType::Defined(struct_resolver.unwrap().get_type(r).id)
            }
            t => unimplemented!("for {:?}", t),
        }
    }
}

struct RegisterMapper {
    assembler_register_map: FxHashMap<RegisterId<AssemblerCtx>, RegisterId<LlvmCtx>>,
    counter: Counter<RegisterId<LlvmCtx>>,
}

impl RegisterMapper {
    pub fn new() -> Self {
        Self {
            assembler_register_map: Default::default(),
            counter: Default::default(),
        }
    }

    pub fn new_register(&self) -> RegisterId<LlvmCtx> {
        self.counter.next()
    }

    pub fn map(&mut self, id: RegisterId<AssemblerCtx>) -> RegisterId<LlvmCtx> {
        // TODO: use `new_register` when rust gets split borrows
        let counter = &self.counter;
        *(self.assembler_register_map)
            .entry(id)
            .or_insert_with(|| counter.next())
    }
}

#[derive(Default)]
struct GlobalRecordStructBuilder<'rt_types, 'duration, 'counter> {
    counter: Counter<StructId<LlvmCtx>>,
    builders: FxHashMap<
        (FunctionId<AssemblerCtx>, BlockId<AssemblerCtx>),
        RecordStructBuilder<'rt_types, 'duration, 'counter>,
    >,
}

impl<'rt, 'd, 'c> GlobalRecordStructBuilder<'rt, 'd, 'c> {
    pub fn get(
        &self,
        fn_id: FunctionId<AssemblerCtx>,
        b_id: BlockId<AssemblerCtx>,
    ) -> &RecordStructBuilder<'rt, 'd, 'c> {
        self.builders.get(&(fn_id, b_id)).unwrap()
    }

    // TODO: somehow fix this to accept a `self`
    pub fn into_structs(&self) -> FxHashMap<StructId<LlvmCtx>, Struct> {
        let mut structs = FxHashMap::default();

        for (_, s) in self.builders.iter() {
            for (k, ss) in s.structs.iter() {
                structs.insert(*k, ss.llvm.clone());
            }
        }

        structs
    }
}

struct RecordStructBuilder<'rt_types, 'register_map, 'counter> {
    rt_types: &'rt_types RuntimeTypes,
    reg_map: &'register_map RegMap<AssemblerCtx>,
    counter: &'counter Counter<StructId<LlvmCtx>>,
    mapping: FxHashMap<AllocationId<NoContext>, StructId<LlvmCtx>>,
    structs: FxHashMap<StructId<LlvmCtx>, SkeletonStruct>,
}

struct SkeletonStruct {
    id: StructId<LlvmCtx>,
    llvm: Struct,
    field_idx: FxHashMap<ShapeKey, usize>,
}

impl<'rt, 'r, 'c> RecordStructBuilder<'rt, 'r, 'c> {
    pub fn new(
        rt_types: &'rt RuntimeTypes,
        reg_map: &'r RegMap<AssemblerCtx>,
        counter: &'c Counter<StructId<LlvmCtx>>,
    ) -> Self {
        Self {
            rt_types,
            reg_map,
            counter,
            mapping: Default::default(),
            structs: Default::default(),
        }
    }

    pub fn build_type<'a>(
        &mut self,
        id: AllocationId<NoContext>,
        history: impl Iterator<Item = &'a RecordShape>,
    ) {
        let mut field_idx = FxHashMap::default();

        let history = history.collect::<Vec<_>>();
        let shape = history
            .into_iter()
            .fold(RecordShape::default(), |a, b| a.union(b));

        let struct_id = self.counter.next();
        let mut fields = vec![];

        for (idx, (k, v)) in shape.fields().enumerate() {
            // don't bother making structurs for constant fields, since we can always assume the values there
            // TODO: can we somehow have the prior phases of the compielr handle this?
            if self.reg_map.is_const_typ(v) {
                continue;
            }

            field_idx.insert(k.clone(), idx);
            fields.push(v.clone().into_llvm(self.rt_types, &self));
        }

        self.structs
            .insert(
                struct_id,
                SkeletonStruct {
                    id: struct_id,
                    llvm: Struct { fields },
                    field_idx,
                },
            )
            .expect_free();

        self.mapping.insert(id, struct_id).expect_free();
    }

    pub fn get_type(&self, id: AllocationId<NoContext>) -> &SkeletonStruct {
        let s_id = self.mapping.get(&id).unwrap();
        self.structs.get(s_id).unwrap()
    }
}

pub fn translate(program: Program) -> BackendIR<'static> {
    let mut constants = ConstantMapper::new(&program.constants);
    constants.map_all();

    let rt_types = RuntimeTypes::new();

    let mut global_structs = GlobalRecordStructBuilder::default();
    for (f_id, f) in program.functions.iter() {
        for (b_id, b) in f.blocks.iter() {
            let mut record_struct_builder =
                RecordStructBuilder::new(&rt_types, &b.register_types, &global_structs.counter);
            for (id, history) in b.register_types.allocations() {
                record_struct_builder.build_type(
                    id,
                    history
                        .iter()
                        .map(|id| b.register_types.get_shape_by_id(id)),
                );
            }
            global_structs
                .builders
                .insert((*f_id, *b_id), record_struct_builder);
        }
    }

    let mut ext_fns = ExternalFunctionMapper::new(&rt_types);
    ext_fns.extend(program.external_functions.clone());

    let mut functions = FxHashMap::default();
    for (fn_id, function) in program.functions.iter() {
        let struct_defs = global_structs
            .builders
            .get(&(*fn_id, function.entry_block))
            .unwrap();
        let id = fn_id.map_context();

        let is_entrypoint = id == program.entrypoint.map_context();

        let name = is_entrypoint.then_some("main").unwrap_or("");
        let linkage = is_entrypoint
            .then_some(Some(LLVMLinkage::External))
            .unwrap_or(None);

        let return_type = function
            .return_type
            .clone()
            .into_llvm(&rt_types, struct_defs);

        let mut reg_map = RegisterMapper::new();
        let mut runtime = None;

        let parameters = if is_entrypoint {
            // TODO: support `argc`/`argv`/etc.
            vec![]
        } else {
            let runtime_reg = reg_map.new_register();
            runtime = Some(runtime_reg);

            let mut params = vec![Parameter {
                r#type: llvm::ValueType::Opaque(rt_types.runtime).into_ptr(),
                register: runtime_reg,
            }];

            params.extend(
                (function.entry_block().parameters.iter()).map(|p| Parameter {
                    r#type: p.typ.clone().into_llvm(&rt_types, struct_defs),
                    register: reg_map.map(p.register),
                }),
            );

            params
        };

        let entry_block = function.entry_block.map_context();

        let mut blocks: FxHashMap<
            BlockId<LlvmCtx>,
            (Vec<llvm::Instruction>, Vec<llvm::Instruction>),
        > = FxHashMap::default();

        for (&id, block) in function.blocks.iter() {
            let llvm_id = id.map_context();
            let phi_insts = Vec::with_capacity(block.parameters.len());
            let instructions = Vec::new();

            blocks.insert(llvm_id, (phi_insts, instructions));
        }

        for (&id, block) in function.blocks.iter() {
            let llvm_id = id.map_context();
            let is_entryblock = llvm_id == entry_block;

            let (phi_insts, instructions) = blocks.get_mut(&llvm_id).unwrap();

            if !is_entryblock {
                for p in block.parameters.iter() {
                    phi_insts.push(llvm::Instruction::Phi(reg_map.map(p.register), vec![]))
                }
            }

            // this isn't exactly a sound guarantee to make, since it's always
            // possible for consumers of jssat as a library (if that happens)
            // to emit a method which jumps back to the entry block, but we'll
            // assume that they don't do that
            let runtime = match runtime {
                Some(r) => r,
                None => {
                    let rt_reg = reg_map.new_register();

                    instructions.push(llvm::Instruction::Call(
                        Some(rt_reg),
                        Callable::External(ext_fns.jssatrt_runtime_new),
                        vec![],
                    ));

                    rt_reg
                }
            };

            let reg_types = &block.register_types;

            let defined_structs = global_structs.get(*fn_id, id);

            let mut rt_regs = FxHashSet::default();
            for instruction in block.instructions.iter() {
                match instruction {
                    &assembler::Instruction::MakeFnPtr(result, block_id) => {
                        // fnptrs are implemented with a number and switch where to go based on that number
                        // TODO: make function pointer values "smartly" allocated - e.g. for fnptrs like
                        // BlockId(2) | BlockId(7) | BlockId(9) instead of => 2 | 7 | 9 it should be allocated
                        // numbers 0, 1, and 2
                        instructions.push(llvm::Instruction::LoadNumber {
                            result: reg_map.map(result),
                            value: NumberValue::UnsignedNative(block_id.value()),
                        });
                    }
                    assembler::Instruction::Widen {
                        result,
                        input,
                        from,
                        to,
                    } => {
                        //
                        match (from, to) {
                            (
                                type_annotater::ValueType::ExactString(_),
                                type_annotater::ValueType::Any,
                            ) => {
                                instructions.push(llvm::Instruction::Call(
                                    Some(reg_map.map(*result)),
                                    Callable::External(ext_fns.jssatrt_any_new_string),
                                    vec![runtime, reg_map.map(*input)],
                                ));
                            }
                            (
                                type_annotater::ValueType::ExactInteger(_),
                                type_annotater::ValueType::Any,
                            ) => {
                                instructions.push(llvm::Instruction::Call(
                                    Some(reg_map.map(*result)),
                                    Callable::External(ext_fns.jssatrt_any_new_int),
                                    vec![runtime, reg_map.map(*input)],
                                ));
                            }
                            (f, t) => {
                                unimplemented!("conversion not implemented for {:?} -> {:?}", f, t)
                            }
                        };
                    }
                    assembler::Instruction::Call(
                        result,
                        assembler::Callable::Static(fn_id),
                        args,
                    ) => {
                        let mut calling_args = vec![runtime];
                        calling_args.extend(args.iter().map(|r| reg_map.map(*r)));

                        instructions.push(llvm::Instruction::Call(
                            result.map(|r| reg_map.map(r)),
                            llvm::Callable::Static(fn_id.map_context()),
                            calling_args,
                        ));
                    }
                    assembler::Instruction::Call(
                        result,
                        assembler::Callable::Extern(fn_id),
                        args,
                    ) => {
                        let calling_args = args
                            .iter()
                            .map(|r| {
                                if rt_regs.contains(r) {
                                    // TODO: encode this into `reg_map`?
                                    runtime
                                } else {
                                    reg_map.map(*r)
                                }
                            })
                            .collect();

                        instructions.push(llvm::Instruction::Call(
                            result.map(|r| reg_map.map(r)),
                            llvm::Callable::External(ext_fns.map_existing(*fn_id)),
                            calling_args,
                        ));
                    }
                    &assembler::Instruction::MakeTrivial(inst) => {
                        if let TrivialItem::Runtime = inst.item {
                            rt_regs.insert(inst.result);
                        } else {
                            instructions.push(llvm::Instruction::LoadNumber {
                                result: reg_map.map(inst.result),
                                value: NumberValue::SignedArbitrary(1, 0),
                            });
                        }
                    }
                    &assembler::Instruction::MakeString(result, id) => {
                        let const_ptr_8 = reg_map.new_register();
                        let id = constants.map_existing(id);
                        instructions.push(llvm::Instruction::LoadConstantPtr(const_ptr_8, id));

                        let const_ptr_16 = reg_map.new_register();
                        instructions.push(llvm::Instruction::ChangePtrSize {
                            result: const_ptr_16,
                            input: const_ptr_8,
                            size: ValueType::BitType(16),
                        });

                        let const_len = reg_map.new_register();

                        let numeric_const_len = constants.const_len(id);
                        debug_assert!(numeric_const_len % 2 == 0);
                        instructions.push(llvm::Instruction::LoadNumber {
                            result: const_len,
                            value: NumberValue::UnsignedNative(numeric_const_len / 2),
                        });

                        let result = reg_map.map(result);
                        instructions.push(llvm::Instruction::Call(
                            Some(result),
                            Callable::External(ext_fns.jssatrt_string_new_utf16),
                            vec![runtime, const_ptr_16, const_len],
                        ));
                    }
                    &assembler::Instruction::MakeNumber(result, value) => {
                        instructions.push(llvm::Instruction::LoadNumber {
                            result: reg_map.map(result),
                            value: NumberValue::SignedArbitrary(64, value),
                        });
                    }
                    &assembler::Instruction::MakeBoolean(result, value) => {
                        // match C convention, opts should be centered around this
                        let value = match value {
                            true => NumberValue::UnsignedArbitrary(1, 1),
                            false => NumberValue::UnsignedArbitrary(1, 0),
                        };

                        instructions.push(llvm::Instruction::LoadNumber {
                            result: reg_map.map(result),
                            value,
                        });
                    }
                    assembler::Instruction::Unreachable => {
                        instructions.push(llvm::Instruction::Unreachable);
                    }
                    assembler::Instruction::Noop => {}
                    &assembler::Instruction::RecordNew(r_o) => {
                        if let type_annotater::ValueType::Record(r) = reg_types.get(r_o) {
                            let s = defined_structs.get_type(*r);

                            let result = reg_map.map(r_o);

                            instructions.push(llvm::Instruction::New {
                                result,
                                struct_type: s.id,
                            });
                        } else {
                            panic!("cannot new record")
                        }
                    }
                    assembler::Instruction::RecordGet {
                        result,
                        record,
                        key,
                    } => {
                        if let type_annotater::ValueType::Record(r) = reg_types.get(*record) {
                            let s = defined_structs.get_type(*r);

                            let shape_key = match *key {
                                RecordKey::Prop(r) => match reg_types.get(r) {
                                    type_annotater::ValueType::String => ShapeKey::String,
                                    type_annotater::ValueType::ExactString(s) => {
                                        ShapeKey::Str(s.clone())
                                    }
                                    type_annotater::ValueType::Any
                                    | type_annotater::ValueType::Runtime
                                    | type_annotater::ValueType::Number
                                    | type_annotater::ValueType::ExactInteger(_)
                                    | type_annotater::ValueType::Boolean
                                    | type_annotater::ValueType::Bool(_)
                                    | type_annotater::ValueType::Undefined
                                    | type_annotater::ValueType::Null
                                    | type_annotater::ValueType::Record(_)
                                    | type_annotater::ValueType::FnPtr(_) => todo!(),
                                },
                                RecordKey::Slot(r) => ShapeKey::InternalSlot(r),
                            };

                            let mut field_idx = None;
                            for (k, idx) in s.field_idx.iter() {
                                if k == &shape_key {
                                    field_idx = Some(*idx);
                                }
                            }
                            let field_idx = field_idx.unwrap();

                            instructions.push(llvm::Instruction::Load {
                                result: reg_map.map(*result),
                                from_struct: reg_map.map(*record),
                                field_index: field_idx,
                            });
                        } else {
                            panic!("cannot get non-record")
                        }
                    }
                    assembler::Instruction::RecordSet {
                        record, key, value, ..
                    } => {
                        if let type_annotater::ValueType::Record(r) = reg_types.get(*record) {
                            let s = defined_structs.get_type(*r);

                            // TODO: deduplicate this
                            let shape_key = match *key {
                                RecordKey::Prop(r) => match reg_types.get(r) {
                                    type_annotater::ValueType::String => ShapeKey::String,
                                    type_annotater::ValueType::ExactString(s) => {
                                        ShapeKey::Str(s.clone())
                                    }
                                    type_annotater::ValueType::Any
                                    | type_annotater::ValueType::Runtime
                                    | type_annotater::ValueType::Number
                                    | type_annotater::ValueType::ExactInteger(_)
                                    | type_annotater::ValueType::Boolean
                                    | type_annotater::ValueType::Bool(_)
                                    | type_annotater::ValueType::Undefined
                                    | type_annotater::ValueType::Null
                                    | type_annotater::ValueType::Record(_)
                                    | type_annotater::ValueType::FnPtr(_) => todo!(),
                                },
                                RecordKey::Slot(r) => ShapeKey::InternalSlot(r),
                            };

                            let mut field_idx = None;
                            for (k, idx) in s.field_idx.iter() {
                                if k == &shape_key {
                                    field_idx = Some(*idx);
                                }
                            }
                            let field_idx = field_idx.unwrap();

                            instructions.push(llvm::Instruction::Store {
                                value: reg_map.map(*value),
                                into_struct: reg_map.map(*record),
                                field_index: field_idx,
                            });
                        } else {
                            panic!("cannot get non-record")
                        }
                    }
                    assembler::Instruction::OpLessThan(inst) => {
                        let result = reg_map.map(inst.result);
                        let lhs = reg_map.map(inst.lhs);
                        let rhs = reg_map.map(inst.rhs);
                        instructions.push(llvm::Instruction::OpLessThan(LessThan {
                            result,
                            lhs,
                            rhs,
                        }));
                    }
                }
            }

            let mut phi_jump = |from: BlockId<LlvmCtx>, blk_jmp: &assembler::BlockJump| {
                let target_id = blk_jmp.0.map_context();
                let (phi, _) = blocks.get_mut(&target_id).unwrap();

                debug_assert_eq!(blk_jmp.1.len(), phi.len());
                for (arg, phi_inst) in blk_jmp.1.iter().zip(phi) {
                    if let llvm::Instruction::Phi(_, incoming) = phi_inst {
                        incoming.push((from, reg_map.map(*arg)));
                    } else {
                        unreachable!("only phis should be in the phi side");
                    }
                }

                //
                target_id
            };

            match &block.end {
                assembler::EndInstruction::Unreachable => {
                    let (_, instructions) = blocks.get_mut(&llvm_id).unwrap();
                    instructions.push(llvm::Instruction::Unreachable);
                }
                assembler::EndInstruction::Jump(jump) => {
                    let blk_id = phi_jump(llvm_id, jump);
                    let (_, instructions) = blocks.get_mut(&llvm_id).unwrap();
                    instructions.push(llvm::Instruction::Jump(blk_id));
                }
                assembler::EndInstruction::JumpIf {
                    condition,
                    true_path,
                    false_path,
                } => {
                    let true_path = phi_jump(llvm_id, true_path);
                    let false_path = phi_jump(llvm_id, false_path);
                    let (_, instructions) = blocks.get_mut(&llvm_id).unwrap();
                    instructions.push(llvm::Instruction::JumpIf {
                        condition: reg_map.map(*condition),
                        true_path,
                        false_path,
                    });
                }
                assembler::EndInstruction::Return(register) => {
                    let (_, instructions) = blocks.get_mut(&llvm_id).unwrap();
                    instructions.push(llvm::Instruction::Return(register.map(|r| reg_map.map(r))));
                }
            };
        }

        let blocks = blocks
            .into_iter()
            .map(|(k, (mut phi, inst))| {
                (k, {
                    phi.extend(inst);
                    phi
                })
            })
            .collect::<FxHashMap<_, _>>();

        let llvm_function = Function {
            name,
            linkage,
            return_type,
            parameters,
            entry_block,
            blocks,
        };

        functions.insert(id, llvm_function);
    }

    let constants = constants.into_llvm_constants();
    let external_functions = ext_fns.into_external_functions();
    let structs = global_structs.into_structs();
    let opaque_structs = rt_types.into_opaque_structs();

    BackendIR {
        constants,
        opaque_structs,
        structs,
        external_functions,
        functions,
    }
}
