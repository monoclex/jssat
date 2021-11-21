use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::frontend::ir::*;
use crate::id::{Counter, IdCompat};
use crate::UnwrapNone;

use crate::isa::*;

pub type BlockId = crate::id::BlockId<crate::id::IrCtx>;
pub type FunctionId = crate::id::FunctionId<crate::id::IrCtx>;
pub type ConstantId = crate::id::ConstantId<crate::id::IrCtx>;
pub type RegisterId = crate::id::RegisterId<crate::id::IrCtx>;
pub type ExternalFunctionId = crate::id::ExternalFunctionId<crate::id::IrCtx>;

// TODO: these should be doctests probably, but those don't run in a binary
// crate
#[test]
#[should_panic]
pub fn panics_on_drop_with_function_start_without_function_end() {
    let builder = ProgramBuilder::new();
    #[allow(unused_variables)]
    let (my_fn, []) = builder.start_function();
    // Oops! This accidentally got commented out!
    // builder.end_function(my_fn);
}

pub struct ProgramBuilder {
    pub dealer: AtomDealer,
    entrypoint: Option<FunctionId>,
    constants: Vec<Constant>,
    external_functions: Vec<ExternalFunction>,
    functions: FxHashMap<FunctionId, Function>,
    gen_function_id: Counter<FunctionId>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExtFnIdTyped<const PARAMETERS: usize>(pub ExternalFunctionId);

impl ProgramBuilder {
    pub fn new() -> Self {
        ProgramBuilder {
            dealer: AtomDealer::new(),
            entrypoint: None,
            constants: vec![],
            external_functions: vec![],
            functions: FxHashMap::default(),
            gen_function_id: Counter::new(),
        }
    }

    // TODO: is such a method really necessary? do we have to enforce that all
    // programs in jssat have an endpoint at all?
    /// Automatically creates a blank entrypoint function. This is useful if you
    /// don't need an entrypoint, and are running arbitrary functions with the
    /// interpreter.
    pub fn create_blank_entrypoint(&mut self) -> FnSignature<0> {
        let mut f = self.start_function_main();
        let b = f.start_block_main();
        f.end_block(b.ret(None));
        self.end_function(f)
    }

    pub fn finish(self) -> IR {
        let entrypoint = self.entrypoint.expect("expected an entrypoint function! generate an entrypoint function using ProgramBuilder::start_function_main()");

        fn into_map<I: IdCompat, E>(collection: impl IntoIterator<Item = E>) -> FxHashMap<I, E> {
            collection
                .into_iter()
                .enumerate()
                .map(|(idx, element)| (I::new_with_value(idx), element))
                .collect()
        }

        IR {
            entrypoint,
            constants: into_map(self.constants),
            external_functions: into_map(self.external_functions),
            functions: self.functions,
        }
    }

    pub fn constant(&mut self, payload: Vec<u8>) -> ConstantId {
        self.constants.push(Constant { payload });

        let id = self.constants.len() - 1;
        ConstantId::new_with_value(id)
    }

    pub fn constant_str<S: Into<String>>(&mut self, message: S) -> ConstantId {
        self.constant(message.into().into_bytes())
    }

    pub fn constant_str_utf16<S: Into<String>>(&mut self, message: S) -> ConstantId {
        let message = message.into();
        let mut payload = Vec::with_capacity(message.len() * 2);

        let utf16_payload = message
            .encode_utf16()
            .into_iter()
            .flat_map(|x| std::array::IntoIter::new(x.to_ne_bytes()));

        payload.extend(utf16_payload);

        self.constant(payload)
    }

    pub fn external_function<N: ToString, const PARAMETERS: usize>(
        &mut self,
        name: N,
        parameters: [FFIValueType; PARAMETERS],
        return_type: FFIReturnType,
    ) -> ExtFnIdTyped<PARAMETERS> {
        let id = self.external_function_dynargs(name, parameters.to_vec(), return_type);
        ExtFnIdTyped(id)
    }

    pub fn external_function_dynargs<N: ToString>(
        &mut self,
        name: N,
        parameters: Vec<FFIValueType>,
        return_type: FFIReturnType,
    ) -> ExternalFunctionId {
        self.external_functions.push(ExternalFunction {
            name: name.to_string(),
            parameters: parameters.to_vec(),
            return_type,
        });

        let id = self.external_functions.len() - 1;
        ExternalFunctionId::new_with_value(id)
    }

    pub fn start_function_main(&mut self) -> FunctionBuilder<0> {
        assert!(
            matches!(self.entrypoint, None),
            "can only define one entrypoint function"
        );

        let (builder, []) = self.start_function();
        self.entrypoint = Some(builder.id);
        builder
    }

    pub fn start_function<const PARAMETERS: usize>(
        &self,
    ) -> (FunctionBuilder<PARAMETERS>, [RegisterId; PARAMETERS]) {
        let id = self.gen_function_id.next();

        // TODO: is there a better way to do this?
        // PARAMETERS = 3, parameters = [RegisterId::new_with_value_const(0), ...(1),
        // ...(2)]
        let mut parameters = [RegisterId::new_const(); PARAMETERS];
        for (idx, parameter) in parameters.iter_mut().enumerate() {
            *parameter = RegisterId::new_with_value_const(idx);
        }

        (FunctionBuilder::new(id), parameters)
    }

    pub fn end_function<const PARAMETERS: usize>(
        &mut self,
        mut builder: FunctionBuilder<PARAMETERS>,
    ) -> FnSignature<PARAMETERS> {
        let signature = builder.signature();

        builder.is_ok_to_drop = true;
        let function = builder.finish();

        self.functions.insert(signature.id, function).expect_free();

        signature
    }
}

impl Default for ProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub struct FunctionBuilder<const PARAMETERS: usize> {
    pub id: FunctionId,
    gen_block_id: Counter<BlockId>,
    gen_register_id: Arc<Counter<RegisterId>>,
    entrypoint: Option<BlockId>,
    blocks: FxHashMap<BlockId, FunctionBlock>,
    /// To prevent mistakes, if the user does not call `end_function` after
    /// having declared a builder with `start_function`, then this field will
    /// remain `false`. If, when being dropped, this field is `false`, a panic
    /// will be issued to tell the user to make the appropriate call to
    /// `end_function`. Ideally, this would be done at compile time with
    /// ownership principles, but any design (that I can come up with) that
    /// uses mutability would prevent the builder from being used in a
    /// multithreaded environment.
    pub(crate) is_ok_to_drop: bool,
}

impl<const P: usize> FunctionBuilder<P> {
    pub(crate) fn new(id: FunctionId) -> Self {
        Self {
            id,
            gen_block_id: Counter::new(),
            gen_register_id: Arc::new(Counter::new_with_value(P)),
            entrypoint: None,
            blocks: FxHashMap::default(),
            is_ok_to_drop: false,
        }
    }

    fn finish(self) -> Function {
        Function {
            parameters: (0..P)
                .into_iter()
                .map(|p| Parameter {
                    register: RegisterId::new_with_value_const(p),
                })
                .collect(),
            entry_block: self.entrypoint.expect("expected entry block"),
            // TODO: find some safer way to move out data
            blocks: self.blocks.clone(),
        }
    }

    pub const fn parameter_const<const PARAMETER: usize>(&self) -> RegisterId {
        // TODO: make this a constant assertion
        // this is currently not possible because use of generic parameters from
        // outer functions (in this case, the `T` in `impl<T>`) is illegal
        // (so we can't use `P` in a constant context)
        assert!(
            PARAMETER < P,
            "constant parameter must be less than total amount of parameters"
        );

        RegisterId::new_with_value_const(PARAMETER)
    }

    pub fn parameter(&self, parameter: usize) -> RegisterId {
        assert!(
            parameter < P,
            "parameter argument must be less than total amount of parameters"
        );

        RegisterId::new_with_value(parameter)
    }

    pub fn signature(&self) -> FnSignature<P> {
        FnSignature { id: self.id }
    }

    pub fn start_block_main(&mut self) -> BlockBuilder<0> {
        assert!(
            matches!(self.entrypoint, None),
            "can only define one entrypointt block"
        );

        let (builder, []) = self.start_block();
        self.entrypoint = Some(builder.id);

        builder
    }

    pub fn start_block<const PARAMETERS: usize>(
        &mut self,
    ) -> (BlockBuilder<PARAMETERS>, [RegisterId; PARAMETERS]) {
        let id = self.gen_block_id.next();

        // TODO: is there a better way to do this?
        // PARAMETERS = 3, parameters = [RegisterId::new_with_value_const(0), ...(1),
        // ...(2)]
        let mut parameters = [RegisterId::new_const(); PARAMETERS];
        for parameter in parameters.iter_mut() {
            *parameter = self.gen_register_id.next();
        }

        let builder = BlockBuilder::new(id, self.gen_register_id.clone(), parameters);

        (builder, parameters)
    }

    pub fn start_block_dynargs(&mut self, arg_len: usize) -> (DynBlockBuilder, Vec<RegisterId>) {
        let id = self.gen_block_id.next();

        let mut parameters = Vec::with_capacity(arg_len);
        for _ in 0..arg_len {
            parameters.push(self.gen_register_id.next());
        }

        let builder = DynBlockBuilder::new(id, self.gen_register_id.clone(), parameters.clone());

        (builder, parameters)
    }

    pub fn end_block<const PARAMETERS: usize>(
        &mut self,
        builder: FinalizedBlockBuilder<PARAMETERS>,
    ) -> BlkSignature<PARAMETERS> {
        BlkSignature(self.end_block_dyn(builder.0))
    }

    pub fn end_block_dyn(&mut self, mut builder: DynFinalizedBlockBuilder) -> DynBlkSignature {
        let signature = builder.builder.signature();

        builder.is_ok_to_drop = true;
        let block = builder.finish();

        self.blocks.insert(signature.id, block).expect_free();

        signature
    }
}

impl<const P: usize> Drop for FunctionBuilder<P> {
    fn drop(&mut self) {
        if !self.is_ok_to_drop {
            panic!("A `FunctionBuilder` (created with `start_function`) was dropped without `end_function` being called.");
        }
    }
}

#[derive(Clone, Copy)]
pub struct FnSignature<const PARAMETERS: usize> {
    pub id: FunctionId,
}

#[derive(Clone, Copy)]
pub struct DynBlkSignature {
    pub id: BlockId,
}

#[derive(Clone, Copy)]
pub struct BlkSignature<const PARAMETERS: usize>(DynBlkSignature);

impl<const P: usize> Deref for BlkSignature<P> {
    type Target = DynBlkSignature;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone)]
pub struct BlockBuilder<const PARAMETERS: usize>(DynBlockBuilder);

impl<const P: usize> Deref for BlockBuilder<P> {
    type Target = DynBlockBuilder;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const P: usize> DerefMut for BlockBuilder<P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<const P: usize> BlockBuilder<P> {
    fn new(
        id: BlockId,
        gen_register_id: Arc<Counter<RegisterId>>,
        parameters: [RegisterId; P],
    ) -> Self {
        Self(DynBlockBuilder::new(
            id,
            gen_register_id,
            parameters.to_vec(),
        ))
    }

    pub fn into_dynamic(self) -> DynBlockBuilder {
        self.0
    }

    pub fn jmp<const PARAMETERS: usize>(
        self,
        block: BlkSignature<PARAMETERS>,
        values: [RegisterId; PARAMETERS],
    ) -> FinalizedBlockBuilder<P> {
        self.jmp_dynargs(block.id, values.to_vec())
    }

    pub fn jmp_dynargs(self, block: BlockId, values: Vec<RegisterId>) -> FinalizedBlockBuilder<P> {
        FinalizedBlockBuilder(self.0.jmp_dynargs(block, values))
    }

    pub fn ret(self, value: Option<RegisterId>) -> FinalizedBlockBuilder<P> {
        FinalizedBlockBuilder(self.0.ret(value))
    }

    pub fn jmpif<const PARAMS_TRUE: usize, const PARAMS_FALSE: usize>(
        self,
        condition: RegisterId,
        block_true: BlkSignature<PARAMS_TRUE>,
        values_true: [RegisterId; PARAMS_TRUE],
        block_false: BlkSignature<PARAMS_FALSE>,
        values_false: [RegisterId; PARAMS_FALSE],
    ) -> FinalizedBlockBuilder<P> {
        self.jmpif_dynargs(
            condition,
            block_true.id,
            values_true.to_vec(),
            block_false.id,
            values_false.to_vec(),
        )
    }

    pub fn jmpif_dynargs(
        self,
        condition: RegisterId,
        block_true: BlockId,
        values_true: Vec<RegisterId>,
        block_false: BlockId,
        values_false: Vec<RegisterId>,
    ) -> FinalizedBlockBuilder<P> {
        FinalizedBlockBuilder(self.0.jmpif_dynargs(
            condition,
            block_true,
            values_true,
            block_false,
            values_false,
        ))
    }

    pub fn signature(&self) -> BlkSignature<P> {
        BlkSignature(self.0.signature())
    }
}

#[derive(Clone)]
pub struct DynBlockBuilder {
    pub id: BlockId,
    gen_register_id: Arc<Counter<RegisterId>>,
    parameters: Vec<RegisterId>,
    instructions: Vec<Instruction>,
    pub(crate) is_ok_to_drop: bool,
}

impl DynBlockBuilder {
    fn new(
        id: BlockId,
        gen_register_id: Arc<Counter<RegisterId>>,
        parameters: Vec<RegisterId>,
    ) -> Self {
        Self {
            id,
            gen_register_id,
            parameters: parameters.to_vec(),
            instructions: vec![],
            is_ok_to_drop: false,
        }
    }

    fn with_result(&mut self, inst: impl FnOnce(&mut Self, RegisterId)) -> RegisterId {
        let result = self.gen_register_id.next();
        inst(self, result);
        result
    }

    fn push_inst(&mut self, inst: impl FnOnce(RegisterId) -> Instruction) -> RegisterId {
        self.with_result(|me, result| me.instructions.push(inst(result)))
    }

    /// Used in the emitter API. Basically a hacky workaround, shouldn't need to
    /// exist but /shrug
    pub(crate) fn add_parameter(&mut self) -> RegisterId {
        let id = self.gen_register_id.next();
        self.parameters.push(id);
        id
    }

    /// this will be the unreachable instruction when added, but for the time
    /// being, it causes a panic if the symbolic execution engine reaches it
    #[track_caller]
    pub fn unreachable(&mut self) -> RegisterId {
        self.push_inst(|result| Instruction::Unreachable(Unreachable { result }))
    }

    #[track_caller]
    pub fn comment(&mut self, message: &'static str) {
        self.instructions.push(Instruction::Comment(Comment {
            message,
            location: std::panic::Location::caller(),
        }));
    }

    pub fn assert(&mut self, condition: RegisterId, message: &'static str) {
        self.instructions
            .push(Instruction::Assert(Assert { condition, message }))
    }

    pub fn is_type_of(&mut self, value: RegisterId, kind: ValueType) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::IsType(IsType {
            result,
            value,
            kind: CompareType::Kind(kind),
        }));
        result
    }

    pub fn is_type_as(&mut self, value: RegisterId, other: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::IsType(IsType {
            result,
            value,
            kind: CompareType::Register(other),
        }));
        result
    }

    pub fn get_runtime(&mut self) -> RegisterId {
        let result = self.gen_register_id.next();

        self.instructions
            .push(Instruction::GetRuntime(GetRuntime { result }));

        result
    }

    pub fn make_string(&mut self, constant: ConstantId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::MakeBytes(Make {
            result,
            item: constant,
        }));
        result
    }

    pub fn make_number_decimal(&mut self, value: i64) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::MakeInteger(Make {
            result,
            item: value,
        }));
        result
    }

    pub fn make_bool(&mut self, value: bool) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::MakeBoolean(Make {
            result,
            item: value,
        }));
        result
    }

    pub fn make_atom(&mut self, atom: Atom) -> RegisterId {
        self.push_inst(|result| Instruction::MakeAtom(Make { result, item: atom }))
    }

    pub fn make_fnptr(&mut self, function: FunctionId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::GetFnPtr(Make {
            result,
            item: function,
        }));
        result
    }

    pub fn generalize(&mut self, value: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::Generalize(Generalize { result, value }));
        result
    }

    pub fn record_new(&mut self) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::NewRecord(NewRecord { result }));
        result
    }

    pub fn record_get_prop(&mut self, record: RegisterId, property: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::RecordGet(RecordGet {
            result,
            shape: (),
            record,
            key: RecordKey::Prop(property),
        }));
        result
    }

    pub fn record_get_atom(&mut self, record: RegisterId, atom: Atom) -> RegisterId {
        self.push_inst(|result| {
            Instruction::RecordGet(RecordGet {
                result,
                shape: (),
                record,
                key: RecordKey::Atom(atom),
            })
        })
    }

    pub fn record_set_prop(&mut self, record: RegisterId, property: RegisterId, value: RegisterId) {
        self.instructions.push(Instruction::RecordSet(RecordSet {
            shape: (),
            record,
            key: RecordKey::Prop(property),
            value: Some(value),
        }));
    }

    pub fn record_set_atom(&mut self, record: RegisterId, atom: Atom, value: RegisterId) {
        self.instructions.push(Instruction::RecordSet(RecordSet {
            shape: (),
            record,
            key: RecordKey::Atom(atom),
            value: Some(value),
        }));
    }

    pub fn record_del_prop(&mut self, record: RegisterId, property: RegisterId) {
        self.instructions.push(Instruction::RecordSet(RecordSet {
            shape: (),
            record,
            key: RecordKey::Prop(property),
            value: None,
        }))
    }

    pub fn record_del_atom(&mut self, record: RegisterId, atom: Atom) {
        self.instructions.push(Instruction::RecordSet(RecordSet {
            shape: (),
            record,
            key: RecordKey::Atom(atom),
            value: None,
        }));
    }

    pub fn record_has_prop(&mut self, record: RegisterId, property: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::RecordHasKey(RecordHasKey {
                result,
                shape: (),
                record,
                key: RecordKey::Prop(property),
            }));
        result
    }

    pub fn record_has_atom(&mut self, record: RegisterId, atom: Atom) -> RegisterId {
        self.push_inst(|result| {
            Instruction::RecordHasKey(RecordHasKey {
                result,
                shape: (),
                record,
                key: RecordKey::Atom(atom),
            })
        })
    }

    pub fn list_new(&mut self) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::NewList(NewList { result }));
        result
    }

    pub fn list_get(&mut self, list: RegisterId, key: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::ListGet(ListGet {
            result,
            list,
            key: ListKey::Index(key),
        }));
        result
    }

    pub fn list_set(&mut self, list: RegisterId, key: RegisterId, value: RegisterId) {
        self.instructions.push(Instruction::ListSet(ListSet {
            list,
            key: ListKey::Index(key),
            value: Some(value),
        }));
    }

    pub fn list_del(&mut self, list: RegisterId, key: RegisterId) {
        self.instructions.push(Instruction::ListSet(ListSet {
            list,
            key: ListKey::Index(key),
            value: None,
        }))
    }

    pub fn list_has(&mut self, list: RegisterId, key: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::ListHasKey(ListHasKey {
            result,
            list,
            key: ListKey::Index(key),
        }));
        result
    }

    pub fn list_len(&mut self, list: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::ListLen(ListLen { result, list }));
        result
    }

    fn binop(
        result: RegisterId,
        lhs: RegisterId,
        rhs: RegisterId,
        op: BinaryOperator,
    ) -> Instruction {
        Instruction::BinOp(BinOp {
            result,
            op,
            lhs,
            rhs,
        })
    }

    pub fn add(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Self::binop(result, lhs, rhs, BinaryOperator::Add));
        result
    }

    pub fn compare_equal(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Self::binop(result, lhs, rhs, BinaryOperator::Equals));
        result
    }

    pub fn compare_less_than(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Self::binop(result, lhs, rhs, BinaryOperator::LessThan));
        result
    }

    pub fn or(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Self::binop(result, lhs, rhs, BinaryOperator::Or));
        result
    }

    pub fn and(&mut self, lhs: RegisterId, rhs: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Self::binop(result, lhs, rhs, BinaryOperator::And));
        result
    }

    pub fn negate(&mut self, operand: RegisterId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::Negate(Negate { result, operand }));
        result
    }

    pub fn call<const PARAMETERS: usize>(
        &mut self,
        function_signature: FnSignature<PARAMETERS>,
        values: [RegisterId; PARAMETERS],
    ) {
        self.call_dynargs(function_signature.id, values.to_vec())
    }

    pub fn call_dynargs(&mut self, fn_id: FunctionId, args: Vec<RegisterId>) {
        self.instructions.push(Instruction::CallStatic(Call {
            result: None,
            calling: fn_id,
            args,
        }));
    }

    pub fn call_virt<const PARAMETERS: usize>(
        &mut self,
        fn_ptr: RegisterId,
        values: [RegisterId; PARAMETERS],
    ) {
        self.call_virt_dynargs(fn_ptr, values.to_vec())
    }

    pub fn call_virt_dynargs(&mut self, fn_ptr: RegisterId, args: Vec<RegisterId>) {
        self.instructions.push(Instruction::CallVirt(Call {
            result: None,
            calling: fn_ptr,
            args,
        }));
    }

    pub fn call_virt_with_result<const PARAMETERS: usize>(
        &mut self,
        fn_ptr: RegisterId,
        values: [RegisterId; PARAMETERS],
    ) -> RegisterId {
        self.call_virt_dynargs_with_result(fn_ptr, values.to_vec())
    }

    pub fn call_virt_dynargs_with_result(
        &mut self,
        fn_ptr: RegisterId,
        args: Vec<RegisterId>,
    ) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::CallVirt(Call {
            result: Some(result),
            calling: fn_ptr,
            args,
        }));
        result
    }

    pub fn call_with_result<const PARAMETERS: usize>(
        &mut self,
        function_signature: FnSignature<PARAMETERS>,
        values: [RegisterId; PARAMETERS],
    ) -> RegisterId {
        self.call_dynargs_with_result(function_signature.id, values.to_vec())
    }

    pub fn call_dynargs_with_result(
        &mut self,
        fn_id: FunctionId,
        args: Vec<RegisterId>,
    ) -> RegisterId {
        let result = self.gen_register_id.next();

        self.instructions.push(Instruction::CallStatic(Call {
            result: Some(result),
            calling: fn_id,
            args,
        }));

        result
    }

    pub fn call_external_function<const PARAMETERS: usize>(
        &mut self,
        external_function: ExtFnIdTyped<PARAMETERS>,
        values: [RegisterId; PARAMETERS],
    ) {
        self.call_external_function_dynargs(external_function.0, values.to_vec())
    }

    pub fn call_external_function_dynargs(
        &mut self,
        fn_id: ExternalFunctionId,
        args: Vec<RegisterId>,
    ) {
        self.instructions.push(Instruction::CallExtern(Call {
            result: None,
            calling: fn_id,
            args,
        }));
    }

    pub fn call_external_function_with_result<const PARAMETERS: usize>(
        &mut self,
        external_function: ExtFnIdTyped<PARAMETERS>,
        values: [RegisterId; PARAMETERS],
    ) -> RegisterId {
        self.call_external_function_dynargs_with_result(external_function.0, values.to_vec())
    }

    pub fn call_external_function_dynargs_with_result(
        &mut self,
        fn_id: ExternalFunctionId,
        args: Vec<RegisterId>,
    ) -> RegisterId {
        let result = self.gen_register_id.next();

        self.instructions.push(Instruction::CallExtern(Call {
            result: Some(result),
            calling: fn_id,
            args,
        }));

        result
    }

    pub fn jmp_dynargs(
        mut self,
        block: BlockId,
        values: Vec<RegisterId>,
    ) -> DynFinalizedBlockBuilder {
        self.is_ok_to_drop = true;
        DynFinalizedBlockBuilder {
            builder: self,
            is_ok_to_drop: false,
            end_control_flow: ControlFlowInstruction::Jmp(Jump(BlockJump(block, values))),
        }
    }

    pub fn ret(mut self, value: Option<RegisterId>) -> DynFinalizedBlockBuilder {
        self.is_ok_to_drop = true;
        DynFinalizedBlockBuilder {
            builder: self,
            is_ok_to_drop: false,
            end_control_flow: ControlFlowInstruction::Ret(Return(value)),
        }
    }

    pub fn jmpif_dynargs(
        mut self,
        condition: RegisterId,
        block_true: BlockId,
        values_true: Vec<RegisterId>,
        block_false: BlockId,
        values_false: Vec<RegisterId>,
    ) -> DynFinalizedBlockBuilder {
        self.is_ok_to_drop = true;
        DynFinalizedBlockBuilder {
            builder: self,
            is_ok_to_drop: false,
            end_control_flow: ControlFlowInstruction::JmpIf(JumpIf {
                condition,
                if_so: BlockJump(block_true, values_true),
                other: BlockJump(block_false, values_false),
            }),
        }
    }

    pub fn signature(&self) -> DynBlkSignature {
        DynBlkSignature { id: self.id }
    }
}

pub struct FinalizedBlockBuilder<const PARAMETERS: usize>(DynFinalizedBlockBuilder);

impl<const P: usize> Deref for FinalizedBlockBuilder<P> {
    type Target = DynFinalizedBlockBuilder;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const P: usize> DerefMut for FinalizedBlockBuilder<P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct DynFinalizedBlockBuilder {
    builder: DynBlockBuilder,
    end_control_flow: ControlFlowInstruction,
    is_ok_to_drop: bool,
}

impl DynFinalizedBlockBuilder {
    fn finish(mut self) -> FunctionBlock {
        self.is_ok_to_drop = true;

        // TODO: find a faster way to do this (but needs to still be safe)
        let parameters = &self.builder.parameters;
        let instructions = self.builder.instructions.clone();
        let end = self.end_control_flow.clone();
        FunctionBlock {
            parameters: parameters.to_vec(),
            instructions,
            end,
        }
    }
}

impl Drop for DynFinalizedBlockBuilder {
    fn drop(&mut self) {
        if !self.is_ok_to_drop {
            panic!("A `DynFinalizedBlockBuilder` (created with a finalizing method) was dropped without `end_block` being called.");
        }
    }
}
