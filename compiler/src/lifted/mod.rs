use rustc_hash::{FxHashMap, FxHashSet};
use tinyvec::TinyVec;

use crate::{
    frontend::ir::{self, ControlFlowInstruction, FFIValueType, Instruction, IR},
    id::{Counter, IrCtx, LiftedCtx},
    isa::{BlockJump, ISAInstruction, Jump, JumpIf, Return},
    retag::{
        BlkToFn, CnstPassRetagger, CnstRetagger, ExtFnPassRetagger, ExtFnRetagger, FnMapRetagger,
        FnRetagger, RegPassRetagger, RegRetagger,
    },
    UnwrapNone,
};

type ExternalFunctionId = crate::id::ExternalFunctionId<LiftedCtx>;
type FunctionId = crate::id::FunctionId<LiftedCtx>;
type ConstantId = crate::id::ConstantId<LiftedCtx>;
type RegisterId = crate::id::RegisterId<LiftedCtx>;

#[derive(Debug, Clone)]
pub struct LiftedProgram {
    pub entrypoint: FunctionId,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub functions: FxHashMap<FunctionId, Function>,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub parameters: Vec<FFIValueType>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<RegisterId>,
    pub instructions: Vec<Instruction<LiftedCtx, LiftedCtx>>,
    pub end: EndInstruction,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EndInstruction {
    Jump(Jump<FunctionId, LiftedCtx>),
    JumpIf(JumpIf<FunctionId, LiftedCtx>),
    Return(Return<LiftedCtx>),
}

impl EndInstruction {
    pub fn declared_register(&self) -> Option<RegisterId> {
        match self {
            EndInstruction::Jump(inst) => inst.declared_register(),
            EndInstruction::JumpIf(inst) => inst.declared_register(),
            EndInstruction::Return(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        match self {
            EndInstruction::Jump(inst) => inst.used_registers(),
            EndInstruction::JumpIf(inst) => inst.used_registers(),
            EndInstruction::Return(inst) => inst.used_registers(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            EndInstruction::Jump(inst) => inst.used_registers_mut(),
            EndInstruction::JumpIf(inst) => inst.used_registers_mut(),
            EndInstruction::Return(inst) => inst.used_registers_mut(),
        }
    }

    pub fn paths(&self) -> Vec<&BlockJump<FunctionId, LiftedCtx>> {
        match self {
            EndInstruction::Jump(inst) => inst.paths(),
            EndInstruction::JumpIf(inst) => inst.paths(),
            EndInstruction::Return(_) => Vec::new(),
        }
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId, LiftedCtx>> {
        match self {
            EndInstruction::Jump(inst) => inst.paths_mut(),
            EndInstruction::JumpIf(inst) => inst.paths_mut(),
            EndInstruction::Return(_) => Vec::new(),
        }
    }
}

pub fn lift(ir: IR) -> LiftedProgram {
    // retag constants
    let mut c_retagger = CnstPassRetagger::default();
    let mut constants = FxHashMap::default();
    for (id, value) in ir.constants.into_iter() {
        let id = c_retagger.retag_new(id);
        let value = Constant {
            payload: value.payload,
        };
        constants.insert(id, value).expect_free();
    }

    // retag external fns
    let mut e_retagger = ExtFnPassRetagger::default();
    let mut external_functions = FxHashMap::default();
    for (id, ext_fn) in ir.external_functions.into_iter() {
        let id = e_retagger.retag_new(id);
        let value = ExternalFunction {
            parameters: ext_fn.parameters,
        };
        external_functions.insert(id, value).expect_free();
    }

    // retag functions
    let mut fn_retagger = FnMapRetagger::default();
    for (id, _) in ir.functions.iter() {
        fn_retagger.retag_new(*id);
    }

    // create function id generator to generate function ids after the highest
    // retagged function id (meaning we'll only get unique function ids)
    let fn_id_gen = Counter::new_with_value(fn_retagger.counter());

    // assert that any function ids generated with `new_fn_ids`
    // will not overlap with any existing function ids
    if cfg!(debug_assertions) {
        let mut retagger = fn_retagger.clone();
        let new_fn_ids = fn_id_gen.dup();

        assert_eq!(
            retagger.retag_new(retagger.free()),
            new_fn_ids.dup().next(),
            "newly generated function ids should start right after all retagged functions"
        );
    }

    // lift functions
    let mut functions = FxHashMap::default();
    for (id, func) in ir.functions.into_iter() {
        let lifted_blocks = lift_function(
            func,
            fn_retagger.retag_old(id),
            &fn_id_gen,
            &fn_retagger,
            &e_retagger,
            &c_retagger,
        );

        functions.extend(lifted_blocks);
    }

    let entrypoint = fn_retagger.retag_old(ir.entrypoint);

    LiftedProgram {
        entrypoint,
        constants,
        external_functions,
        functions,
    }
}

fn lift_function(
    function: ir::Function,
    fn_id: FunctionId,
    fn_id_gen: &Counter<FunctionId>,
    fn_retagger: &impl FnRetagger<IrCtx, LiftedCtx>,
    e_retagger: &impl ExtFnRetagger<IrCtx, LiftedCtx>,
    c_retagger: &impl CnstRetagger<IrCtx, LiftedCtx>,
) -> FxHashMap<FunctionId, Function> {
    let mut lifted_ids = FxHashMap::default();
    for (id, _) in function.blocks.iter() {
        // if this block is the entry block, we don't want to generate a unique
        // id for it - we want to use the function id provided for it
        let key = match *id == function.entry_block {
            true => fn_id,
            false => fn_id_gen.next(),
        };

        lifted_ids.insert(*id, key);
    }

    let mut reg_retagger = RegPassRetagger::default();
    // SAFETY: we are ignoring checks because every block does not have full
    // register awareness. there may be registers used-but-not-declared, so we
    // must ignore those
    reg_retagger.ignore_checks();

    let mut lifted = FxHashMap::default();

    for (id, blk) in function.blocks.into_iter() {
        let lifted_id = *lifted_ids.get(&id).unwrap();

        let mut lifted_blk = fn_block_to_fn(
            blk,
            &lifted_ids,
            &mut reg_retagger,
            fn_retagger,
            e_retagger,
            c_retagger,
        );

        lift_used_but_not_declared(&mut lifted_blk);

        lifted.insert(lifted_id, lifted_blk);
    }

    loop {
        let mut any_patched = false;
        let view = lifted.clone();
        for (id, function) in lifted.iter_mut() {
            let is_entrypoint = *id == fn_id;
            any_patched = patch_child_flow(function, &view, is_entrypoint) || any_patched;
        }

        if !any_patched {
            break;
        }
    }

    lifted
}

fn fn_block_to_fn(
    fn_blk: ir::FunctionBlock,
    blk_to_fn: &impl BlkToFn<IrCtx, LiftedCtx>,
    retagger: &mut impl RegRetagger<IrCtx, LiftedCtx>,
    fn_retagger: &impl FnRetagger<IrCtx, LiftedCtx>,
    e_retagger: &impl ExtFnRetagger<IrCtx, LiftedCtx>,
    c_retagger: &impl CnstRetagger<IrCtx, LiftedCtx>,
) -> Function {
    let parameters = (fn_blk.parameters.into_iter())
        .map(|r| retagger.retag_new(r))
        .collect();

    let instructions = (fn_blk.instructions.into_iter())
        .map(|r| r.retag(retagger, e_retagger, fn_retagger, c_retagger))
        .collect();

    let end = match fn_blk.end {
        ControlFlowInstruction::Jmp(inst) => {
            EndInstruction::Jump(inst.retag_to_blk(retagger, blk_to_fn))
        }
        ControlFlowInstruction::JmpIf(inst) => {
            EndInstruction::JumpIf(inst.retag_to_blk(retagger, blk_to_fn))
        }
        ControlFlowInstruction::Ret(inst) => EndInstruction::Return(inst.retag(retagger)),
    };

    Function {
        parameters,
        instructions,
        end,
    }
}

/// This function will find all used-but-not-declared registers, and make them
/// parameters of the function.
///
/// # Invariant
///
/// All added registers to this function will be a register valid in the
/// original function. This means that checking if a function can pass a
/// parameter to a child or not is as simple as checking if the register is
/// declared in the caller.
fn lift_used_but_not_declared(function: &mut Function) {
    let mut declared = FxHashSet::default();
    let mut used = FxHashSet::default();

    declared.extend(function.parameters.iter().copied());

    for inst in function.instructions.iter() {
        if let Some(reg) = inst.assigned_to() {
            let register_exists = declared.insert(reg);
            debug_assert!(register_exists, "should never re-declare registers");
        }

        used.extend(inst.used_registers());
    }

    if let Some(reg) = function.end.declared_register() {
        let register_exists = declared.insert(reg);
        debug_assert!(register_exists, "should never re-declare registers");
    }

    used.extend(function.end.used_registers());

    let used_but_not_declared = used.difference(&declared);
    function.parameters.extend(used_but_not_declared);
}

/// This function will look for child blocks which have more registers than are
/// passed to it. By using the invariant specified in [`lift_used_but_not_declared`],
/// registers needed that are not declared will be lifted up into the parameters
/// of the current block to satisfy the children.
///
/// If any children were patched, `true` is returned.
///
/// If no children were patched, `false` is returned.
fn patch_child_flow(
    function: &mut Function,
    functions: &FxHashMap<FunctionId, Function>,
    is_entrypoint: bool,
) -> bool {
    let mut fn_params_modified = false;

    // TODO: put this in a function?
    let mut declared = FxHashSet::default();

    declared.extend(function.parameters.iter().copied());

    for reg in function
        .instructions
        .iter()
        .filter_map(|inst| inst.assigned_to())
    {
        let register_exists = declared.insert(reg);
        debug_assert!(register_exists, "should never re-declare registers");
    }

    if let Some(reg) = function.end.declared_register() {
        let register_exists = declared.insert(reg);
        debug_assert!(register_exists, "should never re-declare registers");
    }

    for path in function.end.paths_mut() {
        let target_fn = functions
            .get(&path.0)
            .expect("expected to jump to valid function");

        let passed_registers = &mut path.1;

        debug_assert!(
            target_fn.parameters.len() >= passed_registers.len(),
            "cannot have less params than passed registers"
        );

        let requested_but_not_passed = target_fn.parameters.iter().skip(passed_registers.len());
        for unpassed_register in requested_but_not_passed.copied() {
            // using the invariant in `lift_used_but_not_declared`, to check if
            // we can pass a register to a child block is as simple as checking
            // if we ourselves have this parameter.
            if declared.contains(&unpassed_register) {
                passed_registers.push(unpassed_register);
            } else {
                // if we're modifying the params of the entrypoint function,
                // something is wrong; if registers have crept up to the entry
                // block, we have somehow used a register but never declared it.
                if is_entrypoint {
                    panic!("error: used-but-not-declared register reached entrypoint of function but found no declaration.");
                }

                fn_params_modified = true;
                function.parameters.push(unpassed_register);
            }
        }
    }

    fn_params_modified
}
