use std::hash::{BuildHasherDefault, Hash};
use std::panic::UnwindSafe;

use bimap::{BiHashMap, BiMap};
use petgraph::data::Build;
use petgraph::graph::NodeIndex;
use petgraph::visit::{EdgeRef, NodeIndexable};
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

use crate::frontend::ir::{ControlFlowInstruction, Function, FunctionBlock, Instruction, IR};
use crate::id::*;
use crate::name::DebugName;

use super::ir::BasicBlockJump;

pub type ControlFlowGraph = petgraph::graph::DiGraph<BlockId, ()>;
pub type ValueFlowGraph = petgraph::graph::DiGraph<RegisterId, ()>;

pub type BiFxHashMap<K, V> =
    BiHashMap<K, V, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>;

fn new_bifxhashmap<K: Eq + Hash, V: Eq + Hash>() -> BiFxHashMap<K, V> {
    BiFxHashMap::with_hashers(Default::default(), Default::default())
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub derived_from: (FunctionId, BlockId),
    pub parameters: Vec<RegisterId>,
    pub instructions: Vec<Instruction>,
    pub end: ControlFlowInstruction,
}

pub fn translate(ir: &IR) -> Vec<BasicBlock> {
    let mut blocks = Vec::new();

    for (id, function) in ir.functions.iter() {
        blocks.extend(translate_function(*id, function));
    }

    blocks
}

/// A function's representation, as it's being rewritten. The parameters of a
/// function are merged into the first block
struct RewritingFn {
    entry: BlockId,
    blocks: FxHashMap<BlockId, FunctionBlock>,
}

struct Algo<'duration> {
    flow: &'duration Flow,
    function: &'duration mut RewritingFn,
    reg_counter: &'duration mut Counter<RegisterId>,
    blocks: FxHashMap<NodeIndex, BlockState>,
}

struct BlockState {
    /// Map of [register present in function definition -> rewritten register]
    replacements: BiFxHashMap<RegisterId, RegisterId>,
}

impl<'d> Algo<'d> {
    pub fn new(
        flow: &'d Flow,
        function: &'d mut RewritingFn,
        reg_counter: &'d mut Counter<RegisterId>,
    ) -> Self {
        Self {
            flow,
            function,
            reg_counter,
            blocks: Default::default(),
        }
    }

    /// magic function to do the big hard graph theory problem!!!!!!!
    pub fn solve(&mut self) {
        // make sure all dependencies are modelled by basic block arguments
        for node in self.flow.map.right_values().cloned() {
            self.lift_first_level_indirect_dependencies(node);
        }

        // first, figure out a path to travel along for patching
        // the order shouldn't matter at all, ideally for optimal performance
        // we'd go down one of the paths that the path of execution would take
        // but who cares
        let stack = self.flow.cfg.node_indices().collect::<Vec<_>>();

        // now, it may be helpful to visualize this process as a tree:
        // A -> B -> C -> D
        // ^ step 1: patch A
        //      ^ step 2: patch B
        // ^ step 2.5: if B was patched, re-patch A
        //           ^ step 3: patch C
        //      ^ step 3.33: if C was patched, re-patch C
        // ^ step 3.67: if B was patched, re-patch A
        //                ^ step 4: patch D
        //           ^ step 4.25: if D was patched, re-patch C
        //      ^ step 4.5: if C was patched, re-patch B
        // ^ step 4.75: if B was patched, re-patch A
        // done!
        for forward in 0..stack.len() {
            let node = stack[forward];

            let did_patch = self.patch_arguments_to_children(node);
            if did_patch {
                for backward in forward..0 {
                    let node = stack[backward];
                    let did_patch = self.patch_arguments_to_children(node);
                    if !did_patch {
                        break;
                    }
                }
            }
        }

        // now we should have a completely rewritten function properly!
    }

    /// At this stage, all function blocks simply use registers from various
    /// blocks in the function. We wish to totally eliminate any implicit
    /// registers, by making them all explicit registers of the function block.
    fn lift_first_level_indirect_dependencies(&mut self, node: NodeIndex) {
        let block_id = self.flow.map.get_by_right(&node).unwrap();
        let block = self.function.blocks.get(block_id).unwrap();

        let declared = block.declared_registers();
        let used = block.used_registers();

        // any registers that are used but not declared need to be passed to this block
        let need_to_find = used
            .difference(&declared)
            .copied()
            .collect::<FxHashSet<_>>();

        // generate new registers, specifically only used for this block, that
        // replace the `need_to_find` registers
        let mut replacements = new_bifxhashmap();
        let block = self.function.blocks.get_mut(block_id).unwrap();
        let params = &mut block.parameters;

        for register in need_to_find.iter().copied() {
            let replacement_register = self.reg_counter.next();

            replacements.insert(register, replacement_register);
            params.push(replacement_register);
        }

        for register in block
            .instructions
            .iter_mut()
            .flat_map(|i| i.used_registers_mut())
            .chain(block.end.used_registers_mut())
        {
            if let Some(replacement) = replacements.get_by_left(&*register) {
                *register = *replacement;
            }
        }

        self.blocks.insert(node, BlockState { replacements });
    }

    /// This function assumes that all the dependencies of a block are modeled
    /// by its arguments. If this holds true, then we know that we must provide
    /// all the dependencies to the block. If we do not have these dependencies,
    /// we can add additional arguments to the current block to give to the
    /// child block.
    ///
    /// First, we determine who our children are, and what dependencies they
    /// need but do not have.
    ///
    /// Then, we add to the current block the parameters necessary that would
    /// allow us to pass down those dependencies to the children block, and
    /// then state that the current block requires those dependencies. Ideally,
    /// this could be performed as many times as necessary until the entire tree
    /// no longer has anything to patch.
    ///
    /// Returns `true` if patching occurred, otherwise returns `false`.
    fn patch_arguments_to_children(&mut self, node: NodeIndex) -> bool {
        let block_id = self.flow.map.get_by_right(&node).unwrap();

        let children = self
            .flow
            .cfg
            .edges(node)
            .map(|e| (e.target(), self.blocks.get(&e.target()).unwrap()))
            .collect::<Vec<_>>();

        let block = self.function.blocks.get(block_id).unwrap();
        // var: map of { child block register <-> register in original function block }
        let mut deps_needed = Vec::new();

        for BasicBlockJump(id, params) in block.end.children() {
            let child_block = self.function.blocks.get(id).unwrap();
            let state = self
                .blocks
                .get(self.flow.map.get_by_left(id).unwrap())
                .unwrap();

            // if we are alreaady calling the block with all of the arguments
            // it wants, there is nothing to do
            let fulfilled = child_block.parameters.len() == params.len();
            if fulfilled {
                continue;
            }

            // we want to: `jmp Child(a)`
            // but our child: `Child(a, b, c)`
            // thus, we need to map `b, c` to registers
            // during this algo, parameters are added IFF there's a replacement
            // so, we know x -> b, y -> c where x, y are registers in another block
            debug_assert!(child_block.parameters.len() > params.len());
            for i in params.len()..child_block.parameters.len() {
                // i: 1
                // in `Child(a, b, c)`, params = [a, b, c], params[i] -> b
                let param_register = child_block.parameters[i];
                // replacements | { x -> b }, thus find x via <- b
                let find_register = *state.replacements.get_by_right(&param_register).unwrap();
                // note: find `b` by using `x`
                deps_needed.push((param_register, find_register));
            }
        }

        // now, we know all dependencies children need
        // var: map of { child block register <-> register in this block }
        let mut provision_plan = FxHashMap::default();

        // first, figure out dependencies that can be provided
        let block = self.function.blocks.get_mut(block_id).unwrap();
        let state = self.blocks.get_mut(&node).unwrap();
        for (child_block_register, register_in_original_fn) in deps_needed.into_iter() {
            // let x = reg in orig fn, a = replacement reg (in this block's params)
            // replacements | { x -> a },
            if let Some(replacement) = state.replacements.get_by_left(&register_in_original_fn) {
                // we can provision this register
                provision_plan
                    .insert(child_block_register, *replacement)
                    .expect_none("we shouldn't have duplicates, this is just in case");
            } else {
                // we do not know of this register
                // declare that we require it, add it as a parameter of this block
                let this_block_register = self.reg_counter.next();
                state // TODO: function to encapsulate this replacing thing
                    .replacements
                    .insert(register_in_original_fn, this_block_register);
                block.parameters.push(this_block_register);

                // now that we know of this register, we can provision it
                provision_plan
                    .insert(child_block_register, this_block_register)
                    .expect_none("we shouldn't have duplicates, this is just in case");
            }
        }

        let block = self.function.blocks.get(block_id).unwrap();

        // provision parameters accordingly
        let mut final_provision_plan = FxHashMap::default();

        for BasicBlockJump(child_id, params) in block.end.children() {
            let child_block = self.function.blocks.get(child_id).unwrap();

            if child_block.parameters.len() == params.len() {
                continue;
            }

            debug_assert!(child_block.parameters.len() > params.len());

            // we can't actually modify the params here because of rust lifetime rules
            let mut extend_params_with = Vec::new();
            for i in params.len()..child_block.parameters.len() {
                let register_to_pass = *provision_plan.get(&child_block.parameters[i]).unwrap();
                extend_params_with.push(register_to_pass);
            }

            final_provision_plan.insert(*child_id, extend_params_with);
        }

        let will_rewrite_any_children = final_provision_plan.len() > 0;

        let block = self.function.blocks.get_mut(block_id).unwrap();

        for BasicBlockJump(child_id, params) in block.end.children_mut() {
            let extend_params_with = final_provision_plan.remove(child_id).unwrap();
            params.extend(extend_params_with);
        }

        will_rewrite_any_children
    }
}

pub fn translate_function(fn_id: FunctionId, func: &Function) -> Vec<BasicBlock> {
    let flow = compute_flow(func);

    let mut reg_counter = compute_highest_register(func)
        .map(Counter::after)
        .unwrap_or_else(Counter::new);

    // we will need to mutate something as we gradually get every basic block
    // into slowly a purer and purer state
    let mut granular_rewrite = to_rewriting_fn(func);

    // so currently, our blocks have the following issues:
    // 1. blocks in functions may depend upon registers declared in other blocks
    // 2. registers in blocks isn't allocated optimally (NOT required)

    let mut algo = Algo::new(&flow, &mut granular_rewrite, &mut reg_counter);
    algo.solve();

    granular_rewrite
        .blocks
        .into_iter()
        .map(|(id, block)| BasicBlock {
            derived_from: (fn_id, id),
            parameters: block.parameters,
            instructions: block.instructions,
            end: block.end,
        })
        .collect()
}

struct Flow {
    map: BiHashMap<BlockId, NodeIndex, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>,
    cfg: ControlFlowGraph,
}

/// Generates a forwards and backwards graph, directing control flow from the
/// current block to the ones before it.
fn compute_flow(f: &Function) -> Flow {
    let mut flow = ControlFlowGraph::default();

    let mut block_to_node = new_bifxhashmap();
    for (id, _) in f.blocks.iter() {
        block_to_node.insert(*id, flow.add_node(*id));
    }
    let block_to_node = block_to_node;

    let map = |id| *block_to_node.get_by_left(id).unwrap();

    for (id, block) in f.blocks.iter() {
        let from = map(id);

        match &block.end {
            ControlFlowInstruction::Ret(_) => {}
            ControlFlowInstruction::Jmp(path) => {
                flow.add_edge(from, map(&path.0), ());
            }
            ControlFlowInstruction::JmpIf {
                condition: _,
                true_path,
                false_path,
            } => {
                flow.add_edge(from, map(&true_path.0), ());
                flow.add_edge(from, map(&false_path.0), ());
            }
        }
    }

    Flow {
        map: block_to_node,
        cfg: flow,
    }
}

fn compute_highest_register(f: &Function) -> Option<RegisterId> {
    // we aren't assigning `highest` to an `Option<RegisterId>` because moving
    // it out of an option pattern repeaatedly is kinda meh
    let mut has_encountered_register = false;
    let mut highest = RegisterId::new();

    let mut compare_highest = |current: RegisterId| {
        // if we have compared another register with the initial register being
        // the lowest possible value, we can only be comparing it to registers
        // of also the lowest value (meaning that the highest value is 0) or
        // greater, meaning that after we compare at least once we'll be
        // guaranteed to have a register to return
        has_encountered_register = true;

        if current.value() > highest.value() {
            highest = RegisterId::new_with_value(current.value())
        }
    };

    // the only time we will ever have a new register is when we stumble on
    // more declarations. declarations only exist in two cases:

    for (_, block) in f.blocks.iter() {
        // 1. a block declares a register as a parameter
        for param in block.parameters.iter() {
            compare_highest(*param);
        }

        // 2. a register is declared during assignment
        for inst in block.instructions.iter() {
            if let Some(register) = inst.assigned_to() {
                compare_highest(register);
            }
        }
    }

    has_encountered_register.then_some(highest)
}

fn to_rewriting_fn(f: &Function) -> RewritingFn {
    let mut result_fn = RewritingFn {
        entry: f.entry_block,
        blocks: FxHashMap::default(),
    };

    for (id, block) in f.blocks.iter() {
        // we already handled the entry block specially
        if *id == f.entry_block {
            // for the entry block, merge parameters as parameters on the block
            let block = FunctionBlock {
                parameters: f.parameters.iter().map(|p| p.register).collect(),
                instructions: block.instructions.clone(),
                end: block.end.clone(),
            };

            result_fn.blocks.insert(*id, block);
        } else {
            result_fn.blocks.insert(*id, block.clone());
        }
    }

    result_fn
}

impl FunctionBlock {
    fn declared_registers(&self) -> FxHashSet<RegisterId> {
        let mut declared = self
            .parameters
            .clone()
            .into_iter()
            .collect::<FxHashSet<_>>();

        for inst in self.instructions.iter() {
            if let Some(reg) = inst.assigned_to() {
                declared.insert(reg);
            }
        }

        declared
    }

    fn used_registers(&self) -> FxHashSet<RegisterId> {
        let mut used = FxHashSet::default();

        for inst in self.instructions.iter() {
            used.extend(inst.used_registers());
        }

        used.extend(self.end.used_registers());
        used
    }
}
