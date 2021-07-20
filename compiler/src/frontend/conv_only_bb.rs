use crate::frontend::ir::{ControlFlowInstruction, Function, Instruction, IR};
use crate::frontend::isa::BlockJump;
use crate::frontend::retag::BlkRetagger;
use crate::frontend::retag::{RegGenPassRetagger, RegRetagger};
use crate::id::*;
use crate::UnwrapNone;
use bimap::BiHashMap;
use petgraph::graph::NodeIndex;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use std::hash::{BuildHasherDefault, Hash};

use super::retag::{
    BlkMapRetagger, BlkPassRetagger, ExtFnPassRetagger, FnPassRetagger, FnRetagger, RegGenRetagger,
    RegPassRetagger,
};
use crate::frontend::retag::ExtFnRetagger;

pub type ControlFlowGraph = petgraph::graph::DiGraph<BlockId<PureBbCtx>, ()>;
pub type ValueFlowGraph = petgraph::graph::DiGraph<RegisterId<IrCtx>, ()>;

pub type BiFxHashMap<K, V> =
    BiHashMap<K, V, BuildHasherDefault<FxHasher>, BuildHasherDefault<FxHasher>>;

fn new_bifxhashmap<K: Eq + Hash, V: Eq + Hash>() -> BiFxHashMap<K, V> {
    BiFxHashMap::with_hashers(Default::default(), Default::default())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HostBlock {
    pub original_function: FunctionId<IrCtx>,
    pub original_block: BlockId<IrCtx>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId<PureBbCtx>,
    pub parameters: Vec<RegisterId<PureBbCtx>>,
    pub instructions: Vec<Instruction<PureBbCtx>>,
    pub end: ControlFlowInstruction<PureBbCtx, PureBbCtx>,
}

#[derive(Debug, Clone)]
pub struct PureBlocks {
    map_to_host: BiFxHashMap<HostBlock, BlockId<PureBbCtx>>,
    pub(crate) blocks: FxHashMap<BlockId<PureBbCtx>, Block>,
}

impl PureBlocks {
    pub fn new(map: FxHashMap<HostBlock, Block>) -> Self {
        let mut map_to_host = new_bifxhashmap();
        let mut map_id = FxHashMap::default();

        for (key, block) in map.into_iter() {
            println!("iserting ito map-to-host: {:?} |-> {:?}", key, block.id);
            map_to_host.insert(key, block.id);
            println!("curr map-to-host: {:?}", &map_to_host);
            map_id.insert(block.id, block);
        }

        Self {
            map_to_host,
            blocks: map_id,
        }
    }

    pub fn get_block(&self, id: BlockId<PureBbCtx>) -> &Block {
        self.blocks.get(&id).unwrap()
    }

    pub fn get_block_by_host(
        &self,
        original_function: FunctionId<IrCtx>,
        original_block: BlockId<IrCtx>,
    ) -> &Block {
        let id = self.get_block_id_by_host(original_function, original_block);
        self.get_block(id)
    }

    pub fn get_block_id_by_host(
        &self,
        original_function: FunctionId<IrCtx>,
        original_block: BlockId<IrCtx>,
    ) -> BlockId<PureBbCtx> {
        let host_block = HostBlock {
            original_function,
            original_block,
        };

        *self.map_to_host.get_by_left(&host_block).unwrap()
    }
}

pub fn translate(ir: &IR) -> PureBlocks {
    let mut blocks = FxHashMap::default();

    let mut fn_retagger = FnPassRetagger::default();
    for (id, _) in ir.functions.iter() {
        fn_retagger.retag_new(*id);
    }

    let mut ext_fn_retagger = ExtFnPassRetagger::default();
    for (id, _) in ir.external_functions.iter() {
        ext_fn_retagger.retag_new(*id);
    }

    let mut blk_retagger = BlkMapRetagger::default();

    for (id, function) in ir.functions.iter() {
        // UNIQUENESS GUARANTEE:
        // every block is a { (fn id, blk id) |-> blk } pairing
        // that means that both the function id and block id must be unique in
        // order for all elements to be unique
        // the function ids are guaranteed to be unique, as we're iterating over
        // a unique mapping of { fn id |-> func } pairing
        // the block ids are guaranteed to be unique by the assertion inside
        // thus, we can safely extend `blocks` and know nothing is being overwritten
        let results = translate_function(
            *id,
            function,
            &ext_fn_retagger,
            &fn_retagger,
            &mut blk_retagger,
        );
        println!(
            "we are converting pure {:?}, {:?} :: {:?}",
            id, function, results
        );
        blocks.extend(results);
    }

    println!("done pureblocks, got: {:?}", blocks);
    PureBlocks::new(blocks)
}

/// A function's representation, as it's being rewritten. The parameters of a
/// function are merged into the first block
#[derive(Debug)]
struct RewritingFn {
    _entry: BlockId<PureBbCtx>,
    blocks: FxHashMap<BlockId<PureBbCtx>, RewritingFunctionBlock>,
}

#[derive(Debug, Clone)]
pub struct RewritingFunctionBlock {
    pub original_id: BlockId<IrCtx>,
    pub id: BlockId<PureBbCtx>,
    pub parameters: Vec<RegisterId<PureBbCtx>>,
    pub instructions: Vec<Instruction<PureBbCtx>>,
    pub end: ControlFlowInstruction<PureBbCtx, PureBbCtx>,
}

struct Algo<'duration, R> {
    flow: &'duration Flow,
    function: &'duration mut RewritingFn,
    retagger: &'duration mut R,
    // block_id_gen: &'duration Counter<BlockId<PureBbCtx>>,
    blocks: FxHashMap<NodeIndex, BlockState>,
}

struct BlockState {
    /// Map of { anonymous registers |-> real registers }.
    ///
    /// Used to figure out which real registers are required by a block, by
    /// looking up the anonymous register in this map to obtain the real
    /// register that backs it.
    ///
    /// Anonymous registers are registers added via the conversion process,
    /// real registers are the registers that actually exist in the code.
    reals: FxHashMap<RegisterId<PureBbCtx>, RegisterId<PureBbCtx>>,

    /// Map of { anonymous registers |-> anonymous registers }
    ///
    /// Used to figure out which real registers are required by a block, by
    /// looking up the anonymous register in this map to obtain the real
    /// register that backs it.
    ///
    /// Anonymous registers are registers added via the conversion process,
    /// real registers are the registers that actually exist in the code.
    anonymous: FxHashMap<RegisterId<PureBbCtx>, RegisterId<PureBbCtx>>,
}

impl<'d, R> Algo<'d, R>
where
    R: RegGenRetagger<IrCtx, PureBbCtx>,
{
    pub fn new(
        flow: &'d Flow,
        function: &'d mut RewritingFn,
        retagger: &'d mut R,
        // block_id_gen: &'d Counter<BlockId<PureBbCtx>>,
    ) -> Self {
        Self {
            flow,
            function,
            retagger,
            // block_id_gen,
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
        let blk_ids = stack
            .iter()
            .map(|r| self.flow.map.get_by_right(r).unwrap())
            .collect::<Vec<_>>();
        println!("patching order: {:?} |-> {:?}", stack, blk_ids);

        // just going to keep patching until it works.
        // TODO: think about a better algo
        loop {
            let mut patched_any = false;
            for node in stack.iter() {
                if self.patch_arguments_to_children(*node) {
                    patched_any = true;
                    break;
                }
            }

            if patched_any {
                continue;
            } else {
                break;
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
        // thus, they are orphans in that they are used but not declared
        let orphan_registers = used
            .difference(&declared)
            .copied()
            .collect::<FxHashSet<_>>();

        let mut anonymous = FxHashMap::default();
        let mut reals = FxHashMap::default();

        let block = self.function.blocks.get_mut(block_id).unwrap();
        let params = &mut block.parameters;

        // for all the registers that we have declared, we can provide those
        // so we shall mark them as real
        for declared in declared {
            reals.insert(declared, declared);
        }

        for orphan in orphan_registers.iter().copied() {
            // create an anonymous register that will replace the orphan
            let anonymous_register = self.retagger.gen();

            anonymous.insert(anonymous_register, orphan).expect_free();
            reals.insert(orphan, anonymous_register).expect_free();

            params.push(anonymous_register);
        }

        // replace all orphans with anonymous registers
        for register in block
            .instructions
            .iter_mut()
            .flat_map(|i| i.used_registers_mut())
            .chain(block.end.used_registers_mut())
        {
            if let Some(anonymous) = reals.get(register) {
                *register = *anonymous;
            }
        }

        self.blocks
            .insert(node, BlockState { reals, anonymous })
            .expect_free();
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

        let block = self.function.blocks.get(block_id).unwrap();

        let mut add_params_to_child = FxHashMap::default();
        let mut new_params = Vec::new();

        // figure out who our child blocks are, and what dependencies they need
        for (child_idx, BlockJump(id, params)) in block.end.children().into_iter().enumerate() {
            let mut insert_params = Vec::new();

            let child_block = self.function.blocks.get(id).unwrap();

            // if we are already calling the block with all of the arguments it
            // wants, there is nothing to do
            let fulfilled = child_block.parameters.len() == params.len();
            if fulfilled {
                continue;
            }

            debug_assert!(
                child_block.parameters.len() > params.len(),
                "if a block is not fulfilled, it should require more params than it has"
            );

            // our child block has registers that it does not have fulfilled
            for i in params.len()..child_block.parameters.len() {
                let param_reg = child_block.parameters[i];

                // the parameter in the child block is an anonymous register.
                // figure out if there's a corresponding real register for that
                let child_state = self
                    .blocks
                    .get(self.flow.map.get_by_left(id).unwrap())
                    .unwrap();
                if let Some(&real_reg) = child_state.anonymous.get(&param_reg) {
                    // so now we need to figure out if we have a corresponding register
                    let state = self.blocks.get(&node).unwrap();
                    if let Some(&anon_replacement_reg) = state.reals.get(&real_reg) {
                        // we do have one! replace this register
                        insert_params.push(anon_replacement_reg);
                    } else {
                        // we do not have a replacement for this register.
                        // create a replacement register now
                        let anon_replacement_reg = self.retagger.gen();

                        // record that this block now depends upon this register
                        new_params.push(anon_replacement_reg);

                        // pass the replacement to our child
                        insert_params.push(anon_replacement_reg);

                        // record the replacement
                        let state = self.blocks.get_mut(&node).unwrap();
                        state.anonymous.insert(anon_replacement_reg, real_reg);
                        state.reals.insert(real_reg, anon_replacement_reg);
                    }
                } else {
                    panic!("there should always be a real register");
                }
            }

            add_params_to_child.insert(child_idx, insert_params);
        }

        let block = self.function.blocks.get_mut(block_id).unwrap();

        // we do the mutations here to get around rust borrow checking
        // it makes the code harder to understand but /shrug

        // patch up our parameters
        let mut did_patch = !new_params.is_empty();
        block.parameters.extend(new_params);

        // add the params to every child
        for (child_idx, BlockJump(id, params)) in block.end.children_mut().into_iter().enumerate() {
            if let Some(add_to_child) = add_params_to_child.remove(&child_idx) {
                did_patch = did_patch || !add_to_child.is_empty();
                params.extend(add_to_child);
            }
        }

        did_patch
    }
}

pub fn translate_function(
    fn_id: FunctionId<IrCtx>,
    func: &Function,
    ext_fn_retagger: &ExtFnPassRetagger<IrCtx, IrCtx>,
    fn_retagger: &impl FnRetagger<IrCtx, IrCtx>,
    blk_retagger: &mut BlkMapRetagger<IrCtx, PureBbCtx>,
) -> FxHashMap<HostBlock, Block> {
    let highest_register = compute_highest_register(func);
    let mut retagger = RegGenPassRetagger::new(highest_register.unwrap_or_default());

    // we will need to mutate something as we gradually get every basic block
    // into slowly a purer and purer state
    let mut granular_rewrite = to_rewriting_fn(func, blk_retagger, ext_fn_retagger, fn_retagger);

    let flow = compute_flow(&granular_rewrite);
    println!("the flow: {:?}", flow);

    // so currently, our blocks have the following issues:
    // 1. blocks in functions may depend upon registers declared in other blocks
    // 2. registers in blocks isn't allocated optimally (NOT required)

    let mut algo = Algo::new(
        &flow,
        &mut granular_rewrite,
        &mut retagger,
        // &block_id_gen,
    );
    algo.solve();

    // INVARIANT: Unique block ids
    // all block ids must be unique, and all function ids paired with a block id
    // are the same (within this functiton), we know that we must ensure all blocks
    // are unique
    let mut unique_ids = FxHashSet::default();
    granular_rewrite
        .blocks
        .iter()
        .map(|(id, _)| id)
        .for_each(|id| {
            debug_assert!(
                unique_ids.insert(*id),
                "this id must not be present already"
            );
        });

    granular_rewrite
        .blocks
        .into_iter()
        .map(|(_, block)| {
            (
                HostBlock {
                    original_function: fn_id,
                    original_block: block.original_id,
                },
                Block {
                    id: block.id,
                    parameters: block.parameters,
                    instructions: block.instructions,
                    end: block.end,
                },
            )
        })
        .collect()
}

#[derive(Debug, Clone)]
struct Flow {
    map: BiHashMap<
        BlockId<PureBbCtx>,
        NodeIndex,
        BuildHasherDefault<FxHasher>,
        BuildHasherDefault<FxHasher>,
    >,
    cfg: ControlFlowGraph,
}

/// Generates a forwards and backwards graph, directing control flow from the
/// current block to the ones before it.
fn compute_flow(f: &RewritingFn) -> Flow {
    let mut flow = ControlFlowGraph::default();

    let mut block_to_node = new_bifxhashmap();
    for (id, _) in f.blocks.iter() {
        block_to_node.insert(*id, flow.add_node(*id)).expect_free();
    }
    let block_to_node = block_to_node;

    let map = |id| *block_to_node.get_by_left(id).unwrap();

    for (id, block) in f.blocks.iter() {
        let from = map(id);

        for path in block.end.children() {
            flow.add_edge(from, map(&path.0), ());
        }
    }

    Flow {
        map: block_to_node,
        cfg: flow,
    }
}

fn compute_highest_register(f: &Function) -> Option<RegisterId<IrCtx>> {
    // we aren't assigning `highest` to an `Option<RegisterId>` because moving
    // it out of an option pattern repeaatedly is kinda meh
    let mut has_encountered_register = false;
    let mut highest = RegisterId::new();

    let mut compare_highest = |current: RegisterId<IrCtx>| {
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

fn to_rewriting_fn(
    f: &Function,
    blk_retagger: &mut BlkMapRetagger<IrCtx, PureBbCtx>,
    ext_fn_retagger: &ExtFnPassRetagger<IrCtx, IrCtx>,
    fn_retagger: &impl FnRetagger<IrCtx, IrCtx>,
) -> RewritingFn {
    // TODO: we should've gotten a runtime error while mapping things if
    // using `BlkMapRetagger` instead of `BlkPassRetagger` is the fix (fix for what??????)
    // (jeez i need to comment better)
    for (id, _) in f.blocks.iter() {
        blk_retagger.retag_new(*id);
    }

    let mut result_fn = RewritingFn {
        _entry: blk_retagger.retag_old(f.entry_block),
        blocks: FxHashMap::default(),
    };

    let mut retagger = RegPassRetagger::default();
    // it would be very difficult to properly be concerned about registers being used,
    // so we don't. it would be very effort to get this working "properly", and slow
    retagger.ignore_checks();

    for (id, block) in f.blocks.iter() {
        println!("converting {:?} |=> {:?}", id, block);

        let is_entry_block = *id == f.entry_block;

        let parameters: Vec<RegisterId<PureBbCtx>> = match is_entry_block {
            // the entry block has no parameters, so we must use the parameters
            // from the function itself
            true => f
                .parameters
                .iter()
                .map(|p| retagger.retag_new(p.register))
                .collect(),
            false => block
                .parameters
                .iter()
                .map(|r| retagger.retag_new(*r))
                .collect(),
        };

        let instructions = block
            .instructions
            .iter()
            .map(|i| i.clone().retag(&mut retagger, ext_fn_retagger, fn_retagger))
            .collect();

        let end = block.end.clone().retag(&retagger, blk_retagger);

        let bb_id = blk_retagger.retag_old(*id);

        let block = RewritingFunctionBlock {
            original_id: *id,
            id: bb_id,
            parameters,
            instructions,
            end,
        };

        result_fn.blocks.insert(bb_id, block).expect_free();
    }

    result_fn
}

impl RewritingFunctionBlock {
    fn declared_registers(&self) -> FxHashSet<RegisterId<PureBbCtx>> {
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

    fn used_registers(&self) -> FxHashSet<RegisterId<PureBbCtx>> {
        let mut used = FxHashSet::default();

        for inst in self.instructions.iter() {
            used.extend(inst.used_registers());
        }

        used.extend(self.end.used_registers());
        used
    }
}
