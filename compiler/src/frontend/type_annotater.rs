
use std::collections::VecDeque;
use std::sync::Arc;

use thiserror::Error;
use rustc_hash::FxHashMap;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

use crate::frontend::ir::*;
use crate::id::*;
use crate::name::DebugName;

use super::conv_only_bb::Block;

/// Type annotation mechanism in JSSAT.
///
/// This works by symbolically executing the JSSAT IR, and emitting equivalent functions.
// can't use references because we need them to live 'static for tokio::spawn to work
pub fn annotate(ir: &IR, blocks: Vec<Block>) -> SymbolicEngine {
    let mut entrypoints = FxHashMap::default();
    for (id, func) in ir.functions.iter() {
        entrypoints.insert(*id, func.entry_block);
    }

    let symb_exec_eng =
        SymbolicEngineToken::new(blocks, entrypoints, ir.external_functions.clone());

    let explore_req = symb_exec_eng.clone().explore_fn(BlockExecutionKey {
        function: ir.entrypoint,
        block: ir.functions.get(&ir.entrypoint).unwrap().entry_block,
        parameters: vec![],
    });

    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(explore_req);
    drop(rt);

    let mutex = Arc::try_unwrap(symb_exec_eng.0).expect("nothing should be using the mutex");
    Mutex::into_inner(mutex)
}

#[derive(Clone)]
struct SymbolicEngineToken(Arc<Mutex<SymbolicEngine>>);

#[derive(Debug)]
pub struct SymbolicEngine {
    // can't use references because we need them to live 'static for tokio::spawn to work,
    // which is why it's Arc<T> and not &T
    //
    // need a Vec<Arc<T>> rather than a Vec<T> because in `explore_block` we need to have
    // a reference to a block which requires us to lock the engine, but we need to temporarily
    // unlock the engine in order to figure out the type of a function call. we'd need
    // to release and then regain our reference, but that's annoying, so Vec<Arc<T>> it is
    pub blocks: Arc<Vec<Arc<Block>>>,
    pub entrypoints: FxHashMap<FunctionId, BlockId>,
    pub executions: Executions,
    pub ext_fns: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub typed_blocks: FxHashMap<BlockKey, TypedFunction>,
    pub new_fn_ids: Counter<FunctionId>,
}


#[derive(Debug)]
pub struct Executions {
    // mapping of ORIGINAL fn id + block id to NEW fn id + block id
    // TODO: annotate these Contexts
    executions: FxHashMap<(FunctionId, BlockId), Vec<(Vec<ValueType>, BlockExecution)>>,
}

impl Executions {
    pub fn new() -> Self {
        Self {
            executions: Default::default(),
        }
    }

    pub fn get(&self, key: &BlockExecutionKey) -> Option<&BlockExecution> {
        self.executions
            .get(&(key.function, key.block))
            .and_then(|blocks| {
                blocks
                    .iter()
                    .filter(|(p, _)| p == &key.parameters)
                    .map(|(_, block)| block)
                    .next()
            })
    }

    pub fn insert(&mut self, key: BlockExecutionKey, execution: BlockExecution) {
        let executions = self
            .executions
            .entry((key.function, key.block))
            .or_insert_with(|| Vec::with_capacity(1));

        executions.push((key.parameters, execution));
    }

    pub fn all_fn_invocations(&self) -> impl Iterator<Item = (FunctionId, BlockId, &Vec<ValueType>, &BlockExecution)> {
        self.executions.iter()
            .flat_map(|(k, v)| v.iter().map(move |e| (k, e)))
            .map(|((fn_id, blk), (args, cf))| (*fn_id, *blk, args, cf))
    }
}

impl Default for Executions {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct BlockKey {
    pub function: FunctionId,
    pub block: BlockId,
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockExecutionKey {
    pub function: FunctionId,
    pub block: BlockId,
    pub parameters: Vec<ValueType>,
}

impl BlockExecutionKey {
    fn key(&self) -> BlockKey {
        BlockKey {
            function: self.function,
            block: self.block,
        }
    }
}

#[derive(Debug)]
pub enum BlockExecution {
    InProgress(BlockKey),
    Finished(BlockKey),
}

impl BlockExecution {
    pub fn key(&self) -> BlockKey {
        match self {
            BlockExecution::InProgress(a) |
            BlockExecution::Finished(a) => *a,
        }
    }
}

#[derive(Debug)]
pub struct TypedFunction {
    pub return_type: ReturnType,
    pub eval_blocks: Vec<(BlockId, Vec<ValueType>, ExplorationBranch, FxHashMap<RegisterId, ValueType>)>,
}

impl TypedFunction {
    pub fn find(&self, block: &BlockId, args: &Vec<ValueType>) -> (&ExplorationBranch, &FxHashMap<RegisterId, ValueType>) {
        (self.eval_blocks.iter())
            .filter(|(blk, blk_args, _, _)| blk == block && blk_args == args)
            .map(|(_, _, branch, map)| (branch, map))
            .next()
            .unwrap()
    }
}

impl SymbolicEngineToken {
    fn new(
        blocks: Vec<Block>,
        entrypoints: FxHashMap<FunctionId, BlockId>,
        ext_fns: FxHashMap<ExternalFunctionId, ExternalFunction>,
    ) -> Self {
        Self(Arc::new(Mutex::new(SymbolicEngine {
            blocks: Arc::new(blocks.into_iter().map(Arc::new).collect()),
            entrypoints,
            ext_fns,
            executions: Executions::new(),
            typed_blocks: FxHashMap::default(),
            new_fn_ids: Counter::new(),
        })))
    }

    fn explore_fn_later(&self, block: BlockExecutionKey) -> JoinHandle<ReturnType> {
        let me = self.clone();
        tokio::task::spawn(me.explore_fn(block))
    }

    async fn explore_fn(self, block: BlockExecutionKey) -> ReturnType {
        let mut me = (self.0.try_lock()).expect("Lock should be contentionless");

        // first, check if we've already executed this block with the values present
        let key = match me.executions.get(&block) {
            // we've invoked this function and computed it fully before
            Some(BlockExecution::Finished(re)) => {
                return me.typed_blocks.get(re).unwrap().return_type.clone();
            }
            // if we're in progress of executing this exact function, that means
            // we've taken on such a path that calls the same exact function.
            // by returning `Never`, we display this recursiveness - this path
            // is one that would never end. If there are other paths in the
            // function, this return type will get more accurate as the other
            // paths are combined.
            Some(BlockExecution::InProgress(_)) => return ReturnType::Never,
            None => {
                let block_key = BlockKey {
                    function: me.new_fn_ids.next(),
                    block: BlockId::new(),
                };

                me.executions
                    .insert(block.clone(), BlockExecution::InProgress(block_key));

                block_key
            }
        };

        drop(me);

        // we choose never as `Never union T = T`, making it the most unifyable default
        let mut return_type = ReturnType::Never;
        let mut eval_blocks = Vec::new();

        let mut block_stack = VecDeque::new();
        block_stack.push_back(block);

        while let Some(exec_key) = block_stack.pop_front() {
            let has_evaled_block = {
                eval_blocks.iter().any(|(block, keys, _, _)| *block == exec_key.block && keys == &exec_key.parameters)
            };

            if has_evaled_block {
                continue;
            }

            let block = exec_key.block;
            let params = exec_key.parameters.clone();

            let Exploration {
                control_flow,
                types,
            } = self.explore_block(exec_key).await;

            match &control_flow {
                ExplorationBranch::Branch(next) => {
                    block_stack.extend(next.clone());
                }
                ExplorationBranch::Complete(ret_type) => return_type.unify(ret_type.clone()),
            };

            eval_blocks.push((block, params, control_flow, types));
        }
        
        let typed = TypedFunction {
            return_type,
            eval_blocks
        };

        let mut me = (self.0.try_lock()).expect("Lock should be contentionless");
        me.typed_blocks.insert(key, typed);

        me.typed_blocks.get(&key).unwrap().return_type.clone()
    }

    async fn explore_block(&self, key: BlockExecutionKey) -> Exploration {
        let mut me = (self.0.try_lock()).expect("Lock should be contentionless");

        // TODO: use a hashmap
        let block = (me.blocks.iter())
            .filter(|b| b.derived_from == (key.function, key.block))
            .next()
            .expect("expected to find a block")
            .clone();

        let mut types = FxHashMap::default();

        debug_assert_eq!(block.parameters.len(), key.parameters.len(), "at {:?}", key);
        for (parameter, r#type) in block.parameters.iter().zip(key.parameters.iter()) {
            types.insert(*parameter, r#type.clone());
        }

        for inst in block.instructions.iter() {
            let map = |r: &RegisterId| types.get(r).unwrap();

            match inst {
                Instruction::GetRuntime(reg) => {
                    types.insert(*reg, ValueType::Runtime);
                }
                Instruction::MakeString(reg, str) => {
                    types.insert(*reg, ValueType::ExactString(*str));
                }
                Instruction::MakeNumber(reg, value) => {
                    types.insert(*reg, ValueType::ExactNumber(*value));
                }
                Instruction::CompareLessThan(reg, lhs, rhs) => {
                    let comparison = match (map(lhs).is_comparable(), map(rhs).is_comparable()) {
                        (None, _) => unimplemented!("LHS is uncomparable"),
                        (_, None) => unimplemented!("RHS is uncomparable"),
                        (Some(lhs), Some(rhs)) => lhs.perform_less_than(&rhs),
                    };

                    types.insert(*reg, comparison.to_value_type());
                }
                Instruction::Add(reg, lhs, rhs) => {
                    let addition = match (map(lhs).is_addable(), map(rhs).is_addable()) {
                        (_, None) => unimplemented!("LHS is uncomparable"),
                        (None, _) => unimplemented!("RHS is uncomparable"),
                        (Some(a), Some(b)) => a.perform_add(&b).expect("should be able to add"),
                    };

                    types.insert(*reg, addition.to_value_type());
                }
                Instruction::Call(result, func, arg_regs) => {
                    let args = arg_regs.iter().map(map).cloned().collect::<Vec<_>>();

                    match func {
                        Callable::External(id) if result.is_some() => {
                            let ext_fn = me.ext_fns.get(id).unwrap();

                            let ret_type = match &ext_fn.return_type {
                                FFIReturnType::Void => todo!(
                                    "figure out what to do with void return type being assigned"
                                ),
                                FFIReturnType::Value(v) => ffi_value_type_to_value_type(v),
                            };

                            types.insert(result.unwrap(), ret_type);

                            debug_assert_eq!(args.len(), ext_fn.parameters.len());
                            for (arg_typ, ffi_typ) in args.iter().zip(ext_fn.parameters.iter()) {
                                assert!(FFICoerce::can_coerce(arg_typ, ffi_typ));
                            }
                        }
                        Callable::External(id) => {
                            // we don't care about the return type, so /shrug
                            // TODO: dedup code?
                            let ext_fn = me.ext_fns.get(id).unwrap();

                            debug_assert_eq!(args.len(), ext_fn.parameters.len());
                            for (arg_typ, ffi_typ) in args.iter().zip(ext_fn.parameters.iter()) {
                                assert!(FFICoerce::can_coerce(arg_typ, ffi_typ), "cannot coerce {:?} to {:?} on {:?}", arg_typ, ffi_typ, inst);
                            }
                        }
                        Callable::Static(id) => {
                            let entrypoint = *me.entrypoints.get(id).unwrap();

                            drop(me);

                            let key = BlockExecutionKey {
                                function: *id,
                                block: entrypoint,
                                parameters: args,
                            };

                            let ret_type = self
                                .explore_fn_later(key)
                                .await
                                .expect("couldnt explore function call");

                            match ret_type {
                                ReturnType::Void if result.is_none() => {}
                                ReturnType::Void => todo!("figure out what to do"),
                                ReturnType::Value(value) if matches!(result, Some(_)) => {
                                    types.insert(result.unwrap(), value);
                                }
                                ReturnType::Value(_) => {
                                    // we computed the function return type but it's never used
                                    // /shrug
                                }
                                ReturnType::Never => todo!("return never"),
                            };

                            me = (self.0.try_lock()).expect("Lock should be contentionless");
                        }
                    };
                }
                Instruction::Unreachable => {
                    todo!("return from block as `never`")
                }
            }
        }

        let map_bsc_blk_jmp = |BasicBlockJump(block, args)| BlockExecutionKey {
            function: key.function,
            block,
            parameters: args
                .into_iter()
                .map(|r| types.get(&r).expect(&format!("in {:?} -> {:?}({:?})", &key, &block, r)).clone())
                .collect(),
        };

        let control_flow = match &block.end {
            ControlFlowInstruction::Jmp(target) => {
                ExplorationBranch::Branch(vec![map_bsc_blk_jmp(target.clone())])
            }
            ControlFlowInstruction::JmpIf {
                condition,
                true_path,
                false_path,
            } => {
                let cond = types.get(condition).unwrap();

                ExplorationBranch::Branch(match cond {
                    ValueType::Bool(true) => vec![map_bsc_blk_jmp(true_path.clone())],
                    ValueType::Bool(false) => vec![map_bsc_blk_jmp(false_path.clone())],
                    ValueType::Boolean => vec![
                        map_bsc_blk_jmp(true_path.clone()),
                        map_bsc_blk_jmp(false_path.clone()),
                    ],
                    _ => unimplemented!("cannot operate on condition {:?}", cond),
                })
            }
            ControlFlowInstruction::Ret(Some(reg)) => {
                ExplorationBranch::Complete(ReturnType::Value(types.get(reg).unwrap().clone()))
            }
            ControlFlowInstruction::Ret(None) => ExplorationBranch::Complete(ReturnType::Void),
        };

        Exploration {
            control_flow,
            types,
        }
    }
}

struct Exploration {
    control_flow: ExplorationBranch,
    types: FxHashMap<RegisterId, ValueType>,
}

#[derive(Debug)]
pub enum ExplorationBranch {
    Branch(Vec<BlockExecutionKey>),
    Complete(ReturnType),
}

// #[derive(Debug)]
// struct SymbolicExecutionEngine<'ir> {
//     ir: &'ir IR,
//     free_fn_id: FunctionId,
//     executions: FxHashMap<(FunctionId, Vec<ValueType>), FnExecution>,
//     typed_functions: FxHashMap<FunctionId, TypedFunction>,
// }

// #[derive(Debug)]
// pub enum FnExecution {
//     Finished(FunctionId),
//     Executing(FunctionId),
// }

// impl SymbolicExecutionEngine<'_> {
//     pub fn new<'ir>(ir: &'ir IR) -> SymbolicExecutionEngine<'ir> {
//         SymbolicExecutionEngine {
//             ir,
//             free_fn_id: FunctionId::new(),
//             executions: FxHashMap::default(),
//             typed_functions: FxHashMap::default(),
//         }
//     }

//     /// # symbolically_execute
//     ///
//     /// This function will symbolically execute a function until completion,
//     /// taking all possible paths of execution until a return type is found for
//     /// the function.
//     fn symbolically_execute(
//         &mut self,
//         function_id: &FunctionId,
//         parameters: Vec<ValueType>,
//     ) -> (FunctionId, &ReturnType) {
//         // TODO: figure out a plan of action to not over-allocate function ids
//         // most times when executing `symbolically_execute` we won't ever use this
//         let self_fn_id = self.free_fn_id.next_and_mut();

//         let function = (self.ir.functions.get(function_id)).expect("valid function id");

//         debug_assert_eq!(function.parameters.len(), parameters.len());
//         debug_assert_eq!(function.blocks.len(), 1, "control flow not supported atm");

//         // TODO: the current logic could deadlock or falsely claim for a function return `Never`
//         //       this should be fixed by accounting for multiple paths of execution in the future.
//         //       as of right now, that's future me's problem. sorry :hugging:
//         //
//         // check the current status of symbolic execution for a function like this one
//         // this will handle edge cases like recursion
//         match self.executions.get(&(*function_id, parameters.clone())) {
//             // we've already symbolically executed this function before
//             Some(FnExecution::Finished(_type_definition)) => {
//                 todo!("return type_definition?");
//             }
//             // nothing inserted
//             None => {
//                 // insert a "currently executing" status
//                 self.executions.insert(
//                     (*function_id, parameters.clone()),
//                     FnExecution::Executing(self_fn_id),
//                 );
//             }
//             // a record that the function is currently being symbolically executed
//             // reaching here would mean that we are most likely in some sort of recursion
//             Some(FnExecution::Executing(_)) => {
//                 // generalize the `ValueType` parameters.
//                 // this is because we want to break out of the following scenarios:
//                 //
//                 // ```js
//                 // function recurse_forever(n) {
//                 //   return recurse_forever(n + 1);
//                 // }
//                 //
//                 // recurse_forever(0)
//                 // ```
//                 //
//                 // without generalization, the above function would recurse forever,
//                 // and be symbolically executed forever, as the types of the parameters
//                 // passed to it would be ExactNumber(0), ExactNumber(1), etc.
//                 //
//                 // with generalization, the types the function is symbolically executed for
//                 // become ExactNumber(0), Number, and seeing that it will only call itself,
//                 // it can get the return type Never.
//                 let generalized = parameters
//                     .iter()
//                     .map(|t| t.generalize())
//                     .collect::<Vec<_>>();

//                 match self.executions.get(&(*function_id, generalized.clone())) {
//                     // we've already executed it and we have a definition for it
//                     Some(FnExecution::Finished(fn_id)) => {
//                         return (
//                             *fn_id,
//                             &self.typed_functions.get(fn_id).unwrap().return_type,
//                         );
//                     }
//                     // TODO: if we're executing multiple paths, we should wait for all paths
//                     //       of execution to finish before making the call that the function
//                     //       never returns.
//                     //
//                     // if we are currently working on symbolically executing the general function,
//                     // it's probably a recursive function. conclude that we should return a Never.
//                     Some(FnExecution::Executing(function_id)) => {
//                         return (*function_id, &ReturnType::Never);
//                     }
//                     // haven't executed the generic function yet, execute it.
//                     //
//                     // if we end up going from a specific form of execution
//                     // to a more generic one, that means that the definition
//                     // of the specific function should really just be the generic
//                     // definition.
//                     None => {
//                         // TODO: maybe it could cause problems to claim that the specific
//                         // and generic function are exactly one and the same, when they have
//                         // different IDs and stuff? oh well, /shrug
//                         return self.symbolically_execute(function_id, generalized);
//                     }
//                 };
//             }
//         };

//         // at this point, we've confirmed that we're not in a recursive loop
//         // (as recursiveness will be handled by the previous match)

//         let mut typed_fn_blocks = FxHashMap::default();
//         let mut register_types = FxHashMap::<RegisterId, ValueType>::default();

//         // annotate the parameter registers with types
//         for (register_id, parameter) in function
//             .parameters
//             .iter()
//             .map(|p| p.register)
//             .zip(parameters.iter())
//         {
//             register_types.insert(register_id, parameter.clone());
//         }

//         let (block_id, block) = function.blocks.iter().next().unwrap();
//         let mut instructions = Vec::new();
//         let mut is_prematurely_exiting_due_to_infinite_recursion = false;

//         for instruction in block.instructions.iter() {
//             match instruction {
//                 // external functions are simpler to deal with, because they have definite arguments.
//                 // moreover, there's no need to malleate typeless arguments into external function args
//                 // because the external function arguments **must** be correct, whereas with dynamic IR
//                 // we **must** produce a program rather than call out type errors.
//                 Instruction::Call(result, Callable::External(fn_id), args) => {
//                     let parameter_types = args
//                         .iter()
//                         .map(|r| register_types.get(r).unwrap().clone())
//                         .collect::<Vec<_>>();

//                     let external_fn =
//                         (self.ir.external_functions.get(&fn_id)).expect("expected external fn");

//                     // type checking: assert that the registers are passable to the FFI function
//                     for (idx, (ext_fn_type, arg_type)) in external_fn
//                         .parameters
//                         .iter()
//                         .zip(parameter_types.iter())
//                         .enumerate()
//                     {
//                         let ffi_as_value_type = ffi_value_type_to_value_type(ext_fn_type);
//                         if !arg_type.assignable_to(&ffi_as_value_type) {
//                             panic!(
//                                 "register {:?} type {:?} not assignable FFI type {:?} type {:?} to in instruction {:?}",
//                                 args[idx], arg_type, ext_fn_type, ffi_as_value_type, instruction
//                             );
//                         }
//                     }

//                     // aassign type to register, if one is wanted
//                     match (result, &external_fn.return_type) {
//                         (None, _) => {}
//                         (Some(result_register), FFIReturnType::Value(v)) => {
//                             register_types
//                                 .insert(*result_register, ffi_value_type_to_value_type(v));
//                         }
//                         (Some(_), FFIReturnType::Void) => {
//                             panic!("cannot assign void to register at {:?}", instruction);
//                         }
//                     };

//                     instructions.push(Instruction::Call(
//                         *result,
//                         Callable::External(*fn_id),
//                         args.clone(),
//                     ));
//                 }
//                 Instruction::Call(result, Callable::Static(fn_id), args) => {
//                     let parameter_types = args
//                         .iter()
//                         .map(|r| register_types.get(r).unwrap().clone())
//                         .collect::<Vec<_>>();

//                     let (id, return_type) = self.symbolically_execute(&fn_id, parameter_types);

//                     match (result, return_type) {
//                         (_, ReturnType::Never) => {
//                             instructions.push(Instruction::Call(
//                                 None,
//                                 Callable::Static(id),
//                                 args.clone(),
//                             ));
//                             instructions.push(Instruction::Unreachable);
//                             is_prematurely_exiting_due_to_infinite_recursion = true;
//                             break;
//                         }
//                         (None, _) => {}
//                         (Some(register), ReturnType::Value(v)) => {
//                             register_types.insert(*register, v.clone());
//                         }
//                         (Some(_), ReturnType::Void) => {
//                             panic!("cannot assign void to register at {:?}", instruction);
//                         }
//                     };

//                     instructions.push(Instruction::Call(
//                         *result,
//                         Callable::Static(id),
//                         args.clone(),
//                     ));
//                 }
//                 Instruction::GetRuntime(register) => {
//                     register_types.insert(*register, ValueType::Runtime);

//                     instructions.push(Instruction::GetRuntime(*register));
//                 }
//                 Instruction::MakeString(register, constant) => {
//                     register_types.insert(*register, ValueType::ExactString(*constant));

//                     instructions.push(Instruction::MakeString(*register, *constant));
//                 }
//                 Instruction::Unreachable => {
//                     instructions.push(Instruction::Unreachable);
//                 }
//                 Instruction::MakeNumber(_, _) => todo!(),
//                 Instruction::CompareLessThan(_, _, _) => todo!(),
//                 Instruction::Add(_, _, _) => todo!(),
//             }
//         }

//         let (end_instruction, return_type) = if is_prematurely_exiting_due_to_infinite_recursion {
//             // if we're infinitely recursing,
//             // that means the return type of this function is `Never`
//             //
//             // we can never return any data as we can never complete (Ret(none))
//             // and signal to the caller that we never return (ReturnType::Never)
//             (ControlFlowInstruction::Ret(None), ReturnType::Never)
//         } else {
//             (
//                 block.end.clone(),
//                 match &block.end {
//                     ControlFlowInstruction::Ret(Some(register)) => {
//                         ReturnType::Value(register_types.get(register).unwrap().clone())
//                     }
//                     ControlFlowInstruction::Ret(None) => ReturnType::Void,
//                     ControlFlowInstruction::Jmp(_) => todo!(),
//                     ControlFlowInstruction::JmpIf { .. } => todo!(),
//                 },
//             )
//         };

//         typed_fn_blocks.insert(
//             *block_id,
//             FunctionBlock {
//                 parameters: todo!(),
//                 instructions,
//                 end: end_instruction,
//             },
//         );

//         // now we've annotated the function totally. we can insert the information we know about the function
//         let top_free_register = register_types
//             .iter()
//             .map(|(k, _)| *k)
//             .max_by(|a, b| a.value().cmp(&b.value()))
//             .map(|r| r.next())
//             .unwrap_or(RegisterId::new());

//         let typed_parameters = function
//             .parameters
//             .iter()
//             .zip(parameters.iter())
//             .map(|(p, t)| Parameter {
//                 name: p.name.clone(),
//                 register: p.register,
//                 r#type: t.clone(),
//             })
//             .collect::<Vec<_>>();

//         self.typed_functions.insert(
//             self_fn_id,
//             TypedFunction {
//                 name: function.name.clone(),
//                 top_free_register,
//                 parameters: typed_parameters,
//                 return_type,
//                 entry_block: function.entry_block,
//                 blocks: typed_fn_blocks,
//                 register_types,
//             },
//         );

//         self.executions.insert(
//             (*function_id, parameters.clone()),
//             FnExecution::Finished(self_fn_id),
//         );

//         (
//             self_fn_id,
//             &self.typed_functions.get(&self_fn_id).unwrap().return_type,
//         )
//     }
// }

// #[derive(Debug)]
// pub struct TypedFunction {
//     pub name: DebugName,
//     /// The highest register ID that is unavailable. All registers after this
//     /// one must also be unclaimed. This is so that the skeleton phase can gen
//     /// code for the LLVM phase
//     pub top_free_register: RegisterId,
//     pub parameters: Vec<Parameter>,
//     pub return_type: ReturnType,
//     // pub control_flow: ControlFlowGraph,
//     // pub register_flow: ValueFlowGraph,
//     pub entry_block: BlockId,
//     pub blocks: FxHashMap<BlockId, FunctionBlock>,
//     pub register_types: FxHashMap<RegisterId, ValueType>,
// }

#[derive(Debug)]
pub struct Parameter {
    pub name: DebugName,
    pub register: RegisterId,
    pub r#type: ValueType,
}

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

#[derive(Debug, Clone, PartialEq)]
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
    ExactString(ConstantId),
    Number,
    ExactNumber(f64),
    BytePointer,
    /// Pointer to data of the specified size. Pointer(16) -> `i16*`.
    Pointer(u16),
    Word,
    Boolean,
    Bool(bool),
    // /// # [`ValueType::Union`]
    // ///
    // /// The type assigned to a value if it is determined to take two possible
    // /// paths of execution.
    // Union(Vec<ValueType>),
}

// impl ValueType {
//     fn generalize(&self) -> ValueType {
//         match self {
//             ValueType::Any => ValueType::Any,
//             ValueType::Runtime => ValueType::Runtime,
//             ValueType::String => ValueType::String,
//             ValueType::ExactString(_) => ValueType::String,
//             ValueType::BytePointer => ValueType::BytePointer,
//             ValueType::Pointer(s) => ValueType::Pointer(*s),
//             ValueType::Word => ValueType::Word,
//             ValueType::Union(inner) => {
//                 // TODO: if there are inner `Union`s, flat_map 'em
//                 ValueType::Union(inner.iter().map(|v| v.generalize()).collect())
//             }
//         }
//     }

//     /// Panics if the input type isn't an inheritence type.
//     fn generalize_up_one(&self) -> ValueType {
//         match self {
//             ValueType::Any => panic!("cannot generalize up one for an Any"),
//             ValueType::String => ValueType::Any,
//             ValueType::ExactString(_) => ValueType::String,
//             ValueType::Runtime
//             | ValueType::Union(_)
//             | ValueType::BytePointer
//             | ValueType::Pointer(_)
//             | ValueType::Word => {
//                 panic!("expected inheritable")
//             }
//         }
//     }

//     fn assignable_to(&self, target: &ValueType) -> bool {
//         // two types that are equivalent are always assignable to one another
//         if self == target {
//             return true;
//         }

//         {
//             // these are native types that are only equal to their target{
//             debug_assert!(
//                 self != target,
//                 "native types are only equal to their target. if self is never target, this guarantee is upheld"
//             );

//             if matches!(
//                 self,
//                 ValueType::Runtime | ValueType::BytePointer | ValueType::Word
//             ) {
//                 return false;
//             }

//             if matches!(
//                 target,
//                 ValueType::Runtime | ValueType::BytePointer | ValueType::Word
//             ) {
//                 return false;
//             }
//         }

//         {
//             // TODO: handle unions
//             if let ValueType::Union(_) = self {
//                 todo!("unions");
//             }

//             if let ValueType::Union(_) = target {
//                 todo!("unions");
//             }
//         }

//         {
//             // now use inheritence logic

//             debug_assert!(
//                 matches!(
//                     self,
//                     ValueType::Any | ValueType::String | ValueType::ExactString(_)
//                 ),
//                 "at this point, we should only be dealing with types with inheritence semantics"
//             );

//             debug_assert!(
//                 matches!(
//                     target,
//                     ValueType::Any | ValueType::String | ValueType::ExactString(_)
//                 ),
//                 "at this point, we should only be dealing with types with inheritence semantics"
//             );

//             debug_assert!(
//                 self != target,
//                 "we know self != target, so we must generalize self one level"
//             );

//             // if we are the most general type, we cannot store the most general
//             // type into a more specific type.
//             if let ValueType::Any = self {
//                 return false;
//             }

//             // TODO: instead of going through the entire `assignable_to`, just bring
//             //       it back into this function. would just need self == target -> true
//             //       and self == Any -> false in a loop
//             return self.generalize_up_one().assignable_to(target);
//         }
//     }
// }

pub fn ffi_value_type_to_value_type(ffi_value_type: &FFIValueType) -> ValueType {
    match ffi_value_type {
        FFIValueType::Any => ValueType::Any,
        FFIValueType::Runtime => ValueType::Runtime,
        FFIValueType::BytePointer => ValueType::BytePointer,
        FFIValueType::Pointer(size) => ValueType::Pointer(*size),
        FFIValueType::Word => ValueType::Word,
        FFIValueType::String => ValueType::String,
    }
}

impl ValueType {
    pub fn is_comparable(&self) -> Option<ValueComparable> {
        match self {
            // TODO: *is* an `Any` comparable? or should we force the user to unwrap it
            ValueType::Any => todo!(),
            ValueType::Number => Some(ValueComparable::Number),
            ValueType::ExactNumber(value) => Some(ValueComparable::Num(*value)),
            _ => None,
        }
    }

    pub fn is_addable(&self) -> Option<ValueAddable> {
        match self {
            ValueType::ExactString(_) => todo!(),
            ValueType::String => Some(ValueAddable::String),
            ValueType::Number => Some(ValueAddable::Number),
            ValueType::ExactNumber(n) => Some(ValueAddable::Num(*n)),
            _ => None
        }
    }
}

pub enum ValueAddable {
    Number,
    Num(f64),
    String,
    // Str(Vec<u8>),
}

#[derive(Debug, Error)]
pub enum AdditionError {
    #[error("the types are incompatible")]
    IncompatibleTypes,
}

impl ValueAddable {
    pub fn perform_add(&self, other: &ValueAddable) -> Result<ValueAddable, AdditionError> {
        Ok(match (self, other) {
            (ValueAddable::Number, ValueAddable::Number) => ValueAddable::Number,
            (ValueAddable::Number, ValueAddable::Num(_)) => ValueAddable::Number,
            (ValueAddable::Num(_), ValueAddable::Number) => ValueAddable::Number,
            (ValueAddable::Num(a), ValueAddable::Num(b)) => ValueAddable::Num(*a + *b),
            (ValueAddable::String, ValueAddable::String) => ValueAddable::String,
            (ValueAddable::Number, ValueAddable::String) |
            (ValueAddable::Num(_), ValueAddable::String) |
            (ValueAddable::String, ValueAddable::Number) |
            (ValueAddable::String, ValueAddable::Num(_)) => return Err(AdditionError::IncompatibleTypes),
        })
    }
}

pub enum ValueComparable {
    Number,
    Num(f64)
}

impl ValueComparable {
    pub fn perform_less_than(&self, other: &ValueComparable) -> ValueConditional {
        match (self, other) {
            // TODO: use guarantees (e.g. "x < y") to make more informed decisions
            (ValueComparable::Number, _) |
            (_, ValueComparable::Number) => ValueConditional::Boolean,
            (ValueComparable::Num(lhs), ValueComparable::Num(rhs)) => ValueConditional::Bool(lhs < rhs),
        }
    }
}

pub trait ToValueType {
    fn to_value_type(self) -> ValueType;
}

// impl<V: ToValueType> Into<ValueType> for V {
//     fn into(self) -> ValueType {
//         self.to_value_type()
//     }
// }

impl ToValueType for ValueConditional {
    fn to_value_type(self) -> ValueType {
        match self {
            ValueConditional::Boolean => ValueType::Boolean,
            ValueConditional::Bool(b) => ValueType::Bool(b),
        }
    }
}

impl ToValueType for ValueAddable {
    fn to_value_type(self) -> ValueType {
        match self {
            ValueAddable::Number => ValueType::Number,
            ValueAddable::Num(n) => ValueType::ExactNumber(n),
            ValueAddable::String => ValueType::String,
        }
    }
}

pub enum ValueConditional {
    Boolean,
    Bool(bool)
}

struct FFICoerce;

impl FFICoerce {
    // TODO: figure out how to merge "can_coerce" and "do_coerce" into one, so
    // that in order for something to be coercible there must be a conversion
    // routine that can handle it - all enforced at compile time
    pub fn can_coerce(value_type: &ValueType, ffi: &FFIValueType) -> bool {
        match (ffi, value_type) {
            (FFIValueType::Any, ValueType::Any) 
            | (FFIValueType::Any, ValueType::String) 
            | (FFIValueType::Any, ValueType::ExactString(_)) 
            | (FFIValueType::Any, ValueType::Number) 
            | (FFIValueType::Any, ValueType::ExactNumber(_)) 
            // | (FFIValueType::Any, ValueType::Word) 
            | (FFIValueType::Any, ValueType::Boolean) 
            | (FFIValueType::Any, ValueType::Bool(_)) 
            | (FFIValueType::Runtime, ValueType::Runtime) 
            | (FFIValueType::BytePointer, ValueType::BytePointer) 
            | (FFIValueType::BytePointer, ValueType::Pointer(8)) 
            | (FFIValueType::Pointer(8), ValueType::BytePointer) 
            // | (FFIValueType::Word, ValueType::Number) 
            // | (FFIValueType::Word, ValueType::ExactNumber(_))
            | (FFIValueType::Word, ValueType::Word)
            | (FFIValueType::String, ValueType::String) 
            | (FFIValueType::String, ValueType::ExactString(_)) 
            => true,
            (FFIValueType::Pointer(p1), ValueType::Pointer(p2)) if p1 == p2 => true,
            (_, _) => false
        }
    }
}

impl ReturnType {
    fn unify(&mut self, other: ReturnType) {
        match (&self, other) {
            (ReturnType::Value(_), ReturnType::Value(_)) => todo!("unify 2 values"),
            (a, b) if *a == &b => {
                // do nothing, as both types are the same
            },
            (ReturnType::Never, other) => { *self = other; },
            (_, ReturnType::Never) => {}
            (ReturnType::Void, ReturnType::Void) => {},
            (ReturnType::Void, ReturnType::Value(_)) => todo!(),
            (ReturnType::Value(_), ReturnType::Void) => todo!(),
        }
    }
}