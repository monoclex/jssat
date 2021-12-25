//! The abstract interpretation module in JSSAT.

use std::borrow::Cow;
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use jssat_ir::collections::StrictZip;
use jssat_ir::id::{Counter, LiftedCtx, RegisterId, Tag, UnionId, UniqueListId, UniqueRecordId};
use jssat_ir::isa::BlockJump;
use rustc_hash::FxHashMap;

use crate::lifted::{FunctionId, LiftedProgram};
use crate::types::{List, Record, Type, TypeCtx, TypeCtxMut, TypeDuplication};

pub struct AbsIntEngine<'program> {
    program: &'program LiftedProgram,
    record_id: Counter<UniqueRecordId<LiftedCtx>>,
    list_id: Counter<UniqueListId<LiftedCtx>>,
    recursion_detector: RecursionDetector,
    function_cache: FunctionCache,
}

impl<'p> AbsIntEngine<'p> {
    pub fn new(program: &'p LiftedProgram) -> Self {
        Self {
            program,
            record_id: Default::default(),
            list_id: Default::default(),
            recursion_detector: Default::default(),
            function_cache: Default::default(),
        }
    }

    pub fn call(&mut self, function: FunctionId, args: TypeCtx) -> EvalResult {
        println!("@ {:?}", function);
        // 1. check previous execution results
        let idx = self.function_cache.state(function, args);
        let (args, state) = &mut self.function_cache[idx];

        match state {
            EvaluationState::Completed(result) => return result.clone(),
            EvaluationState::PartiallyCompleted(result) => return result.clone(),
            EvaluationState::InProgress => return EvalResult::new_never(),
            EvaluationState::NeverExecuted => {}
        };

        *state = EvaluationState::InProgress;

        // 2. if we're too recursive, generalize our arguments
        let is_recursion = self.recursion_detector.enter_fn(function);
        if is_recursion {
            todo!("generalize arguments, then continue execution")
        }

        // 3. simulate the function
        let mut current_state = TypeCtx::new();
        duplicate_registers(&mut current_state, args);

        let code = self.program.functions.get(&function).unwrap();

        current_state.borrow_mut(|mut state| {
            for instruction in code.instructions.iter() {
                use jssat_ir::frontend::ir::InstructionData::*;

                match &instruction.data {
                    Comment(_) => {}
                    NewRecord(i) => {
                        let unique_id = self.record_id.next();
                        let record = Record::new(unique_id);
                        state.insert(i.result, state.make_type_record(record));
                    }
                    RecordGet(i) => {
                        let record = state.get(&i.record).unwrap();
                        let record = record.unwrap_record();

                        use jssat_ir::isa::RecordKey::*;
                        let key = match i.key {
                            DynAtom(r) | Prop(r) => *state.get(&r).unwrap(),
                            Atom(a) => Type::Atom(a),
                        };
                        todo!()

                        // record.borrow().get(i.key)
                    }
                    RecordSet(i) => {
                        let rec = *state.get(&i.record).unwrap();
                        let mut rec = rec.unwrap_record().borrow_mut();

                        use jssat_ir::isa::RecordKey::*;
                        let key = match i.key {
                            DynAtom(r) | Prop(r) => *state.get(&r).unwrap(),
                            Atom(a) => Type::Atom(a),
                        };

                        match i.value {
                            Some(v) => {
                                let value = *state.get(&v).unwrap();
                                rec.insert(key, value);
                            }
                            None => {
                                rec.remove(&key);
                            }
                        };
                    }
                    RecordHasKey(i) => {
                        let rec = *state.get(&i.record).unwrap();
                        let rec = rec.unwrap_record().borrow();

                        use jssat_ir::isa::RecordKey::*;
                        let key = match i.key {
                            DynAtom(r) | Prop(r) => *state.get(&r).unwrap(),
                            Atom(a) => Type::Atom(a),
                        };

                        let has_key = rec.contains_key(&key);
                        state.insert(i.result, Type::Bool(has_key));
                    },
                    NewList(i) => {
                        let unique_id = self.list_id.next();
                        let list = List::new(unique_id);
                        state.insert(i.result, state.make_type_list(list));
                    }
                    ListGet(_) => todo!(),
                    ListSet(i) => {
                        let list = state.get(&i.list).unwrap();
                        let mut list = list.unwrap_list().borrow_mut();

                        let elem = match i.key {
                            jssat_ir::isa::ListKey::Index(reg) => state.get(&reg).unwrap(),
                        };

                        match elem {
                            Type::Number => todo!("change list type to more general"),
                            Type::Int(i) => {
                                if *i < 0 {
                                    panic!("invalid program")
                                }

                                let i = *i as usize;
                                if i > list.len() {
                                    panic!("invalid program")
                                }

                                if i == list.len() {
                                    list.push(*elem);
                                } else {
                                    list[i] = *elem;
                                }
                            }
                            Type::Union(_) => todo!("need to recursively re-apply the list set operator for every union variant"),
                            Type::Float(_) => todo!("debating whether or not to implement this"),
                            _ => panic!("not well formed jssat ir"),
                        };
                    }
                    ListHasKey(_) => todo!(),
                    ListLen(i) => {
                        let list = state.get(&i.list).unwrap();
                        let list = list.unwrap_list().borrow();
                        state.insert(i.result, Type::Int(list.len() as i64));
                    }
                    GetFnPtr(i) => {
                        state.insert(i.result, Type::FnPtr(i.item));
                    }
                    CallStatic(i) => {
                        self.call_fn(&mut state, i.result, i.calling, &i.args);
                    }
                    CallExtern(_) => todo!(),
                    CallVirt(_) => todo!(),
                    MakeAtom(i) => {
                        state.insert(i.result, Type::Atom(i.item));
                    }
                    MakeBytes(i) => {
                        let bytes = self.program.constants.get(&i.item).unwrap();
                        state.insert(i.result, state.make_type_byts(&bytes.payload));
                    }
                    MakeInteger(i) => {
                        state.insert(i.result, Type::Int(i.item));
                    }
                    MakeBoolean(i) => {
                        state.insert(i.result, Type::Bool(i.item));
                    },
                    BinOp(i) => {
                        let lhs = *state.get(&i.lhs).unwrap();
                        let rhs = *state.get(&i.rhs).unwrap();

                        use jssat_ir::isa::BinaryOperator::*;
                        use Type::*;
                        let res_typ = match i.op {
                            Add => todo!(),
                            And => {
                                match (lhs, rhs) {
                                    (Boolean, Boolean) |
                                    (Boolean, Bool(_)) |
                                    (Bool(_), Boolean) => Boolean,
                                    (Bool(a), Bool(b)) => Bool(a && b),
                                    // TODO(specification): bitwise ops here?
                                    _ => panic!("invalid program"),
                                }
                            },
                            Or => todo!(),
                            Equals => {
                                match (lhs, rhs) {
                                    (Atom(a), Atom(b)) => Bool(a == b),
                                    (Atom(_), Record(_)) => Bool(false),
                                    (Record(_), Atom(_)) => Bool(false),
                                    (Record(a), Record(b)) => {
                                        todo!("uhm")
                                    }
                                    (a, b) => panic!("idk: {:?} vs {:?}", a, b)
                                }
                            },
                            LessThan => {
                                match (lhs, rhs) {
                                    (Number, Number) => Boolean,
                                    (Int(_), Number) => Boolean,
                                    (Number, Int(_)) => Boolean,
                                    (Int(a), Int(b)) => Bool(a < b),
                                    _ => panic!("invalid program"),
                                }
                            },
                        };

                        state.insert(i.result, res_typ);
                    },
                    Negate(i) => {
                        let operand = *state.get(&i.operand).unwrap();

                        use Type::*;
                        let res_typ = match operand {
                            Boolean => Boolean,
                            Bool(x) => Bool(!x),
                            Number => Number,
                            Int(x) => Int(-x),
                            _ => panic!("invalid program"),
                        };

                        state.insert(i.result, res_typ);
                    },
                    Generalize(_) => todo!(),
                    Assert(i) => {
                        let cond = state.get(&i.condition).unwrap();

                        match cond {
                            Type::Bool(true) => {},
                            Type::Bool(false) => {
                                panic!("assertion failed: {}", i.message);
                            },
                            Type::Boolean => {
                                println!("warning: assertion hit `Boolean`: {}", i.message);
                            },
                            _ => panic!("assertion passed non-bool: {:?} - {}", cond, i.message),
                        };
                    },
                    IsType(i) => {
                        use jssat_ir::isa::ValueType;
                        use jssat_ir::isa::CompareType::*;
                        let v = match i.kind {
                            Register(r) => {
                                let typ =  *state.get(&r).unwrap();
                                match typ {
                                    // TODO: handle BigNumber and Runtime
                                    Type::Any => todo!(),
                                    Type::Nothing => unimplemented!(),
                                    Type::Bytes => ValueType::Bytes,
                                    Type::Number => ValueType::Number,
                                    Type::Boolean => ValueType::Boolean,
                                    Type::Atom(_) => ValueType::Atom,
                                    Type::Int(_) => ValueType::Number,
                                    Type::Float(_) => ValueType::Number,
                                    Type::Bool(_) => ValueType::Boolean,
                                    Type::FnPtr(_) => ValueType::FnPtr,
                                    Type::Byts(_) => ValueType::Bytes,
                                    Type::List(_) => ValueType::List,
                                    Type::Record(_) => ValueType::Record,
                                    Type::Union(_) => todo!(),
                                }
                            },
                            Kind(v) => v,
                        };

                        let reg_typ = *state.get(&i.value).unwrap();
                        let is_eq = match (reg_typ, v) {
                            (Type::Nothing, _) => panic!("huh"),
                            (Type::Number, ValueType::BigNumber) => todo!("idk"),
                            (Type::Int(_), ValueType::BigNumber) => todo!("idk"),
                            (Type::Float(_), ValueType::BigNumber) => todo!("idk"),
                            (Type::Any, _) => None,
                            (Type::Bytes, ValueType::Bytes) |
                            (Type::Number, ValueType::Number) |
                            (Type::Boolean, ValueType::Boolean) |
                            (Type::Atom(_), ValueType::Atom) |
                            (Type::Int(_), ValueType::Number) |
                            (Type::Float(_), ValueType::Number) |
                            (Type::Bool(_), ValueType::Boolean) |
                            (Type::FnPtr(_), ValueType::FnPtr) |
                            (Type::Byts(_), ValueType::Bytes) |
                            (Type::List(_), ValueType::List) |
                            (Type::Record(_), ValueType::Record) => Some(true),
                            (Type::Union(_), _) => todo!(),
                            _ => Some(false),
                        };

                        let matches = match is_eq {
                            None => Type::Boolean,
                            Some(x) => Type::Bool(x),
                        };

                        state.insert(i.result, matches);
                    },
                    GetRuntime(_) => todo!(),
                    Unreachable(_) => todo!(),
                }
            }
        });

        // 4. completion: unwind, store results
        use jssat_ir::lifted::EndInstruction::*;

        // initialize the type context with the return value in `None`
        let mut ret_ctx = current_state.borrow_mut(|mut state| match &code.end {
            Jump(i) => {
                let result = self.do_blk_jmp(&mut state, &i.0);
                result
            }
            JumpIf(i) => {
                let cond = *state.get(&i.condition).unwrap();

                use Type::*;
                match cond {
                    Boolean => todo!(),
                    Bool(branch) => {
                        let branch = match branch {
                            true => &i.if_so,
                            false => &i.other,
                        };

                        let result = self.do_blk_jmp(&mut state, branch);
                        result
                    }
                    _ => panic!("invalid program"),
                }
            }
            Return(i) => match i.0 {
                Some(ret_reg) => {
                    let typ = state.get(&ret_reg).unwrap();
                    TypeCtx::new_initial(None, *typ)
                }
                None => TypeCtx::new(),
            },
        });

        // unwind, we are done evaluating 100%
        self.recursion_detector.exit_fn(function);

        // copy the state of all the parameters into the return result too
        current_state.borrow(|state| {
            ret_ctx.copy_from_map(&state, code.parameters.iter().cloned(), Some);
        });

        let result = EvalResult::new_ret(ret_ctx);

        let (_, state) = &mut self.function_cache[idx];
        *state = EvaluationState::Completed(result.clone());
        result
    }

    fn do_blk_jmp(
        &mut self,
        state: &mut TypeCtxMut<LiftedCtx, RegisterId<LiftedCtx>>,
        jmp: &BlockJump<FunctionId, LiftedCtx>,
    ) -> TypeCtx<LiftedCtx, Option<RegisterId<LiftedCtx>>> {
        // TODO: when taking two block jumps in parallel, we'll want better control
        //   over the values copied back into registers, as we'll need to make unions
        let result = self.call_fn(state, None, jmp.0, &jmp.1);

        let ret_typ = result.ctx.borrow(|ctx| match ctx.get(&None) {
            Some(t) => TypeCtx::new_initial(None, *t),
            _ => TypeCtx::new(),
        });

        ret_typ
    }

    fn call_fn(
        &mut self,
        state: &mut TypeCtxMut<LiftedCtx, RegisterId<LiftedCtx>>,
        reg_result: Option<RegisterId<LiftedCtx>>,
        function: FunctionId,
        calling_args: &[RegisterId<LiftedCtx>],
    ) -> Arc<EvalResultInner> {
        // 1. prepare for calling function
        let mut args = TypeCtx::new();

        let target_fn_regs = &self.program.functions.get(&function).unwrap().parameters;

        debug_assert_eq!(calling_args.len(), target_fn_regs.len());

        let mut target_fn_args_iter = target_fn_regs.iter().cloned();

        let src_regs = calling_args.iter().copied();
        let map_reg = |_| target_fn_args_iter.next().unwrap();
        args.copy_from_map(state, src_regs, map_reg);

        // 2. call function
        let result = self.call(function, args);

        let result = match result {
            EvalResult::Never => {
                todo!("TODO: bail out of function without dying");
            }
            EvalResult::Present(result) => result,
        };

        // 3. copy state back
        let dest_arg_to_src = (target_fn_regs.iter().cloned())
            .strict_zip(calling_args.iter().cloned())
            .collect::<FxHashMap<_, _>>();

        let (ret_typ, new_typs) = result.ctx.borrow(|result| {
            let mut dup = TypeDuplication::new(state);

            let ret_typ = result.get(&None).map(|t| dup.duplicate_type(*t));

            let new_typs = result
                .iter()
                .filter_map(|(k, v)| k.map(|reg| (reg, *v)))
                .map(|(k, typ)| {
                    let src_reg = *dest_arg_to_src.get(&k).unwrap();
                    let typ = dup.duplicate_type(typ);
                    (src_reg, typ)
                })
                .collect::<Vec<_>>();

            (ret_typ, new_typs)
        });

        for (reg, typ) in new_typs {
            state.overwrite(reg, typ);
        }

        match (reg_result, ret_typ) {
            (Some(reg), Some(typ)) => state.insert(reg, typ),
            (Some(_), None) => panic!("invalid program"),
            (None, _) => {}
        };

        result
    }
}

fn duplicate_registers(current_state: &mut TypeCtx, args: &TypeCtx) {
    current_state.borrow_mut(|mut current_state| {
        args.borrow(|args| {
            let mut dup = TypeDuplication::new(&mut current_state);

            let pairs = (args.iter())
                .map(|(k, v)| (*k, dup.duplicate_type(*v)))
                .collect::<Vec<_>>();

            for (k, v) in pairs {
                current_state.insert(k, v);
            }
        });
    });
}

// --- FUNCTION INVOCATION CACHE ---

#[derive(Default)]
struct FunctionCache {
    invocations: FxHashMap<FunctionId, Vec<(TypeCtx, EvaluationState)>>,
}

#[derive(Clone, Copy)]
struct InvocationIndex(FunctionId, usize);

#[derive(Clone)]
pub enum EvalResult {
    Never,
    Present(Arc<EvalResultInner>),
}

pub struct EvalResultInner {
    ctx: TypeCtx<LiftedCtx, Option<RegisterId<LiftedCtx>>>,
}

impl EvalResult {
    pub fn new_ret(ctx: TypeCtx<LiftedCtx, Option<RegisterId<LiftedCtx>>>) -> Self {
        Self::Present(Arc::new(EvalResultInner { ctx }))
    }

    pub fn new_never() -> Self {
        Self::Never
    }

    // if this was created with `new_never`, `is_partial` returns true. otherwise it
    // returns false
    pub fn is_partial(&self) -> bool {
        matches!(self, Self::Never)
    }

    pub fn return_type(&self) -> TypeCtx<crate::id::LiftedCtx, ()> {
        todo!()
    }
}

enum EvaluationState {
    NeverExecuted,
    InProgress,
    PartiallyCompleted(EvalResult),
    Completed(EvalResult),
}

impl FunctionCache {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn state(&mut self, function: FunctionId, args: TypeCtx) -> InvocationIndex {
        let mut fn_invocations = self.invocations.entry(function).or_default();

        let mut index = None;

        args.borrow(|args| {
            for (idx, (other_key, _)) in fn_invocations.iter_mut().enumerate() {
                if other_key.borrow(|other_key| other_key == args) {
                    index = Some(idx);
                    break;
                }
            }
        });

        let index = match index {
            Some(i) => i,
            None => {
                let index = fn_invocations.len();
                fn_invocations.push((args, EvaluationState::NeverExecuted));
                index
            }
        };

        InvocationIndex(function, index)
    }
}

impl Index<InvocationIndex> for FunctionCache {
    type Output = (TypeCtx, EvaluationState);

    fn index(&self, index: InvocationIndex) -> &Self::Output {
        &self.invocations[&index.0][index.1]
    }
}

impl IndexMut<InvocationIndex> for FunctionCache {
    fn index_mut(&mut self, index: InvocationIndex) -> &mut Self::Output {
        &mut self.invocations.get_mut(&index.0).unwrap()[index.1]
    }
}

// --- RECURSION DETECTION LOGIC ---

/// The number of times for a function to be called within itself to be
/// considered recursion. This will need some fine tuning as JSSAT grows and
/// experiences a wider variety of programs.
const ARBITRARY_RECURSION_THRESHOLD: usize = 10;

#[derive(Clone, Default)]
struct RecursionDetector {
    calls: FxHashMap<FunctionId, usize>,
}

impl RecursionDetector {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enter_fn(&mut self, function: FunctionId) -> bool {
        let value = self.calls.entry(function).or_insert(0);
        *value += 1;
        *value >= ARBITRARY_RECURSION_THRESHOLD
    }

    fn exit_fn(&mut self, function: FunctionId) {
        let value = self.calls.get_mut(&function).unwrap();
        *value -= 1;
    }
}

// --- child worker ---
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum TotalKey<T: Tag> {
    Record(UniqueRecordId<T>),
    Union(UnionId<T>),
}

type TotalTypeCtx<T = LiftedCtx> = TypeCtx<T, TotalKey<T>>;

struct TotalType {
    ctx: TotalTypeCtx,
}

// fn construct_total(results: &[Results]) -> TotalType {
//     let mut total = TotalTypeCtx::new();
//     total.borrow_mut(|mut total| {
//         for result in results {
//             for ctx in &result.type_info {
//                 ctx.borrow(|ctx| {
//                     let mut dup = TypeDuplication::new(&mut total);
//                     let mut records = Vec::new();

//                     for (k, v) in ctx.iter() {
//                         let total_v = dup.duplicate_type(*v);

//                         if let Type::Record(handle) = total_v {
//                             let unique_id = handle.borrow().unique_id();
//                             records.push((total_v, unique_id));
//                         }
//                     }

//                     for (rec_typ, unique_id) in records {
//                         let record =
// total.get(&TotalKey::Record(unique_id)).unwrap();                         let
// mut union = record.unwrap_union().borrow_mut();
// union.push(rec_typ);                     }
//                 });
//             }
//         }
//     });

//     TotalType { ctx: total }
// }
