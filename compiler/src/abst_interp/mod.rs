//! The abstract interpretation module in JSSAT.

use std::ops::{Index, IndexMut};
use std::sync::Arc;

use domino::moment::MomentApi;
use jssat_ir::collections::StrictZip;
use jssat_ir::id::{Counter, LiftedCtx, RegisterId, Tag, UnionId, UniqueListId, UniqueRecordId};
use jssat_ir::isa::BlockJump;
use jssat_ir::value_snapshot::{ValueSnapshotArena, SnapshotValue, SnapshotList};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::lifted::{FunctionId, LiftedProgram};
use crate::types::{List, Record, Type, TypeCtx, TypeCtxMut, TypeDuplication, TypeCtxImmut};

/// Collects statistics during abstract interpretation.
pub trait AbsIntCollector<T: Tag> {
    fn set_inst_position(&mut self, index: usize);
    fn set_inst_position_end(&mut self);

    fn record(&mut self, register: RegisterId<T>, typ: Type<T>);
    fn commit_changes(&mut self);

    fn fn_start(&mut self, function: FunctionId, kind: EvaluationStateKind);
    fn fn_end(&mut self);
}

/// Collector that implements [`AbsIntCollector`] but does not collect any
/// information. This is useful when wanting to run the abstract interpreter at
/// maximum speed, as it does not have to do any processing when collecting
/// information.
pub struct NilCollector;

impl<T: Tag> AbsIntCollector<T> for NilCollector {
    fn set_inst_position(&mut self, _index: usize) {}
    fn set_inst_position_end(&mut self) {}

    fn record(&mut self, _register: RegisterId<T>, _typ: Type<T>) {}
    fn commit_changes(&mut self) {}

    fn fn_start(&mut self, _function: FunctionId, _kind: EvaluationStateKind) {}
    fn fn_end(&mut self) {}
}

pub struct MomentCollector<'code, T: Tag> {
    code: &'code LiftedProgram,
    pub moment: MomentApi,
    pos: Vec<Option<usize>>,
    ty_ctx: TypeCtx<T>,
    function: Vec<FunctionId>,
    seen: FxHashSet<RegisterId<LiftedCtx>>
}

impl<'code, T: Tag> MomentCollector<'code, T> {
    pub fn new(code: &'code LiftedProgram) -> Self {
        Self {
            code,
            pos: Vec::new(),
            moment: MomentApi::new(code),
            ty_ctx: TypeCtx::new(),
            function: Vec::new(),
            seen: FxHashSet::default(),
        }
    }
}

impl<'code> AbsIntCollector<LiftedCtx> for MomentCollector<'code, LiftedCtx> {
    fn set_inst_position(&mut self, index: usize) {
        *self.pos.last_mut().unwrap() = Some(index);
    }

    fn set_inst_position_end(&mut self) {
        let last = self.pos.last_mut().unwrap();
        *last = match *last {
            Some(n) => Some(n + 1),
            None => Some(0),
        };
    }

    fn record(&mut self, register: RegisterId<LiftedCtx>, typ: Type<'_, LiftedCtx>) {
        if !self.seen.insert(register) {
            return;
        }

        self.ty_ctx.borrow_mut(|mut ctx| {
            let typ = ctx.duplicate_type(typ);
            ctx.insert(register, typ);
        });
    }

    #[track_caller]
    fn commit_changes(&mut self) {
        let arena = self.ty_ctx.borrow(|ctx| {
            let mut visitor = SnapshotVisitor::new();

            for (k, v) in ctx.iter() {
                visitor.primary = Some(*k);
                visitor.visit(*v);
            }

            let arena = visitor.arena;

            if arena.registers.is_empty() {
                // eprintln!("warning: not comitting empty arena (TODO: fix this?)");
                // return Some(arena);
                return None;
            }

            Some(arena)
        });

        if let Some(arena) = arena {
            let source_map_idx = try {
                let fn_id = self.function.last()?;
                let function = self.code.functions.get(fn_id)?;
                let pos = *self.pos.last()?;
                let pos = pos?;
                let inst = function.instructions.get(pos)?;
                inst.source_map_idx?
            };

            self.moment.snapshot(self.pos.last().unwrap().unwrap(), source_map_idx, arena);
            self.ty_ctx = TypeCtx::new();
            self.seen = FxHashSet::default();
        }
    }

    fn fn_start(&mut self, function: FunctionId, kind: EvaluationStateKind) {
        self.moment.enter(function);
        self.function.push(function);
        self.pos.push(None);
    }

    fn fn_end(&mut self) {
        self.moment.exit();
        self.function.pop();
        self.pos.pop();
    }
}

struct SnapshotVisitor<T, P> {
    arena: ValueSnapshotArena,
    seen: FxHashMap<T, SnapshotValue>,
    primary: Option<P>,
    rec_id: usize,
    list_id: usize,
}

impl<T, P> SnapshotVisitor<T, P> {
    pub fn new() -> Self {
        Self {
            arena: Default::default(),
            seen: Default::default(),
            primary: None,
            rec_id: 0,
            list_id: 0,
        }
    }
}

impl<'ctx, T: Tag> SnapshotVisitor<Type<'ctx, T>, RegisterId<LiftedCtx>> {
    fn visit(&mut self, typ: Type<'ctx, T>) -> SnapshotValue {
        let primary = self.primary;
        self.primary = None;

        let snapshot_value = match typ {
            Type::Any => todo!(),
            Type::Nothing => todo!(),
            Type::Bytes => todo!(),
            Type::Number => todo!(),
            Type::Boolean => todo!(),
            Type::Atom(x) => SnapshotValue::Atom(x),
            Type::Int(x) => SnapshotValue::Number(x),
            Type::Float(x) => todo!(),
            Type::Bool(x) => SnapshotValue::Boolean(x),
            Type::FnPtr(x) => SnapshotValue::FnPtr(x),
            Type::Byts(x) => SnapshotValue::Bytes(x.to_vec()),
            Type::List(x) => {
                if let Some(seen) = self.seen.get(&typ) {
                    if let Some(reg) = primary {
                        self.arena.registers.insert(reg, seen.clone());
                    }

                    return seen.clone();
                }

                let id = self.list_id;
                self.list_id += 1;

                self.seen.insert(typ, SnapshotValue::List(id));

                let snapshot_types = x.borrow().iter()
                    .map(|typ| self.visit(*typ))
                    .collect::<Vec<_>>();

                self.arena.lists.insert(id, SnapshotList(snapshot_types));
                SnapshotValue::List(id)
            }
            Type::Record(x) => {
                if let Some(seen) = self.seen.get(&typ) {
                    if let Some(reg) = primary {
                        self.arena.registers.insert(reg, seen.clone());
                    }

                    return seen.clone();
                }

                let id = self.rec_id;
                self.rec_id += 1;

                self.seen.insert(typ, SnapshotValue::Record(id));

                let record = x.borrow().iter()
                    .map(|(key_typ, value_typ)| (self.visit(*key_typ), self.visit(*value_typ)))
                    .collect::<FxHashMap<_, _>>();

                self.arena.records.insert(id, jssat_ir::value_snapshot::SnapshotRecord(record));
                SnapshotValue::Record(id)
            }
            Type::Union(x) => {
                todo!()
            }
        };

        if let Some(reg) = primary {
            self.arena.registers.insert(reg, snapshot_value.clone());
        }

        self.seen.insert(typ, snapshot_value.clone());
        snapshot_value
    }
}

pub struct AbsIntEngine<'program, C> {
    program: &'program LiftedProgram,
    record_id: Counter<UniqueRecordId<LiftedCtx>>,
    list_id: Counter<UniqueListId<LiftedCtx>>,
    recursion_detector: RecursionDetector,
    function_cache: FunctionCache,
    pub collector: C,
}

impl<'p> AbsIntEngine<'p, NilCollector> {
    pub fn new(program: &'p LiftedProgram) -> Self {
        Self::new_with_collector(program, NilCollector)
    }
}

trait TypeCtxImmutExt<'ctx, T: Tag> {
    fn rget(&self, reg: RegisterId<T>) -> Result<Type<'ctx, T>, AbsIntError>;
}

impl<'borrow, 'ctx, T: Tag> TypeCtxImmutExt<'ctx, T> for TypeCtxImmut<'borrow, 'ctx, T, RegisterId<T>> {
    fn rget(&self, reg: RegisterId<T>) -> Result<Type<'ctx, T>, AbsIntError> {
        self.get(&reg).ok_or(AbsIntError::NotInSSA)
    }
}

use thiserror::Error;

#[derive(Error, Debug)]
pub enum AbsIntError {
    #[error("Rejected program: not in SSA form")]
    NotInSSA,
    #[error("Invalid program: type error")]
    TypeError,
    #[error("Invalid program: key of record does not exist")]
    RegKeyDNE,
}

impl<'p, C: AbsIntCollector<LiftedCtx>> AbsIntEngine<'p, C> {
    pub fn new_with_collector(program: &'p LiftedProgram, collector: C) -> Self {
        Self {
            program,
            record_id: Default::default(),
            list_id: Default::default(),
            recursion_detector: Default::default(),
            function_cache: Default::default(),
            collector,
        }
    }

    pub fn call(&mut self, function: FunctionId, args: TypeCtx) -> Result<EvalResult, AbsIntError> {
        // 1. check previous execution results
        let idx = self.function_cache.state(function, args);
        let (args, state) = &mut self.function_cache[idx];

        // self.collector.fn_start(function, EvaluationStateKind::from(&*state));
        match state {
            EvaluationState::Completed(result) => return Ok(result.clone()),
            EvaluationState::PartiallyCompleted(result) => return Ok(result.clone()),
            EvaluationState::InProgress => return Ok(EvalResult::new_never()),
            EvaluationState::NeverExecuted => {}
        };
        self.collector.fn_start(function, EvaluationStateKind::from(&*state));

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

        let closure_result: Result<(), AbsIntError> = current_state.borrow_mut(|mut state| {
            try {
            for (idx, instruction) in code.instructions.iter().enumerate() {
                self.collector.set_inst_position(idx);

                use jssat_ir::frontend::ir::InstructionData::*;

                macro_rules! insert {
                    ($state:expr, $key:expr, $value:expr) => {
                        {
                            let key = $key;
                            let value = $value;
                            $state.insert(key, value);
                            self.collector.record(key, value);
                        }
                    }
                }

                // pre
                for register in instruction.data.used_registers() {
                    let typ = state.rget(register)?;
                    self.collector.record(register, typ);
                }
                self.collector.commit_changes();
                
                match &instruction.data {
                    Comment(_) => {}
                    NewRecord(i) => {
                        let unique_id = self.record_id.next();
                        let record = Record::new(unique_id);
                        insert!(state, i.result, state.make_type_record(record));
                    }
                    RecordGet(i) => {
                        let record_typ = state.rget(i.record)?;
                        let record = record_typ.try_into_record().ok_or(AbsIntError::TypeError)?;

                        use jssat_ir::isa::RecordKey::*;
                        let key = match i.key {
                            DynAtom(r) | Prop(r) => state.rget(r)?,
                            Atom(a) => Type::Atom(a),
                        };

                        let record = record.borrow();
                        let k = record.get(&key).ok_or(AbsIntError::RegKeyDNE)?;
                        insert!(state, i.result, *k);
                    }
                    RecordSet(i) => {
                        let rec_typ = state.rget(i.record)?;
                        let mut rec = rec_typ.try_into_record().ok_or(AbsIntError::TypeError)?.borrow_mut();

                        use jssat_ir::isa::RecordKey::*;
                        let key = match i.key {
                            DynAtom(r) | Prop(r) => state.rget(r)?,
                            Atom(a) => Type::Atom(a),
                        };

                        match i.value {
                            Some(v) => {
                                let value = state.rget(v)?;
                                rec.insert(key, value);
                            }
                            None => {
                                rec.remove(&key);
                            }
                        };

                        // drop(rec);
                        // self.collector.record(i.record, rec_typ);
                    }
                    RecordHasKey(i) => {
                        let rec = state.rget(i.record)?;
                        let rec = rec.try_into_record().ok_or(AbsIntError::TypeError)?.borrow();

                        use jssat_ir::isa::RecordKey::*;
                        let key = match i.key {
                            DynAtom(r) | Prop(r) => state.rget(r)?,
                            Atom(a) => Type::Atom(a),
                        };

                        let has_key = rec.contains_key(&key);
                        insert!(state, i.result, Type::Bool(has_key));
                    },
                    NewList(i) => {
                        let unique_id = self.list_id.next();
                        let list = List::new(unique_id);
                        insert!(state, i.result, state.make_type_list(list));
                    }
                    ListGet(i) => {
                        let list = state.get(&i.list).unwrap();
                        let list = list.unwrap_list().borrow();

                        let elem = match i.key {
                            jssat_ir::isa::ListKey::Index(reg) => state.get(&reg).unwrap(),
                        };

                        let typ = match elem {
                            Type::Number => todo!("change list type to more general"),
                            Type::Int(i) => {
                                if i < 0 {
                                    panic!("invalid program")
                                }

                                let i = i as usize;
                                if i > list.len() {
                                    panic!("invalid program")
                                }

                                list.get(i).unwrap()
                            }
                            Type::Union(_) => todo!("need to recursively re-apply the list set operator for every union variant"),
                            Type::Float(_) => todo!("debating whether or not to implement this"),
                            _ => panic!("not well formed jssat ir"),
                        };

                        insert!(state, i.result, *typ);
                    },
                    ListSet(i) => {
                        let list_typ = state.rget(i.list)?;
                        let mut list = list_typ.unwrap_list().borrow_mut();

                        let elem = match i.key {
                            jssat_ir::isa::ListKey::Index(reg) => state.get(&reg).unwrap(),
                        };

                        let value = match i.value {
                            Some(r) => Some(state.rget(r)?),
                            None => None
                        };

                        match elem {
                            Type::Number => todo!("change list type to more general"),
                            Type::Int(i) => {
                                if i < 0 {
                                    panic!("invalid program")
                                }

                                let i = i as usize;
                                if i > list.len() {
                                    panic!("invalid program")
                                }

                                match value {
                                    Some(value) => {
                                        if i == list.len() {
                                            list.push(value);
                                        } else {
                                            list[i] = value;
                                        }
                                    },
                                    None => {
                                        list.splice(i..(i + 1), []);
                                    }
                                }
                            }
                            Type::Union(_) => todo!("need to recursively re-apply the list set operator for every union variant"),
                            Type::Float(_) => todo!("debating whether or not to implement this"),
                            _ => panic!("not well formed jssat ir"),
                        };
                        
                        drop(list);
                        self.collector.record(i.list, list_typ);
                    }
                    ListHasKey(_) => todo!(),
                    ListLen(i) => {
                        let list = state.get(&i.list).unwrap();
                        let list = list.unwrap_list().borrow();
                        insert!(state, i.result, Type::Int(list.len() as i64));
                    }
                    GetFnPtr(i) => {
                        insert!(state, i.result, Type::FnPtr(i.item));
                    }
                    CallStatic(i) => {
                        self.call_fn(&mut state, i.result, i.calling, &i.args)?;
                    }
                    CallExtern(_) => todo!(),
                    CallVirt(i) => {
                        let fnptr = state.get(&i.calling).unwrap();
                        let fnptr = fnptr.unwrap_fnptr();

                        self.call_fn(&mut state, i.result, fnptr, &i.args)?;
                    },
                    MakeAtom(i) => {
                        insert!(state, i.result, Type::Atom(i.item));
                    }
                    MakeBytes(i) => {
                        let bytes = self.program.constants.get(&i.item).unwrap();
                        insert!(state, i.result, state.make_type_byts(&bytes.payload));
                    }
                    MakeInteger(i) => {
                        insert!(state, i.result, Type::Int(i.item));
                    }
                    MakeBoolean(i) => {
                        insert!(state, i.result, Type::Bool(i.item));
                    },
                    BinOp(i) => {
                        let lhs = state.rget(i.lhs)?;
                        let rhs = state.rget(i.rhs)?;

                        use jssat_ir::isa::BinaryOperator::*;
                        use Type::*;
                        let res_typ = match i.op {
                            Add => {
                                match (lhs, rhs) {
                                    (Int(a), Int(b)) => Int(a + b),
                                    (Int(_), Number) |
                                    (Number, Int(_)) |
                                    (Number, Number) => Number,
                                    _ => todo!("2op add: {:?}, {:?}", lhs, rhs),
                                }
                            },
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
                            Or => {
                                match (lhs, rhs) {
                                    (Bool(a), Bool(b)) => Bool(a || b),
                                    (Bool(_), Boolean) |
                                    (Boolean, Bool(_)) |
                                    (Boolean, Boolean) => Boolean,
                                    (a, b) => panic!("2op or: {:?} vs {:?}", a, b),
                                }
                            },
                            Equals => {
                                match (lhs, rhs) {
                                    (Atom(a), Atom(b)) => Bool(a == b),
                                    (Atom(_), Record(_)) => Bool(false),
                                    (Record(_), Atom(_)) => Bool(false),
                                    (Record(a), Record(b)) => {
                                        todo!("uhm")
                                    }
                                    (Bool(a), Bool(b)) => Bool(a == b),
                                    (Bool(_), Boolean) |
                                    (Boolean, Bool(_)) |
                                    (Boolean, Boolean) => Boolean,
                                    (a, b) => panic!("2op eq: {:?} vs {:?}", a, b)
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

                        insert!(state, i.result, res_typ);
                    },
                    Negate(i) => {
                        let operand = state.rget(i.operand)?;

                        use Type::*;
                        let res_typ = match operand {
                            Boolean => Boolean,
                            Bool(x) => Bool(!x),
                            Number => Number,
                            Int(x) => Int(-x),
                            _ => panic!("invalid program"),
                        };

                        insert!(state, i.result, res_typ);
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
                                let typ =  state.rget(r)?;
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

                        let reg_typ = state.rget(i.value)?;
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

                        insert!(state, i.result, matches);
                    },
                    GetRuntime(_) => todo!(),
                    Unreachable(_) => todo!(),
                };

                // post
                for register in instruction.data.used_registers() {
                    let typ = state.rget(register)?;
                    self.collector.record(register, typ);
                }
                
                if let Some(result) = instruction.data.assigned_to() {
                    self.collector.record(result, state.rget(result)?);
                }

                self.collector.commit_changes();
            }
            }
        });
        closure_result?;

        // 4. completion: unwind, store results
        use jssat_ir::lifted::EndInstruction::*;

        // initialize the type context with the return value in `None`
        self.collector.set_inst_position_end();
        let ret_ctx: Result<TypeCtx<LiftedCtx, Option<RegisterId<LiftedCtx>>>, AbsIntError> = current_state.borrow_mut(|mut state| {
            try {
                match &code.end {
                    Jump(i) => {
                        let result = self.do_blk_jmp(&mut state, &i.0)?;
                        result
                    }
                    JumpIf(i) => {
                        let cond = state.rget(i.condition)?;

                    use Type::*;
                        match cond {
                            Boolean => todo!(),
                            Bool(branch) => {
                                let branch = match branch {
                                    true => &i.if_so,
                                    false => &i.other,
                                };

                                let result = self.do_blk_jmp(&mut state, branch)?;
                                result
                            }
                            _ => panic!("invalid program"),
                        }
                    }
                    Return(i) => match i.0 {
                        Some(ret_reg) => {
                            let typ = state.rget(ret_reg)?;
                            TypeCtx::new_initial(None, typ)
                        }
                        None => TypeCtx::new(),
                    },
                }
            }
        });

        let mut ret_ctx: TypeCtx<LiftedCtx, Option<RegisterId<LiftedCtx>>> = ret_ctx?;

        // unwind, we are done evaluating 100%
        self.collector.fn_end();
        self.recursion_detector.exit_fn(function);

        // copy the state of all the parameters into the return result too
        current_state.borrow(|state| {
            ret_ctx.copy_from_map(&state, code.parameters.iter().cloned(), Some);
        });

        let result = EvalResult::new_ret(ret_ctx);

        let (_, state) = &mut self.function_cache[idx];
        *state = EvaluationState::Completed(result.clone());
        Ok(result)
    }

    fn do_blk_jmp(
        &mut self,
        state: &mut TypeCtxMut<LiftedCtx, RegisterId<LiftedCtx>>,
        jmp: &BlockJump<FunctionId, LiftedCtx>,
    ) -> Result<TypeCtx<LiftedCtx, Option<RegisterId<LiftedCtx>>>, AbsIntError> {
        // TODO: when taking two block jumps in parallel, we'll want better control
        //   over the values copied back into registers, as we'll need to make unions
        let result = self.call_fn(state, None, jmp.0, &jmp.1)?;

        let ret_typ = result.ctx.borrow(|ctx| match ctx.get(&None) {
            Some(t) => TypeCtx::new_initial(None, t),
            _ => TypeCtx::new(),
        });

        Ok(ret_typ)
    }

    fn call_fn(
        &mut self,
        state: &mut TypeCtxMut<LiftedCtx, RegisterId<LiftedCtx>>,
        reg_result: Option<RegisterId<LiftedCtx>>,
        function: FunctionId,
        calling_args: &[RegisterId<LiftedCtx>],
    ) -> Result<Arc<EvalResultInner>, AbsIntError> {
        // 1. prepare for calling function
        let mut args = TypeCtx::new();

        let target_fn_regs = &self.program.functions.get(&function).unwrap().parameters;

        debug_assert_eq!(calling_args.len(), target_fn_regs.len());

        let mut target_fn_args_iter = target_fn_regs.iter().cloned();

        let src_regs = calling_args.iter().copied();
        let map_reg = |_| target_fn_args_iter.next().unwrap();
        args.copy_from_map(state, src_regs, map_reg);

        // 2. call function
        let result = self.call(function, args)?;

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

            let ret_typ = result.get(&None).map(|t| dup.duplicate_type(t));

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
            (Some(reg), Some(typ)) => {
                state.insert(reg, typ);
            },
            (Some(_), None) => panic!("invalid program"),
            (None, _) => {}
        };

        Ok(result)
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

#[derive(enum_kinds::EnumKind)]
#[enum_kind(EvaluationStateKind)]
pub enum EvaluationState {
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
        let fn_invocations = self.invocations.entry(function).or_default();

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
