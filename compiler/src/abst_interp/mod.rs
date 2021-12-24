//! The abstract interpretation module in JSSAT.

use std::fmt::Debug;
use std::ops::{Index, IndexMut};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use jssat_ir::id::{LiftedCtx, Tag, UnionId, UniqueRecordId};
use rustc_hash::FxHashMap;
// use petgraph::graph::DiGraph;
use thiserror::Error;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::sync::oneshot::error::RecvError;
use tokio::sync::oneshot::{channel, Sender};
use tokio::task::JoinHandle;

use crate::lifted::{Function, FunctionId, LiftedProgram, RegisterId};
use crate::types::{Type, TypeCtx, TypeDuplication};

struct AbsIntEngine<'program> {
    program: &'program LiftedProgram,
    recursion_detector: RecursionDetector,
    function_cache: FunctionCache,
}

struct AbsIntEngineRaw<'engine> {
    recursion_detector: &'engine mut RecursionDetector,
    function_cache: &'engine mut FunctionCache,
}

impl<'p> AbsIntEngine<'p> {
    pub fn new(program: &'p LiftedProgram) -> Self {
        Self {
            program,
            recursion_detector: Default::default(),
            function_cache: Default::default(),
        }
    }

    pub fn call(&mut self, function: FunctionId, args: TypeCtx) -> EvalResult {
        // 1. check previous execution results
        let idx = self.function_cache.state(function, args);
        let (args, state) = &mut self.function_cache[idx];

        match state {
            EvaluationState::Completed(result) => return result.clone(),
            EvaluationState::PartiallyCompleted(result) => return result.clone(),
            EvaluationState::InProgress => return EvalResult::new_never(),
            EvaluationState::NeverExecuted => {}
        };

        // 2. if we're too recursive, generalize our arguments
        let is_recursion = self.recursion_detector.enter_fn(function);
        if is_recursion {
            todo!("generalize arguments, then continue execution")
        }

        // 3. simulate the function
        let code = self.program.functions.get(&function).unwrap();

        self.call(function, TypeCtx::new());

        self.recursion_detector.exit_fn(function);
        todo!()
    }
}

// --- FUNCTION INVOCATION CACHE ---

#[derive(Default)]
struct FunctionCache {
    invocations: FxHashMap<FunctionId, Vec<(TypeCtx, EvaluationState)>>,
}

#[derive(Clone, Copy)]
struct InvocationIndex(FunctionId, usize);

#[derive(Clone)]
struct EvalResult(Arc<()>);

impl EvalResult {
    pub fn new_never() -> Self {
        todo!()
    }

    // if this was created with `new_never`, `is_partial` returns true. otherwise it
    // returns false
    pub fn is_partial(&self) -> bool {
        todo!()
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
struct ChildWorker<'program> {
    code: &'program LiftedProgram,
}

struct Results {
    type_info: Vec<TypeCtx>,
    return_type: TypeCtx<LiftedCtx, ()>,
}

impl<'p> ChildWorker<'p> {
    async fn work<'ctx>(&self, function_id: FunctionId, type_ctx: TypeCtx) -> Results {
        todo!()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum TotalKey<T: Tag> {
    Record(UniqueRecordId<T>),
    Union(UnionId<T>),
}

type TotalTypeCtx<T = LiftedCtx> = TypeCtx<T, TotalKey<T>>;

struct TotalType {
    ctx: TotalTypeCtx,
}

fn construct_total(results: &[Results]) -> TotalType {
    let mut total = TotalTypeCtx::new();
    total.borrow_mut(|mut total| {
        for result in results {
            for ctx in &result.type_info {
                ctx.borrow(|ctx| {
                    let mut dup = TypeDuplication::new(&mut total);
                    let mut records = Vec::new();

                    for (k, v) in ctx.iter() {
                        let total_v = dup.duplicate_type(*v);

                        if let Type::Record(handle) = total_v {
                            let unique_id = handle.borrow().unique_id();
                            records.push((total_v, unique_id));
                        }
                    }

                    for (rec_typ, unique_id) in records {
                        let record = total.get(&TotalKey::Record(unique_id)).unwrap();
                        let mut union = record.unwrap_union().borrow_mut();
                        union.push(rec_typ);
                    }
                });
            }
        }
    });

    TotalType { ctx: total }
}
//

#[derive(Clone, Default)]
struct CallGraph {
    edges: Vec<CallEdge>,
}

impl CallGraph {
    pub fn add_edge(&mut self, from: Option<FunctionId>, to: FunctionId) -> usize {
        for edge in &mut self.edges {
            if edge.from == from && edge.to == to {
                edge.count += 1;
                return edge.count;
            }
        }

        self.edges.push(CallEdge { from, to, count: 1 });
        1
    }
}

#[derive(Clone)]
struct CallEdge {
    from: Option<FunctionId>,
    to: FunctionId,
    count: usize,
}

#[derive(Clone, Default)]
pub struct Callstack {
    frames: Vec<FunctionId>,
    graph: CallGraph,
}

impl Callstack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_frame(&self, function: FunctionId) -> Self {
        let mut callstack = self.clone();

        let from = self.frames.last().copied();
        let to = function;
        callstack.graph.add_edge(from, to);

        callstack
    }
}

/// Controls the execution of workers when abstractly interpreting a JSSAT IR
/// program.
pub struct Honcho {
    code: LiftedProgram,
    listen: UnboundedReceiver<ToHonchoMsg>,
    handle: UnboundedSender<ToHonchoMsg>,

    children: Vec<HonchoChild>,
    api_id: AtomicUsize,
}

struct HonchoChild {
    handle: Option<JoinHandle<()>>,
    respond: Option<Sender<EvalResp>>,
    callstack: Callstack,
}

impl Honcho {
    pub fn new(code: LiftedProgram) -> Self {
        let (handle, listen) = unbounded_channel();

        Self {
            code,
            handle,
            listen,
            children: Default::default(),
            api_id: Default::default(),
        }
    }

    pub fn start(mut self) -> (HonchoApi, JoinHandle<()>) {
        let api = self.make_api();

        self.children.push(HonchoChild {
            handle: None,
            respond: None,
            callstack: Callstack::new(),
        });

        let handle = tokio::task::spawn(self.main());

        (api, handle)
    }

    fn make_api(&self) -> HonchoApi {
        let caller_id = CallerId(self.api_id.fetch_add(1, Ordering::Relaxed));
        let outgoing = self.handle.clone();
        HonchoApi {
            outgoing,
            caller_id,
        }
    }

    async fn main(mut self) {
        while let Some(ToHonchoMsg(caller, request)) = self.listen.recv().await {
            let child = self.children.get_mut(caller.0).expect("child present");

            match request {
                Request::Close(respond) => {
                    respond.send(CloseResponse(self)).expect("to succeed");
                    return;
                }
                Request::Evaluate(respond, request) => {
                    let function = self
                        .code
                        .functions
                        .get(&request.function)
                        .expect("a function");

                    let callstack = child.callstack.add_frame(request.function);

                    let honcho_api = self.make_api();
                    debug_assert_eq!(
                        honcho_api.caller_id.0,
                        self.children.len(),
                        "next space in `children` should be this child"
                    );

                    let context = EvaluationContext {
                        honcho_api,
                        type_ctx: request.type_ctx,
                        code: function.clone(),
                    };

                    let handle = tokio::task::spawn(context.execute());

                    let child = HonchoChild {
                        handle: Some(handle),
                        respond: Some(respond),
                        callstack,
                    };

                    self.children.push(child);
                }
                Request::GenerateCode(f) => {
                    f();
                    todo!()
                }
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct CallerId(usize);

// keeping this a tuple in case we want to add sender identity
// e.g. ToHonchoMsg(Thread::Idx(69), req)
pub struct ToHonchoMsg(CallerId, Request);

enum Request {
    Close(Sender<CloseResponse>),
    Evaluate(Sender<EvalResp>, EvaluationRequest),
    GenerateCode(Box<dyn FnOnce() -> bool + Send>),
}

pub struct CloseResponse(Honcho);

impl Debug for CloseResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CloseResponse").finish()
    }
}

pub struct HonchoApi {
    outgoing: UnboundedSender<ToHonchoMsg>,
    caller_id: CallerId,
}

#[derive(Error, Debug)]
pub enum ApiError {
    #[error("Error sending a message to the `Honcho`. This shouldn't happen")]
    SendError,
    #[error("Error receiving a reply")]
    RecvError(#[from] RecvError),
}

impl HonchoApi {
    pub async fn eval(&self, request: EvaluationRequest) -> Result<EvalResp, ApiError> {
        let (req, resp) = channel();

        self.outgoing
            .send(ToHonchoMsg(self.caller_id, Request::Evaluate(req, request)))
            .map_err(|_| ApiError::SendError)?;

        Ok(resp.await?)
    }

    pub async fn close(&self) -> Result<Honcho, ApiError> {
        let (req, resp) = channel();

        self.outgoing
            .send(ToHonchoMsg(self.caller_id, Request::Close(req)))
            .map_err(|_| ApiError::SendError)?;

        let CloseResponse(honcho) = resp.await?;

        Ok(honcho)
    }
}

pub struct EvaluationRequest {
    pub type_ctx: TypeCtx,
    pub function: FunctionId,
}

pub struct EvaluationContext {
    honcho_api: HonchoApi,
    type_ctx: TypeCtx,
    code: Function,
}

pub struct EvalResp {
    type_ctx: TypeCtx,
    returns: RegisterId,
}

impl EvaluationContext {
    pub async fn execute(self) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use tokio::runtime;

    use crate::{
        abst_interp::{EvaluationRequest, Honcho},
        frontend::builder::ProgramBuilder,
        lifted::LiftedProgram,
        types::TypeCtx,
    };

    #[test]
    fn honcho_spawns_and_closes() {
        let mut program_builder = ProgramBuilder::new();
        program_builder.create_blank_entrypoint();
        let program = program_builder.finish();
        let program = crate::lifted::lift(program);

        let runtime = runtime::Builder::new_current_thread().build().unwrap();
        runtime.block_on(work(program));

        async fn work(program: LiftedProgram) -> Honcho {
            let entry = program.entrypoint;

            let honcho = Honcho::new(program);
            let (api, handle) = honcho.start();

            let request = EvaluationRequest {
                type_ctx: TypeCtx::new(),
                function: entry,
            };

            let _evaluated = api.eval(request).await.expect("should be fine");

            let honcho = api.close().await.expect("should close");
            handle.await.expect("honcho should be done");

            honcho
        }
    }
}
