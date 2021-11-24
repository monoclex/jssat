//! The abstract interpretation module in JSSAT.

use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering};

// use petgraph::graph::DiGraph;
use thiserror::Error;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::sync::oneshot::error::RecvError;
use tokio::sync::oneshot::{channel, Sender};
use tokio::task::JoinHandle;

use crate::lifted::{Function, FunctionId, LiftedProgram, RegisterId};
use crate::types::TypeCtx;

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
