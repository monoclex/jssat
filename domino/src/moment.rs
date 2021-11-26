//! Moment API. Used to profile the interpreter for Domino the Debugger.

use std::rc::Rc;

use derive_more::{Deref, DerefMut};
use jssat_ir::lifted::{Function, FunctionId, LiftedProgram};
use rustc_hash::FxHashMap;

use crate::{server_data::FrameCode, Data, RawFrame, RawFrameCode, Snapshot};

pub struct MomentApi {
    functions: FxHashMap<FunctionId, CodeInfo>,
    snapshots: Vec<CallstackPtr>,
    callstack: MomentState,
}

impl From<MomentApi> for Data {
    fn from(api: MomentApi) -> Self {
        let functions = api
            .functions
            .into_iter()
            .map(|(k, v)| (k, { Rc::new(RawFrameCode { lines: v.lines }) }))
            .collect::<FxHashMap<_, _>>();

        let mut rc_cache = FxHashMap::default();

        Self {
            snapshots: (api.snapshots.into_iter())
                .map(|x| Snapshot {
                    code: functions.get(&x.clone().0.unwrap().0.func).unwrap().clone(),
                    frame: x.into_raw_frame(&mut rc_cache).unwrap(),
                })
                .collect(),
        }
    }
}

impl CallstackPtr {
    fn into_raw_frame(
        self,
        rc_cache: &mut FxHashMap<*const Callframe, Rc<RawFrame>>,
    ) -> Option<Rc<RawFrame>> {
        let call_frame = self.0?;
        let pointer = Rc::as_ptr(&call_frame);

        match rc_cache.get(&pointer) {
            Some(value) => Some(value.clone()),
            None => {
                let frame = Rc::new(RawFrame {
                    function: call_frame.0.func,
                    inst_idx: call_frame.0.inst_idx,
                    parent: call_frame.1.clone().into_raw_frame(rc_cache),
                });

                rc_cache.insert(pointer, frame.clone());
                Some(frame)
            }
        }
    }
}

#[derive(Clone, Default, Deref, DerefMut)]
struct MomentState(Vec<CallstackPtr>);

impl MomentState {
    fn parent(&self) -> CallstackPtr {
        match self.as_slice() {
            [] | [_] => CallstackPtr(None),
            [.., snd_last, _] => snd_last.clone(),
        }
    }

    fn current(&mut self) -> Option<&mut CallstackPtr> {
        match self.as_mut_slice() {
            [] => None,
            [.., last] => Some(last),
        }
    }
}

pub struct CodeInfo {
    lines: Vec<String>,
}

impl CodeInfo {
    pub fn new(function: &Function) -> Self {
        let mut lines = vec![];

        for line in function.instructions.iter() {
            let mut s = String::new();
            line.display(&mut s).unwrap();
            lines.push(s);
        }

        Self { lines }
    }
}

#[derive(Clone)]
pub struct FrameInfo {
    func: FunctionId,
    inst_idx: usize,
}

#[derive(Clone)]
pub struct Callframe(FrameInfo, CallstackPtr);

#[derive(Clone)]
pub struct CallstackPtr(Option<Rc<Callframe>>);

impl CallstackPtr {
    pub fn new(callstack: Callframe) -> Self {
        Self(Some(Rc::new(callstack)))
    }
}

impl MomentApi {
    pub fn new(code: &LiftedProgram) -> Self {
        MomentApi {
            functions: (code.functions.iter())
                .map(|(id, f)| (*id, CodeInfo::new(f)))
                .collect(),
            snapshots: Vec::new(),
            callstack: MomentState::default(),
        }
    }

    pub fn enter(&mut self, func: FunctionId) {
        let info = FrameInfo { func, inst_idx: 0 };

        let this_node = Callframe(info, self.callstack.parent());
        self.callstack.push(CallstackPtr::new(this_node));
    }

    pub fn snapshot(&mut self, inst_idx: usize) {
        // modify the current callframe to be set to `inst_idx`
        // this is so that if we go into a function, then all the outer callframes will
        // be set at the position of the function call
        let current = self.callstack.current().expect("must be in function");

        let callframe = current.0.clone().expect("must be in callstack");
        let callframe = (*callframe).clone();

        let mut info = callframe.0.clone();
        info.inst_idx = inst_idx;

        let callframe = Callframe(info, callframe.1);
        let ptr = CallstackPtr(Some(Rc::new(callframe)));

        *current = ptr.clone();
        self.snapshots.push(ptr);
    }

    pub fn exit(&mut self) {
        self.callstack.pop();
    }
}
