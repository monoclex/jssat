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
            .map(|(k, v)| {
                (k, {
                    Rc::new(RawFrameCode {
                        fn_name: v.fn_name,
                        lines: v.lines,
                    })
                })
            })
            .collect::<FxHashMap<_, _>>();

        Self {
            snapshots: (api.snapshots.into_iter())
                .map(|x| Snapshot {
                    code: functions
                        .get(&x.clone().0.unwrap().info.func)
                        .unwrap()
                        .clone(),
                    frame: x.into_raw_frame(&functions).unwrap(),
                })
                .collect(),
        }
    }
}

impl CallstackPtr {
    fn into_raw_frame(
        self,
        functions: &FxHashMap<FunctionId, Rc<RawFrameCode>>,
    ) -> Option<Rc<RawFrame>> {
        let call_frame = self.0?;

        let frame = Rc::new(RawFrame {
            raw_frame_code: functions.get(&call_frame.info.func).unwrap().clone(),
            function: call_frame.info.func,
            inst_idx: call_frame.info.inst_idx,
            moment: call_frame.moment,
            next_moment: call_frame.next_moment,
            prev_moment: call_frame.prev_moment,
            parent: call_frame.parent.clone().into_raw_frame(functions),
        });

        Some(frame)
    }
}

#[derive(Clone, Default, Deref, DerefMut)]
struct MomentState(Vec<CallstackPtr>);

impl MomentState {
    fn current(&self) -> CallstackPtr {
        match self.as_slice() {
            [.., last] => last.clone(),
            _ => CallstackPtr(None),
        }
    }

    fn current_mut(&mut self) -> Option<&mut CallstackPtr> {
        match self.as_mut_slice() {
            [] => None,
            [.., last] => Some(last),
        }
    }
}

pub struct CodeInfo {
    fn_name: Option<String>,
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

        let mut s = String::new();
        function.end.display(&mut s).unwrap();
        lines.push(s);

        Self {
            fn_name: function.name.clone(),
            lines,
        }
    }
}

#[derive(Clone)]
pub struct FrameInfo {
    func: FunctionId,
    inst_idx: usize,
}

#[derive(Clone)]
pub struct Callframe {
    info: FrameInfo,
    moment: usize,
    next_moment: Option<usize>,
    prev_moment: Option<usize>,
    parent: CallstackPtr,
}

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

        let prev_moment = self.callstack.current().0.map(|callframe| callframe.moment);

        let this_node = Callframe {
            info,
            moment: self.snapshots.len(),
            next_moment: None,
            prev_moment,
            parent: self.callstack.current(),
        };

        self.callstack.push(CallstackPtr::new(this_node));
    }

    pub fn snapshot(&mut self, inst_idx: usize) {
        // on snapshot, do a few thigns:
        //
        // callstack:
        // +-----+
        // | now | <- at the top of our callstack, we maintain an up-to-date
        // +-----+    callframe about where we're executing at. this is so
        // |     |    that when we enter into more frames, we can then clone
        // |  .  |    the top of the callstack, and know that it points to
        // |  .  |    the moment in time we began executing that function
        // |  .  |
        // |     | our goal is to first update that callstack to point to the
        //         current instruction we're on
        //
        // previous moment:
        //
        // snapshots:
        // [ m1, m2, ..., mn ]
        //
        // when we take a snapshot, we know we're on the next instruction of
        // a function, and that the top of the callstack points to our current
        // selves.
        //
        // however, in order to facilitate stepping entire calls at a time, we
        // have to link the previous moment with the current moment. because the
        // top of the callstack always records the current moment, we can get
        // the top of the callstack's "current moment", which is really *old*,
        // and refers to the *previous moment*. from there, we can update the
        // snapshot we took, and include what the next moment is (the real
        // current moment)

        let current_moment = self.snapshots.len();

        // get the top of the callstack
        let current = self.callstack.current_mut().expect("must be in function");

        // we can't mutate `CallstackPtr` directly, so we will clone the inner
        // callframe data and modify it
        let old_callframe = current.0.clone().expect("must be in callstack");
        let old_callframe = (*old_callframe).clone();

        let mut info = old_callframe.info.clone();
        info.inst_idx = inst_idx; // update the current instruction we're on

        // `old_callframe`'s `moment` refers to the old moment *it* was at.
        // we need to modify the old moment, to point to what the next moment
        // should be

        let mut prev_moment = None;

        // we might not have taken any snapshots
        if let Some(previous_callframe) = self.snapshots.get_mut(old_callframe.moment) {
            // again, we can't mutate it directly, so clone it and then put the
            // clone back in
            let mut prev_callframe = (*previous_callframe.0.clone().unwrap()).clone();
            let prev_moment_idx = prev_callframe.moment;
            prev_callframe.next_moment = Some(current_moment); // update the moment
            *previous_callframe = CallstackPtr::new(prev_callframe);

            prev_moment = Some(prev_moment_idx);
        }

        // construct the current callframe to replace the top of the callstack
        let callframe = Callframe {
            info,
            moment: current_moment,
            next_moment: None,
            prev_moment,
            parent: old_callframe.parent,
        };

        let ptr = CallstackPtr(Some(Rc::new(callframe)));

        // replace the top of the callstack, and insert as the current moment
        *current = ptr.clone();
        self.snapshots.push(ptr);
    }

    pub fn exit(&mut self) {
        self.callstack.pop();
    }
}
