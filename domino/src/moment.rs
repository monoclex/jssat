//! Moment API. Used to profile the interpreter for Domino the Debugger.

use std::rc::Rc;

use jssat_ir::{
    lifted::{Function, FunctionId, LiftedProgram},
    pyramid_api::{LayerPtr, PyramidApi},
};
use rustc_hash::FxHashMap;

use crate::{Data, RawFrame, RawFrameCode, Snapshot};

pub struct MomentApi {
    functions: FxHashMap<FunctionId, CodeInfo>,
    pyramid_api: PyramidApi<FrameInfo>,
}

impl MomentApi {
    pub fn into_data(self) -> Data {
        let functions = self
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

        Data {
            snapshots: (self.pyramid_api.snapshots.into_iter())
                .map(|x| Snapshot {
                    code: functions
                        .get(&x.clone().0.unwrap().info.func)
                        .unwrap()
                        .clone(),
                    frame: into_raw_frame(x, &functions).unwrap(),
                })
                .collect(),
        }
    }
}

fn into_raw_frame(
    ptr: LayerPtr<FrameInfo>,
    functions: &FxHashMap<FunctionId, Rc<RawFrameCode>>,
) -> Option<Rc<RawFrame>> {
    let call_frame = ptr.0?;

    let frame = Rc::new(RawFrame {
        raw_frame_code: functions.get(&call_frame.info.func).unwrap().clone(),
        function: call_frame.info.func,
        inst_idx: call_frame.info.inst_idx,
        moment: call_frame.moment,
        next_moment: call_frame.next_moment,
        prev_moment: call_frame.prev_moment,
        parent: into_raw_frame(call_frame.parent.clone(), functions),
    });

    Some(frame)
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

impl MomentApi {
    pub fn new(code: &LiftedProgram) -> Self {
        MomentApi {
            functions: (code.functions.iter())
                .map(|(id, f)| (*id, CodeInfo::new(f)))
                .collect(),
            pyramid_api: PyramidApi::new(),
        }
    }

    pub fn enter(&mut self, func: FunctionId) {
        let info = FrameInfo { func, inst_idx: 0 };
        self.pyramid_api.begin(info);
    }

    pub fn snapshot(&mut self, inst_idx: usize) {
        self.pyramid_api.sample(|mut frame| {
            frame.inst_idx = inst_idx;
            frame
        });
    }

    pub fn exit(&mut self) {
        self.pyramid_api.end();
    }
}
