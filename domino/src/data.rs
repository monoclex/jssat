use std::rc::Rc;

use jssat_ir::lifted::FunctionId;

use crate::server_data::*;

pub struct Data {
    pub snapshots: Vec<Snapshot>,
}

pub struct Snapshot {
    pub code: Rc<RawFrameCode>,
    pub frame: Rc<RawFrame>,
}

pub struct RawFrameCode {
    pub lines: Vec<String>,
}

pub struct RawFrame {
    pub function: FunctionId,
    pub inst_idx: usize,
    pub parent: Option<Rc<RawFrame>>,
}

impl Data {
    pub fn overview(&self) -> Overview {
        Overview {
            total_moments: self.snapshots.len(),
        }
    }

    pub fn moment(&self, index: usize) -> Moment {
        let snapshot = self.snapshots.get(index).unwrap();

        let mut callstack = vec![];

        let mut curr_frame = Some(&snapshot.frame);

        while let Some(frame) = curr_frame {
            callstack.push(Frame {
                preview: format!("function {}", frame.function),
            });

            curr_frame = frame.parent.as_ref();
        }

        let lines = snapshot.code.lines.clone();
        let lines = (lines.into_iter())
            .map(|line| CodeLine {
                display: line,
                source: SourceSpan {},
            })
            .collect();

        let mut code = FrameCode {
            lines,
            highlighted: snapshot.frame.inst_idx,
        };

        Moment { callstack, code }
    }
}
