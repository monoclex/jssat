use std::rc::Rc;

use jssat_ir::{
    frontend::source_map::{SourceMapIdx, SourceMapImpl},
    lifted::FunctionId,
};

use crate::server_data::*;

pub struct Data {
    pub snapshots: Vec<Snapshot>,
    pub sources: SourceMapImpl,
}

pub struct Snapshot {
    pub code: Rc<RawFrameCode>,
    pub frame: Rc<RawFrame>,
}

pub struct RawFrameCode {
    pub fn_name: Option<String>,
    pub lines: Vec<String>,
}

pub struct RawFrame {
    pub raw_frame_code: Rc<RawFrameCode>,
    pub function: FunctionId,
    pub inst_idx: usize,
    pub moment: usize,
    pub next_moment: Option<usize>,
    pub prev_moment: Option<usize>,
    pub parent: Option<Rc<RawFrame>>,
    pub source_idx: Option<SourceMapIdx>,
}

impl Data {
    pub fn overview(&self) -> Overview {
        Overview {
            total_moments: self.snapshots.len(),
            sources: vec![Source {
                name: None,
                text: self.sources.source.clone(),
            }],
        }
    }

    pub fn moment(&self, index: usize) -> Moment {
        let snapshot = self.snapshots.get(index).unwrap();

        let mut callstack = vec![];

        let mut curr_frame = Some(&snapshot.frame);

        while let Some(frame) = curr_frame {
            callstack.push(Frame {
                preview: format!(
                    "function {} @ {}",
                    frame.function,
                    frame.raw_frame_code.fn_name.as_deref().unwrap_or("")
                ),
                moment: frame.moment,
                next_moment: frame.next_moment,
                prev_moment: frame.prev_moment,
            });

            curr_frame = frame.parent.as_ref();
        }

        let lines = snapshot.code.lines.clone();
        let lines = (lines.into_iter())
            .map(|line| CodeLine { display: line })
            .collect();

        let code = FrameCode {
            lines,
            highlighted: snapshot.frame.inst_idx,
        };

        let source = snapshot.frame.source_idx.map(|s| {
            let mut snapshot = (self.sources.pyramid.snapshots)
                .get(s.0)
                .expect("should be valid sourcemapidx");

            let mut locations = vec![];
            while let Some(x) = &snapshot.0 {
                locations.push(SourceLocation {
                    start: SourceSpan {
                        line: x.info.start.line,
                        column: x.info.start.column,
                    },
                    end: SourceSpan {
                        line: x.info.end.line,
                        column: x.info.end.column,
                    },
                });

                snapshot = &x.parent
            }

            MomentSource {
                // we don't support multiple source maps yet
                source_id: 0,
                locations,
            }
        });

        Moment {
            callstack,
            code,
            source,
        }
    }
}
