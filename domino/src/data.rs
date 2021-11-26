use crate::server_data::*;

pub struct Data {}

impl Data {
    pub fn overview(&self) -> Overview {
        Overview { total_moments: 100 }
    }

    pub fn moment(&self, index: usize) -> Moment {
        let mut callstack = vec![];
        for i in 0..index {
            callstack.push(Frame {
                preview: format!("epico {}!", i),
            });
        }

        Moment {
            callstack,
            code: FrameCode {
                lines: vec![
                    CodeLine {
                        display: "r1 = 1".into(),
                        source: SourceSpan {},
                    },
                    CodeLine {
                        display: format!("r2 = {}", index),
                        source: SourceSpan {},
                    },
                    CodeLine {
                        display: "r3 = 3".into(),
                        source: SourceSpan {},
                    },
                ],
                highlighted: 1,
            },
        }
    }
}
