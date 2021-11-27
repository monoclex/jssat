use std::{rc::Rc, sync::Arc};

use jssat_ir::{
    frontend::source_map::{SourceMapIdx, SourceMapImpl},
    isa::AtomDealer,
    lifted::FunctionId,
    value_snapshot::{SnapshotRecordKey, SnapshotValue, ValueSnapshotArena},
    UnwrapNone,
};
use rustc_hash::FxHashMap;

use crate::server_data::*;

pub struct Data {
    pub snapshots: Vec<Snapshot>,
    pub sources: SourceMapImpl,
    pub dealer: Arc<AtomDealer>,
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
    pub values: Rc<ValueSnapshotArena>,
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

        let values = &snapshot.frame.values;

        let mut mvalues = MomentValues {
            registers: FxHashMap::default(),
            records: FxHashMap::default(),
            lists: FxHashMap::default(),
        };

        mvalues.port(&self.dealer, values);

        Moment {
            callstack,
            code,
            source,
            values: mvalues,
        }
    }
}

impl MomentValues {
    fn port(&mut self, dealer: &Arc<AtomDealer>, arena: &ValueSnapshotArena) {
        for (register, value) in arena.registers.iter() {
            let value = self.port_value(dealer, value);
            self.registers.insert(register.get_the_value(), value);
        }

        for (id, record) in arena.records.iter() {
            let mut rec = vec![];

            for (key, value) in record.0.iter() {
                rec.push((self.port_key(dealer, key), self.port_value(dealer, value)));
            }

            self.records.insert(*id, rec);
        }

        for (id, list) in arena.lists.iter() {
            let mut mlist = vec![];

            for value in list.0.iter() {
                mlist.push(self.port_value(dealer, value));
            }

            self.lists.insert(*id, mlist);
        }
    }

    fn port_key(&mut self, dealer: &Arc<AtomDealer>, key: &SnapshotRecordKey) -> MomentRecordKey {
        match key {
            SnapshotRecordKey::Atom(x) => {
                let name = dealer
                    .try_resolve_name(*x)
                    .map(|s| s.to_owned())
                    .unwrap_or_else(|| format!("?{}?", x.0));

                MomentRecordKey::new_atom(name)
            }
            SnapshotRecordKey::Bytes(x) => {
                let s = (String::from_utf8(x.clone()).ok())
                    .or_else(|| from_utf16(x))
                    .unwrap_or_else(|| String::from_utf8_lossy(x).to_string());

                MomentRecordKey::new_bytes(s)
            }
            SnapshotRecordKey::Number(x) => MomentRecordKey::new_num(*x),
            SnapshotRecordKey::Boolean(x) => MomentRecordKey::new_bool(*x),
            SnapshotRecordKey::FnPtr(x) => MomentRecordKey::new_fnptr(x.get_the_value()),
        }
    }

    fn port_value(&mut self, dealer: &Arc<AtomDealer>, value: &SnapshotValue) -> MomentValue {
        match value {
            SnapshotValue::Atom(x) => {
                let name = dealer
                    .try_resolve_name(*x)
                    .map(|s| s.to_owned())
                    .unwrap_or_else(|| format!("?{}?", x.0));

                MomentValue::new_atom(name)
            }
            SnapshotValue::Bytes(x) => {
                let s = (String::from_utf8(x.clone()).ok())
                    .or_else(|| from_utf16(x))
                    .unwrap_or_else(|| String::from_utf8_lossy(x).to_string());

                MomentValue::new_bytes(s)
            }
            SnapshotValue::Number(x) => MomentValue::new_num(*x),
            SnapshotValue::Boolean(x) => MomentValue::new_bool(*x),
            SnapshotValue::FnPtr(x) => MomentValue::new_fnptr(x.get_the_value()),
            SnapshotValue::Record(x) => MomentValue::new_rec(*x),
            SnapshotValue::List(x) => MomentValue::new_list(*x),
            SnapshotValue::Runtime => MomentValue::new_runtime(),
        }
    }
}

fn from_utf16(bytes: &[u8]) -> Option<String> {
    let bytes = bytes
        .chunks_exact(2)
        .map(|x| u16::from_ne_bytes([x[0], x[1]]))
        .collect::<Vec<u16>>();

    String::from_utf16(&bytes).ok()
}
