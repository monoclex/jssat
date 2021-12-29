//! Contains structures that are sent to the client
// i would've used `https://github.com/Aleph-Alpha/ts-rs` to send structures to the client,
// but it uses far too many deps

use miniserde::Serialize;
use rustc_hash::FxHashMap;

/// Represents information necessary for basic app function
#[derive(Serialize)]
pub struct Overview {
    #[serde(rename = "totalMoments")]
    pub total_moments: usize,
    pub sources: Vec<Source>,
}

/// Represents a single source file that JSSAT IR was generated from.
#[derive(Serialize)]
pub struct Source {
    /// The name of the source file.
    pub name: Option<String>,
    /// The source code of the source file.
    pub text: String,
}

/// Represents a span to the original source of some thing.
#[derive(Serialize)]
pub struct SourceSpan {
    pub line: usize,
    pub column: usize,
}

/// Represents a precise moment in time.
#[derive(Serialize)]
pub struct Moment {
    /// A list of frames to preview, with the first element being considered
    /// the top of the callstack, and the last element being the bottom.
    pub callstack: Vec<Frame>,
    /// The instructions of the frame at the top of the callstack.
    pub code: FrameCode,
    /// The source code correlating to this moment, if any.
    pub source: Option<MomentSource>,
    /// The values present at this moment
    pub values: MomentValues,
}

#[derive(Serialize)]
pub struct Frame {
    /// A string preview of the function name and arguments.
    pub preview: String,
    /// The moment at which this frame is at.
    pub moment: usize,
    /// The next moment that is in this frame. When `moment` is advanced by
    /// 1, it will automatically step into functions and such. However, this
    /// behavior is not always desired. This property gets the next moment
    /// that is *sitll in this frame*, which has the effect of stepping over
    /// function calls.
    #[serde(rename = "nextMoment")]
    pub next_moment: Option<usize>,
    /// The previous moment that is in this frame. See [`next_moment`] for
    /// more details.
    #[serde(rename = "prevMoment")]
    pub prev_moment: Option<usize>,
}

#[derive(Serialize)]
pub struct FrameCode {
    /// Every line of code in the frame. The first element maps to the first
    /// line of code, with the last element mapping to the last line of
    /// code.
    pub lines: Vec<CodeLine>,
    /// The index of the highlighted line of code in `lines`.
    pub highlighted: usize,
    /// The header of the function being viewed
    pub header: String,
}

#[derive(Serialize)]
pub struct CodeLine {
    pub display: String,
}

#[derive(Serialize)]
pub struct MomentSource {
    /// The ID of the source that this is from. This represents an index
    /// into the `Vec<Source>` that was present in the [`Overview`]
    #[serde(rename = "sourceId")]
    pub source_id: usize,
    /// A list of locations that this instruction is contained within, with
    /// the top being closest to the point when the instruction was emitted.
    pub locations: Vec<SourceLocation>,
}

#[derive(Serialize)]
pub struct SourceLocation {
    pub start: SourceSpan,
    pub end: SourceSpan,
}

#[derive(Serialize)]
pub struct MomentValues {
    pub registers: FxHashMap<usize, MomentValue>,
    pub lists: FxHashMap<usize, Vec<MomentValue>>,
    pub records: FxHashMap<usize, Vec<(MomentValue, MomentValue)>>,
}

// enums don't exist in miniserde
#[derive(Serialize)]
pub struct MomentValue {
    pub kind: &'static str,
    pub atom: Option<String>,
    pub num: Option<i64>,
    pub bool: Option<bool>,
    pub bytes: Option<String>,
    pub fnptr: Option<usize>,
    pub rec: Option<usize>,
    pub list: Option<usize>,
}

impl MomentValue {
    pub fn new_atom(x: String) -> Self {
        Self {
            kind: "atom",
            atom: Some(x),
            num: None,
            bool: None,
            bytes: None,
            fnptr: None,
            rec: None,
            list: None,
        }
    }

    pub fn new_num(x: i64) -> Self {
        Self {
            kind: "num",
            atom: None,
            num: Some(x),
            bool: None,
            bytes: None,
            fnptr: None,
            rec: None,
            list: None,
        }
    }

    pub fn new_bool(x: bool) -> Self {
        Self {
            kind: "bool",
            atom: None,
            num: None,
            bool: Some(x),
            bytes: None,
            fnptr: None,
            rec: None,
            list: None,
        }
    }

    pub fn new_bytes(x: String) -> Self {
        Self {
            kind: "bytes",
            atom: None,
            num: None,
            bool: None,
            bytes: Some(x),
            fnptr: None,
            rec: None,
            list: None,
        }
    }

    pub fn new_fnptr(x: usize) -> Self {
        Self {
            kind: "fnptr",
            atom: None,
            num: None,
            bool: None,
            bytes: None,
            fnptr: Some(x),
            rec: None,
            list: None,
        }
    }

    pub fn new_rec(x: usize) -> Self {
        Self {
            kind: "rec",
            atom: None,
            num: None,
            bool: None,
            bytes: None,
            fnptr: None,
            rec: Some(x),
            list: None,
        }
    }

    pub fn new_list(x: usize) -> Self {
        Self {
            kind: "list",
            atom: None,
            num: None,
            bool: None,
            bytes: None,
            fnptr: None,
            rec: None,
            list: Some(x),
        }
    }

    pub fn new_runtime() -> Self {
        Self {
            kind: "runtime",
            atom: None,
            num: None,
            bool: None,
            bytes: None,
            fnptr: None,
            rec: None,
            list: None,
        }
    }
}
