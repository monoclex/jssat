//! Contains structures that are sent to the client
// i would've used `https://github.com/Aleph-Alpha/ts-rs` to send structures to the client,
// but it uses far too many deps

use miniserde::Serialize;

pub use callstack::*;

/// Represents information necessary for basic app function
#[derive(Serialize)]
pub struct Overview {
    #[serde(rename = "totalMoments")]
    pub total_moments: usize,
}

/// Represents a span to the original source of some thing.
#[derive(Serialize)]
pub struct SourceSpan {}

pub mod callstack {
    use super::SourceSpan;
    use miniserde::Serialize;

    /// Represents a precise moment in time.
    #[derive(Serialize)]
    pub struct Moment {
        /// A list of frames to preview, with the first element being considered
        /// the top of the callstack, and the last element being the bottom.
        pub callstack: Vec<Frame>,
        /// The source code of the frame at the top of the callstack.
        pub code: FrameCode,
    }

    #[derive(Serialize)]
    pub struct Frame {
        /// A string preview of the function name and arguments
        pub preview: String,
    }

    #[derive(Serialize)]
    pub struct FrameCode {
        /// Every line of code in the frame. The first element maps to the first
        /// line of code, with the last element mapping to the last line of
        /// code.
        pub lines: Vec<CodeLine>,
        /// The index of the highlighted line of code in `lines`.
        pub highlighted: usize,
    }

    #[derive(Serialize)]
    pub struct CodeLine {
        pub display: String,
        pub source: SourceSpan,
    }
}
