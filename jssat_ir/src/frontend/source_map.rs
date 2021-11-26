//! Contains utilities for building source maps.

use crate::pyramid_api::PyramidApi;

#[derive(Clone, Copy, Debug)]
pub struct SourceMapIdx(usize);

pub struct SourceMap {
    source: String,
    pyramid: PyramidApi<SourceSpan>,
}

#[derive(Clone, Copy)]
pub struct SourceSpan {
    pub start: SourcePos,
    pub end: SourcePos,
}

#[derive(Clone, Copy)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
}

impl SourceMap {
    pub fn new(source: String) -> Self {
        Self {
            source,
            pyramid: PyramidApi::new(),
        }
    }

    pub fn begin(&mut self, span: SourceSpan) {
        self.pyramid.begin(span);
    }

    pub fn emit_inst(&mut self, span: SourceSpan) -> SourceMapIdx {
        let idx = self.pyramid.snapshots.len();
        self.pyramid.sample(|_| span);
        SourceMapIdx(idx)
    }

    pub fn end(&mut self) {
        self.pyramid.end();
    }
}
