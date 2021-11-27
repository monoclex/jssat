//! Contains utilities for building source maps.

use std::sync::Mutex;
use crate::pyramid_api::PyramidApi;

#[derive(Clone, Copy, Debug)]
pub struct SourceMapIdx(usize);

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

// it would be a hassle to figure out lifetime stuff in the codegen sooo
// wrapping it with a mutex so we can share it (lol)
pub struct SourceMap(Mutex<SourceMapImpl>);

impl SourceMap {
    pub fn new(source: String) -> Self {
        Self(Mutex::new(SourceMapImpl::new(source)))
    }

    pub fn begin(&self, span: SourceSpan) {
        let mut me = self.0.lock().unwrap();
        me.begin(span);
    }

    pub fn sample(&self, span: SourceSpan) -> SourceMapIdx {
        let mut me = self.0.lock().unwrap();
        me.sample(span)
    }

    pub fn end(&self) {
        let mut me = self.0.lock().unwrap();
        me.end();
    }
}

pub struct SourceMapImpl {
    source: String,
    pyramid: PyramidApi<SourceSpan>,
}

impl SourceMapImpl {
    pub fn new(source: String) -> Self {
        Self {
            source,
            pyramid: PyramidApi::new(),
        }
    }

    pub fn begin(&mut self, span: SourceSpan) {
        self.pyramid.begin(span);
    }

    pub fn sample(&mut self, span: SourceSpan) -> SourceMapIdx {
        let idx = self.pyramid.snapshots.len();
        self.pyramid.sample(|_| span);
        SourceMapIdx(idx)
    }

    pub fn end(&mut self) {
        self.pyramid.end();
    }
}
