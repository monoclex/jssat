//! A general purpose sampler for hierarchical callsites.
//!
//! This module contains both the functionality necessary for the Moment API
//! used in the JSSAT Interpreter, and for JSSAT Source Maps. Below, we explain
//! the reasoning behind why this module is used for both collecting moments in
//! the interpreter and for source maps.
//!
//! # Interpreter
//!
//! In an interpreter, we may be nested several layers deep in code:
//!
//! ```norun
//! F():
//!     X1 = 1
//!     CALL G()
//!     X2 = 2
//!
//! G():
//!     Y1 = 1
//!     CALL H()
//!     Y2 = 2
//!
//! H():
//!     Z1 = 1
//!     Z2 = 2
//! ```
//!
//! At any given instruction, we are at a state where we're inside something
//! else. For example, consider `Z1 = 1`, we are:
//!
//! - at `Z1 = 1` of `H()`
//! - inside `CALL H()` of `G()`
//! - inside `CALL G()` of `F()`
//!
//! # Source Maps
//!
//! To demonstrate this module, consider the following source code:
//!
//! ```norun
//! (function f(x)
//!     ((y = ((x + 1) * 2))
//!      (z = (sqrt y))
//!      (return (y + z))))
//! ```
//!
//! and the equivalent instructions:
//!
//! ```norun
//! F(X):
//!     Y1 = X + 1
//!     Y  = Y1 * 2
//!     Z  = SQRT Y
//!     R  = Y + Z
//!     RET R
//! ```
//!
//! When executing `x + 1`, we are inside multiple levels:
//!
//! - at `(x + 1)`
//! - inside `(_ * 2)`
//! - inside `(y = _)`
//! - inside `(_, (z = ...), (return ...))`
//! - inside `(function f(x) _)`
//!
//! Similar to instructions, source code also has a conceptual callstack.
//!
//! # This Module
//!
//! This module abstracts out the behavior present in both scenarios to be more
//! general purpose. The following primitives are provided:
//!
//! - [`Pyramid::begin()`]: begins a layer
//! - [`Pyramid::sample()`]: performs a "sample", connecting that sample to the
//!   current layers and those undeneath it
//! - [`Pyramid::end()`]: ends a layer

use derive_more::{Deref, DerefMut};
use std::rc::Rc;

pub struct PyramidApi<T> {
    pub snapshots: Vec<LayerPtr<T>>,
    pub layers: Layers<T>,
}

#[derive(Deref, DerefMut)]
pub struct Layers<T>(pub Vec<LayerPtr<T>>);

impl<T> Default for Layers<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> Clone for Layers<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Layers<T> {
    fn current(&self) -> LayerPtr<T> {
        match self.as_slice() {
            [.., last] => last.clone(),
            _ => LayerPtr(None),
        }
    }

    fn current_mut(&mut self) -> Option<&mut LayerPtr<T>> {
        match self.as_mut_slice() {
            [] => None,
            [.., last] => Some(last),
        }
    }
}

#[derive(Clone)]
pub struct Sample<T> {
    pub info: T,
    pub moment: usize,
    pub next_moment: Option<usize>,
    pub prev_moment: Option<usize>,
    pub parent: LayerPtr<T>,
}

pub struct LayerPtr<T>(pub Option<Rc<Sample<T>>>);

impl<T> LayerPtr<T> {
    pub fn new(sample: Sample<T>) -> Self {
        Self(Some(Rc::new(sample)))
    }
}

impl<T> Clone for LayerPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> PyramidApi<T> {
    pub fn new() -> Self {
        PyramidApi {
            snapshots: Vec::new(),
            layers: Layers::default(),
        }
    }

    pub fn begin(&mut self, info: T) {
        let prev_moment = self.layers.current().0.map(|callframe| callframe.moment);

        let this_node = Sample {
            info,
            moment: self.snapshots.len(),
            next_moment: None,
            prev_moment,
            parent: self.layers.current(),
        };

        self.layers.push(LayerPtr::new(this_node));
    }

    pub fn end(&mut self) {
        self.layers.pop();
    }
}

impl<T> Default for PyramidApi<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> PyramidApi<T> {
    // NOTE: the comments here were from when this wasn't the PyramidApi, and was
    //   the more concrete MomentApi in the interpreter.
    pub fn sample(&mut self, make_info: impl FnOnce(T) -> T) {
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
        let current = self.layers.current_mut().expect("must be in function");

        // we can't mutate `CallstackPtr` directly, so we will clone the inner
        // callframe data and modify it
        let old_callframe = current.0.clone().expect("must be in callstack");
        let old_callframe = (*old_callframe).clone();

        let info = make_info(old_callframe.info); // update the current instruction we're on

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
            *previous_callframe = LayerPtr::new(prev_callframe);

            prev_moment = Some(prev_moment_idx);
        }

        // construct the current callframe to replace the top of the callstack
        let callframe = Sample {
            info,
            moment: current_moment,
            next_moment: None,
            prev_moment,
            parent: old_callframe.parent,
        };

        let ptr = LayerPtr(Some(Rc::new(callframe)));

        // replace the top of the callstack, and insert as the current moment
        *current = ptr.clone();
        self.snapshots.push(ptr);
    }
}
