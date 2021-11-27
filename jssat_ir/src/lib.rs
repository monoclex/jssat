#![feature(const_panic)]
#![feature(const_fn_trait_bound)]
#![feature(const_option)]
#![feature(associated_type_bounds)]

pub mod collections;
pub mod frontend;
pub mod id;
pub mod isa;
pub mod lifted;
pub mod pyramid_api;
pub mod retag;
pub mod value_snapshot;

/// can't have nice things :'( https://github.com/rust-lang/rust/issues/62633
pub trait UnwrapNone {
    fn expect_none(self, msg: &str);

    // lazy/hacky code but w/e
    fn expect_free(self);
}

impl<T> UnwrapNone for Option<T> {
    #[track_caller]
    fn expect_none(self, msg: &str) {
        assert!(matches!(self, None), "{}", msg);
    }

    #[track_caller]
    fn expect_free(self) {
        self.expect_none("must be free insertion slot");
    }
}
