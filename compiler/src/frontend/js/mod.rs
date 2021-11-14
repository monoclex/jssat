mod frontend_pseudo;

// #[cfg(not(feature = "link-swc"))]
pub use frontend_pseudo::traverse;

pub mod ast;
pub mod ecmascript;
