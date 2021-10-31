#[cfg(feature = "link-swc")]
mod frontend_swc;

#[cfg(feature = "link-swc")]
pub use frontend_swc::traverse;

mod frontend_pseudo;

#[cfg(not(feature = "link-swc"))]
pub use frontend_pseudo::traverse;

pub mod ecmascript;
