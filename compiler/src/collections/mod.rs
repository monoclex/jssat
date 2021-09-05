pub mod poor_hashmap;
pub use poor_hashmap::PoorMap;

pub mod set;
pub use set::Set;

type FxHasher = std::hash::BuildHasherDefault<rustc_hash::FxHasher>;
pub type FxBiHashMap<K, V> = bimap::BiHashMap<K, V, FxHasher, FxHasher>;
