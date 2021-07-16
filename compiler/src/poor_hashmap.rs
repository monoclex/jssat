//! A hashmap for when you cannot afford to `#[derive(Hash)]`
use assoc::AssocExt;

#[derive(Debug, Clone)]
pub struct PoorMap<K, V> {
    items: Vec<(K, V)>,
}

impl<K, V> Default for PoorMap<K, V> {
    fn default() -> Self {
        Self {
            items: Default::default(),
        }
    }
}

impl<K, V> PoorMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.items.iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = (K, V)> {
        self.items.into_iter()
    }
}

impl<K, V> PoorMap<K, V>
where
    K: PartialEq,
{
    fn items(&self) -> &impl AssocExt<K, V> {
        &self.items
    }

    fn items_mut(&mut self) -> &mut impl AssocExt<K, V> {
        &mut self.items
    }
}

impl<K, V> PoorMap<K, V>
where
    K: PartialEq,
{
    pub fn entry(&mut self, key: K) -> assoc::vec::Entry<K, V> {
        self.items_mut().entry(key)
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        self.items().get(key)
    }

    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        self.items_mut().get_mut(key)
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.items_mut().insert(key, value)
    }

    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: std::borrow::Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        self.items_mut().remove(key)
    }
}

impl<K, V> PartialEq for PoorMap<K, V>
where
    K: PartialEq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        // TODO: make this O(n) instead of O(n^2)
        for primary in self.items.iter() {
            let mut primary_in_secondary = false;
            for secondary in other.items.iter() {
                if primary == secondary {
                    primary_in_secondary = true;
                    break;
                }
            }

            if !primary_in_secondary {
                return false;
            }
        }
        true
    }
}

impl<K, V> Eq for PoorMap<K, V>
where
    K: Eq,
    V: Eq,
{
}
