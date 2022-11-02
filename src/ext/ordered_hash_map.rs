use std::{
    collections::HashMap,
    hash::{BuildHasher, Hash},
    slice,
    vec::IntoIter,
};

/// Guarantees iteration in insertion order.
/// This is a stronger guarantee than [`super::hash_map::ConstHashMap`], which only guarantees
/// iteration in an order that is randomly determined (but predictable) based on which keys are
/// inserted, and stronger still than [`HashMap`], which has no predictable iteration order.
pub struct OrderedHashMap<K, V, S> {
    inner: HashMap<K, V, S>,
    insertion_order: Vec<K>,
}
impl<K: Eq + Hash + Copy, V, S: BuildHasher> OrderedHashMap<K, V, S> {
    pub fn iter(&self) -> OrderedHashMapIter<K, V, S> {
        OrderedHashMapIter {
            inner: &self.inner,
            key_iter: self.insertion_order.iter(),
        }
    }

    pub fn into_iter(self) -> OrderedHashMapIntoIter<K, V, S> {
        OrderedHashMapIntoIter {
            inner: self.inner,
            key_iter: self.insertion_order.into_iter(),
        }
    }

    pub fn insert_or_modify<F, G>(&mut self, key: K, insert: F, modify: G) -> &mut V
    where
        F: FnOnce() -> V,
        G: FnOnce(&mut V),
    {
        self.inner.entry(key).and_modify(modify).or_insert_with(|| {
            self.insertion_order.push(key);
            insert()
        })
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }
}
impl<K, V, S: BuildHasher + Default> Default for OrderedHashMap<K, V, S> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            insertion_order: Default::default(),
        }
    }
}

pub struct OrderedHashMapIter<'k, K, V, S> {
    inner: &'k HashMap<K, V, S>,
    key_iter: slice::Iter<'k, K>,
}

impl<'k, K: Eq + Hash, V, S: BuildHasher> Iterator for OrderedHashMapIter<'k, K, V, S> {
    type Item = (&'k K, &'k V);

    fn next(&mut self) -> Option<Self::Item> {
        self.key_iter
            .next()
            .map(|k| self.inner.get_key_value(k).unwrap())
    }
}

pub struct OrderedHashMapIntoIter<K, V, S: BuildHasher> {
    inner: HashMap<K, V, S>,
    key_iter: IntoIter<K>,
}
impl<K: Eq + Hash, V, S: BuildHasher> Iterator for OrderedHashMapIntoIter<K, V, S> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.key_iter
            .next()
            .map(|k| self.inner.remove_entry(&k).unwrap())
    }
}
