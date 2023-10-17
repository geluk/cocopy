use std::{
    collections::{
        hash_map::{Values, ValuesMut},
        HashMap,
    },
    hash::Hash,
    slice,
    vec::IntoIter,
};

/// Guarantees iteration in insertion order.
/// This is a stronger guarantee than [`super::hash_map::ConstHashMap`], which only guarantees
/// iteration in an order that is randomly determined (but predictable) based on which keys are
/// inserted, and stronger still than [`HashMap`], which has no predictable iteration order.
#[derive(Debug)]
pub struct OrderedHashMap<K, V> {
    inner: HashMap<K, V>,
    insertion_order: Vec<K>,
}
impl<K: Eq + Hash + Clone, V> OrderedHashMap<K, V> {
    pub fn insert(&mut self, key: K, value: V) {
        let previous = self.inner.insert(key.clone(), value);
        if previous.is_none() {
            self.insertion_order.push(key);
        }
    }

    pub fn insert_or_modify<F, G>(&mut self, key: K, insert: F, modify: G) -> &mut V
    where
        F: FnOnce() -> V,
        G: FnOnce(&mut V),
    {
        self.inner
            .entry(key.clone())
            .and_modify(modify)
            .or_insert_with(|| {
                self.insertion_order.push(key);
                insert()
            })
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }

    pub fn remove(&mut self, entry: &K) -> Option<V> {
        let value = self.inner.remove(entry);
        if value.is_some() {
            self.insertion_order.retain(|k| k != entry);
        }
        value
    }

    pub fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (k, v) in iter.into_iter() {
            self.insert(k, v);
        }
    }

    pub fn values(&self) -> Values<K, V> {
        self.inner.values()
    }

    pub fn values_mut(&mut self) -> ValuesMut<K, V> {
        self.inner.values_mut()
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }
}
impl<K, V> OrderedHashMap<K, V> {
    pub fn new() -> Self {
        Default::default()
    }
}
impl<K, V> OrderedHashMap<K, V> {
    pub fn iter(&self) -> OrderedHashMapIter<K, V> {
        OrderedHashMapIter {
            inner: &self.inner,
            key_iter: self.insertion_order.iter(),
        }
    }

    pub fn into_iter(self) -> OrderedHashMapIntoIter<K, V> {
        OrderedHashMapIntoIter {
            inner: self.inner,
            key_iter: self.insertion_order.into_iter(),
        }
    }
}
impl<K: Eq + Hash + Clone, V> FromIterator<(K, V)> for OrderedHashMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut hash_map = Self::new();
        for (k, v) in iter {
            hash_map.insert(k, v);
        }

        hash_map
    }
}

impl<K, V> Default for OrderedHashMap<K, V> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            insertion_order: Default::default(),
        }
    }
}

pub struct OrderedHashMapIter<'k, K, V> {
    inner: &'k HashMap<K, V>,
    key_iter: slice::Iter<'k, K>,
}

impl<'k, K: Eq + Hash, V> Iterator for OrderedHashMapIter<'k, K, V> {
    type Item = (&'k K, &'k V);

    fn next(&mut self) -> Option<Self::Item> {
        self.key_iter
            .next()
            .map(|k| self.inner.get_key_value(k).unwrap())
    }
}

pub struct OrderedHashMapIntoIter<K, V> {
    inner: HashMap<K, V>,
    key_iter: IntoIter<K>,
}
impl<K: Eq + Hash, V> Iterator for OrderedHashMapIntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.key_iter
            .next()
            .map(|k| self.inner.remove_entry(&k).unwrap())
    }
}
