use std::{collections::HashMap, hash::Hash, slice, vec::IntoIter};

pub struct OrderedHashMap<K, V> {
    inner: HashMap<K, V>,
    insertion_order: Vec<K>,
}
impl<K: Eq + Hash + Copy, V> OrderedHashMap<K, V> {
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
