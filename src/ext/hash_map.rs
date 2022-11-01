use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::BuildHasher,
};

/// Type alias for a hash map using a hasher with a constant state seed.
pub type ConstHashMap<K, V> = HashMap<K, V, ConstState>;

/// A constant state seed for a hashmap, whose [`Self::build_hasher()`] method always returns
/// [`DefaultHasher::new()`]
#[derive(Default, Debug, Clone, Copy)]
pub struct ConstState;
impl BuildHasher for ConstState {
    type Hasher = DefaultHasher;

    fn build_hasher(&self) -> Self::Hasher {
        DefaultHasher::new()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    pub fn const_state_guarantees_predictable_iteration_order() {
        let mut hashmap_a = HashMap::with_hasher(ConstState);
        let mut hashmap_b = HashMap::with_hasher(ConstState);

        for x in 0..10 {
            hashmap_a.insert(x, x * 2);
            hashmap_b.insert(x, x * 2);
        }

        for ((key_a, _), (key_b, _)) in hashmap_a.into_iter().zip(hashmap_b.into_iter()) {
            assert_eq!(key_a, key_b);
        }
    }
}
