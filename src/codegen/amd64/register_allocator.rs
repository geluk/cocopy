use std::{collections::HashMap, slice::Iter};

use crate::il::Name;

use super::x86::Register;

pub struct RegisterAllocator {
    iterator: Iter<'static, Register>,
    allocations: HashMap<Name, Register>,
}
impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            iterator: Register::iter(),
            allocations: HashMap::new(),
        }
    }

    pub fn lookup(&mut self, name: Name) -> Register {
        let mut iter = self.iterator.clone();
        let register = *self
            .allocations
            .entry(name)
            .or_insert_with(|| *iter.next().expect("Ran out of registers to allocate!"));

        self.iterator = iter;
        register
    }
}
