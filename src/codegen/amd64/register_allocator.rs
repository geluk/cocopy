use std::{
    collections::{hash_map::Entry, HashMap},
    slice::Iter,
};

use crate::il::Name;

use super::x86::Register;

pub struct RegisterAllocator {
    iterator: Iter<'static, Register>,
    allocations: HashMap<Name, Register>,
    reverse_allocations: HashMap<Register, Name>,
}
impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            iterator: Register::iter(),
            allocations: HashMap::new(),
            reverse_allocations: HashMap::new(),
        }
    }

    pub fn lookup(&mut self, name: &Name) -> Option<Register> {
        self.allocations.get(name).copied()
    }

    pub fn allocate(&mut self, name: Name) -> Option<Register> {
        for &reg in self.iterator.clone() {
            if let Entry::Vacant(entry) = self.reverse_allocations.entry(reg) {
                self.allocations.insert(name.clone(), reg);
                entry.insert(name);
                return Some(reg);
            }
        }
        None
    }

    pub fn holds(&self, register: &Register) -> Option<&Name> {
        self.reverse_allocations.get(register)
    }

    pub fn release(&mut self, name: &Name) {
        self.allocations.remove(name);
    }
}
