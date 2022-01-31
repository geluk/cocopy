use std::collections::{hash_map::Entry, HashMap, HashSet};

use crate::il::Name;

use super::x86::Register;

pub struct RegisterAllocator {
    allocations: HashSet<Register>,
    bindings: HashMap<Name, Register>,
}
impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            allocations: HashSet::new(),
            bindings: HashMap::new(),
        }
    }

    /// Bind a name to a free register. If this name is already bound, returns the bound register.
    pub fn bind(&mut self, name: Name) -> Register {
        if let Entry::Occupied(ocp) = self.bindings.entry(name.clone()) {
            *ocp.get()
        } else {
            let reg = self.allocate().expect("Ran out of registers to allocate!");
            self.bindings.insert(name, reg);
            reg
        }
    }

    /// Unbind this name, releasing the register.
    /// Returns [`Some`] if the name was bound to a register, or [`None`] otherwise.
    pub fn unbind(&mut self, name: &Name) -> Option<Register> {
        match self.bindings.remove(name) {
            Some(reg) => self.release(&reg),
            None => None,
        }
    }

    /// Lookup a name, returning [`Some`] if it is bound to a register, or [`None`] otherwise.
    pub fn lookup(&self, name: &Name) -> Option<Register> {
        self.bindings.get(name).copied()
    }

    /// Try to allocate a new register. Returns [`None`] if all registers are occupied.
    pub fn allocate(&mut self) -> Option<Register> {
        for &reg in Register::iter() {
            if !self.allocations.contains(&reg) {
                self.allocations.insert(reg);
                return Some(reg);
            }
        }
        None
    }

    /// Release a register, allowing it to be allocated again later.
    /// Returns [`Some`] if the register was allocated, or [`None`] otherwise.
    pub fn release(&mut self, reg: &Register) -> Option<Register> {
        self.allocations.take(reg)
    }
}
