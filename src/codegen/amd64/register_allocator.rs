use std::collections::{
    hash_map::{Entry, Keys},
    HashMap, HashSet,
};

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

    pub fn iter_bound_names(&self) -> Keys<Name, Register> {
        self.bindings.keys()
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

    /// Bind a name to the given register. The name may not be bound already,
    /// and the register must have been allocated first.
    pub fn bind_to(&mut self, name: Name, register: Register) {
        match self.lookup(&name) {
            Some(_) => panic!("Cannot bind a name already bound to a different register"),
            None if self.is_free(register) => {
                panic!("Cannot bind a name to an unallocated register")
            }
            None => {
                self.bindings.insert(name, register);
            }
        }
    }

    /// Unbind this name, releasing the register.
    /// Returns [`Some`] if the name was bound to a register, or [`None`] otherwise.
    pub fn unbind(&mut self, name: &Name) -> Option<Register> {
        match self.bindings.remove(name) {
            Some(reg) => self.release(reg),
            None => None,
        }
    }

    /// Notifies the allocator that a value has been moved from `source` to `target`.
    /// If a binding for `source` exists, it is updated to point to `target`.
    pub fn notify_move(&mut self, source: Register, target: Register) {
        if self.is_free(target) {
            panic!(
                "Cannot process a binding move into unallocated register {}",
                target
            )
        }
        if let Some(reg) = self.bindings.values_mut().find(|v| v == &&source) {
            *reg = target;
        }
    }

    /// Lookup a name, returning [`Some`] if it is bound to a register, or [`None`] otherwise.
    pub fn lookup(&self, name: &Name) -> Option<Register> {
        self.bindings.get(name).copied()
    }

    /// Checks if a register has not been allocated yet.
    pub fn is_free(&self, register: Register) -> bool {
        !self.allocations.contains(&register)
    }

    /// Allocate a register. Panics if the register is not free.
    pub fn allocate_reg(&mut self, register: Register) {
        if !self.allocations.contains(&register) {
            self.allocations.insert(register);
        } else {
            panic!("{} is already allocated", register)
        }
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
    pub fn release(&mut self, reg: Register) -> Option<Register> {
        self.allocations.take(&reg)
    }

    pub fn debug(&self) {
        println!("Allocator state:");
        for (name, reg) in &self.bindings {
            println!("    {} -> {}", name, reg);
        }
        for reg in &self.allocations {
            println!("    alloc {}", reg);
        }
    }
}
