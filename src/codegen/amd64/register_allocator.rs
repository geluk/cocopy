use std::collections::HashMap;

use crate::{
    codegen::register_allocation::{Allocation, Allocator, Lifetime},
    listing::{Listing, Position},
    prelude::*,
};

use super::{
    procedure_compiler::{DeferredLine, DeferredOperand, DeferredReg, Target},
    x86::*,
};

pub struct RegisterAllocator<'l> {
    requests: HashMap<&'l DeferredReg, AllocationRequest>,
    listing: &'l Listing<DeferredLine>,
}
impl<'l> RegisterAllocator<'l> {
    pub fn preprocess(listing: &'l Listing<DeferredLine>) -> Allocator<DeferredReg, Register> {
        let preprocessor = Self {
            requests: Default::default(),
            listing,
        };
        preprocessor.assign_regs()
    }

    fn assign_regs(mut self) -> Allocator<DeferredReg, Register> {
        self.determine_lifetimes();

        for (name, rq) in self.requests.iter() {
            trace!(
                "Lifetime of {name:?} is {}:{}",
                rq.lifetime.start().0,
                rq.lifetime.end().0
            );
            for (position, reg) in rq.locks.iter() {
                trace!(" -> at {}: must live in {reg}", position.0);
            }
        }

        let mut allocator = Allocator::new(Register::iter().copied().collect());

        for (name, request) in self.requests {
            debug!("Allocating {name:?}");
            match request.locks.len() {
                0 => {
                    allocator.allocate(name.clone(), request.lifetime);
                }
                1 => {
                    let (pos, reg) = request.locks.into_iter().next().unwrap();
                    allocator.lock_to(name, request.lifetime, pos, reg);
                }
                _ => (),
            }
        }

        for alloc in allocator.iter_reg_allocations() {
            debug!(
                "At {} {:?} to {}",
                alloc.lifetime(),
                alloc.name(),
                alloc.register()
            );
        }

        allocator
    }

    fn determine_lifetimes(&mut self) {
        // Process instructions
        for (pos, line) in self.listing.iter_lines() {
            if let DeferredLine::Instr(instr) = line {
                if let Some(Target::Deferred(reg)) = &instr.target {
                    self.expand_lifetime(reg, pos);
                }

                for operand in instr.operands.iter() {
                    if let DeferredOperand::Reg(reg, _) = operand {
                        self.expand_lifetime(reg, pos)
                    }
                }
            } else if let DeferredLine::LockRequest(name, reg) = line {
                self.add_lock(name, pos, *reg);
            }
        }
    }

    fn expand_lifetime(&mut self, reg: &'l DeferredReg, position: Position) {
        self.requests
            .entry(reg)
            .and_modify(|r| r.lifetime = r.lifetime.expand(position))
            .or_insert(AllocationRequest {
                lifetime: Lifetime::from_position(position),
                locks: Default::default(),
            });
    }

    fn add_lock(&mut self, name: &'l DeferredReg, lock_point: Position, register: Register) {
        let name = self
            .requests
            .get_mut(name)
            .expect("Attempted to lock unknown name");

        if let Some(existing) = name.locks.insert(lock_point, register) {
            panic!("Attempted to lock {lock_point:?} to {register:?}, but it was already locked to {existing:?}")
        }
    }
}

struct AllocationRequest {
    lifetime: Lifetime,
    locks: HashMap<Position, Register>,
}
