use std::collections::HashMap;

use crate::{
    codegen::register_allocation::{Allocator, Lifetime},
    ext::ordered_hash_map::OrderedHashMap,
    listing::{Listing, Position},
    prelude::*,
};

use super::{
    defer::*,
    op_semantics::{Direction, HasOpSemantics},
    x86::*,
};

pub struct LifetimeAnalysis<'l> {
    requests: OrderedHashMap<&'l DeferredReg, AllocationRequest>,
    listing: &'l Listing<DeferredLine>,
    avoids: Vec<(Position, Register)>,
}
impl<'l> LifetimeAnalysis<'l> {
    /// Analyse the lifetimes of variables in the given listing, and allocate the variables to
    /// registers.
    pub fn create_allocator_for(
        listing: &'l Listing<DeferredLine>,
    ) -> Allocator<DeferredReg, Register> {
        let mut analysis = Self {
            requests: Default::default(),
            avoids: vec![],
            listing,
        };
        analysis.determine_lifetimes();
        analysis.make_allocator()
    }

    fn make_allocator(self) -> Allocator<DeferredReg, Register> {
        for (name, rq) in self.requests.iter() {
            trace!("Lifetime of {name} is {}", rq.lifetime);
            for (position, reg) in rq.locks.iter() {
                trace!(" -> at {}: must live in {reg}", position.0);
            }
        }

        let mut allocator = Allocator::new(Register::iter().copied().collect());

        for (position, register) in self.avoids {
            trace!("at {position}: avoid {register}");
            allocator.avoid(position, register);
        }

        for (name, request) in self.requests.into_iter() {
            allocator.allocate(name.clone(), request.lifetime);

            for (pos, reg) in request.locks {
                allocator.lock_to(name, pos, reg);
            }
        }

        for (name, alloc) in allocator.iter_allocations() {
            debug!("Allocate {name} with lifetime {} into:", alloc.lifetime());
            for slice in alloc.iter_reg_slices() {
                debug!(" - {} at register {}", slice.lifetime(), slice.register())
            }
            for slice in alloc.iter_stack_slices() {
                debug!(" - {} at stack offset {}", slice.lifetime(), slice.offset())
            }
        }

        debug!("Assigning stack offsets");
        allocator.assign_stack_offsets();
        allocator
    }

    /// Determine the lifetimes of all variables.
    fn determine_lifetimes(&mut self) {
        // Process instructions
        for (pos, line) in self.listing.iter_lines() {
            match line {
                DeferredLine::Instr(instr) => {
                    for (index, operand) in instr.operands.iter().enumerate() {
                        if let DeferredOperand::Reg(reg) = &operand {
                            let pos = match instr.op.op_semantics(index).direction() {
                                Direction::Read => pos.read(),
                                Direction::Write => pos.write(),
                            };

                            self.expand_lifetime(reg, pos)
                        }
                    }
                }
                DeferredLine::ImplicitRead(reg) => {
                    self.expand_lifetime(reg, pos);
                }
                DeferredLine::LockRequest(name, reg) => {
                    self.expand_lifetime(name, pos);
                    self.add_lock(name, pos, *reg);
                }
                DeferredLine::ImplicitWrite(reg) => {
                    self.avoids.push((pos, *reg));
                }
                _ => (),
            }
        }
    }

    fn expand_lifetime(&mut self, reg: &'l DeferredReg, position: Position) {
        self.requests.insert_or_modify(
            reg,
            || AllocationRequest {
                lifetime: Lifetime::from_position(position),
                locks: Default::default(),
            },
            |r| r.lifetime = r.lifetime.expand(position),
        );
    }

    fn add_lock(&mut self, name: &'l DeferredReg, lock_point: Position, register: Register) {
        let name = self
            .requests
            .get_mut(&name)
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
