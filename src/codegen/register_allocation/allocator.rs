use std::collections::hash_map::{Entry, Iter};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

use log::trace;

use crate::listing::Position;

use super::allocation::*;

/// Assigns variable names to registers or to a position on the stack.
/// Also exposes operations to definitively assign a name to a certain
/// register in case the operational semantics of an instruction require it.
#[derive(Debug)]
pub struct Allocator<N: Eq, R: Copy + Eq> {
    registers: Vec<R>,
    name_allocations: HashMap<N, NameAllocation<R>>,
}

impl<N: Eq + Clone + Debug + Hash, R: Copy + Eq + Hash + Debug> Allocator<N, R> {
    pub fn new(registers: Vec<R>) -> Self {
        Self {
            registers,
            name_allocations: Default::default(),
        }
    }

    /// Assign the given name to a register or stack position within the
    /// provided lifetime. Names are preferentially assigned to a register,
    /// but if no free registers remain, they will be stored on the stack.
    /// Panics if the name was already allocated before.
    pub fn allocate(&mut self, name: N, lifetime: Lifetime) -> &mut NameAllocation<R> {
        let allocation = self.allocate_unchecked(lifetime);

        match self.name_allocations.entry(name) {
            Entry::Occupied(ocp) => {
                panic!(
                    "Attempted to allocate '{:?}', but it was already allocated.",
                    ocp.key()
                );
            }
            Entry::Vacant(vac) => vac.insert(allocation),
        }
    }

    /// Lock a name to the given target register.
    /// The allocator will guarantee that its value will be available in the
    /// target register at the point specified by `lock_point`, but it may
    /// choose to store the variable elsewhere before and/or after this point
    /// if there are multiple contenders for the same register.
    /// If this happens, a swap instruction is emitted.
    pub fn lock_to(&mut self, name: &N, lock_point: Position, target_reg: R) {
        // Temporarily taking the allocation out of the map is the easiest way
        // to allow us to mutate it while holding references to other
        // allocations. We just need to make sure to put it back!
        let (name, mut allocation) = self
            .name_allocations
            .remove_entry(name)
            .unwrap_or_else(|| panic!("Attempted to lock unallocated name '{name:?}'"));

        let alloc_lifetime = allocation.lifetime();
        trace!("Try to lock {name:?} (with lifetime {alloc_lifetime}) to {target_reg:?} at {lock_point}");

        // Maybe we already assigned it to the right register by sheer dumb luck?
        if allocation
            .reg_slice_at(lock_point)
            .expect("Attempted to look up invalid position")
            .register()
            == target_reg
        {
            // Well now that's convenient!
            trace!("No need to lock {name:?} to {target_reg:?}, it's already there");
            // Don't forget to put it back though:
            self.name_allocations.insert(name, allocation);
            // Cave Johnson, we're done here.
            return;
        }

        let slice_in_question = allocation.reg_slice_at_mut(lock_point);

        // In keeping with the trend of trying the laziest thing that could
        // possibly work first, maybe we can just move our entire allocation
        // slice over to the target register?
        let lifetime = slice_in_question.lifetime();
        if !self
            .name_allocations
            .values()
            .any(|a| a.conflicts_on_lifetime(lifetime, target_reg))
        {
            // Yes We Can!
            trace!("No conflicting allocations on {target_reg:?}, moving entire slice");
            slice_in_question.move_and_lock_to(target_reg, lock_point);
            self.name_allocations.insert(name, allocation);
            return;
        }

        let conflicting_names: Vec<_> = self
            .name_allocations
            .iter()
            .filter(|(_, a)| a.conflicts_on_lifetime(lifetime, target_reg))
            .map(|(n, a)| (n.clone(), a.lifetime()))
            .collect();

        // We have some overlapping allocations. Let's deal with them one by one.
        for (name, lifetime) in conflicting_names {
            // If it's not locked to the target register, we can just kick it out.
            let free_registers = self.free_registers(lifetime);

            let alloc = self.name_allocations.get_mut(&name).unwrap();
            if alloc.try_kick_from(target_reg, lifetime, free_registers) {
                // Hey, that worked!
                continue;
            }
            // Can't kick it out, so we'll need to split some slices here.
            // This is difficult, do this later.
            todo!("Overlapping allocation has a slice locked to the target register");
        }

        // Now we have no more overlapping allocations, so we can move our slice.
        trace!("Removed conflicting allocations on {target_reg:?}, moving entire slice");
        slice_in_question.move_and_lock_to(target_reg, lock_point);
        self.name_allocations.insert(name, allocation);
    }

    /// Assign stack offsets to stack-allocated variables. This must be done
    /// after register assignment is complete, otherwise not all stack-allocated
    /// variables may receive an offset.
    pub fn assign_stack_offsets(&mut self) {
        // TODO: This should really be part of a finalize() call that closes
        // the allocator and produces a new type, `Allocations` or something.

        // This could be done better, by packing non-overlapping lifetimes into
        // the stack, but this way is Good Enough™.
        for (index, allocation) in self
            .name_allocations
            .values_mut()
            .filter(|a| a.has_stack_allocation())
            .enumerate()
        {
            for stack_alloc in allocation.iter_stack_slices_mut() {
                stack_alloc.set_offset(BaseOffset::words(index));
            }
        }
    }

    /// Lookup the storage destination for a variable at the given location.
    pub fn lookup(&self, name: &N, position: Position) -> Destination<R> {
        trace!("Look up {position} for {name:?}");
        self.name_allocations[name].slice_at(position)
    }

    pub fn iter_allocations(&self) -> Iter<N, NameAllocation<R>> {
        self.name_allocations.iter()
    }

    pub fn live_regs_at(&self, position: Position) -> Vec<&RegAllocation<R>> {
        self.name_allocations
            .values()
            .filter_map(|a| a.reg_slice_at(position))
            .collect()
    }

    /// Assign a register or a stack position to the given variable during the
    /// provided lifetime. Unlike [`Self::allocate()`], it will accept
    /// conflicting allocations. The caller is responsible for resolving
    /// these conflicts afterwards.
    fn allocate_unchecked(&mut self, lifetime: Lifetime) -> NameAllocation<R> {
        let next_free = self.free_registers(lifetime).into_iter().next();
        match next_free {
            Some(register) => {
                NameAllocation::from_reg(RegAllocation::new(register, lifetime, vec![]))
            }
            None => {
                // Stack allocations are deferred: their position on the stack
                // will not be determined yet, as these allocations may continue
                // to be added and removed while register allocation is taking
                // place. Once all registers have been definitively assigned,
                // we can determine the maximum number of concurrent stack
                // allocations in order to determine how big our stack frame
                // will need to be.
                let allocation = StackAllocation::new(lifetime);
                NameAllocation::from_stack(allocation)
            }
        }
    }

    fn free_registers(&self, lifetime: Lifetime) -> Vec<R> {
        let occupied_regs = self.occupied_registers(lifetime);
        self.registers
            .iter()
            .copied()
            .filter(|r| !occupied_regs.contains(r))
            .collect()
    }

    /// Returns a set containing the registers that are occupied at some point
    /// during the given lifetime.
    fn occupied_registers(&self, lifetime: Lifetime) -> HashSet<R> {
        self.name_allocations
            .values()
            .flat_map(|v| v.reg_slices_within(lifetime))
            .map(|s| s.register())
            .collect()
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::{assert_matches::assert_matches, slice::Iter};

    #[test]
    pub fn lifetime_contained_in_other() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(2), Position(3));

        assert!(one.overlaps(two));
        assert!(two.overlaps(one));
    }

    #[test]
    pub fn lifetime_ends_overlap() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(9), Position(15));

        assert!(one.overlaps(two));
        assert!(two.overlaps(one));
    }

    #[test]
    pub fn lifetime_no_overlap() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(11), Position(15));

        assert!(!one.overlaps(two));
        assert!(!two.overlaps(one));
    }

    #[test]
    pub fn lifetime_touches() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(10), Position(15));

        assert!(!one.overlaps(two));
        assert!(!two.overlaps(one));
    }

    #[test]
    pub fn lifetime_equal() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(1), Position(10));

        assert!(one.overlaps(two));
        assert!(two.overlaps(one));
    }

    #[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    enum Reg {
        A,
        B,
        C,
        D,
    }
    impl Reg {
        pub fn iter() -> Iter<'static, Self> {
            use self::Reg::*;
            [A, B, C, D].iter()
        }
    }

    macro_rules! lifetime {
        ($start:expr, $end:expr) => {
            Lifetime::new(Position($start), Position($end))
        };
    }

    macro_rules! make_allocator {
        () => {
            Allocator::new(Reg::iter().copied().collect())
        };
    }

    macro_rules! assert_is_reg {
        ($dst:expr, $reg:expr) => {
            assert_matches!($dst, Destination::Reg(r) if r.register() == $reg);
        };
    }

    macro_rules! assert_is_stack {
        ($dst:expr) => {{
            assert_matches!($dst, Destination::Stack(_));
        }};
    }

    macro_rules! assert_allocates_reg {
        ($alloc:expr, $reg:expr) => {{
            let alloc = $alloc;
            let first_slice = alloc.slice_at(alloc.lifetime().start());
            assert_is_reg!(first_slice, $reg);
        }};
    }

    macro_rules! assert_allocates_stack {
        ($alloc:expr) => {{
            let alloc = $alloc;
            let first_slice = alloc.slice_at(alloc.lifetime().start());
            assert_is_stack!(first_slice);
        }};
    }

    macro_rules! allocate {
        ($allocator:expr, $name:expr, $start:expr, $end:expr) => {
            $allocator.allocate($name, lifetime!($start, $end))
        };
    }

    #[test]
    pub fn allocate_uses_first_register_for_first_allocation() {
        let mut allocator = make_allocator!();

        let allocation = allocate!(allocator, "foo", 0, 10);

        assert_allocates_reg!(allocation, Reg::A);
    }

    #[test]
    pub fn allocate_uses_first_free_register() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "one", 0, 10);
        allocate!(allocator, "two", 1, 10);
        let allocation = allocate!(allocator, "three", 9, 15);

        assert_allocates_reg!(allocation, Reg::C);
    }

    #[test]
    pub fn allocate_allows_non_overlapping_allocations_on_same_register() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "foo", 0, 10);
        let allocation = allocate!(allocator, "bar", 10, 20);

        assert_allocates_reg!(allocation, Reg::A);
    }

    #[test]
    pub fn allocate_spills_to_stack_when_regs_full() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "a", 0, 10);
        allocate!(allocator, "b", 1, 10);
        allocate!(allocator, "c", 1, 10);
        allocate!(allocator, "d", 1, 10);
        let allocation = allocate!(allocator, "stack", 9, 15);

        assert_allocates_stack!(allocation);
    }

    #[test]
    #[should_panic]
    pub fn allocate_panics_on_conflicting_allocations() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "a", 0, 10);
        allocate!(allocator, "a", 9, 20);
    }

    #[test]
    pub fn lookup_returns_allocated_register_destination() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "one", 0, 10);
        allocate!(allocator, "two", 1, 10);
        allocate!(allocator, "three", 9, 15);

        let destination = allocator.lookup(&"two", Position(5));

        assert_is_reg!(destination, Reg::B);
    }

    #[test]
    pub fn lookup_returns_allocated_stack_destination() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "a", 0, 10);
        allocate!(allocator, "b", 1, 10);
        allocate!(allocator, "c", 1, 10);
        allocate!(allocator, "d", 1, 10);
        allocate!(allocator, "s", 9, 15);

        let destination = allocator.lookup(&"s", Position(9));

        assert_is_stack!(destination)
    }

    #[test]
    pub fn lock_to_shifts_existing_register() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "one", 0, 10);
        allocate!(allocator, "two", 4, 14);

        allocator.lock_to(&"two", Position(4), Reg::A);

        let one = allocator.lookup(&"one", Position(0));
        let two = allocator.lookup(&"two", Position(4));

        assert_is_reg!(one, Reg::B);
        assert_is_reg!(two, Reg::A);
    }
}
