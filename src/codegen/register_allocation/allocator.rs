use std::fmt::Debug;
use std::hash::Hash;
use std::slice::Iter;
use std::{collections::HashSet, fmt::Display};

use crate::listing::Position;

use super::allocation::{Allocation, BaseOffset, Lifetime, RegAllocation, StackAllocation};

/// Assigns variable names to registers or to a position on the stack.
/// Also exposes operations to definitively assign a name to a certain
/// register in case the operational semantics of an instruction require it.
#[derive(Debug)]
pub struct Allocator<N: Eq, R: Copy + Eq> {
    registers: Vec<R>,
    register_allocations: Vec<RegAllocation<N, R>>,
    stack_allocations: Vec<StackAllocation<N>>,
}

/// A destination in which the value of a variable will be held.
/// Values can be stored in a register or on the stack.
#[derive(Debug)]
pub enum Destination<N: Eq, R: Copy + Eq> {
    Reg(RegAllocation<N, R>),
    Stack(StackAllocation<N>),
}
impl<N: Eq, R: Copy + Eq + Display> Destination<N, R> {
    pub fn describe(&self) -> String {
        match self {
            Destination::Reg(reg) => format!("{}", reg.register()),
            Destination::Stack(ofs) => format!("stack offset {:?}", ofs.offset()),
        }
    }
}

impl<N: Eq + Clone + Debug, R: Copy + Eq + Hash + Debug> Allocator<N, R> {
    pub fn new(registers: Vec<R>) -> Self {
        Self {
            registers,
            register_allocations: Default::default(),
            stack_allocations: Default::default(),
        }
    }

    /// Assign a register or a stack position to the given variable during the
    /// provided lifetime. Variables are preferentially assigned to a register,
    /// but if no free registers remain, they will be stored on the stack.
    /// Panics if a conflicting allocation exists. An allocation is considered
    /// to conflict if it applies to the same variable and its lifetime overlaps
    /// the provided lifetime.
    pub fn allocate(&mut self, name: N, lifetime: Lifetime) -> Destination<N, R> {
        assert!(
            !self.has_conflicting_allocation(&name, lifetime),
            "Conflicting allocations for {:?}",
            name
        );

        self.allocate_unchecked(name, lifetime)
    }

    /// Lock a variable with the provided lifetime to the given target register.
    /// The allocator will guarantee that its value will be available in the
    /// target register at the point specified by `lock_point`, but it may
    /// choose to store the variable elsewhere before and/or after this point
    /// if another variable also needs to be locked to the same register.
    /// If this happens, a swap instruction is emitted.
    pub fn lock_to(&mut self, name: &N, lifetime: Lifetime, lock_point: Position, target_reg: R) {
        let stack_allocation = self
            .stack_allocations
            .iter()
            .find(|a| a.conflicts_with(name, lifetime))
            .cloned();

        let reg_allocations: Vec<_> = self
            .register_allocations
            .iter()
            .filter(|a| a.register() == target_reg && a.lifetime().overlaps(lifetime))
            .cloned()
            .collect();

        if reg_allocations.is_empty() {
            if let Some(a) = stack_allocation {
                self.drop_stack_alloc(&a);
            }
            self.add_reg_alloc(name.clone(), target_reg, lifetime, vec![lock_point]);
        } else if reg_allocations.iter().any(|a| a.name() == name) {
            // This name is already assigned to the right register.
            // That means its lifetime should agree with ours, which means
            // it should be the only existing allocation:
            assert_eq!(
                reg_allocations.len(),
                1,
                "Conflicting allocations for {:?}",
                target_reg,
            );
            let existing_alloc = reg_allocations.first().unwrap();
            // Furthermore, the lifetimes should match up. If they don't,
            // our caller has specified the wrong lifetime.
            assert_eq!(
                existing_alloc.lifetime(),
                lifetime,
                "Lifetime mismatch between existing: {:?} and new: {:?}",
                existing_alloc.lifetime(),
                lifetime
            );
            // Lastly, there should not be a conflicting stack allocation in
            // this case.
            assert!(stack_allocation.is_none())
        } else {
            for allocation in reg_allocations {
                if allocation.locks().is_empty() {
                    // Another name is already allocated to this register,
                    // but it is not locked. Find it a new place.
                    // We should do this first before dropping the old
                    // allocation, so we'll use allocate_unchecked here.
                    self.allocate_unchecked(allocation.name().clone(), allocation.lifetime());
                    self.drop_reg_alloc(&allocation);
                } else {
                    // Another name is already allocated definitively to this
                    // register.
                    todo!("Deal with conflicting locked allocations");
                }
            }

            self.add_reg_alloc(name.clone(), target_reg, lifetime, vec![lock_point]);
        }
    }

    /// Assign stack offsets to stack-allocated variables. This must be done
    /// after register assignment is complete, otherwise not all stack-allocated
    /// variables may receive an offset.
    pub fn assign_stack_offsets(&mut self) {
        // TODO: this assignment is naïve, as it does not pack non-overlapping
        // lifetimes at the same stack offset.
        for (index, allocation) in self.stack_allocations.iter_mut().enumerate() {
            allocation.set_offset(BaseOffset::words(index));
        }
    }

    /// Lookup the storage destination for a variable at the given location.
    pub fn lookup(&self, name: &N, position: Position) -> Destination<N, R> {
        self.register_allocations
            .iter()
            .find(|a| a.contains(name, position))
            .map(|a| Destination::Reg(a.clone()))
            .or_else(|| {
                self.stack_allocations
                    .iter()
                    .find(|a| a.contains(name, position))
                    .map(|a| Destination::Stack(a.clone()))
            })
            .unwrap_or_else(|| panic!("Could not find allocation for {:?}", name))
    }

    /// Returns `true` if an allocation exists that conflicts with the given
    /// name and lifetime.
    pub fn has_conflicting_allocation(&self, name: &N, lifetime: Lifetime) -> bool {
        let test_regs = || {
            self.register_allocations
                .iter()
                .any(|a| a.conflicts_with(name, lifetime))
        };
        let test_stack = || {
            self.stack_allocations
                .iter()
                .any(|a| a.conflicts_with(name, lifetime))
        };
        test_regs() || test_stack()
    }

    pub fn iter_reg_allocations(&self) -> Iter<RegAllocation<N, R>> {
        self.register_allocations.iter()
    }

    /// Assign a register or a stack position to the given variable during the
    /// provided lifetime. Unlike [`Self::allocate()`], it will accept
    /// conflicting allocations. The caller is responsible for resolving
    /// these conflicts afterwards.
    fn allocate_unchecked(&mut self, name: N, lifetime: Lifetime) -> Destination<N, R> {
        let occupied_regs = self.occupied_registers(lifetime);
        let free_reg = self
            .registers
            .iter()
            .find(|r| !occupied_regs.contains(r))
            .copied();

        match free_reg {
            Some(register) => {
                Destination::Reg(self.add_reg_alloc(name, register, lifetime, vec![]).clone())
            }
            None => {
                // Stack allocations are deferred: their position on the stack
                // will not be determined yet, as these allocations may continue
                // to be added and removed while register allocation is taking
                // place. Once all registers have been definitively assigned,
                // we can determine the maximum number of concurrent stack
                // allocations in order to determine how big our stack frame
                // will need to be.
                let allocation = StackAllocation::new(name, lifetime);
                self.stack_allocations.push(allocation.clone());
                Destination::Stack(allocation)
            }
        }
    }

    /// Add a new register allocation with the given parameters.
    fn add_reg_alloc(
        &mut self,
        name: N,
        register: R,
        lifetime: Lifetime,
        locks: Vec<Position>,
    ) -> &RegAllocation<N, R> {
        self.register_allocations
            .push(RegAllocation::new(name, register, lifetime, locks));
        self.register_allocations.last().unwrap()
    }

    /// Remove the provided register allocation.
    fn drop_reg_alloc(&mut self, allocation: &RegAllocation<N, R>) {
        self.register_allocations.retain(|e| e != allocation)
    }

    /// Remove the provided stack allocation.
    fn drop_stack_alloc(&mut self, allocation: &StackAllocation<N>) {
        self.stack_allocations.retain(|a| a != allocation)
    }

    /// Returns a set containing the registers that are occupied at some point
    /// during the given lifetime.
    fn occupied_registers(&self, lifetime: Lifetime) -> HashSet<R> {
        self.register_allocations
            .iter()
            .filter(|a| a.lifetime().overlaps(lifetime))
            .map(|a| a.register())
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

    macro_rules! assert_allocates_reg {
        ($alloc:expr, $reg:expr) => {
            assert_matches!($alloc, Destination::Reg(r) if r.register() == $reg)
        };
    }

    macro_rules! assert_allocates_stack {
        ($alloc:expr) => {
            assert_matches!($alloc, Destination::Stack(_))
        };
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

        let allocation = allocator.lookup(&"two", Position(5));

        assert_allocates_reg!(allocation, Reg::B);
    }

    #[test]
    pub fn lookup_returns_allocated_stack_destination() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "a", 0, 10);
        allocate!(allocator, "b", 1, 10);
        allocate!(allocator, "c", 1, 10);
        allocate!(allocator, "d", 1, 10);
        allocate!(allocator, "s", 9, 15);

        let allocation = allocator.lookup(&"s", Position(9));

        assert_allocates_stack!(allocation);
    }

    #[test]
    pub fn lock_to_shifts_existing_register() {
        let mut allocator = make_allocator!();

        allocate!(allocator, "one", 0, 10);

        allocator.lock_to(&"two", lifetime!(4, 14), Position(4), Reg::A);

        let one = allocator.lookup(&"one", Position(0));
        let two = allocator.lookup(&"two", Position(4));

        assert_allocates_reg!(one, Reg::B);
        assert_allocates_reg!(two, Reg::A);
    }
}
