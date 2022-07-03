use std::collections::{hash_map::Entry, HashMap, HashSet};

use super::Variable;

/// A set of variables. Mainly used for calculating the ɸ-function.
#[derive(Clone)]
pub struct Variables(HashMap<String, usize>);
impl Variables {
    /// Construct an empty set of variables.
    pub fn none() -> Self {
        Self(HashMap::new())
    }

    /// Construct a set of variables from a [`HashMap`] of names to subscripts.
    pub fn new(variables: HashMap<String, usize>) -> Self {
        Self(variables)
    }

    /// Construct a set containing a single variable.
    pub fn one(var: Variable) -> Self {
        let mut hashmap = HashMap::new();
        hashmap.insert(var.name, var.subscript);
        Self(hashmap)
    }

    /// Insert the given variable into the set. If a variable with this name already exists,
    /// its subscript is overwritten.
    pub fn insert(&mut self, var: Variable) {
        self.0.insert(var.name, var.subscript);
    }

    /// Collect the variables from an an iterator of variables, keeping the highest subscript
    /// for each variable.
    pub fn collect<I: Iterator<Item = Variables>>(iterator: I) -> Variables {
        iterator.fold(Variables::none(), |a, b| a.combine(b))
    }

    /// Combine this set of variables with another, producing a new set.
    /// When duplicate variables are encountered, the highest subscript is kept.
    pub fn combine(mut self, other: Variables) -> Self {
        for (name, value) in other.0 {
            match self.0.entry(name) {
                Entry::Occupied(mut ocp) => {
                    let existing = ocp.get_mut();
                    *existing = value.max(*existing);
                }
                Entry::Vacant(vac) => {
                    vac.insert(value);
                }
            }
        }
        self
    }

    /// Determines the ɸ-functions that should be emitted for the variables assigned
    /// in two branches.
    pub fn calculate_phi(mut self, mut branches: Vec<Variables>) -> Vec<(String, Vec<usize>)> {
        let branch_assignments = Self::get_all_branch_assignments(&branches);

        let mut phi_functions = vec![];

        for variable in branch_assignments {
            let parent_subscript = self.take_subscript(&variable);

            let mut branch_subs: Vec<_> = branches
                .iter_mut()
                .filter_map(|v| v.0.remove(&variable))
                .collect();

            // If some branch does not reassign the variable, it may still have its
            // original value so we must include its original subscript in the ɸ-function.
            if branch_subs.len() != branches.len() {
                branch_subs.push(parent_subscript);
            }

            phi_functions.push((variable, branch_subs));
        }

        phi_functions
    }

    /// Removes the given variable and returns its subscript.
    fn take_subscript(&mut self, variable: &str) -> usize {
        self.0
            .remove(variable)
            // If we ever allow implicit declaration, we'll need to do something here
            .expect("Variable assigned in branch was not declared before!")
    }

    fn get_all_branch_assignments(branches: &[Variables]) -> HashSet<String> {
        branches.iter().flat_map(|v| v.0.keys().cloned()).collect()
    }
}
