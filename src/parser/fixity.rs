//! Operator associativity and precedence.
use std::cmp::Ordering;

use crate::ast::untyped::{BinOp, TerOp, UnOp};

/// An N-ary operator, with N > 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NAryOp {
    Binary(BinOp),
    Ternary(TerOp),
    Member,
    Index,
    FunctionCall,
}

pub type Precedence = u8;

/// A combination of associativity and precedence
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fixity {
    assoc: Assoc,
    precedence: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
    Left,
    None,
    Right,
}
impl Fixity {
    pub fn none() -> Self {
        Self {
            assoc: Assoc::None,
            precedence: 0,
        }
    }

    pub fn for_unop(op: UnOp) -> Self {
        let (assoc, precedence) = match op {
            UnOp::Not => (Assoc::None, 4),
            UnOp::Negate => (Assoc::None, 8),
        };
        Self { assoc, precedence }
    }

    pub fn for_n_ary_op(op: NAryOp) -> Self {
        match op {
            NAryOp::Binary(bin) => Self::for_binop(bin),
            NAryOp::Ternary(ter) => Self::for_terop(ter),
            NAryOp::Member | NAryOp::Index | NAryOp::FunctionCall => Self {
                assoc: Assoc::Left,
                precedence: 9,
            },
        }
    }

    pub fn for_binop(op: BinOp) -> Self {
        let (assoc, precedence) = match op {
            BinOp::Or => (Assoc::Left, 2),
            BinOp::And => (Assoc::Left, 3),
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::LessThanEqual
            | BinOp::GreaterThanEqual
            | BinOp::Equal
            | BinOp::NotEqual
            | BinOp::Is => (Assoc::None, 5),
            BinOp::Add | BinOp::Subtract => (Assoc::Left, 6),
            BinOp::Multiply | BinOp::IntDiv | BinOp::Remainder => (Assoc::Left, 7),
        };
        Self { assoc, precedence }
    }

    pub fn for_terop(op: TerOp) -> Self {
        let (assoc, precedence) = match op {
            TerOp::If => (Assoc::Right, 1),
        };
        Self { assoc, precedence }
    }

    /// Assuming `self` is found in left-hand position, and `rhs` is found in right-hand position,
    /// returns whether `self` precedes `other`.
    pub fn precedes_rhs(&self, rhs: &Fixity) -> bool {
        match self.precedence.cmp(&rhs.precedence) {
            Ordering::Greater => true,
            Ordering::Equal => self.assoc == Assoc::Left,
            Ordering::Less => false,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn higher_precedence_always_precedes_lower_precedence() {
        let higher = Fixity {
            assoc: Assoc::Left,
            precedence: 2,
        };
        let lower = Fixity {
            assoc: Assoc::Right,
            precedence: 1,
        };

        assert!(higher.precedes_rhs(&lower));
        assert!(!lower.precedes_rhs(&higher));
    }

    #[test]
    fn lower_precedence_never_precedes_higher_precedence() {
        let higher = Fixity {
            assoc: Assoc::Right,
            precedence: 2,
        };
        let lower = Fixity {
            assoc: Assoc::Left,
            precedence: 1,
        };

        assert!(!lower.precedes_rhs(&higher));
        assert!(higher.precedes_rhs(&lower));
    }

    #[test]
    fn lhs_precedes_rhs_when_left_associative_with_equal_precedence() {
        let lhs = Fixity {
            assoc: Assoc::Left,
            precedence: 2,
        };
        let rhs = Fixity {
            assoc: Assoc::Left,
            precedence: 2,
        };

        assert!(lhs.precedes_rhs(&rhs))
    }

    #[test]
    fn rhs_precedes_lhs_when_right_associative_with_equal_precedence() {
        let lhs = Fixity {
            assoc: Assoc::Right,
            precedence: 2,
        };
        let rhs = Fixity {
            assoc: Assoc::Right,
            precedence: 2,
        };

        assert!(!lhs.precedes_rhs(&rhs))
    }
}
