use crate::{
    ast::{
        typed::{BinOp as Top, BoolOp, CmpOp, IntOp},
        untyped::{BinOp as Uop, *},
        TypeSpec,
    },
    span::Span,
};

use super::error::*;

/// Describes how the domain (input) types of an operation relate to its codomain (output) type.
enum Operation {
    /// An associative operation; `a -> a -> a`.
    /// Its input types are the same as the output type.
    Semigroup(TypeSpec, Top),
    /// A binary function (?). It may accept several input types, as long as both input parameters
    /// are of the same type. The output type is always the same, regardless of the input type.
    ///
    /// For instance:
    /// ```
    /// a -> a -> r
    /// b -> b -> r
    /// [...]
    /// ```
    /// The first, vector field specifies the input types that are accepted.
    /// The `TypeSpec` field specifies the output type that is produced.
    BinFunc(Vec<TypeSpec>, TypeSpec, Top),
}

/// Checks whether an operation can be applied to its operands.
pub struct BinOpChecker {
    op: BinOp,
    lhs_type: TypeSpec,
    rhs_type: TypeSpec,
    lhs_span: Span,
    rhs_span: Span,
    lhs_err: Option<TypeError>,
    rhs_err: Option<TypeError>,
}
impl BinOpChecker {
    /// Given an operation and two well-typed operands, checks whether the operation can be applied
    /// to its operands.
    pub fn check(
        op: BinOp,
        lhs_type: TypeSpec,
        rhs_type: TypeSpec,
        lhs_span: Span,
        rhs_span: Span,
    ) -> Result<(TypeSpec, Top), Vec<TypeError>> {
        Self {
            op,
            lhs_type,
            rhs_type,
            lhs_span,
            rhs_span,
            lhs_err: None,
            rhs_err: None,
        }
        .check_internal()
    }

    fn check_internal(mut self) -> Result<(TypeSpec, Top), Vec<TypeError>> {
        let op_tuple = match self.get_op() {
            Operation::Semigroup(ty, op) => {
                if self.lhs_type != ty {
                    self.err_lhs();
                }
                if self.rhs_type != ty {
                    self.err_rhs();
                }
                (ty, op)
            }
            Operation::BinFunc(source_types, ret, op) => {
                if source_types.contains(&self.lhs_type) {
                    if self.rhs_type != self.lhs_type {
                        self.err_rhs();
                    }
                } else {
                    self.err_lhs();
                }
                (ret, op)
            }
        };

        let errs = self.collect_errors();
        if errs.is_empty() {
            Ok(op_tuple)
        } else {
            Err(errs)
        }
    }

    /// Determines the operation type for the operator we've received.
    fn get_op(&self) -> Operation {
        use TypeSpec::*;
        use Uop::*;
        match self.op {
            // bool or bool -> bool
            Or => Operation::Semigroup(Bool, Top::Bool(BoolOp::Or)),
            And => Operation::Semigroup(Bool, Top::Bool(BoolOp::And)),
            // int == int -> bool
            Equal => Operation::BinFunc(vec![Int, Bool], Bool, Top::Compare(CmpOp::Equal)),
            NotEqual => Operation::BinFunc(vec![Int, Bool], Bool, Top::Compare(CmpOp::NotEqual)),
            // int + int -> int
            Add => Operation::Semigroup(Int, Top::IntArith(IntOp::Add)),
            Subtract => Operation::Semigroup(Int, Top::IntArith(IntOp::Subtract)),
            Multiply => Operation::Semigroup(Int, Top::IntArith(IntOp::Multiply)),
            IntDiv => Operation::Semigroup(Int, Top::IntArith(IntOp::Divide)),
            Remainder => Operation::Semigroup(Int, Top::IntArith(IntOp::Remainder)),
            // int < int -> bool
            LessThan => Operation::BinFunc(vec![Int], Bool, Top::Compare(CmpOp::LessThan)),
            GreaterThan => Operation::BinFunc(vec![Int], Bool, Top::Compare(CmpOp::GreaterThan)),
            LessThanEqual => {
                Operation::BinFunc(vec![Int], Bool, Top::Compare(CmpOp::LessThanEqual))
            }
            GreaterThanEqual => {
                Operation::BinFunc(vec![Int], Bool, Top::Compare(CmpOp::GreaterThanEqual))
            }
            Is => todo!("Implement 'is' type check"),
        }
    }

    fn err_lhs(&mut self) {
        self.lhs_err = Some(TypeError::new(
            TypeErrorKind::BinOperandType(self.op, self.lhs_type.clone()),
            self.lhs_span,
        ));
    }
    fn err_rhs(&mut self) {
        self.rhs_err = Some(TypeError::new(
            TypeErrorKind::BinOperandType(self.op, self.rhs_type.clone()),
            self.rhs_span,
        ));
    }

    fn collect_errors(self) -> Vec<TypeError> {
        self.lhs_err
            .into_iter()
            .chain(self.rhs_err.into_iter())
            .collect()
    }
}
