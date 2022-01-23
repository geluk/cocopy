use crate::parser::syntax_tree::*;

use super::error::*;

/// Describes how the domain (input) types of an operation relate to its codomain (output) type.
enum Operation {
    /// An associative operation; `a -> a -> a`.
    /// Its input types are the same as the output type.
    Semigroup(TypeSpec),
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
    BinFunc(Vec<TypeSpec>, TypeSpec),
    /// A function call. Its input and output types are determined by the type of the function.
    FunctionCall,
}

/// Checks whether an operation can be applied to its operands.
pub struct BinOpChecker<'a> {
    expr: &'a BinExpr,
    lhs_type: TypeSpec,
    rhs_type: TypeSpec,
    lhs_err: Option<TypeError>,
    rhs_err: Option<TypeError>,
}

impl<'a> BinOpChecker<'a> {
    /// Given an operation and two well-typed operands, checks whether the operation can be applied
    /// to its operands.
    pub fn check(
        expr: &'a BinExpr,
        lhs_type: TypeSpec,
        rhs_type: TypeSpec,
    ) -> Result<TypeSpec, Vec<TypeError>> {
        Self {
            expr,
            lhs_type,
            rhs_type,
            lhs_err: None,
            rhs_err: None,
        }
        .check_internal()
    }

    fn check_internal(mut self) -> Result<TypeSpec, Vec<TypeError>> {
        let ret_type = match self.get_op() {
            Operation::Semigroup(ty) => {
                if self.lhs_type != ty {
                    self.err_lhs();
                }
                if self.rhs_type != ty {
                    self.err_rhs();
                }
                ty
            }
            Operation::BinFunc(source_types, ret) => {
                if source_types.contains(&self.lhs_type) {
                    if self.rhs_type != self.lhs_type {
                        self.err_rhs();
                    }
                } else {
                    self.err_lhs();
                }
                ret
            }
            Operation::FunctionCall => {
                if let TypeSpec::Function(args, ret) = &self.lhs_type {
                    let ret = *ret.clone();
                    if args.len() != 1 {
                        self.err_rhs();
                    }
                    ret
                } else {
                    return Err(vec![TypeError::new(
                        TypeErrorKind::NotCallable(self.lhs_type.clone()),
                        self.expr.lhs.span,
                    )]);
                }
            }
        };

        let errs = self.collect_errors();
        if errs.is_empty() {
            Ok(ret_type)
        } else {
            Err(errs)
        }
    }

    /// Determines the operation type for the operator we've received.
    fn get_op(&self) -> Operation {
        use BinOp::*;
        use TypeSpec::*;
        match self.expr.op {
            // bool or bool -> bool
            Or | And => Operation::Semigroup(Bool),
            // One of:
            //   int == int -> bool
            //   bool == bool -> bool
            Equal | NotEqual => Operation::BinFunc(vec![Int, Bool], Bool),
            // int + int -> int
            Add | Subtract | Multiply | IntDiv | Remainder => Operation::Semigroup(Int),
            // int < int -> bool
            LessThan | GreaterThan | LessThanEqual | GreaterThanEqual => {
                Operation::BinFunc(vec![Int], Bool)
            }
            MemberAccess => todo!("Implement member access type checks"),
            Index => todo!("Implement array index type checks"),
            FunctionCall => Operation::FunctionCall,
            Is => todo!("Implement 'is' type check"),
        }
    }

    fn err_lhs(&mut self) {
        self.lhs_err = Some(TypeError::new(
            TypeErrorKind::BinOperandType(self.expr.op, self.lhs_type.clone()),
            self.expr.lhs.span,
        ));
    }
    fn err_rhs(&mut self) {
        self.rhs_err = Some(TypeError::new(
            TypeErrorKind::BinOperandType(self.expr.op, self.rhs_type.clone()),
            self.expr.rhs.span,
        ));
    }

    fn collect_errors(self) -> Vec<TypeError> {
        self.lhs_err
            .into_iter()
            .chain(self.rhs_err.into_iter())
            .collect()
    }
}
