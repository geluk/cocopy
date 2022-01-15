use std::fmt::{self, Display};

pub struct Program {}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Unary(Box<UnExpr>),
    Binary(Box<BinExpr>),
    Ternary(Box<TerExpr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Identifier(id) => write!(f, "{}", id),
            Expr::Binary(bin) => write!(f, "{}", bin),
            Expr::Unary(un) => write!(f, "{}", un),
            Expr::Ternary(un) => write!(f, "{}", un),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i32),
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Boolean(true) => write!(f, "True"),
            Literal::Boolean(false) => write!(f, "False"),
        }
    }
}

#[derive(Debug)]
pub struct UnExpr {
    pub op: UnOp,
    pub rhs: Expr,
}

impl Display for UnExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", self.op, self.rhs)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    Negate,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("not")
    }
}

#[derive(Debug)]
pub struct BinExpr {
    pub lhs: Expr,
    pub op: BinOp,
    pub rhs: Expr,
}

impl Display for BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            BinOp::MemberAccess => write!(f, "({}{}{})", self.lhs, self.op, self.rhs),
            BinOp::Index => write!(f, "{}[{}]", self.lhs, self.rhs),
            BinOp::FunctionCall => write!(f, "{}({})", self.lhs, self.rhs),
            _ => write!(f, "({} {} {})", self.lhs, self.op, self.rhs),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Symbols
    Add,
    Subtract,
    Multiply,
    IntDiv,
    Remainder,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
    MemberAccess,
    Index,
    FunctionCall,
    // Keywords
    Or,
    And,
    Is,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ch = match self {
            BinOp::Add => "+",
            BinOp::Subtract => "-",
            BinOp::Multiply => "*",
            BinOp::IntDiv => "//",
            BinOp::Remainder => "%",
            BinOp::LessThan => "<",
            BinOp::GreaterThan => ">",
            BinOp::LessThanEqual => "<=",
            BinOp::GreaterThanEqual => ">=",
            BinOp::Equal => "==",
            BinOp::NotEqual => "!=",
            BinOp::MemberAccess => ".",
            BinOp::Index => "[]",
            BinOp::Or => "or",
            BinOp::And => "and",
            BinOp::Is => "is",
            BinOp::FunctionCall => "()",
        };
        f.write_str(ch)
    }
}

#[derive(Debug)]
pub struct TerExpr {
    pub lhs: Expr,
    pub op: TerOp,
    pub mhs: Expr,
    pub rhs: Expr,
}

impl Display for TerExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            TerOp::If => write!(f, "({} if {} else {})", self.lhs, self.mhs, self.rhs),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TerOp {
    If,
}
