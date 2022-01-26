//! Untyped Abstract Syntax Tree nodes.
use std::fmt::{self, Display};

use crate::span::Span;

use super::TypeSpec;

#[derive(Debug, Default)]
pub struct Program {
    pub var_defs: Vec<VarDef>,
    pub statements: Vec<Statement>,
}
impl Program {
    pub fn new() -> Self {
        Self {
            var_defs: vec![],
            statements: vec![],
        }
    }

    pub fn add_var_def(&mut self, var_def: VarDef) {
        self.var_defs.push(var_def);
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}
impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for var in &self.var_defs {
            writeln!(f, "{}", var)?;
        }
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct VarDef {
    pub name: String,
    pub type_spec: TypeSpec,
    pub value: Literal,
    pub span: Span,
}
impl Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {} = {}", self.name, self.type_spec, self.value)
    }
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<StmtKind>,
}
impl Block {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}

#[derive(Debug)]
pub struct If {
    pub expression: ExprKind,
    pub block: Block,
    pub elifs: Vec<Elif>,
    pub else_block: Option<Block>,
}

#[derive(Debug)]
pub struct Elif {
    pub expression: ExprKind,
    pub block: Block,
}

#[derive(Debug)]
pub struct Statement {
    pub span: Span,
    pub stmt_kind: StmtKind,
}
impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.stmt_kind)
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Pass,
    Evaluate(Expr),
    Return(Option<Expr>),
    Assign(Assign),
}
impl Display for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StmtKind::*;
        match self {
            Pass => f.write_str("pass\n"),
            Evaluate(expr) => writeln!(f, "{}", expr),
            Return(None) => f.write_str("return\n"),
            Return(Some(expr)) => writeln!(f, "return {}", expr),
            Assign(assign) => writeln!(f, "{}", assign),
        }
    }
}

#[derive(Debug)]
pub struct Assign {
    pub target: Expr,
    pub value: Expr,
    pub span: Span,
}
impl Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.target, self.value)
    }
}
#[derive(Debug)]
pub struct Expr {
    pub expr_kind: ExprKind,
    pub span: Span,
}
impl Expr {
    pub fn new(expr_kind: ExprKind, span: Span) -> Self {
        Self { expr_kind, span }
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr_kind)
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(String),
    Member(Box<MemberExpr>),
    Index(Box<IndexExpr>),
    FunctionCall(Box<FunCallExpr>),
    MethodCall(Box<MetCallExpr>),
    Unary(Box<UnExpr>),
    Binary(Box<BinExpr>),
    Ternary(Box<TerExpr>),
}
impl Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Literal(lit) => write!(f, "{}", lit),
            Identifier(id) => write!(f, "{}", id),
            Member(mem) => write!(f, "{}", mem),
            Index(idx) => write!(f, "{}", idx),
            FunctionCall(fun) => write!(f, "{}", fun),
            MethodCall(met) => write!(f, "{}", met),
            Binary(bin) => write!(f, "{}", bin),
            Unary(un) => write!(f, "{}", un),
            Ternary(un) => write!(f, "{}", un),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i32),
    Boolean(bool),
    None,
}
impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Integer(i) => write!(f, "{}", i),
            Boolean(true) => write!(f, "True"),
            Boolean(false) => write!(f, "False"),
            None => write!(f, "None"),
        }
    }
}

#[derive(Debug)]
pub struct FunCallExpr {
    pub name: String,
    pub name_span: Span,
    pub params: Expr,
}
impl Display for FunCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}({}))", self.name, self.params)
    }
}

#[derive(Debug)]
pub struct MetCallExpr {
    pub member: MemberExpr,
    pub params: Expr,
}
impl Display for MetCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}({}))", self.member, self.params)
    }
}

#[derive(Debug)]
pub struct MemberExpr {
    pub lhs: Expr,
    pub rhs: String,
}
impl Display for MemberExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}.{})", self.lhs, self.rhs)
    }
}

#[derive(Debug)]
pub struct IndexExpr {
    pub lhs: Expr,
    pub rhs: Expr,
}
impl Display for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}[{}])", self.lhs, self.rhs)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
        write!(f, "({} {} {})", self.lhs, self.op, self.rhs)
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
    // Keywords
    Or,
    And,
    Is,
}
impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinOp::*;
        let ch = match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            IntDiv => "//",
            Remainder => "%",
            LessThan => "<",
            GreaterThan => ">",
            LessThanEqual => "<=",
            GreaterThanEqual => ">=",
            Equal => "==",
            NotEqual => "!=",
            Or => "or",
            And => "and",
            Is => "is",
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
impl Display for TerOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TerOp::If => f.write_str("if"),
        }
    }
}
