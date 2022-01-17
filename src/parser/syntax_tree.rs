use std::{
    fmt::{self, Display},
    str::FromStr,
};

use super::error::Reason;

#[derive(Debug)]
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
            write!(f, "{}\n", var)?;
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
}
impl Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {} = {}", self.name, self.type_spec, self.value)
    }
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}
impl Block {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}

#[derive(Debug)]
pub struct If {
    pub expression: Expr,
    pub block: Block,
    pub elifs: Vec<Elif>,
    pub else_block: Option<Block>,
}

#[derive(Debug)]
pub struct Elif {
    pub expression: Expr,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    None,
    Int,
    Bool,
    Array(Box<TypeSpec>),
}
impl Display for TypeSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeSpec::None => f.write_str("<None>"),
            TypeSpec::Int => f.write_str("int"),
            TypeSpec::Bool => f.write_str("bool"),
            TypeSpec::Array(inner) => write!(f, "[{}]", inner),
        }
    }
}
impl FromStr for TypeSpec {
    type Err = Reason;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => TypeSpec::Int,
            "bool" => TypeSpec::Bool,
            _ => return Err(Reason::UnknownType(s.to_string())),
        })
    }
}

#[derive(Debug)]
pub enum Statement {
    Pass,
    Evaluate(Expr),
    Return(Option<Expr>),
    Assign(Assign),
}
impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Statement::*;
        match self {
            Pass => f.write_str("pass\n"),
            Evaluate(expr) => write!(f, "{}\n", expr),
            Return(None) => f.write_str("return\n"),
            Return(Some(expr)) => write!(f, "return {}\n", expr),
            Assign(assign) => write!(f, "{}\n", assign),
        }
    }
}

#[derive(Debug)]
pub struct Assign {
    pub target: Expr,
    pub value: Expr,
}
impl Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.target, self.value)
    }
}

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
        use Expr::*;
        match self {
            Literal(lit) => write!(f, "{}", lit),
            Identifier(id) => write!(f, "{}", id),
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
            BinOp::Index => write!(f, "({}[{}])", self.lhs, self.rhs),
            BinOp::FunctionCall => write!(f, "({}({}))", self.lhs, self.rhs),
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
            MemberAccess => ".",
            Index => "[]",
            Or => "or",
            And => "and",
            Is => "is",
            FunctionCall => "()",
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
