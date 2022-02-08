use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::{builtins::Builtin, span::Span};

use super::{
    untyped::{BinOp, Literal, Parameter, TerOp, UnOp},
    TypeSpec,
};

#[derive(Debug)]
pub struct Program {
    pub var_defs: Vec<VarDef>,
    pub func_defs: Vec<FuncDef>,
    pub statements: Vec<Statement>,
    pub used_builtins: Vec<Builtin>,
    pub global_environment: Environment,
}
impl Program {
    pub fn new() -> Self {
        Self {
            var_defs: vec![],
            func_defs: vec![],
            statements: vec![],
            used_builtins: vec![],
            global_environment: Environment::empty(),
        }
    }
}
impl Default for Program {
    fn default() -> Self {
        Self::new()
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
pub struct Environment {
    pub type_map: HashMap<String, TypeSpec>,
}
impl Environment {
    pub fn empty() -> Self {
        Self {
            type_map: HashMap::new(),
        }
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
pub struct FuncDef {
    pub name: String,
    pub return_type: TypeSpec,
    pub parameters: Vec<Parameter>,
    pub decl_span: Span,
    pub body: Block,
    pub span: Span,
}
impl Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "def {}{}:", self.name, self.return_type)
    }
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}
impl Block {
    pub fn new(statements: Vec<Statement>, span: Span) -> Self {
        Self { statements, span }
    }
}
impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct If {
    pub condition: Expr,
    pub body: Block,
    pub elifs: Vec<Elif>,
    pub else_body: Option<Block>,
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
    If(If),
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
            If(if_st) => {
                writeln!(f, "if {}:", if_st.condition)?;
                writeln!(f, "{}", if_st.body)
            }
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
    pub type_spec: TypeSpec,
    pub span: Span,
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

#[derive(Debug)]
pub struct FunCallExpr {
    pub name: String,
    pub name_span: Span,
    pub args: Vec<Expr>,
    pub args_span: Span,
    pub type_spec: TypeSpec,
}
impl Display for FunCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({}({}))",
            self.name,
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug)]
pub struct MetCallExpr {
    pub member: MemberExpr,
    pub args: Vec<Expr>,
    pub args_span: Span,
}
impl Display for MetCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({}({}))",
            self.member,
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
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
    pub type_spec: TypeSpec,
}
impl Display for UnExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", self.op, self.rhs)
    }
}

#[derive(Debug)]
pub struct BinExpr {
    pub lhs: Expr,
    pub op: BinOp,
    pub rhs: Expr,
    pub type_spec: TypeSpec,
}
impl Display for BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.op, self.rhs)
    }
}

#[derive(Debug)]
pub struct TerExpr {
    pub lhs: Expr,
    pub op: TerOp,
    pub mhs: Expr,
    pub rhs: Expr,
    pub type_spec: TypeSpec,
}
impl Display for TerExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            TerOp::If => write!(f, "({} if {} else {})", self.lhs, self.mhs, self.rhs),
        }
    }
}
