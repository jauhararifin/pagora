use crate::{
    scope::Scope,
    tokens::Position,
    types::{FunctionType, Type},
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Unit {
    pub types: HashMap<Rc<String>, Rc<Type>>,
    pub variables: HashMap<Rc<String>, Variable>,
    pub functions: HashMap<Rc<String>, Function>,
    // TODO: add init function here.
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Variable {
    pub name: Rc<String>,
    pub typ: Rc<Type>,
    pub value: Option<Expr>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub typ: FunctionType,
    pub param_names: Vec<Rc<String>>,
    pub body: Option<BlockStatement>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum Const {
    Void,
    IntConst(u64),
    FloatConst(f64),
    BoolConst(bool),
    ArrayConst(Vec<Expr>),
    StringConst(String),
}

// TODO: don't put box in the expr, but put it in the exprkind instead.
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Expr {
    pub position: Position,
    pub is_assignable: bool,
    pub result_type: Rc<Type>,
    pub kind: ExprKind,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum ExprKind {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Index(IndexExpr),
    Cast(CastExpr),
    Call(CallExpr),
    Ident(IdentExpr),
    Const(ConstExpr),
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum BinaryOp {
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    NEq,
    Lt,
    LEq,
    Gt,
    GEq,
    ShiftLeft,
    ShiftRight,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct BinaryExpr {
    pub a: Box<Expr>,
    pub op: BinaryOp,
    pub b: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub enum UnaryOp {
    BitNot,
    Sub,
    Add,
    Not,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Box<Expr>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct CastExpr {
    pub value: Box<Expr>,
    pub target: Rc<Type>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct CallExpr {
    pub target: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct IdentExpr {
    pub name: Rc<String>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct ConstExpr {
    pub value: Const,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum Statement {
    Block(BlockStatement),
    Var(Variable),
    If(IfStatement),
    While(WhileStatement),
    Assign(AssignStatement),
    Call(CallStatement),
    Return(Option<Expr>),
    Break,
    Continue,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct IfStatement {
    pub condition: Expr,
    pub body: Box<Statement>,
    pub else_stmt: Option<Box<Statement>>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: BlockStatement,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct AssignStatement {
    pub receiver: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct CallStatement {
    pub expr: CallExpr,
}
