use std::rc::Rc;

use crate::tokens::Position;

#[derive(Debug)]
pub struct Program {
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Builtin {
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Rc<String>,
    pub typ: Type,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Rc<String>,
    pub typ: Rc<FunctionType>,
    pub param_names: Vec<Rc<String>>,
    pub body: Option<BlockStatement>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Void,
    Int(Rc<IntType>),
    Float(Rc<FloatType>),
    Bool,
    String,
    Array(Rc<ArrayType>),
    Function(Rc<FunctionType>),
}

#[derive(Clone, Debug)]
pub struct IntType {
    pub bits: u8,
    pub signed: bool,
}

#[derive(Debug)]
pub struct FloatType {
    pub bits: u8,
}

#[derive(Debug)]
pub struct ArrayType {
    pub length: Expr,
    pub element_type: Type,
}

#[derive(Debug)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug)]
pub enum Const {
    Void,
    IntConst(u64),
    FloatConst(f64),
    BoolConst(bool),
    ArrayConst(Vec<Const>),
}

#[derive(Debug)]
pub struct Expr {
    pub position: Position,
    pub is_assignable: bool,
    pub result_type: Type,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Index(IndexExpr),
    Cast(CastExpr),
    Call(CallExpr),
    Ident(IdentExpr),
    Const(ConstExpr),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BinaryExpr {
    pub a: Box<Expr>,
    pub op: BinaryOp,
    pub b: Box<Expr>,
}

#[derive(Debug)]
pub enum UnaryOp {
    BitNot,
    Sub,
    Add,
    Not,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug)]
pub struct CastExpr {
    pub value: Box<Expr>,
    pub target: Type,
}

#[derive(Debug)]
pub struct CallExpr {
    pub target: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct IdentExpr {
    pub name: Rc<String>,
}

#[derive(Debug)]
pub struct ConstExpr {
    pub value: Const,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub body: Box<Statement>,
    pub else_stmt: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: BlockStatement,
}

#[derive(Debug)]
pub struct AssignStatement {
    pub receiver: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct CallStatement {
    pub expr: CallExpr,
}
