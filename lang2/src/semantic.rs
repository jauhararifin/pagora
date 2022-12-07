use serde::{Deserialize, Serialize};
use std::rc::Rc;

use crate::tokens::Position;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Program {
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Type {
    pub name: Option<String>, // TODO: this should be qual
    // None name means anonymous type
    pub internal: TypeInternal,
}

impl Type {
    pub fn tuple(items: Vec<Rc<Type>>) -> Rc<Self> {
        Rc::new(Self {
            name: None,
            internal: TypeInternal::Tuple(TupleType { items }),
        })
    }

    pub fn int(name: String, bits: u8, signed: bool) -> Rc<Self> {
        Rc::new(Self {
            name: Some(name),
            internal: TypeInternal::Int(IntType { bits, signed }),
        })
    }

    pub fn is_int(&self) -> bool {
        match self.internal {
            TypeInternal::Int(_) => true,
            _ => false,
        }
    }

    pub fn float(name: String, bits: u8) -> Rc<Self> {
        Rc::new(Self {
            name: Some(name),
            internal: TypeInternal::Float(FloatType { bits }),
        })
    }

    pub fn is_float(&self) -> bool {
        match self.internal {
            TypeInternal::Float(_) => true,
            _ => false,
        }
    }

    pub fn bool(name: String) -> Rc<Self> {
        Rc::new(Self {
            name: Some(name),
            internal: TypeInternal::Bool,
        })
    }

    pub fn is_bool(&self) -> bool {
        match self.internal {
            TypeInternal::Bool => true,
            _ => false,
        }
    }

    pub fn string(name: String) -> Rc<Self> {
        Rc::new(Self {
            name: Some(name),
            internal: TypeInternal::String,
        })
    }

    pub fn is_string(&self) -> bool {
        match self.internal {
            TypeInternal::String => true,
            _ => false,
        }
    }

    pub fn array(name: Option<String>, element_type: Rc<Type>) -> Rc<Self> {
        Rc::new(Self {
            name,
            internal: TypeInternal::Array(ArrayType { element_type }),
        })
    }

    pub fn is_func(&self) -> bool {
        match self.internal {
            TypeInternal::Function(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum TypeInternal {
    Tuple(TupleType),
    Int(IntType),
    Float(FloatType),
    Bool,
    String,
    Array(ArrayType),
    Function(FunctionType),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TupleType {
    pub items: Vec<Rc<Type>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct IntType {
    pub bits: u8,
    pub signed: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct FloatType {
    pub bits: u8,
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize)]
pub struct ArrayType {
    pub element_type: Rc<Type>,
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.element_type == other.element_type
    }

    fn ne(&self, other: &Self) -> bool {
        self.element_type != other.element_type
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct FunctionType {
    pub parameters: Vec<Rc<Type>>,
    pub return_type: Rc<Type>,
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
