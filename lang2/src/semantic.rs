use crate::tokens::Position;

pub struct Program {
    pub variables: Vec<Variable>,
    pub functions: Vec<Variable>,
}

pub struct Variable {
    pub name: String,
    pub typ: Type,
    pub value: Option<Expr>,
}

pub struct Function {
    pub name: String,
    pub typ: FunctionType,
    pub param_names: Vec<String>,
    pub body: Option<BlockStatement>,
}

pub enum Type {
    Void,
    Int(IntType),
    Float(FloatType),
    Bool,
    Array(ArrayType),
}

pub struct IntType {
    pub bits: u8,
    pub signed: bool,
}

pub struct FloatType {
    pub bits: u8,
}

pub struct ArrayType {
    pub length: u32,
    pub element_type: Box<Type>,
}

pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

pub enum Const {
    Void,
    IntConst(u64),
    FloatConst(f64),
    BoolConst(bool),
    ArrayConst(Vec<Const>),
}

pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Index(IndexExpr),
    Cast(CastExpr),
    Call(CallExpr),
    Ident(IdentExpr),
    Const(ConstExpr),
}

pub struct ExprInfo {
    pub position: Position,
    pub is_assignable: bool,
    pub result_type: Box<Type>,
}

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

pub struct BinaryExpr {
    pub a: Box<Expr>,
    pub op: BinaryOp,
    pub b: Box<Expr>,
    pub info: ExprInfo,
}

pub enum UnaryOp {
    BitNot,
    Sub,
    Add,
    Not,
}

pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Box<Expr>,
    pub info: ExprInfo,
}

pub struct IndexExpr {
    pub target: Box<Expr>,
    pub index: Box<Expr>,
    pub info: ExprInfo,
}

pub struct CastExpr {
    pub value: Box<Expr>,
    pub target: Type,
    pub info: ExprInfo,
}

pub struct CallExpr {
    pub target: Box<Expr>,
    pub arguments: Vec<Expr>,
    pub info: ExprInfo,
}

pub struct IdentExpr {
    pub name: String,
    pub info: ExprInfo,
}

pub struct ConstExpr {
    pub value: Const,
    pub info: ExprInfo,
}

pub enum Statement {
    Block(BlockStatement),
    Var(Variable),
    If(IfStatement),
    While(WhileStatement),
    Assign(AssignStatement),
    Call(CallStatement),
    Return(Expr),
    Break,
    Continue,
}

pub struct BlockStatement {}

pub struct IfStatement {
    pub condition: Expr,
    pub body: BlockStatement,
    pub else_stmt: Box<IfStatement>,
}

pub struct WhileStatement {
    pub condition: Expr,
    pub body: BlockStatement,
}

pub struct AssignStatement {
    pub receiver: Box<Expr>,
    pub value: Box<Expr>,
}

pub struct CallStatement {
    pub target: Box<Expr>,
    pub arguments: Vec<Expr>,
}
