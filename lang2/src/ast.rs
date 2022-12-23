use std::rc::Rc;

use crate::tokens::{Position, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct RootSet {
    pub roots: Vec<RootNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RootNode {
    pub items: Vec<ItemNode>,
}

impl RootNode {
    pub fn imports(&self) -> impl Iterator<Item = &ImportNode> {
        self.items.iter().filter_map(|item| item.as_import())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemNode {
    Import(ImportNode),
    Type(TypeNode),
    Var(VarNode),
    Func(FuncNode),
}

impl ItemNode {
    pub fn as_import(&self) -> Option<&ImportNode> {
        match self {
            Self::Import(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&TypeNode> {
        match self {
            Self::Type(node) => Some(node),
            _ => None,
        }
    }

    pub fn name_token(&self) -> &Token {
        match self {
            Self::Import(node) => &node.alias,
            Self::Type(node) => &node.name,
            Self::Var(node) => &node.stmt.name,
            Self::Func(node) => &node.head.name,
        }
    }

    pub fn name(&self) -> Rc<String> {
        match self {
            Self::Import(node) => node.alias.value.clone(),
            Self::Type(node) => node.name.value.clone(),
            Self::Var(node) => node.stmt.name.value.clone(),
            Self::Func(node) => node.head.name.value.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportNode {
    pub import: Token,
    pub alias: Token,
    pub package: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeNode {
    pub pub_tok: Option<Token>,
    pub name: Token,
    pub type_expr: ExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarNode {
    pub pub_tok: Option<Token>,
    pub stmt: VarStmtNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncNode {
    pub pub_tok: Option<Token>,
    pub head: FuncHeadNode,
    pub body: Option<BlockStmtNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncHeadNode {
    pub func: Token,
    pub native: Option<Token>,
    pub name: Token,
    pub open_brac: Token,
    pub parameters: Vec<ParameterNode>,
    pub close_brac: Token,
    pub arrow: Option<Token>,
    pub return_type: Option<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub name: Token,
    pub colon: Token,
    pub type_expr: ExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprNode {
    Ident(Token),
    IntegerLit(Token),
    RealLit(Token),
    BooleanLit(Token),
    StringLit(Token),
    CompositeLit(CompositeLitNode),
    KeyValue(KeyValueExprNode),
    Binary(BinaryExprNode),
    Addr(AddrExprNode),
    Deref(DerefExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Index(IndexExprNode),
    Cast(CastExprNode),
    Selection(SelectionExprNode),
    Grouped(GroupedExprNode),
    Array(ArrayTypeExprNode),
    Struct(StructTypeExprNode),
}

impl ExprNode {
    pub fn position(&self) -> Position {
        match self {
            Self::Ident(node) => node.position.clone(),
            Self::IntegerLit(node) => node.position.clone(),
            Self::RealLit(node) => node.position.clone(),
            Self::BooleanLit(node) => node.position.clone(),
            Self::StringLit(node) => node.position.clone(),
            Self::CompositeLit(node) => node.type_expr.position(),
            Self::KeyValue(node) => node.key.position.clone(),
            Self::Binary(node) => node.a.position(),
            Self::Addr(node) => node.ampersand.position.clone(),
            Self::Deref(node) => node.asterisk.position.clone(),
            Self::Unary(node) => node.op.position.clone(),
            Self::Call(node) => node.target.position(),
            Self::Index(node) => node.target.position(),
            Self::Cast(node) => node.value.position(),
            Self::Selection(node) => node.value.position(),
            Self::Grouped(node) => node.value.position(),
            Self::Array(node) => node.open_square.position.clone(),
            Self::Struct(node) => node.struct_tok.position.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompositeLitNode {
    pub type_expr: Box<ExprNode>,
    pub open_block: Token,
    pub values: Vec<ExprNode>,
    pub close_block: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct KeyValueExprNode {
    pub key: Token,
    pub colon: Token,
    pub value: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TupleLitNode {
    pub open_brac: Token,
    pub values: Vec<ExprNode>,
    pub close_brac: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExprNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AddrExprNode {
    pub ampersand: Token,
    pub value: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DerefExprNode {
    pub asterisk: Token,
    pub value: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryExprNode {
    pub op: Token,
    pub value: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExprNode {
    pub target: Box<ExprNode>,
    pub open_brac: Token,
    pub arguments: Vec<ExprNode>,
    pub close_brac: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IndexExprNode {
    pub target: Box<ExprNode>,
    pub open_square: Token,
    pub index: Box<ExprNode>,
    pub close_square: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CastExprNode {
    pub value: Box<ExprNode>,
    pub as_tok: Token,
    pub target: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SelectionExprNode {
    pub value: Box<ExprNode>,
    pub dot: Token,
    pub selection: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GroupedExprNode {
    pub open_brac: Token,
    pub value: Box<ExprNode>,
    pub close_brac: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructTypeExprNode {
    pub struct_tok: Token,
    pub open_block: Token,
    pub fields: Vec<StructFieldNode>,
    pub close_block: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructFieldNode {
    pub name: Token,
    pub colon: Token,
    pub type_expr: ExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayTypeExprNode {
    pub open_square: Token,
    pub close_square: Token,
    pub element_type: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtNode {
    Block(BlockStmtNode),
    Var(VarStmtNode),
    Return(ReturnStmtNode),
    Keyword(Token),
    If(IfStmtNode),
    While(WhileStmtNode),
    Assign(AssignStmtNode),
    Call(CallExprNode),
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStmtNode {
    pub open_block: Token,
    pub statements: Vec<StmtNode>,
    pub close_block: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarStmtNode {
    pub var: Token,
    pub name: Token,
    pub colon: Option<Token>,
    pub type_expr: Option<ExprNode>,
    pub assign: Option<Token>,
    pub value: Option<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmtNode {
    pub return_tok: Token,
    pub value: Option<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStmtNode {
    pub if_tok: Token,
    pub condition: ExprNode,
    pub body: BlockStmtNode,
    pub else_ifs: Vec<ElseIfStmtNode>,
    pub else_stmt: Option<ElseStmtNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseIfStmtNode {
    pub else_tok: Token,
    pub if_tok: Token,
    pub condition: ExprNode,
    pub body: BlockStmtNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseStmtNode {
    pub else_tok: Token,
    pub body: BlockStmtNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileStmtNode {
    pub while_tok: Token,
    pub condition: ExprNode,
    pub body: BlockStmtNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignStmtNode {
    pub receiver: ExprNode,
    pub assign: Token,
    pub value: ExprNode,
}
