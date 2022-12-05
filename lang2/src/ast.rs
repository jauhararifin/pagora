use crate::tokens::Token;

#[derive(Debug, PartialEq, Eq)]
pub struct RootNode {
    pub items: Vec<Item>,
}

impl RootNode {
    pub fn functions<'a>(&'a self) -> impl Iterator<Item = &'a FuncNode> {
        self.items.iter().filter_map(|item| match item {
            Item::Func(func_node) => Some(func_node),
            _ => None,
        })
    }

    pub fn variables<'a>(&'a self) -> impl Iterator<Item = &'a VarNode> {
        self.items.iter().filter_map(|item| match item {
            Item::Var(var_node) => Some(var_node),
            _ => None,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Item {
    Var(VarNode),
    Func(FuncNode),
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarNode {
    pub var: Token,
    pub name: Token,
    pub colon: Option<Token>,
    pub typ: Option<TypeExprNode>,
    pub assign: Option<Token>,
    pub value: Option<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncNode {
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
    pub return_type: Option<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub name: Token,
    pub colon: Token,
    pub typ: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeExprNode {
    Ident(Token),
    Array(ArrayTypeNode),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayTypeNode {
    pub element_type: Box<TypeExprNode>,
    pub open_square: Token,
    pub length: ExprNode,
    pub close_square: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprNode {
    Ident(Token),
    IntegerLit(Token),
    RealLit(Token),
    BooleanLit(Token),
    StringLit(Token),
    ArrayLit(ArrayLitNode),
    Binary(BinaryExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Index(IndexExprNode),
    Cast(CastExprNode),
    Grouped(GroupedExprNode),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayLitNode {
    pub open_square: Token,
    pub elements: Vec<ExprNode>,
    pub close_square: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExprNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
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
    pub target: Box<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GroupedExprNode {
    pub open_brac: Token,
    pub value: Box<ExprNode>,
    pub close_brac: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtNode {
    Block(BlockStmtNode),
    Var(VarNode),
    Return(ReturnStmtNode),
    Keyword(Token),
    If(IfStmtNode),
    While(WhileStmtNode),
    Assign(AssignStmtNode),
    Call(CallExprNode),
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignStmtNode {
    pub receiver: ExprNode,
    pub assign: Token,
    pub value: ExprNode,
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
pub struct BlockStmtNode {
    pub open_block: Token,
    pub statements: Vec<StmtNode>,
    pub close_block: Token,
}
