use crate::tokens::Token;

pub struct RootNode {
    pub items: Vec<Item>,
}

pub enum Item {
    Var(VarNode),
    Func(FuncNode),
}

pub struct VarNode {
    pub var: Token,
    pub name: Token,
    pub colon: Option<Token>,
    pub typ: Option<TypeExprNode>,
    pub assign: Option<Token>,
    pub value: Option<ExprNode>,
}

pub struct FuncNode {
    pub head: FuncHeadNode,
    pub body: Option<BlockStmtNode>,
}

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

pub struct ParameterNode {
    pub name: Token,
    pub colon: Token,
    pub typ: TypeExprNode,
}

pub enum TypeExprNode {
    Ident(Token),
    Array(ArrayTypeNode),
}

pub struct ArrayTypeNode {
    pub element_type: Box<TypeExprNode>,
    pub open_square: Token,
    pub length: ExprNode,
    pub close_square: Token,
}

pub enum ExprNode {
    Ident(Token),
    IntegerLit(Token),
    RealLit(Token),
    BooleanLit(Token),
    StringLit(Token),
    ArrayLit,
    Binary(BinaryExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Index(IndexExprNode),
    Cast(CastExprNode),
    Grouped(GroupedExprNode),
}

pub struct ArrayLitNode {
    pub open_square: Token,
    pub items: Vec<ExprNode>,
    pub close_sqare: Token,
}

pub struct BinaryExprNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
}

pub struct UnaryExprNode {
    pub op: Token,
    pub value: Box<ExprNode>,
}

pub struct CallExprNode {
    pub target: Box<ExprNode>,
    pub open_brac: Token,
    pub arguments: Vec<ExprNode>,
    pub close_brac: Token,
}

pub struct IndexExprNode {
    pub target: Box<ExprNode>,
    pub open_square: Token,
    pub index: Box<ExprNode>,
    pub close_square: Token,
}

pub struct CastExprNode {
    pub value: Box<ExprNode>,
    pub as_tok: Token,
    pub target: Box<TypeExprNode>,
}

pub struct GroupedExprNode {
    pub open_brac: Token,
    pub value: Box<ExprNode>,
    pub close_brac: Token,
}

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

pub struct AssignStmtNode {
    pub receiver: ExprNode,
    pub assign: Token,
    pub value: ExprNode,
}

pub struct ReturnStmtNode {
    pub return_tok: Token,
    pub value: Option<ExprNode>,
}

pub struct IfStmtNode {
    pub if_tok: Token,
    pub condition: ExprNode,
    pub body: BlockStmtNode,
    pub else_ifs: Vec<ElseIfStmtNode>,
    pub else_stmt: Option<ElseStmtNode>,
}

pub struct ElseIfStmtNode {
    pub else_tok: Token,
    pub if_tok: Token,
    pub condition: ExprNode,
    pub body: BlockStmtNode,
}

pub struct ElseStmtNode {
    pub else_tok: Token,
    pub body: BlockStmtNode,
}

pub struct WhileStmtNode {
    pub while_tok: Token,
    pub condition: ExprNode,
    pub body: BlockStmtNode,
}

pub struct BlockStmtNode {
    pub open_block: Token,
    pub statements: Vec<StmtNode>,
    pub close_block: Token,
}
