use std::rc::Rc;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Default)]
pub struct Position {
    pub line: i64,
    pub col: i64,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub value: Rc<String>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum TokenKind {
    Invalid,
    Eof,
    Comment,

    Var,
    As,
    Function,
    Native,
    Comma,
    Colon,
    Arrow,
    Semicolon,
    Dot,
    Type,
    This,

    OpenBlock,
    CloseBlock,
    OpenSquare,
    CloseSquare,
    OpenBrac,
    CloseBrac,

    Ident,
    IntegerLit,
    RealLit,
    StringLit,
    True,
    False,

    Assign,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    BitOr,
    BitAnd,
    BitXor,
    BitNot,
    ShiftLeft,
    ShiftRight,

    And,
    Not,
    Or,

    Eq,
    NEq,
    Gt,
    GEq,
    Lt,
    LEq,

    If,
    Else,
    While,

    Continue,
    Break,
    Return,
}
