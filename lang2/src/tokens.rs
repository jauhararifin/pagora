use serde::{Deserialize, Serialize};
use std::{fmt::Display, path::PathBuf, rc::Rc};

#[derive(PartialEq, Eq, Debug, Clone, Default, Serialize, Deserialize)]
pub struct Position {
    pub file_path: Rc<PathBuf>,
    pub line: i64,
    pub col: i64,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub value: Rc<String>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, Serialize, Deserialize)]
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
    Pub,
    Struct,
    Tuple,
    Import,

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
