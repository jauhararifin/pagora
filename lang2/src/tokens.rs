#[derive(PartialEq, Debug, Clone)]
pub struct Position {
    pub line: i64,
    pub col: i64,
}

#[derive(PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub value: String,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    Invalid,
    Eof,
    Comment,

    Var,
    As,
    Function,
    Comma,
    Colon,
    Arrow,
    Semicolon,

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
    CharLit,
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

    Array,
    Of,
}
