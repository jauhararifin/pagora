use crate::{
    semantic::{Expr, Type},
    tokens::{Position, Token, TokenKind},
};

#[derive(Debug)]
pub enum CompileError {
    MultiErrors(MultiErrors),
    UnexpectedChar(UnexpectedChar),
    MissingClosingQuote(MissingClosingQuote),
    UnexpectedToken(UnexpectedToken),
    NotAssignable(NotAssignable),
    TypeMismatch(TypeMismatch),
}

impl CompileError {
    pub fn unexpected_char(position: Position, ch: char) -> Self {
        Self::UnexpectedChar(UnexpectedChar { position, ch })
    }

    pub fn missing_closing_quote(position: Position) -> Self {
        Self::MissingClosingQuote(MissingClosingQuote { position })
    }

    pub fn unexpected_token(expected: Vec<TokenKind>, token: Token) -> Self {
        Self::UnexpectedToken(UnexpectedToken { expected, token })
    }
}

#[derive(Debug)]
pub struct MultiErrors {
    pub errors: Vec<CompileError>,
    pub is_too_many: bool,
}

impl MultiErrors {
    pub fn new() -> Self {
        Self {
            errors: vec![],
            is_too_many: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn push(&mut self, err: CompileError) {
        if let CompileError::MultiErrors(errs) = err {
            for err_item in errs.errors.into_iter() {
                self.push(err_item);
            }
        } else {
            if self.errors.len() < 30 {
                self.errors.push(err);
            } else {
                self.is_too_many = true;
            }
        }
    }
}

impl From<MultiErrors> for CompileError {
    fn from(e: MultiErrors) -> Self {
        Self::MultiErrors(e)
    }
}

#[derive(Debug)]
pub struct UnexpectedChar {
    pub position: Position,
    pub ch: char,
}

impl From<UnexpectedChar> for CompileError {
    fn from(e: UnexpectedChar) -> Self {
        Self::UnexpectedChar(e)
    }
}

#[derive(Debug)]
pub struct MissingClosingQuote {
    pub position: Position,
}

impl From<MissingClosingQuote> for CompileError {
    fn from(e: MissingClosingQuote) -> Self {
        Self::MissingClosingQuote(e)
    }
}

#[derive(Debug)]
pub struct UnexpectedToken {
    pub expected: Vec<TokenKind>,
    pub token: Token,
}

impl From<UnexpectedToken> for CompileError {
    fn from(e: UnexpectedToken) -> Self {
        Self::UnexpectedToken(e)
    }
}

#[derive(Debug)]
pub struct NotAssignable {
    pub receiver: Expr,
}

impl From<NotAssignable> for CompileError {
    fn from(e: NotAssignable) -> Self {
        Self::NotAssignable(e)
    }
}

#[derive(Debug)]
pub struct TypeMismatch {
    pub expected: Type,
    pub got: Expr,
}

impl From<TypeMismatch> for CompileError {
    fn from(e: TypeMismatch) -> Self {
        Self::TypeMismatch(e)
    }
}
