use crate::{
    semantic::{Expr, TupleType, Type},
    tokens::{Position, Token, TokenKind},
};
use serde::{Deserialize, Serialize};

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CompileErrorItem {
    pub position: Option<Position>,
    pub message: String,
}

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CompileError {
    messages: Vec<CompileErrorItem>,
}

impl CompileError {
    pub fn new() -> Self {
        Self { messages: vec![] }
    }

    pub fn from_message(position: Option<Position>, message: String) -> Self {
        Self {
            messages: vec![CompileErrorItem { position, message }],
        }
    }

    pub fn push(&mut self, item: CompileError) {
        self.messages.extend(item.messages);
    }

    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }
}

pub fn unexpected_char(position: Position, ch: char) -> CompileError {
    CompileError::from_message(Some(position), format!("Unexpected character {}", ch))
}

pub fn missing_closing_quote(position: Position) -> CompileError {
    CompileError::from_message(
        Some(position),
        format!("Missing closing quote for string literal"),
    )
}

pub fn unexpected_token(token: &Token, expected: &[TokenKind]) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        format!("Expected either {:?} but found {:?}", expected, token.kind),
    )
}

pub fn unexpected_token_for(token: &Token, expected: &str) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        format!("Expected {} but found {:?}", expected, token.kind),
    )
}

pub fn cannot_redeclare_symbool(token: &Token, declared_at: &Token) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        format!(
            "Symbol {} is alredy declared at {}",
            token.value, &declared_at.position
        ),
    )
}

pub fn not_assignable(expr: &Expr) -> CompileError {
    CompileError::from_message(
        Some(expr.position.clone()),
        String::from("Expression is not assignable"),
    )
}

pub fn type_mismatch(expected: &Type, got: &Expr) -> CompileError {
    CompileError::from_message(
        Some(got.position.clone()),
        format!(
            "Type mismatch, expected {} got {}",
            expected, got.result_type
        ),
    )
}

pub fn undefined_type(token: &Token) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        format!("Undefined type {}", token.value),
    )
}

pub fn undefined_symbol(token: &Token) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        format!("Undefined symbol {}", token.value),
    )
}

pub fn cannot_infer_type(position: &Position) -> CompileError {
    CompileError::from_message(Some(position.clone()), format!("Cannot infer type"))
}

pub fn invalid_binary_op(a: &Expr, op: &Token, b: &Expr) -> CompileError {
    CompileError::from_message(
        Some(a.position.clone()),
        format!(
            "Cannot perform binary op {:?} for {} and {}",
            op.kind, a.result_type, b.result_type
        ),
    )
}

pub fn invalid_unary_op(op: &Token, value: &Expr) -> CompileError {
    CompileError::from_message(
        Some(op.position.clone()),
        format!("Cannot perform unary op {:?} for {}", op, value.result_type),
    )
}

pub fn not_a(expected: &str, value: &Expr) -> CompileError {
    CompileError::from_message(
        Some(value.position.clone()),
        format!("Expected {}, but found {}", expected, value.result_type),
    )
}

pub fn invalid_number_of_argument(
    position: &Position,
    expected: usize,
    got: usize,
) -> CompileError {
    CompileError::from_message(
        Some(position.clone()),
        format!("Expected {} arguments, but found {}", expected, got),
    )
}

pub fn cannot_cast(value: &Expr, into: &Type) -> CompileError {
    CompileError::from_message(
        Some(value.position.clone()),
        format!("Cannot cast expression into {}", into),
    )
}

pub fn no_such_field(position: &Position, field_name: &str) -> CompileError {
    CompileError::from_message(
        Some(position.clone()),
        format!("There is no field {} in the struct", field_name),
    )
}

pub fn invalid_tuple_index(position: &Position, tuple: &TupleType, idx: i64) -> CompileError {
    CompileError::from_message(
        Some(position.clone()),
        format!(
            "The tuple only have {} elements, but found index {}",
            tuple.items.len(),
            idx
        ),
    )
}
