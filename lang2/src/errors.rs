use crate::{
    semantic::Expr,
    tokens::{Position, Token, TokenKind},
    types::Type,
};
use serde::{Deserialize, Serialize};
use std::{io, path::Path, rc::Rc};

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

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self {
        CompileError::from_message(None, err.to_string())
    }
}

pub fn unexpected(expected: &str, found: &str, position: Position) -> CompileError {
    CompileError::from_message(
        Some(position),
        format!("Expected {}, but found {}", expected, found),
    )
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

pub fn cannot_redeclare_symbol(token: &Token, declared_at: &Position) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        format!(
            "Symbol {} is alredy declared at {}",
            token.value, declared_at,
        ),
    )
}

// pub fn not_assignable(expr: &Expr) -> CompileError {
//     CompileError::from_message(
//         Some(expr.position.clone()),
//         String::from("Expression is not assignable"),
//     )
// }
//
pub fn type_mismatch(expected: &Type, got: &Expr) -> CompileError {
    CompileError::from_message(
        Some(got.position.clone()),
        format!(
            "Type mismatch, expected {} got {}",
            expected, got.result_type
        ),
    )
}

pub fn undefined_type(pkg_name: Option<&Token>, token: &Token) -> CompileError {
    if let Some(pkg_name) = pkg_name {
        CompileError::from_message(
            Some(token.position.clone()),
            format!("Undefined type {}.{}", pkg_name.value, token.value),
        )
    } else {
        CompileError::from_message(
            Some(token.position.clone()),
            format!("Undefined type {}", token.value),
        )
    }
}

pub fn undefined_symbol(selector: Option<&String>, token: &Token) -> CompileError {
    CompileError::from_message(
        Some(token.position.clone()),
        if let Some(selector) = selector {
            format!("Undefined symbol {}.{}", selector, token.value)
        } else {
            format!("Undefined symbol {}", token.value)
        },
    )
}

pub fn cannot_infer_type(position: &Position) -> CompileError {
    CompileError::from_message(Some(position.clone()), format!("Cannot infer type"))
}

// pub fn invalid_binary_op(a: &Expr, op: &Token, b: &Expr) -> CompileError {
//     CompileError::from_message(
//         Some(a.position.clone()),
//         format!(
//             "Cannot perform binary op {:?} for {} and {}",
//             op.kind, a.result_type, b.result_type
//         ),
//     )
// }
//
// pub fn invalid_unary_op(op: &Token, value: &Expr) -> CompileError {
//     CompileError::from_message(
//         Some(op.position.clone()),
//         format!("Cannot perform unary op {:?} for {}", op, value.result_type),
//     )
// }
//
// pub fn not_a(expected: &str, value: &Expr) -> CompileError {
//     CompileError::from_message(
//         Some(value.position.clone()),
//         format!("Expected {}, but found {}", expected, value.result_type),
//     )
// }
//
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

// pub fn cannot_cast(value: &Expr, into: &Type) -> CompileError {
//     CompileError::from_message(
//         Some(value.position.clone()),
//         format!("Cannot cast expression into {}", into),
//     )
// }
//
pub fn no_such_field(position: &Position, field_name: &str) -> CompileError {
    CompileError::from_message(
        Some(position.clone()),
        format!("There is no field {} in the struct", field_name),
    )
}

// pub fn invalid_tuple_index(position: &Position, tuple: &TupleType, idx: i64) -> CompileError {
//     CompileError::from_message(
//         Some(position.clone()),
//         format!(
//             "The tuple only have {} elements, but found index {}",
//             tuple.items.len(),
//             idx
//         ),
//     )
// }
//
pub fn cannot_use_expr_as_stmt(position: &Position) -> CompileError {
    CompileError::from_message(
        Some(position.clone()),
        String::from("Expected a statement, but found an expr"),
    )
}

pub fn cannot_use_anonymous_pointer(position: &Position) -> CompileError {
    CompileError::from_message(
        Some(position.clone()),
        String::from("Pointer can only be applied on named type"),
    )
}

pub fn import_cycle(cycle: &[Rc<String>]) -> CompileError {
    let cycle: Vec<&str> = cycle.iter().map(|s| s.as_str()).collect();
    let cycle = cycle.join(" -> ").to_string();
    CompileError::from_message(None, format!("Found an import cycle: {}", cycle))
}

pub fn definition_cycle(cycle: &[Rc<String>]) -> CompileError {
    let cycle: Vec<&str> = cycle.iter().map(|s| s.as_str()).collect();
    let cycle = cycle.join(" refers tp ").to_string();
    CompileError::from_message(None, format!("Found a definition cycle: {}", cycle))
}

pub fn missing_package(package_name: &str) -> CompileError {
    CompileError::from_message(None, format!("Package {} not found", package_name))
}

pub fn cannot_open_file(file_path: &Path, err: io::Error) -> CompileError {
    CompileError::from_message(None, format!("Cannot open file {:?}: {}", file_path, err))
}

pub fn invalid_module_file(file_path: &Path, err: &str) -> CompileError {
    CompileError::from_message(
        None,
        format!("Malformed module file {:?}: {}", file_path, err),
    )
}
