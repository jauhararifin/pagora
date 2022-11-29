use crate::tokens::Position;

#[derive(Debug)]
pub enum CompileError {
    MultiErrors(Vec<CompileError>),
    UnexpectedChar { pos: Position, ch: char },
    MissingClosingQuote { pos: Position },
}
