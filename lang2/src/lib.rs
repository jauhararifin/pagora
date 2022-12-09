mod analyzer;
mod ast;
mod builtin;
pub mod errors;
mod parser;
mod scanner;
mod semantic;
pub mod test;
mod tokens;

use analyzer::analyze;
use errors::Result;
use parser::parse;
use scanner::scan;
use semantic::Program;

pub fn translate(source_code: String) -> Result<Program> {
    let tokens = scan(&source_code)?;
    let root_ast = parse(tokens)?;
    let program = analyze(root_ast)?;

    Ok(program)
}
