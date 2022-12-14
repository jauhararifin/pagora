use crate::{
    analyzer::analyze, errors::CompileError, parser::parse, scanner::scan, semantic::Program,
};

pub fn translate(source_code: String) -> Result<Program, CompileError> {
    let tokens = scan(&source_code)?;
    let root_ast = parse(tokens)?;
    let program = analyze(root_ast)?;

    Ok(program)
}
