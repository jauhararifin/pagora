use crate::{
    analyzer::analyze,
    env::Target,
    errors::CompileError,
    parser::parse,
    scanner::scan,
    semantic::{Builtin, Program},
};

pub fn translate(source_code: String, target: Target) -> Result<Program, CompileError> {
    let tokens = scan(&source_code)?;
    let root_ast = parse(tokens)?;
    let program = analyze(
        root_ast,
        target,
        Builtin {
            variables: vec![],
            functions: vec![],
        },
    )?;

    Ok(program)
}
