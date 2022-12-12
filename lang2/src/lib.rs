mod analyzer;
mod ast;
mod builtin;
pub mod errors;
mod loader;
mod package;
mod parser;
mod scanner;
mod scope;
pub mod semantic;
pub mod test;
mod tokens;
mod types;

use analyzer::analyze;
use errors::Result;
use loader::load_package;
use semantic::Unit;

pub fn translate(package_name: &str) -> Result<Unit> {
    let mut package = load_package(package_name)?;
    analyze(&mut package)
}
