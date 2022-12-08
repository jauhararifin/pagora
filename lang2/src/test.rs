use serde::{Deserialize, Serialize};

use crate::{
    errors::{CompileError, Result},
    semantic::Program,
};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TranslateResult {
    Ok(Program),
    Err(CompileError),
}

impl From<Result<Program>> for TranslateResult {
    fn from(result: Result<Program>) -> Self {
        match result {
            Ok(program) => Self::Ok(program),
            Err(errs) => Self::Err(errs),
        }
    }
}
