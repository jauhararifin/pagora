use serde::{Deserialize, Serialize};

use crate::{
    errors::{CompileError, Result},
    semantic::Unit,
};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TranslateResult {
    Ok(Unit),
    Err(CompileError),
}

impl From<Result<Unit>> for TranslateResult {
    fn from(result: Result<Unit>) -> Self {
        match result {
            Ok(program) => Self::Ok(program),
            Err(errs) => Self::Err(errs),
        }
    }
}
