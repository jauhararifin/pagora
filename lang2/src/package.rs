use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::{ast::RootNode, scope::Scope};

pub struct Package {
    pub name: Rc<String>,
    pub scope: Scope,
    pub asts: Vec<RootNode>,
}

#[derive(Serialize, Deserialize)]
pub struct Module {
    pub module: Rc<String>,
}
