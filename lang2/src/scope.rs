use crate::{errors::Result, semantic::Expr, types::Type};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub struct Scope();

impl Scope {
    pub fn new() -> Self {
        let mut scope = Self {};

        let int32t = Rc::new(String::from("int32"));
        scope.bind_type(int32t.clone(), Type::int(&int32t, 32, true));

        scope
    }

    pub fn bind_value(&mut self, name: Rc<String>, value: Rc<Expr>) {
        todo!();
    }

    pub fn lookup_value(&self, name: &String) -> Result<Expr> {
        todo!();
    }

    pub fn lookup_value_complete(&self, name: &String) -> Result<Expr> {
        todo!();
    }

    pub fn bind_type(&mut self, name: Rc<String>, typ: Rc<Type>) {
        todo!();
    }

    pub fn lookup_type(&self, name: &String) -> Result<Expr> {
        todo!();
    }

    pub fn lookup_type_complete(&self, name: &String) -> Result<Expr> {
        todo!();
    }
}
