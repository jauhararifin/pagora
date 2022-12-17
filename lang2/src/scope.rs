use crate::{
    errors::Result,
    semantic::{Expr, Type},
};
use std::rc::Rc;

pub struct Scope();

impl Scope {
    pub fn new() -> Self {
        todo!();
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
