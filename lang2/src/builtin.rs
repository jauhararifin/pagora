use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::semantic::{Function, FunctionType, TupleType, Type, TypeInternal, Variable};

#[derive(Debug, Serialize, Deserialize)]
pub struct Builtin {
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

pub fn get_builtin() -> Builtin {
    let print = Function {
        name: Rc::new("print".into()),
        typ: FunctionType {
            parameters: vec![Rc::new(Type {
                name: Some("string".into()),
                internal: TypeInternal::String,
            })],
            return_type: Rc::new(Type {
                name: None,
                internal: TypeInternal::Tuple(TupleType { items: vec![] }),
            }),
        },
        param_names: vec![Rc::new("msg".into())],
        body: None,
    };

    Builtin {
        variables: vec![],
        functions: vec![print],
    }
}
