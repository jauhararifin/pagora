use crate::semantic::{Function, FunctionType, TupleType, Type, TypeInternal, Variable};
use serde::{Deserialize, Serialize};
use std::rc::Rc;

#[derive(Debug, Serialize, Deserialize)]
pub struct Builtin {
    pub types: Vec<Rc<Type>>,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

pub const INT32_TYPE: &'static str = "int32";
pub const INT64_TYPE: &'static str = "int64";
pub const UINT32_TYPE: &'static str = "uint32";
pub const UINT64_TYPE: &'static str = "uint64";
pub const FLOAT32_TYPE: &'static str = "float32";
pub const FLOAT64_TYPE: &'static str = "float64";
pub const BOOL_TYPE: &'static str = "bool";
pub const STRING_TYPE: &'static str = "string";

pub fn get_builtin() -> Builtin {
    let types: Vec<Rc<Type>> = vec![
        Type::int(INT32_TYPE, 32, true),
        Type::int(INT64_TYPE, 64, true),
        Type::int(UINT32_TYPE, 32, false),
        Type::int(UINT64_TYPE, 64, false),
        Type::float(FLOAT32_TYPE, 32),
        Type::float(FLOAT64_TYPE, 64),
        Type::bool(BOOL_TYPE),
        Type::string(STRING_TYPE),
    ];

    let variables = vec![];

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

    let functions = vec![print];

    Builtin {
        types,
        variables,
        functions,
    }
}
