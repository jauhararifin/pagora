use crate::semantic::Type;
use std::rc::Rc;

pub const INT32_TYPE: &'static str = "int32";
pub const INT64_TYPE: &'static str = "int64";
pub const UINT32_TYPE: &'static str = "uint32";
pub const UINT64_TYPE: &'static str = "uint64";
pub const FLOAT32_TYPE: &'static str = "float32";
pub const FLOAT64_TYPE: &'static str = "float64";
pub const BOOL_TYPE: &'static str = "bool";
pub const STRING_TYPE: &'static str = "string";

pub fn get_builtin(package: &str, name: &str) -> Rc<Type> {
    match (package, name) {
        ("builtin", "int32") => Type::int(INT32_TYPE, 32, true),
        ("builtin", "int64") => Type::int(INT64_TYPE, 32, true),
        ("builtin", "uint32") => Type::int(UINT32_TYPE, 32, false),
        ("builtin", "uint64") => Type::int(UINT64_TYPE, 32, false),
        ("builtin", "float32") => Type::float(FLOAT32_TYPE, 32),
        ("builtin", "float64") => Type::float(FLOAT64_TYPE, 64),
        ("builtin", "string") => Type::string(STRING_TYPE),
        ("builtin", "bool") => Type::bool(BOOL_TYPE),
        _ => panic!("unhandled builtin type {}.{}", package, name),
    }
}
