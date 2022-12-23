use serde::{Deserialize, Serialize};
use std::{fmt::Display, rc::Rc};

use crate::semantic::Expr;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Type {
    pub name: Option<(Rc<String>, Rc<String>)>,
    pub internal: TypeInternal,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref name) = self.name {
            name.1.fmt(f)
        } else {
            write!(f, "{}", &self.internal)
        }
    }
}

impl Type {
    pub fn int(name: (&str, &str), bits: u8, signed: bool) -> Rc<Self> {
        Rc::new(Self {
            name: Some((Rc::new(String::from(name.0)), Rc::new(String::from(name.1)))),
            internal: TypeInternal::Int(IntType { bits, signed }),
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum TypeInternal {
    Pointer(Rc<Type>),
    Tuple(TupleType),
    Struct(StructType),
    Int(IntType),
    Float(FloatType),
    Bool,
    String,
    Array(ArrayType),
    Function(FunctionType),
    Unknown, // this is for dealing with pointer. A pointer doesn't care its inner type.
    Invalid, // invalid internal type marks a type as invalid.
             // Type can be invalid when they have a cyclic dependency with other type.
             // The purpose of having invalid type is to make the compilation can still continue
             // even though we found an invalid type.
}

impl Display for TypeInternal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Pointer(t) => t.fmt(f),
            Self::Tuple(t) => t.fmt(f),
            Self::Struct(t) => t.fmt(f),
            Self::Int(t) => t.fmt(f),
            Self::Float(t) => t.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Array(t) => t.fmt(f),
            Self::Function(t) => t.fmt(f),
            Self::Unknown => write!(f, "(unknown)"),
            Self::Invalid => write!(f, "(invalid)"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TupleType {
    pub items: Vec<Rc<Type>>,
}

impl Display for TupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_tuple("");
        for item in self.items.iter() {
            f.field(item.as_ref());
        }
        f.finish()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

impl StructType {
    pub fn get(&self, name: &String) -> Option<&StructField> {
        todo!();
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct StructField {
    pub name: Rc<String>,
    pub typ: Rc<Type>,
}

impl Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: write proper struct representation
        write!(f, "struct")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct IntType {
    pub bits: u8,
    pub signed: bool,
}

impl Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} bit signed integer", self.bits)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct FloatType {
    pub bits: u8,
}

impl Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} bit float", self.bits)
    }
}

#[derive(Clone, Debug, Eq, Serialize, Deserialize)]
pub struct ArrayType {
    pub element_type: Rc<Type>,
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.element_type == other.element_type
    }

    fn ne(&self, other: &Self) -> bool {
        self.element_type != other.element_type
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[]{}", self.element_type)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct FunctionType {
    pub parameters: Vec<Rc<Type>>,
    pub return_type: Rc<Type>,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut arg_formatter = f.debug_tuple("func");
        for param in self.parameters.iter() {
            arg_formatter.field(param);
        }
        arg_formatter.finish()?;
        write!(f, " -> {}", self.return_type)
    }
}
