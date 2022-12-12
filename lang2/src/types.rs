use crate::{
    ast::{ArrayTypeNode, FuncNode, StructNode, TupleTypeNode, TypeExprNode},
    errors::{cannot_use_anonymous_pointer, undefined_type, Result},
    scope::Scope,
    semantic::{ArrayType, FunctionType, StructField, StructType, TupleType, Type, TypeInternal},
};
use std::rc::Rc;

pub fn analyze_type(scope: &Scope, type_node: &TypeExprNode) -> Result<Rc<Type>> {
    Ok(match type_node {
        TypeExprNode::Tuple(tuple_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Tuple(analyze_tuple_type(scope, tuple_type)?),
        }),
        TypeExprNode::Ident(type_name) => {
            if let Some(symbol) = scope.get_type(None, &type_name.value) {
                return Ok(symbol.clone());
            }
            return Err(undefined_type(None, type_name));
        }
        TypeExprNode::Array(ref array_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Array(analyze_array_type(scope, array_type)?),
        }),
        TypeExprNode::Pointer(type_node) => {
            if let TypeExprNode::Ident(type_name) = type_node.pointee.as_ref() {
                if !scope.has_typename(&type_name.value) {
                    return Err(undefined_type(None, type_name));
                }
                Rc::new(Type {
                    name: None,
                    internal: TypeInternal::Pointer(Rc::new(Type {
                        name: Some(type_name.value.as_ref().clone()),
                        internal: TypeInternal::Unknown,
                    })),
                })
            } else {
                return Err(cannot_use_anonymous_pointer(&type_node.asterisk.position));
            }
        }
        TypeExprNode::Selection(selection_node) => {
            if let Some(typ) = scope.get_type(
                Some(&selection_node.value.value),
                &selection_node.selection.value,
            ) {
                return Ok(typ.clone());
            } else {
                return Err(undefined_type(
                    Some(&selection_node.value),
                    &selection_node.selection,
                ));
            }
        }
    })
}

pub fn analyze_tuple_type(scope: &Scope, tuple_node: &TupleTypeNode) -> Result<TupleType> {
    let mut items = vec![];
    for item_node in tuple_node.fields.iter() {
        items.push(analyze_type(scope, item_node)?);
    }

    Ok(TupleType { items })
}

pub fn analyze_array_type(scope: &Scope, array_node: &ArrayTypeNode) -> Result<ArrayType> {
    let element_type = analyze_type(scope, &array_node.element_type)?;
    Ok(ArrayType { element_type })
}

pub fn analyze_func_type(scope: &Scope, func_node: &FuncNode) -> Result<FunctionType> {
    let mut parameters = vec![];
    for param_node in func_node.head.parameters.iter() {
        parameters.push(analyze_type(scope, &param_node.typ)?);
    }

    let return_type = if let Some(ref return_type_node) = func_node.head.return_type {
        analyze_type(scope, return_type_node)?
    } else {
        Type::tuple(vec![])
    };

    Ok(FunctionType {
        parameters,
        return_type,
    })
}

pub fn analyze_struct_type(scope: &Scope, struct_node: &StructNode) -> Result<StructType> {
    let mut fields = vec![];
    for field in struct_node.fields.iter() {
        let name = field.name.value.clone();
        let typ = analyze_type(scope, &field.typ)?;
        fields.push(StructField { name, typ });
    }

    Ok(StructType { fields })
}

pub fn is_type_equal(a: &Type, b: &Type) -> bool {
    a == b
}
