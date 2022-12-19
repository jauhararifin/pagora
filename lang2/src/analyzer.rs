use crate::{
    ast::{
        ArrayTypeNode, ItemNode, RootSet, StructTypeNode, TupleTypeNode, TypeExprNode, TypeNode,
    },
    errors::{undefined_type, CompileError, Result},
    scope::Scope,
    semantic::Unit,
    types::{ArrayType, StructField, StructType, TupleType, Type, TypeInternal},
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter::zip,
    rc::Rc,
};

pub fn analyze_package(
    imports: &HashMap<Rc<String>, Unit>,
    package_name: &str,
    asts: &RootSet,
) -> Result<Unit> {
    let mut imports = vec![];
    for root in asts.roots.iter() {
        let import_binding: HashMap<Rc<String>, Rc<String>> = HashMap::from_iter(
            root.items
                .iter()
                .filter_map(|item| item.as_import())
                .map(|import| (import.alias.value.clone(), import.package.value.clone())),
        );
        imports.push(import_binding);
    }

    let mut type_asts = vec![];
    let mut var_asts = vec![];
    let mut func_asts = vec![];

    for (import_binding, root) in zip(imports.iter(), asts.roots.iter()) {
        for item in root.items.iter() {
            match item {
                ItemNode::Import(import_node) => (),
                ItemNode::Type(type_node) => {
                    type_asts.push((type_node, import_binding));
                }
                ItemNode::Var(var_node) => {
                    var_asts.push((var_node, import_binding));
                }
                ItemNode::Func(func_node) => {
                    func_asts.push((func_node, import_binding));
                }
            }
        }
    }

    todo!();

    // let mut scope = Scope::new(Rc::new(String::from(package_name)));
    //
    // let mut errors = CompileError::new();
    //
    // let toposort = toposort_types(type_asts.as_slice());
    // for type_node in toposort {
    //     match analyze_type(&scope, Some(type_node.name.value.clone()), &type_node.typ) {
    //         Ok(typ) => {
    //             scope.bind_type(
    //                 type_node.pub_tok.is_some(),
    //                 type_node.name.value.clone(),
    //                 typ,
    //             );
    //         }
    //         Err(err) => errors.push(err),
    //     }
    // }
    //
    // if !errors.is_empty() {
    //     return Err(errors);
    // }
    //
    // todo!();
}

fn toposort_types<'a, 'b>(
    types: &[(&'a TypeNode, &'b HashMap<Rc<String>, Rc<String>>)],
) -> Vec<(&'a TypeNode, &'b HashMap<Rc<String>, Rc<String>>)> {
    let mut refered_by = HashMap::new();

    let mut name_to_ast = HashMap::new();

    for (type_node, import_binding) in types {
        let name = type_node.name.value.clone();
        refered_by
            .entry(name.clone())
            .or_insert(HashSet::<Rc<String>>::new());
        for dep in get_type_dependency(&type_node.typ) {
            refered_by
                .entry(dep)
                .or_insert(HashSet::<Rc<String>>::new())
                .insert(name.clone());
        }

        name_to_ast.insert(name.clone(), *type_node);
    }

    let mut reference_count = HashMap::new();
    for type_name in refered_by.values().flat_map(|v| v.iter()) {
        let v = reference_count.entry(type_name.clone()).or_insert(0);
        *v += 1;
    }

    let mut s = VecDeque::new();
    for (type_name, count) in reference_count.iter() {
        if count == &0 {
            s.push_back(type_name.clone());
        }
    }

    let mut result = vec![];
    while let Some(type_name) = s.pop_front() {
        let refering = refered_by.get(&type_name).unwrap();
        for t in refering {
            let v = reference_count.get_mut(t).unwrap();
            *v -= 1;
            if v == &0 {
                s.push_back(t.clone());
            }
        }
        result.push(type_name);
    }

    let invalid_types: Vec<Rc<String>> = reference_count
        .into_iter()
        .filter_map(|(typ, count)| if count > 0 { Some(typ) } else { None })
        .collect();

    if !invalid_types.is_empty() {
        // TODO: add proper error message.
        panic!("found type cycle in this types: {:?}", invalid_types);
    }

    todo!();

    // result
    //     .iter()
    //     .map(|name| *name_to_ast.get(name).unwrap())
    //     .collect()
}

fn get_type_dependency(type_expr: &TypeExprNode) -> Vec<Rc<String>> {
    match type_expr {
        TypeExprNode::Struct(node) => node
            .fields
            .iter()
            .map(|field| &field.typ)
            .map(get_type_dependency)
            .flatten()
            .collect(),
        TypeExprNode::Tuple(node) => node
            .fields
            .iter()
            .map(get_type_dependency)
            .flatten()
            .collect(),
        TypeExprNode::Ident(node) => vec![node.value.clone()],
        TypeExprNode::Array(_) => vec![],
        TypeExprNode::Pointer(_) => vec![],
        TypeExprNode::Selection(_) => vec![],
    }
}

fn analyze_type(
    scope: &Scope,
    name: Option<Rc<String>>,
    type_node: &TypeExprNode,
) -> Result<Rc<Type>> {
    let name = name.map(|name| (scope.package_name(), name.clone()));
    Ok(match type_node {
        TypeExprNode::Tuple(tuple_type) => Rc::new(Type {
            name,
            internal: TypeInternal::Tuple(analyze_tuple_type(scope, tuple_type)?),
        }),
        TypeExprNode::Struct(ref struct_node) => Rc::new(Type {
            name,
            internal: TypeInternal::Struct(analyze_struct_type(scope, struct_node)?),
        }),
        TypeExprNode::Ident(type_name) => {
            if let Some(symbol) = scope.lookup_type(&type_name.value) {
                return Ok(symbol.clone());
            }
            return Err(undefined_type(None, type_name));
        }
        TypeExprNode::Array(ref array_type) => Rc::new(Type {
            name,
            internal: TypeInternal::Array(analyze_array_type(scope, array_type)?),
        }),
        TypeExprNode::Pointer(type_node) => {
            let element_type = match type_node.pointee.as_ref() {
                TypeExprNode::Ident(name) => {
                    if let Some(typ) = scope.lookup_type(&name.value) {
                        typ
                    } else {
                        Rc::new(Type {
                            name: Some((scope.package_name(), name.value.clone())),
                            internal: TypeInternal::Unknown,
                        })
                    }
                }
                TypeExprNode::Selection(selection) => {
                    let Some(pkg) = scope.lookup_scope(&selection.value.value) else {
                        return Err(undefined_type(None, &selection.value));
                    };
                    let Some((is_public, typ)) = pkg.types.get(&selection.selection.value) else {
                        return Err(undefined_type(None, &selection.value));
                    };
                    if !is_public {
                        return Err(undefined_type(None, &selection.value));
                    }
                    typ.clone()
                }
                t @ _ => analyze_type(scope, None, t)?,
            };
            Rc::new(Type {
                name,
                internal: TypeInternal::Pointer(element_type),
            })
        }
        TypeExprNode::Selection(selection) => {
            // TODO: move this into scope's method.
            let Some(pkg) = scope.lookup_scope(&selection.value.value) else {
                return Err(undefined_type(None, &selection.value));
            };
            let Some((is_public, typ)) = pkg.types.get(&selection.selection.value) else {
                return Err(undefined_type(None, &selection.value));
            };
            if !is_public {
                return Err(undefined_type(None, &selection.value));
            }
            typ.clone()
        }
    })
}

fn analyze_tuple_type(scope: &Scope, tuple_node: &TupleTypeNode) -> Result<TupleType> {
    let mut items = vec![];
    for item_node in tuple_node.fields.iter() {
        items.push(analyze_type(scope, None, item_node)?);
    }

    Ok(TupleType { items })
}

fn analyze_struct_type(scope: &Scope, struct_node: &StructTypeNode) -> Result<StructType> {
    let mut fields = vec![];
    for field in struct_node.fields.iter() {
        fields.push(StructField {
            name: field.name.value.clone(),
            typ: analyze_type(scope, None, &field.typ)?,
        })
    }
    Ok(StructType { fields })
}

fn analyze_array_type(scope: &Scope, array_node: &ArrayTypeNode) -> Result<ArrayType> {
    let element_type = analyze_type(scope, None, &array_node.element_type)?;
    Ok(ArrayType { element_type })
}
