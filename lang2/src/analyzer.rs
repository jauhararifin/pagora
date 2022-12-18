use crate::{
    ast::{ItemNode, RootSet, TypeExprNode},
    errors::Result,
    scope::Scope,
    semantic::Unit,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

pub fn analyze_package(package_name: &str, asts: &RootSet) -> Result<Unit> {
    let toposort = toposort_types(asts);
    todo!();
}

fn toposort_types(asts: &RootSet) -> Vec<Rc<String>> {
    let mut refered_by = HashMap::new();

    for root in asts.roots.iter() {
        let mut import_binding = HashMap::new();
        for import_node in root.imports() {
            import_binding.insert(
                import_node.alias.value.clone(),
                import_node.package.value.clone(),
            );
        }

        for item in root.items.iter() {
            if let ItemNode::Type(type_node) = item {
                let name = type_node.name.value.clone();
                refered_by
                    .entry(name.clone())
                    .or_insert(HashSet::<Rc<String>>::new());
                for dep in get_type_dependency(&type_node.typ) {
                    let dep = if let Some(bind) = import_binding.get(&dep) {
                        bind.clone()
                    } else {
                        dep.clone()
                    };

                    refered_by
                        .entry(dep)
                        .or_insert(HashSet::<Rc<String>>::new())
                        .insert(name.clone());
                }
            }
        }
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

    result
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
        TypeExprNode::Selection(node) => vec![node.value.value.clone()],
    }
}
