use std::{collections::HashMap, iter::zip, rc::Rc};

use crate::{
    ast::{ItemNode, RootSet},
    errors::Result,
    semantic::Unit,
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
