use crate::{
    analyzer::{analyze_expr, analyze_string_lit},
    ast::{ItemNode, RootNode},
    errors::{
        cannot_open_file, import_cycle, invalid_module_file, missing_package, CompileError, Result,
    },
    package::{Module, Package},
    parser::parse,
    scanner::scan,
    scope::Scope,
    semantic::{Type, TypeInternal},
    types::{analyze_func_type, analyze_struct_type, analyze_tuple_type, analyze_type},
};
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::PathBuf,
    rc::Rc,
};

pub fn load_package(package_name: &str) -> Result<Package> {
    let loading_plan = get_package_loading_plan(package_name)?;

    let mut scope_map: HashMap<Rc<String>, Rc<Scope>> = HashMap::new();

    for pkg in loading_plan.iter() {
        let mut scope = Scope::new(pkg.clone());
        let mut asts = vec![];
        let files = load_package_files(pkg)?;
        for source_file in files.iter() {
            let tokens = scan(source_file)?;
            let ast = parse(tokens)?;
            asts.push(ast);
        }

        for root_ast in asts.iter() {
            for import in root_ast.items.iter() {
                if let ItemNode::Import(import_node) = import {
                    let imported_pkg = import_node.package.value.clone();
                    let imported_scope = scope_map.get(&imported_pkg).unwrap();
                    scope.add_import(imported_scope.clone());
                }
            }
        }

        load_single_package(&mut scope, asts.as_slice())?;

        if pkg.as_str() == package_name {
            return Ok(Package {
                name: pkg.clone(),
                scope,
                asts,
            });
        }
        scope_map.insert(pkg.clone(), Rc::new(scope));
    }

    unreachable!();
}

fn get_package_loading_plan(root_package: &str) -> Result<Vec<Rc<String>>> {
    let root_package = Rc::new(root_package.to_string());
    let mut visited: HashSet<Rc<String>> = HashSet::new();
    let mut importer: HashMap<Rc<String>, Rc<String>> = HashMap::new();
    let mut package_order: Vec<Rc<String>> = vec![];

    let mut stack: Vec<Rc<String>> = vec![];
    let mut in_stack: HashSet<Rc<String>> = HashSet::new();

    stack.push(root_package.clone());
    in_stack.insert(root_package.clone());

    while let Some(pkg) = stack.pop() {
        if visited.contains(&pkg) {
            let mut cycle = vec![];

            let pivot = pkg.clone();

            let mut curr = pkg.clone();
            loop {
                cycle.push(curr.clone());
                curr = importer.get(&curr).unwrap().clone();
                if curr == pivot {
                    break;
                }
            }
            cycle.push(pivot);

            return Err(import_cycle(&cycle));
        }
        visited.insert(pkg.clone());
        package_order.push(pkg.clone());

        let package_files = load_package_files(root_package.as_str())?;
        for file_path in package_files.iter() {
            let tokens = scan(file_path)?;
            let ast = parse(tokens)?;
            for item in ast.items.iter() {
                if let ItemNode::Import(import) = item {
                    let pkg_name = Rc::new(analyze_string_lit(&import.package));
                    if !in_stack.contains(&pkg_name) {
                        stack.push(pkg_name.clone());
                        in_stack.insert(pkg_name.clone());
                        importer.insert(pkg_name, pkg.clone());
                    }
                }
            }
        }
    }

    package_order.reverse();
    Ok(package_order)
}

fn load_package_files(package_name: &str) -> Result<Vec<PathBuf>> {
    let package_dirs = get_package_dirs()?;
    let path = package_dirs.iter().find_map(|(mod_name, path)| {
        if package_name.starts_with(mod_name.as_str()) {
            let mut path = path.clone();
            path.push(&package_name[mod_name.len()..].trim_start_matches("/"));
            Some(path)
        } else {
            None
        }
    });

    let Some(path) = path else {
        return Err(missing_package(package_name));
    };

    if !path.is_dir() {
        return Err(missing_package(package_name));
    }

    let mut files: Vec<PathBuf> = vec![];
    for file in fs::read_dir(&path)? {
        let file = file?.path();
        let Some(ext) = file.extension() else {
            continue;
        };
        if ext != "p" {
            continue;
        }
        files.push(file);
    }

    Ok(files)
}

fn get_package_dirs() -> Result<Vec<(Rc<String>, PathBuf)>> {
    let mut path_std = std::env::var("PAGORA_ROOT")
        .map(|path| PathBuf::from(path))
        .or_else(|_| std::env::current_dir())?;
    path_std.push("lib/");

    let mut user_mod_path = std::env::current_dir()?;
    user_mod_path.push("module.yaml");
    let mod_file =
        fs::read_to_string(&user_mod_path).map_err(|err| cannot_open_file(&user_mod_path, err))?;
    let module: Module = serde_yaml::from_str(&mod_file)
        .map_err(|err| invalid_module_file(&user_mod_path, err.to_string().as_str()))?;
    let module = module.module;
    user_mod_path.pop();

    Ok(vec![
        (module, user_mod_path),
        (Rc::new("".to_string()), path_std),
    ])
}

fn load_single_package<'a>(scope: &mut Scope, asts: &[RootNode]) -> Result<()> {
    populate_typenames(scope, asts)?;
    populate_typedefs(scope, asts)?;
    populate_functions(scope, asts)?;
    populate_variables(scope, asts)?;
    Ok(())
}

fn populate_typenames<'a>(scope: &mut Scope, asts: &[RootNode]) -> Result<()> {
    for root_ast in asts {
        scope.repopulate_imports(root_ast);
        for item in root_ast.items.iter() {
            let name = match item {
                ItemNode::Struct(node) => &node.name,
                ItemNode::Tuple(node) => &node.name,
                _ => continue,
            };
            scope.add_typename(name)?;
        }
    }
    Ok(())
}

fn populate_typedefs<'a>(scope: &mut Scope, asts: &[RootNode]) -> Result<()> {
    for root_ast in asts {
        scope.repopulate_imports(root_ast);
        for item in root_ast.items.iter() {
            match item {
                ItemNode::Struct(struct_node) => {
                    let typ = analyze_struct_type(scope, struct_node)?;
                    let typ = Rc::new(Type {
                        name: Some(struct_node.name.value.as_ref().clone()),
                        internal: TypeInternal::Struct(typ),
                    });
                    scope.add_type(&struct_node.name, typ)?;
                }
                ItemNode::Tuple(tuple_node) => {
                    let typ = analyze_tuple_type(scope, &tuple_node.typ)?;
                    let typ = Rc::new(Type {
                        name: Some(tuple_node.name.value.as_ref().clone()),
                        internal: TypeInternal::Tuple(typ),
                    });
                    scope.add_type(&tuple_node.name, typ)?;
                }
                _ => continue,
            }
        }
    }

    Ok(())
}

fn populate_functions(scope: &mut Scope, asts: &[RootNode]) -> Result<()> {
    let mut errors = CompileError::new();

    for root_node in asts {
        scope.repopulate_imports(root_node);
        let func_nodes = root_node.items.iter().filter_map(|item| match item {
            ItemNode::Func(func_node) => Some(func_node),
            _ => None,
        });
        for func_node in func_nodes {
            match analyze_func_type(scope, func_node) {
                Ok(func_type) => {
                    scope.add_value(
                        &func_node.head.name,
                        Rc::new(Type {
                            name: None,
                            internal: TypeInternal::Function(func_type),
                        }),
                    );
                }
                Err(err) => {
                    errors.push(err);
                }
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(())
    }
}

fn populate_variables(scope: &mut Scope, asts: &[RootNode]) -> Result<()> {
    let mut errors = CompileError::new();

    for root_node in asts {
        scope.repopulate_imports(root_node);
        let var_nodes = root_node.items.iter().filter_map(|item| match item {
            ItemNode::Var(var_node) => Some(var_node),
            _ => None,
        });

        for var_node in var_nodes {
            let typ = if let Some(ref type_node) = var_node.stmt.typ {
                analyze_type(scope, type_node)
            } else {
                analyze_expr(scope, var_node.stmt.value.as_ref().unwrap(), None)
                    .map(|expr| expr.result_type)
            };
            match typ {
                Ok(typ) => scope.add_value(&var_node.stmt.name, typ),
                Err(err) => errors.push(err),
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(())
    }
}
