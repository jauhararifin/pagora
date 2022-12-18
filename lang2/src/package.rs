use crate::{
    analyzer::analyze_package,
    ast::RootSet,
    errors::{import_cycle, Result},
    parser::parse,
    scanner::scan,
    scope::Scope,
};
use std::{
    collections::{HashMap, HashSet},
    env, fs,
    path::PathBuf,
    rc::Rc,
    str::FromStr,
};

pub fn load_package_complete(package_name: &str) -> Result<()> {
    let mut imported_by = HashMap::new();
    let mut toposort = vec![];
    let mut visited = HashSet::new();
    let package_name = Rc::new(String::from(package_name));
    let mut stack = vec![package_name.clone()];
    let mut in_stack = HashSet::from([package_name.clone()]);

    while let Some(current_name) = stack.pop() {
        if visited.contains(&current_name) {
            let mut cycle = vec![current_name];
            while cycle.len() < 1 && cycle.first().unwrap() != cycle.last().unwrap() {
                let next_item: &Rc<String> = imported_by.get(cycle.last().unwrap()).unwrap();
                cycle.push(next_item.clone());
            }
            return Err(import_cycle(&cycle));
        }
        visited.insert(current_name.clone());

        let asts = Rc::new(load_package_asts(&current_name)?);
        toposort.push((current_name.clone(), asts.clone()));

        let dependency: Vec<Rc<String>> = asts
            .roots
            .iter()
            .flat_map(|root| root.imports())
            .map(|import| import.package.value.clone())
            .collect();
        for dep in dependency {
            imported_by.insert(dep.clone(), current_name.clone());
            if !in_stack.contains(&dep) {
                stack.push(dep.clone());
                in_stack.insert(dep);
            }
        }
    }

    toposort.reverse();

    let mut units = HashMap::new();
    for (package_name, asts) in toposort {
        let unit = analyze_package(&units, &package_name, &asts)?;
        units.insert(package_name, unit);
    }

    todo!();
}

fn load_package_asts(package_name: &str) -> Result<RootSet> {
    let mut roots = vec![];
    let dir_path = get_package_dir(package_name)?;
    for file in fs::read_dir(dir_path)? {
        let path = file?.path();
        if !path.is_file() {
            continue;
        }
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
        if ext != "p" {
            continue;
        }

        let tokens = scan(&path)?;
        let ast = parse(tokens)?;
        roots.push(ast);
    }

    Ok(RootSet { roots })
}

fn get_package_dir(package_name: &str) -> Result<PathBuf> {
    let path = env::current_dir()?;
    let path = env::var("PAGORA_ROOT").unwrap_or(String::from(path.to_str().unwrap()));
    let mut path = PathBuf::from_str(&path).unwrap();
    path.push("lib");
    for segment in package_name.split("/") {
        path.push(segment);
    }

    Ok(path)
}
