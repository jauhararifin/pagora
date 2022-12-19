use crate::{
    analyzer::analyze_package,
    ast::RootSet,
    errors::{import_cycle, CompileError, Result},
    parser::parse,
    scanner::scan,
    scope::Scope,
    toposort::toposort,
};
use std::{
    collections::{HashMap, HashSet},
    env, fs,
    iter::FromIterator,
    path::PathBuf,
    rc::Rc,
    str::FromStr,
};

pub fn load_package_complete(package_name: &str) -> Result<()> {
    let packages = get_all_packages(package_name)?;

    let adjlist: Vec<(&Rc<String>, Vec<&Rc<String>>)> = packages
        .iter()
        .map(|(name, package_ast)| (name, package_ast.dependencies.iter().collect()))
        .collect();
    let mut errors = CompileError::new();
    let result = toposort(adjlist);
    for cycle in result.cycles {
        let cycle: Vec<Rc<String>> = cycle.into_iter().map(|pkg| pkg.clone()).collect();
        errors.push(import_cycle(&cycle));
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    let load_order = result.orders;

    let mut units = HashMap::new();
    for package_name in load_order {
        let package_ast = packages.get(package_name).unwrap().clone();
        let unit = analyze_package(&units, &package_name, &package_ast.root_set)?;
        units.insert(package_name.clone(), unit);
    }

    todo!();
}

struct PackageAst {
    name: Rc<String>,
    root_set: RootSet,
    dependencies: Vec<Rc<String>>,
}

fn get_all_packages(package_name: &str) -> Result<HashMap<Rc<String>, Rc<PackageAst>>> {
    let package_name = Rc::new(String::from(package_name));
    let mut packages: HashMap<Rc<String>, Rc<PackageAst>> = HashMap::new();

    let mut stack = vec![package_name.clone()];
    let mut in_stack = HashSet::<Rc<String>>::from_iter([package_name.clone()]);
    let mut visited = HashSet::new();
    while let Some(current_package) = stack.pop() {
        if visited.contains(&current_package) {
            continue;
        }
        visited.insert(current_package.clone());
        let root_set = load_package_asts(&current_package)?;
        let dependencies: Vec<Rc<String>> = root_set
            .roots
            .iter()
            .flat_map(|root| root.imports())
            .map(|import| import.package.value.clone())
            .collect();
        for dep in dependencies.iter() {
            if !in_stack.contains(dep) {
                in_stack.insert(dep.clone());
                stack.push(dep.clone());
            }
        }
        let package_ast = Rc::new(PackageAst {
            name: current_package.clone(),
            root_set,
            dependencies,
        });
        packages.insert(current_package.clone(), package_ast);
    }

    Ok(packages)
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
