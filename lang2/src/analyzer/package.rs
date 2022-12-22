use crate::{
    analyzer::dependencies::build_dependency_graph,
    ast::{ImportNode, ItemNode, RootSet},
    errors::{cannot_redeclare_symbol, definition_cycle, CompileError, Result},
    semantic::{Symbol, SymbolKind, Unit},
    tokens::{Position, Token},
    toposort::toposort,
};
use std::{collections::HashMap, iter::zip, rc::Rc};

type Imports<'a> = Rc<HashMap<Rc<String>, &'a ImportNode>>;

pub fn analyze_package(
    imports: &HashMap<Rc<String>, Unit>,
    package_name: Rc<String>,
    asts: &RootSet,
) -> Result<Unit> {
    let orders = build_analyze_plan(imports, &package_name, asts)?;

    let mut item_map = HashMap::new();
    for root_ast in asts.roots.iter() {
        let mut imports: Imports = Rc::new(
            root_ast
                .items
                .iter()
                .filter_map(ItemNode::as_import)
                .map(|node| (node.alias.value.clone(), node))
                .collect(),
        );

        for item in root_ast.items.iter() {
            let name = item.name();
            item_map.insert(name, (item, imports.clone()));
        }
    }

    let mut scope = Scope::new();

    for node in asts
        .roots
        .iter()
        .flat_map(|root| root.items.iter())
        .filter_map(ItemNode::as_type)
    {
        scope.add_to_package(
            &node.name,
            Symbol {
                exported: node.pub_tok.is_some(),
                package: package_name.clone(),
                name: node.name.value.clone(),
                kind: SymbolKind::TypeUnknown,
            },
        )?;
    }

    let mut errors = CompileError::new();
    for item_name in orders.iter() {
        let (item, imports) = item_map.get(item_name).unwrap();
        scope.set_import(imports.clone());
        match analyze_item(&mut scope, item) {
            Ok(symbol) => scope.add_to_package(item.name_token(), symbol)?,
            Err(err) => errors.push(err),
        }
    }

    if errors.is_empty() {
        Ok(Unit::new())
    } else {
        Err(errors)
    }
}

fn build_analyze_plan(
    imports: &HashMap<Rc<String>, Unit>,
    package_name: &str,
    root_set: &RootSet,
) -> Result<Vec<Rc<String>>> {
    let dependency_graph = build_dependency_graph(root_set)?;
    let dependency_graph: Vec<(&Rc<String>, Vec<&Rc<String>>)> = dependency_graph
        .iter()
        .map(|(name, deps)| (name, deps.iter().collect()))
        .collect();
    let result = toposort(dependency_graph);

    let mut errors = CompileError::new();
    for cycle in result.cycles {
        let cycle: Vec<Rc<String>> = cycle.into_iter().map(|v| v.clone()).collect();
        errors.push(definition_cycle(&cycle));
    }

    if errors.is_empty() {
        Ok(result.orders.into_iter().map(|item| item.clone()).collect())
    } else {
        Err(errors)
    }
}

fn analyze_item(scope: &mut Scope, item: &ItemNode) -> Result<Symbol> {
    todo!();
}

struct Scope<'a> {
    current_import: Imports<'a>,
    symbols: HashMap<Rc<String>, (Position, Symbol)>,
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            current_import: Rc::new(HashMap::new()),
            symbols: HashMap::new(),
        }
    }

    fn set_import(&mut self, imports: Imports<'a>) {
        self.current_import = imports;
    }

    fn add_to_package(&mut self, token: &Token, value: Symbol) -> Result<()> {
        if let Some(node) = self.current_import.get(&token.value) {
            return Err(cannot_redeclare_symbol(token, &node.alias.position));
        }
        if let Some((pos, _)) = self.symbols.get(&token.value) {
            return Err(cannot_redeclare_symbol(token, pos));
        }
        let name = token.value.clone();
        let pos = token.position.clone();
        self.symbols.insert(name, (pos, value));
        Ok(())
    }
}
