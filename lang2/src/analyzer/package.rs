use crate::{
    analyzer::dependencies::build_dependency_graph,
    ast::{
        ArrayTypeExprNode, DerefExprNode, ExprNode, FuncNode, ImportNode, ItemNode, RootSet,
        SelectionExprNode, StructTypeExprNode, TypeNode, VarNode,
    },
    errors::{
        cannot_redeclare_symbol, definition_cycle, undefined_symbol, unexpected, unexpected_char,
        CompileError, Result, type_mismatch,
    },
    semantic::{Expr, ExprKind, Object, Symbol, Unit},
    tokens::{Position, Token},
    toposort::toposort,
    types::{ArrayType, StructField, StructType, Type, TypeInternal},
};
use std::{collections::HashMap, iter::zip, rc::Rc};

type Imports<'a> = Rc<HashMap<Rc<String>, &'a ImportNode>>;

pub fn analyze_package(
    packages: &HashMap<Rc<String>, Rc<Unit>>,
    package_name: Rc<String>,
    root_set: &RootSet,
) -> Result<Unit> {
    let orders = build_analyze_plan(&package_name, root_set)?;

    let mut item_map = HashMap::new();
    for root_ast in root_set.roots.iter() {
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

    let mut scope = Scope::new(package_name.clone(), packages);

    for node in root_set
        .roots
        .iter()
        .flat_map(|root| root.items.iter())
        .filter_map(ItemNode::as_type)
    {
        scope.add_to_package(
            &node.name,
            Symbol {
                exported: node.pub_tok.is_some(),
                name: (package_name.clone(), node.name.value.clone()),
                object: Object::Type(Rc::new(Type {
                    name: Some((package_name.clone(), node.name.value.clone())),
                    internal: TypeInternal::Unknown,
                })),
            },
        )?;
    }

    let mut errors = CompileError::new();
    for item_name in orders.iter() {
        let (item, imports) = item_map.get(item_name).unwrap();
        scope.set_import(imports.clone());
        let result = match item {
            ItemNode::Import(_) => continue,
            ItemNode::Type(node) => analyze_type(&mut scope, node),
            ItemNode::Var(node) => analyze_var(&mut scope, node),
            ItemNode::Func(node) => analyze_func(&mut scope, node),
        };
        match result {
            Ok(symbol) => scope.add_to_package(item.name_token(), symbol)?,
            Err(err) => errors.push(err),
        }
    }

    if errors.is_empty() {
        Ok(Unit::new(package_name.clone()))
    } else {
        Err(errors)
    }
}

fn build_analyze_plan(package_name: &str, root_set: &RootSet) -> Result<Vec<Rc<String>>> {
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

fn analyze_type(scope: &mut Scope, type_node: &TypeNode) -> Result<Symbol> {
    let typ = analyze_type_expr(scope, &type_node.type_expr)?;
    let name = (scope.package_name.clone(), type_node.name.value.clone());
    let typ = Rc::new(Type {
        name: Some(name.clone()),
        internal: typ.internal.clone(),
    });
    Ok(Symbol {
        exported: type_node.pub_tok.is_some(),
        name,
        object: Object::Type(typ),
    })
}

fn analyze_type_expr(scope: &Scope, expr: &ExprNode) -> Result<Rc<Type>> {
    match expr {
        ExprNode::Ident(token) => {
            if let Object::Type(typ) = scope.get(&token)? {
                Ok(typ)
            } else {
                Err(undefined_symbol(None, token))
            }
        }
        ExprNode::Selection(node) => analyze_type_selector_expr(scope, node),
        ExprNode::Grouped(node) => analyze_type_expr(scope, &node.value),
        ExprNode::Deref(node) => analyze_type_deref_expr(scope, node),
        ExprNode::Array(node) => analyze_type_array_expr(scope, node),
        ExprNode::Struct(node) => analyze_type_struct_expr(scope, node),
        _ => Err(unexpected("TYPE", "EXPR", expr.position())),
    }
}

fn analyze_type_selector_expr(scope: &Scope, expr: &SelectionExprNode) -> Result<Rc<Type>> {
    let ExprNode::Ident(package_name) = expr.value.as_ref() else {
        return Err(unexpected_char(expr.dot.position.clone(), '.'));
    };

    let Object::Package(unit) = scope.get(&package_name)? else {
        return Err(undefined_symbol(None, &package_name))
    };

    let Some(typ) = unit.get_exported_type(&expr.selection.value) else {
        return Err(undefined_symbol(Some(&package_name.value), &package_name))
    };

    Ok(typ)
}

fn analyze_type_deref_expr(scope: &Scope, node: &DerefExprNode) -> Result<Rc<Type>> {
    if let ExprNode::Ident(name) = node.value.as_ref() {
        return Ok(Rc::new(Type {
            name: None,
            internal: TypeInternal::Unknown,
        }));
    }
    analyze_type_expr(scope, &node.value)
}

fn analyze_type_array_expr(scope: &Scope, node: &ArrayTypeExprNode) -> Result<Rc<Type>> {
    let element_type = analyze_type_expr(scope, &node.element_type)?;
    Ok(Rc::new(Type {
        name: None,
        internal: TypeInternal::Array(ArrayType { element_type }),
    }))
}

fn analyze_type_struct_expr(scope: &Scope, node: &StructTypeExprNode) -> Result<Rc<Type>> {
    let mut errors = CompileError::new();
    let mut fields = vec![];
    for field in node.fields.iter() {
        match analyze_type_expr(scope, &field.type_expr) {
            Ok(typ) => fields.push(StructField {
                name: field.name.value.clone(),
                typ,
            }),
            Err(err) => errors.push(err),
        }
    }

    if errors.is_empty() {
        Ok(Rc::new(Type {
            name: None,
            internal: TypeInternal::Struct(StructType { fields }),
        }))
    } else {
        Err(errors)
    }
}

fn analyze_var(scope: &mut Scope, item: &VarNode) -> Result<Symbol> {
    let type_hint = if let Some(ref type_expr) = item.stmt.type_expr {
        Some(analyze_type_expr(scope, type_expr)?)
    } else {
        None
    };

    let value = if let Some(ref expr_node) = item.stmt.value {
        analyze_value_expr(scope, expr_node, type_hint.clone())?
    } else {
        let typ = analyze_type_expr(
            scope,
            item.stmt
                .type_expr
                .as_ref()
                .expect("invalid state: either type or value should present"),
        )?;
        Rc::new(Expr {
            position: item.stmt.var.position.clone(),
            is_assignable: false,
            result_type: typ,
            kind: ExprKind::DefaultZero,
        })
    };

    let typ = if let Some(typ) = type_hint {
        typ
    } else {
        value.result_type.clone()
    };

    if typ == value.result_type {
        return Err(type_mismatch(&typ, &value))
    }

    Ok(Symbol {
        exported: item.pub_tok.is_some(),
        name: (scope.package_name.clone(), item.stmt.name.value.clone()),
        object: Object::Value(value),
    })
}

fn analyze_value_expr(
    scope: &Scope,
    expr: &ExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Rc<Expr>> {
    match expr {
        ExprNode::Ident(token) => {
            if let Object::Value(val) = scope.get(&token)? {
                Ok(val)
            } else {
                Err(undefined_symbol(None, token))
            }
        }
        ExprNode::Selection(node) => analyze_value_selector_expr(scope, node),
        ExprNode::IntegerLit(node) => todo!(),
        ExprNode::RealLit(node) => todo!(),
        ExprNode::BooleanLit(node) => todo!(),
        ExprNode::StringLit(node) => todo!(),
        ExprNode::CompositeLit(node) => todo!(),
        ExprNode::KeyValue(node) => todo!(),
        ExprNode::Binary(node) => todo!(),
        ExprNode::Addr(node) => todo!(),
        ExprNode::Deref(node) => todo!(),
        ExprNode::Unary(node) => todo!(),
        ExprNode::Call(node) => todo!(),
        ExprNode::Index(node) => todo!(),
        ExprNode::Cast(node) => todo!(),
        ExprNode::Grouped(node) => analyze_value_expr(scope, &node.value, type_hint),
        _ => Err(unexpected("EXPR", "TYPE", expr.position())),
    }
}

fn analyze_value_selector_expr(scope: &Scope, expr: &SelectionExprNode) -> Result<Rc<Expr>> {
    let ExprNode::Ident(package_name) = expr.value.as_ref() else {
        return Err(unexpected_char(expr.dot.position.clone(), '.'));
    };

    let Object::Package(unit) = scope.get(&package_name)? else {
        return Err(undefined_symbol(None, &package_name))
    };

    let Some(value) = unit.get_exported_value(&expr.selection.value) else {
        return Err(undefined_symbol(Some(&package_name.value), &package_name))
    };

    Ok(value)
}

fn analyze_func(scope: &mut Scope, item: &FuncNode) -> Result<Symbol> {
    todo!();
}

struct Scope<'a> {
    package_name: Rc<String>,
    external_packages: &'a HashMap<Rc<String>, Rc<Unit>>,
    current_import: Imports<'a>,
    root_symbols: HashMap<Rc<String>, (Position, Symbol)>,
    local_symbols_map: HashMap<Rc<String>, Vec<(Position, Symbol)>>,
    local_symbols: Vec<Rc<String>>,
}

impl<'a> Scope<'a> {
    fn new(package_name: Rc<String>, packages: &'a HashMap<Rc<String>, Rc<Unit>>) -> Self {
        Self {
            package_name,
            external_packages: packages,
            current_import: Rc::new(HashMap::new()),
            root_symbols: HashMap::new(),
            local_symbols_map: HashMap::new(),
            local_symbols: Vec::new(),
        }
    }

    fn set_import(&mut self, imports: Imports<'a>) {
        self.current_import = imports;
    }

    fn add_to_package(&mut self, token: &Token, value: Symbol) -> Result<()> {
        if let Some(node) = self.current_import.get(&token.value) {
            return Err(cannot_redeclare_symbol(token, &node.alias.position));
        }
        if let Some((pos, _)) = self.root_symbols.get(&token.value) {
            return Err(cannot_redeclare_symbol(token, pos));
        }
        let name = token.value.clone();
        let pos = token.position.clone();
        self.root_symbols.insert(name, (pos, value));
        Ok(())
    }

    fn has_name(&self, name: &Rc<String>) -> bool {
        self.local_symbols.contains(name)
            || self.current_import.contains_key(name)
            || self.root_symbols.contains_key(name)
    }

    fn get(&self, name_tok: &Token) -> Result<Object> {
        let name = &name_tok.value;
        if let Some((_, symbol)) = self.local_symbols_map.get(name).and_then(|v| v.last()) {
            Ok(symbol.object.clone())
        } else if let Some(pkg) = self.current_import.get(name) {
            Ok(Object::Package(
                self.external_packages
                    .get(&pkg.package.value)
                    .expect("invalid state: cannot find external package")
                    .clone(),
            ))
        } else if let Some((_, symbol)) = self.root_symbols.get(name) {
            Ok(symbol.object.clone())
        } else {
            Err(undefined_symbol(None, name_tok))
        }
    }
}
