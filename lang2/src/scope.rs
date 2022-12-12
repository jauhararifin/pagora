use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{ItemNode, RootNode},
    errors::{cannot_redeclare_symbol, Result},
    semantic::Type,
    tokens::{Position, Token},
};

pub struct Scope {
    pub package_name: Rc<String>,
    imports: HashMap<Rc<String>, Rc<Scope>>,
    symbols: HashMap<Rc<String>, Symbol>,
}

impl Scope {
    pub fn new(package_name: Rc<String>) -> Self {
        Self {
            package_name,
            imports: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn repopulate_builtins(&mut self, scope: &Scope) {
        for (key, value) in scope.symbols.iter() {
            self.symbols.insert(key.clone(), value.clone());
        }
    }

    pub fn repopulate_imports(&mut self, ast: &RootNode) {
        self.symbols.retain(|_, val| match val.kind {
            SymbolKind::Scope(_) => false,
            _ => true,
        });

        for item in ast.items.iter() {
            if let ItemNode::Import(import_node) = item {
                let Some(scope) = self.imports.get(&import_node.package.value) else {
                    continue;
                };
                self.symbols.insert(
                    scope.package_name.clone(),
                    Symbol {
                        position: import_node.alias.position.clone(),
                        kind: SymbolKind::Scope(scope.clone()),
                    },
                );
            }
        }
    }

    pub fn add_import(&mut self, scope: Rc<Scope>) {
        self.imports.insert(scope.package_name.clone(), scope);
    }

    pub fn has_typename(&self, name: &Rc<String>) -> bool {
        let Some(item) = self.symbols.get(name) else {
            return false;
        };
        if let SymbolKind::Type(_) = item.kind {
            true
        } else {
            false
        }
    }

    pub fn add_typename(&mut self, name: &Token) -> Result<()> {
        let entry = self.symbols.get(name.value.as_ref());
        if let Some(declared_at) = entry {
            return Err(cannot_redeclare_symbol(name, &declared_at.position));
        }
        self.symbols.insert(
            name.value.clone(),
            Symbol {
                position: name.position.clone(),
                kind: SymbolKind::UnknownType,
            },
        );
        Ok(())
    }

    pub fn get_type(&self, package_name: Option<&String>, name: &String) -> Option<Rc<Type>> {
        let symbol = self.get_symbol(package_name, name)?;
        let SymbolKind::Type(ref t) = symbol.kind else {
            return None;
        };
        Some(t.clone())
    }

    fn get_symbol(&self, package_name: Option<&String>, name: &String) -> Option<&Symbol> {
        if let Some(pkg_name) = package_name {
            let scope_symbol = self.symbols.get(pkg_name)?;
            let SymbolKind::Scope(ref scope) = scope_symbol.kind else {
                return None;
            };
            scope.get_symbol(None, name)
        } else {
            self.symbols.get(name)
        }
    }

    pub fn add_type(&mut self, token: &Token, typ: Rc<Type>) -> Result<()> {
        let entry = self.symbols.get(token.value.as_ref());
        if let Some(item) = entry {
            if let SymbolKind::UnknownType = item.kind {
            } else {
                return Err(cannot_redeclare_symbol(token, &item.position));
            }
        }

        self.symbols.insert(
            token.value.clone(),
            Symbol {
                position: token.position.clone(),
                kind: SymbolKind::Type(typ),
            },
        );
        Ok(())
    }

    pub fn types(&self) -> Vec<Rc<Type>> {
        self.symbols
            .values()
            .filter_map(|item| match &item.kind {
                SymbolKind::Type(typ) => Some(typ.clone()),
                _ => None,
            })
            .collect()
    }

    pub fn get_value(&self, _name: &String) -> Option<Rc<Type>> {
        todo!();
    }

    pub fn add_value(&mut self, _token: &Token, _typ: Rc<Type>) {
        todo!();
    }
}

#[derive(Clone)]
pub struct Symbol {
    position: Position,
    kind: SymbolKind,
}

#[derive(Clone)]
pub enum SymbolKind {
    UnknownType,
    _Val(Rc<Type>),
    Type(Rc<Type>),
    Scope(Rc<Scope>),
}
