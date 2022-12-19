use crate::{
    errors::{cannot_redeclare_symbol, Result},
    semantic::{Expr, Unit},
    tokens::{Position, Token},
    types::Type,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Scope {
    internal: Rc<RefCell<ScopeInternal>>,
}

struct ScopeInternal {
    kind: ScopeKind,
    bindings: HashMap<Rc<String>, Binding>,
    parent: Option<Rc<RefCell<ScopeInternal>>>,
}

struct Binding {
    pub declared_at: Option<Position>,
    pub exported: bool,
    pub kind: BindingKind,
}

enum BindingKind {
    Value(Rc<Type>),
    Type(Rc<Type>),
    Import(Rc<ScopeInternal>),
}

enum ScopeKind {
    Package(Rc<String>),
    Ast,
    Loop,
    Func(Rc<String>),
    Regular,
}

impl Scope {
    pub fn new(package_name: Rc<String>) -> Self {
        let internal = Rc::new(RefCell::new(ScopeInternal {
            kind: ScopeKind::Package(package_name),
            bindings: HashMap::new(),
            parent: None,
        }));
        let mut scope = Self { internal };

        let int32t = Rc::new(String::from("int32"));
        scope.bind_type(false, int32t.clone(), Type::int(("", &int32t), 32, true));

        scope
    }

    pub fn package_name(&self) -> Rc<String> {
        todo!();
    }

    pub fn bind_value(&mut self, name: &Token, typ: Rc<Type>) -> Result<()> {
        let mut internal = self.internal.borrow_mut();

        if let Some(binding) = internal.bindings.get(&name.value) {
            if let Some(ref pos) = binding.declared_at {
                return Err(cannot_redeclare_symbol(name, pos));
            }
        }

        internal.bindings.insert(
            name.value.clone(),
            Binding {
                declared_at: Some(name.position.clone()),
                exported: false,
                kind: BindingKind::Value(typ),
            },
        );

        Ok(())
    }

    pub fn lookup_value(&self, name: &String) -> Option<Expr> {
        todo!();
    }

    pub fn lookup_value_complete(&self, name: &String) -> Option<Expr> {
        todo!();
    }

    pub fn bind_type(&mut self, public: bool, name: Rc<String>, typ: Rc<Type>) {
        todo!();
    }

    pub fn lookup_type(&self, name: &String) -> Option<Rc<Type>> {
        todo!();
    }

    pub fn lookup_type_complete(&self, name: &String) -> Result<Expr> {
        todo!();
    }

    pub fn bind_scope(&mut self, name: Rc<String>, unit: Rc<Unit>) {
        todo!();
    }

    pub fn lookup_scope(&self, name: &String) -> Option<Rc<Unit>> {
        todo!();
    }
}
