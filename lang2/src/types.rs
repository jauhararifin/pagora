use crate::{
    ast::{ArrayTypeNode, FuncNode, Item, StructNode, TupleTypeNode, TypeExprNode},
    errors::{cannot_redeclare_symbool, cannot_use_anonymous_pointer, undefined_type, Result},
    package::load_package,
    parser::parse,
    scanner::scan,
    semantic::{ArrayType, FunctionType, StructField, StructType, TupleType, Type, TypeInternal},
    tokens::{Position, Token},
};
use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    rc::Rc,
};

pub fn load_types(root_package: &str) -> Result<TypeSet> {
    let plan = generate_load_plan(root_package)?;

    let mut type_set = TypeSet::new();

    for pkg in plan {
        let package = load_package(&pkg)?;

        let mut asts = vec![];
        for (_, file_path) in package.files.iter() {
            let source_code = read_to_string(file_path.as_ref())?;
            let tokens = scan(&source_code)?;
            let ast = parse(tokens)?;
            asts.push(ast);
        }

        for root_ast in asts.iter() {
            for item in root_ast.items.iter() {
                let name = match item {
                    Item::Struct(node) => &node.name,
                    Item::Tuple(node) => &node.name,
                    _ => continue,
                };
                type_set.add_typename(name)?;
            }
        }

        for root_ast in asts.iter() {
            for item in root_ast.items.iter() {
                match item {
                    Item::Struct(struct_node) => {
                        let typ = analyze_struct_type(&mut type_set, struct_node)?;
                        let typ = Rc::new(Type {
                            name: Some(struct_node.name.value.as_ref().clone()),
                            internal: TypeInternal::Struct(typ),
                        });
                        type_set.add_type(&struct_node.name, typ);
                    }
                    Item::Tuple(tuple_node) => {
                        let typ = analyze_tuple_type(&mut type_set, &tuple_node.typ)?;
                        let typ = Rc::new(Type {
                            name: Some(tuple_node.name.value.as_ref().clone()),
                            internal: TypeInternal::Tuple(typ),
                        });
                        type_set.add_type(&tuple_node.name, typ);
                    }
                    _ => continue,
                }
            }
        }
    }

    Ok(type_set)
}

// TODO: fix dependenecy cycle error message
// TODO: currently, we load the file, scan it, and parse it just for the sake of building load plan
//       we can improve this by cache the ast so that we don't have to re-parse the files.
fn generate_load_plan(root_package: &str) -> Result<Vec<Rc<String>>> {
    let root_package = Rc::new(root_package.to_string());
    let mut visited_package: HashSet<Rc<String>> = HashSet::new();
    let mut toposort: Vec<Rc<String>> = vec![];

    let mut stack: Vec<Rc<String>> = vec![];
    let mut in_stack: HashSet<Rc<String>> = HashSet::new();

    stack.push(root_package.clone());
    in_stack.insert(root_package.clone());

    while let Some(pkg) = stack.pop() {
        if visited_package.contains(&pkg) {
            todo!("got dependency cycle, but no proper error message yet");
        }
        visited_package.insert(pkg.clone());
        toposort.push(pkg.clone());

        let package = load_package(&root_package)?;
        for (_, file_path) in package.files.iter() {
            let source_code = read_to_string(file_path.as_ref())?;
            let tokens = scan(&source_code)?;
            let ast = parse(tokens)?;
            for item in ast.items.iter() {
                if let Item::Import(import) = item {
                    let pkg_name = import.package.value.clone();
                    if !in_stack.contains(&pkg_name) {
                        stack.push(import.package.value.clone());
                        in_stack.insert(pkg_name.clone());
                    }
                }
            }
        }
    }

    toposort.reverse();
    Ok(toposort)
}

pub fn analyze_type(type_set: &mut TypeSet, type_node: &TypeExprNode) -> Result<Rc<Type>> {
    Ok(match type_node {
        TypeExprNode::Tuple(tuple_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Tuple(analyze_tuple_type(type_set, tuple_type)?),
        }),
        TypeExprNode::Ident(type_name) => {
            if let Some(symbol) = type_set.get_type(&type_name.value) {
                return Ok(symbol.clone());
            }
            return Err(undefined_type(type_name));
        }
        TypeExprNode::Array(ref array_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Array(analyze_array_type(type_set, array_type)?),
        }),
        TypeExprNode::Pointer(type_node) => {
            if let TypeExprNode::Ident(type_name) = type_node.pointee.as_ref() {
                if !type_set.has_typename(&type_name.value) {
                    return Err(undefined_type(type_name));
                }
                Rc::new(Type {
                    name: None,
                    internal: TypeInternal::Pointer(Rc::new(Type {
                        name: Some(type_name.value.as_ref().clone()),
                        internal: TypeInternal::Unknown,
                    })),
                })
            } else {
                return Err(cannot_use_anonymous_pointer(&type_node.asterisk.position));
            }
        }
    })
}

pub fn analyze_tuple_type(type_set: &mut TypeSet, tuple_node: &TupleTypeNode) -> Result<TupleType> {
    let mut items = vec![];
    for item_node in tuple_node.fields.iter() {
        items.push(analyze_type(type_set, item_node)?);
    }

    Ok(TupleType { items })
}

// TODO: consider using lazy static instead of calling method like this.
pub fn get_builtin_type(type_set: &mut TypeSet, name: &str) -> Option<Rc<Type>> {
    type_set
        .get_type(&String::from(name))
        .map(|symbol| symbol.clone())
}

pub fn analyze_array_type(type_set: &mut TypeSet, array_node: &ArrayTypeNode) -> Result<ArrayType> {
    let element_type = analyze_type(type_set, &array_node.element_type)?;
    Ok(ArrayType { element_type })
}

pub fn analyze_func_type(type_set: &mut TypeSet, func_node: &FuncNode) -> Result<FunctionType> {
    let mut parameters = vec![];
    for param_node in func_node.head.parameters.iter() {
        parameters.push(analyze_type(type_set, &param_node.typ)?);
    }

    let return_type = if let Some(ref return_type_node) = func_node.head.return_type {
        analyze_type(type_set, return_type_node)?
    } else {
        Type::tuple(vec![])
    };

    Ok(FunctionType {
        parameters,
        return_type,
    })
}

pub fn analyze_struct_type(type_set: &mut TypeSet, struct_node: &StructNode) -> Result<StructType> {
    let mut fields = vec![];
    for field in struct_node.fields.iter() {
        let name = field.name.value.clone();
        let typ = analyze_type(type_set, &field.typ)?;
        fields.push(StructField { name, typ });
    }

    Ok(StructType { fields })
}

pub fn is_type_equal(a: &Type, b: &Type) -> bool {
    a == b
}

pub struct TypeSet {
    type_names: HashMap<Rc<String>, Position>,
    type_defs: HashMap<Rc<String>, Rc<Type>>,
}

impl TypeSet {
    fn new() -> Self {
        Self {
            type_names: HashMap::new(),
            type_defs: HashMap::new(),
        }
    }

    pub fn has_typename(&self, name: &Rc<String>) -> bool {
        self.type_names.contains_key(name)
    }

    pub fn add_typename(&mut self, name: &Token) -> Result<()> {
        let entry = self.type_names.get(name.value.as_ref());
        if let Some(declared_at) = entry {
            return Err(cannot_redeclare_symbool(name, declared_at));
        }
        self.type_names
            .insert(name.value.clone(), name.position.clone());
        Ok(())
    }

    pub fn get_type(&self, name: &String) -> Option<Rc<Type>> {
        self.type_defs.get(name).map(|t| t.clone())
    }

    pub fn add_type(&mut self, token: &Token, typ: Rc<Type>) {
        if self.type_defs.contains_key(&token.value) {
            unreachable!("cannot redeclare symbol with the same name");
        }
        self.type_defs.insert(token.value.clone(), typ.clone());
    }

    pub fn types(&self) -> Vec<Rc<Type>> {
        self.type_defs.values().map(|t| t.clone()).collect()
    }
}
