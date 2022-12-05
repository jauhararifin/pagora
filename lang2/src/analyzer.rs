use std::{
    collections::{HashMap, HashSet},
    iter::zip,
    rc::Rc,
};

use crate::{
    ast::{
        ArrayLitNode, ArrayTypeNode, AssignStmtNode, BinaryExprNode, BlockStmtNode, CallExprNode,
        CastExprNode, ExprNode, FuncNode, GroupedExprNode, IfStmtNode, IndexExprNode, Item,
        ReturnStmtNode, RootNode, StmtNode, TypeExprNode, UnaryExprNode, VarNode, WhileStmtNode,
    },
    env::{Architecture, Target},
    errors::{
        CompileError, MultiErrors, NotAssignable, TypeMismatch, UndefinedSymbol, UndefinedType,
    },
    semantic::{
        ArrayType, AssignStatement, BlockStatement, Builtin, CallStatement, Const, ConstExpr, Expr,
        ExprKind, FloatType, Function, FunctionType, IdentExpr, IfStatement, IntType, Program,
        Statement, Type, Variable, WhileStatement,
    },
    tokens::{Token, TokenKind},
};

pub fn analyze(root: RootNode, target: Target, builtin: Builtin) -> Result<Program, CompileError> {
    let mut ctx = Context::new(target);
    ctx.add_scope();

    let mut errors = MultiErrors::new();

    populate_builtin_types(&mut ctx);
    for func in builtin.functions {
        ctx.add_builtin_symbol(func.name, Type::Function(func.typ));
    }

    let mut func_nodes = vec![];
    let mut var_nodes = vec![];
    for item in root.items {
        match item {
            Item::Func(func_node) => func_nodes.push(func_node),
            Item::Var(var_node) => var_nodes.push(var_node),
        }
    }

    for func_node in func_nodes.iter() {
        match analyze_func_type(&mut ctx, func_node) {
            Ok(func_type) => {
                ctx.add_user_symbol(&func_node.head.name, Type::Function(Rc::new(func_type)));
            }
            Err(err) => {
                errors.push(err);
            }
        }
    }

    let mut variables = Vec::<Variable>::new();
    for var_node in var_nodes.iter() {
        let name_tok = var_node.name.clone();
        match analyze_variable(&mut ctx, var_node) {
            Ok(variable) => {
                ctx.add_user_symbol(&name_tok, variable.typ.clone());
                variables.push(variable);
            }
            Err(err) => {
                errors.push(err);
            }
        }
    }

    let mut functions = Vec::<Function>::new();
    for func_node in func_nodes {
        match analyze_function(&mut ctx, func_node) {
            Ok(func) => {
                functions.push(func);
            }
            Err(err) => {
                errors.push(err);
            }
        }
    }

    Ok(Program {
        variables,
        functions,
    })
}

const INT_TYPE: &'static str = "int";
const INT32_TYPE: &'static str = "int32";
const INT64_TYPE: &'static str = "int64";
const UINT_TYPE: &'static str = "uint";
const UINT32_TYPE: &'static str = "uint32";
const UINT64_TYPE: &'static str = "uint64";
const FLOAT32_TYPE: &'static str = "float32";
const FLOAT64_TYPE: &'static str = "float64";
const BOOL_TYPE: &'static str = "bool";
const STRING_TYPE: &'static str = "string";

fn populate_builtin_types(ctx: &mut Context) {
    let arch_bit = match ctx.arch {
        Architecture::IA32 => 32,
        Architecture::IA64 => 64,
    };
    ctx.add_builtin_symbol(
        Rc::new(INT_TYPE.into()),
        Type::Type(Box::new(Type::Int(Rc::new(IntType {
            bits: arch_bit,
            signed: true,
        })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(INT32_TYPE.into()),
        Type::Type(Box::new(Type::Int(Rc::new(IntType {
            bits: 32,
            signed: true,
        })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(INT64_TYPE.into()),
        Type::Type(Box::new(Type::Int(Rc::new(IntType {
            bits: 64,
            signed: true,
        })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(UINT_TYPE.into()),
        Type::Type(Box::new(Type::Int(Rc::new(IntType {
            bits: arch_bit,
            signed: false,
        })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(UINT32_TYPE.into()),
        Type::Type(Box::new(Type::Int(Rc::new(IntType {
            bits: 32,
            signed: false,
        })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(UINT64_TYPE.into()),
        Type::Type(Box::new(Type::Int(Rc::new(IntType {
            bits: 64,
            signed: false,
        })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(FLOAT32_TYPE.into()),
        Type::Type(Box::new(Type::Float(Rc::new(FloatType { bits: 32 })))),
    );
    ctx.add_builtin_symbol(
        Rc::new(FLOAT64_TYPE.into()),
        Type::Type(Box::new(Type::Float(Rc::new(FloatType { bits: 64 })))),
    );
    ctx.add_builtin_symbol(Rc::new(BOOL_TYPE.into()), Type::Type(Box::new(Type::Bool)));
    ctx.add_builtin_symbol(
        Rc::new(STRING_TYPE.into()),
        Type::Type(Box::new(Type::String)),
    );
}

fn analyze_variable(ctx: &mut Context, var_node: &VarNode) -> Result<Variable, CompileError> {
    let name = var_node.name.value.clone();

    let value = if let Some(ref expr_node) = var_node.value {
        Some(analyze_expr(ctx, expr_node, None)?)
    } else {
        None
    };

    let typ = if let Some(ref type_node) = var_node.typ {
        analyze_type(ctx, type_node)?
    } else if let Some(ref value) = value {
        value.result_type.clone()
    } else {
        unreachable!("a variable should has type or value");
    };

    Ok(Variable { name, typ, value })
}

fn analyze_function(ctx: &mut Context, func_node: FuncNode) -> Result<Function, CompileError> {
    let name = func_node.head.name.value.clone();

    let func_type = ctx
        .get(name.as_ref())
        .unwrap_or_else(|| panic!("function type for {} is not found", name))
        .get_type();
    let func_type = match func_type {
        Type::Function(func_type) => func_type.clone(),
        _ => unreachable!("symbol {} is not a function", name),
    };

    ctx.current_return_type = func_type.return_type.clone();

    assert_eq!(
        func_type.parameters.len(),
        func_node.head.parameters.len(),
        "umatched parameters length in function {}",
        name
    );

    let param_names: Vec<Rc<String>> = func_node
        .head
        .parameters
        .iter()
        .map(|param| param.name.value.clone())
        .collect();

    let body = if let Some(ref body) = func_node.body {
        let additional_symbols = zip(
            func_type.parameters.iter(),
            func_node.head.parameters.iter(),
        )
        .map(|(param_type, param_node)| Symbol::User {
            token: param_node.name.clone(),
            typ: param_type.clone(),
        })
        .collect();
        Some(analyze_block_statement(ctx, body, additional_symbols)?)
    } else {
        None
    };

    Ok(Function {
        name,
        typ: func_type,
        param_names,
        body,
    })
}

fn analyze_statement(ctx: &mut Context, stmt: &StmtNode) -> Result<Statement, CompileError> {
    Ok(match stmt {
        StmtNode::Block(stmt) => Statement::Block(analyze_block_statement(ctx, stmt, vec![])?),
        StmtNode::Var(stmt) => Statement::Var(analyze_variable(ctx, stmt)?),
        StmtNode::Return(stmt) => Statement::Return(analyze_return_statement(ctx, stmt)?),
        StmtNode::Keyword(stmt) => analyze_keyword_statement(stmt),
        StmtNode::If(stmt) => Statement::If(analyze_if_statement(ctx, &stmt)?),
        StmtNode::While(stmt) => Statement::While(analyze_while_statement(ctx, stmt)?),
        StmtNode::Assign(stmt) => Statement::Assign(analyze_assign_statement(ctx, stmt)?),
        StmtNode::Call(stmt) => Statement::Call(analyze_call_statement(ctx, stmt)?),
    })
}

fn analyze_block_statement(
    ctx: &mut Context,
    stmt: &BlockStmtNode,
    additional_symbols: Vec<Symbol>,
) -> Result<BlockStatement, CompileError> {
    ctx.add_scope();

    for symbol in additional_symbols {
        match symbol {
            Symbol::User { token, typ } => ctx.add_user_symbol(&token, typ),
            Symbol::Builtin { name, typ: _ } => {
                unreachable!("cannot add builtin symbol {} in block statement", name)
            }
        }
    }

    let mut errors = MultiErrors::new();
    let mut statements = Vec::<Statement>::new();
    for item in stmt.statements.iter() {
        match analyze_statement(ctx, item) {
            Ok(statement) => statements.push(statement),
            Err(err) => errors.push(err),
        }
    }

    ctx.pop_scope();

    if !errors.is_empty() {
        Err(errors.into())
    } else {
        Ok(BlockStatement { statements })
    }
}

fn analyze_return_statement(
    ctx: &mut Context,
    stmt: &ReturnStmtNode,
) -> Result<Option<Expr>, CompileError> {
    Ok(if let Some(ref value) = stmt.value {
        Some(analyze_expr(
            ctx,
            value,
            Some(ctx.current_return_type.clone()),
        )?)
    } else {
        None
    })
}

fn analyze_keyword_statement(keyword: &Token) -> Statement {
    match keyword.kind {
        TokenKind::Continue => Statement::Continue,
        TokenKind::Break => Statement::Break,
        _ => unreachable!(
            "Keyword {:?} is not a valid statement keyword",
            keyword.kind
        ),
    }
}

fn analyze_if_statement(ctx: &mut Context, stmt: &IfStmtNode) -> Result<IfStatement, CompileError> {
    let mut else_stmt = if let Some(ref s) = stmt.else_stmt {
        let body = analyze_block_statement(ctx, &s.body, vec![])?;
        Some(Box::new(Statement::Block(body)))
    } else {
        None
    };

    for else_if in stmt.else_ifs.iter().rev() {
        let body = analyze_block_statement(ctx, &else_if.body, vec![])?;
        let else_if_stmt = IfStatement {
            condition: analyze_expr(ctx, &else_if.condition, Some(Type::Bool))?,
            body: Box::new(Statement::Block(body)),
            else_stmt: else_stmt.take(),
        };
        else_stmt = Some(Box::new(Statement::If(else_if_stmt)));
    }

    let body = analyze_block_statement(ctx, &stmt.body, vec![])?;
    Ok(IfStatement {
        condition: analyze_expr(ctx, &stmt.condition, Some(Type::Bool))?,
        body: Box::new(Statement::Block(body)),
        else_stmt,
    })
}

fn analyze_while_statement(
    ctx: &mut Context,
    stmt: &WhileStmtNode,
) -> Result<WhileStatement, CompileError> {
    let condition = analyze_expr(ctx, &stmt.condition, Some(Type::Bool))?;

    ctx.add_loop();
    let body = analyze_block_statement(ctx, &stmt.body, vec![])?;
    ctx.pop_loop();

    Ok(WhileStatement { condition, body })
}

fn analyze_assign_statement(
    ctx: &mut Context,
    stmt: &AssignStmtNode,
) -> Result<AssignStatement, CompileError> {
    let receiver = analyze_expr(ctx, &stmt.receiver, None)?;
    if !receiver.is_assignable {
        return Err(NotAssignable { receiver }.into());
    }

    let value = analyze_expr(ctx, &stmt.value, Some(receiver.result_type.clone()))?;
    if !is_type_equal(&receiver.result_type, &value.result_type) {
        return Err(TypeMismatch {
            expected: receiver.result_type,
            got: value,
        }
        .into());
    }

    Ok(AssignStatement {
        receiver: Box::new(receiver),
        value: Box::new(value),
    })
}

fn analyze_call_statement(
    ctx: &mut Context,
    stmt: &CallExprNode,
) -> Result<CallStatement, CompileError> {
    let expr = analyze_call_expr(ctx, &stmt, None)?;
    let ExprKind::Call(expr) = expr.kind else {
        unreachable!("cannot unwrap expr kind into ExprKind::Call");
    };
    Ok(CallStatement { expr })
}

fn analyze_expr(
    ctx: &mut Context,
    expr_node: &ExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    let value = match expr_node {
        ExprNode::Ident(expr) => analyze_ident_expr(ctx, expr)?,
        ExprNode::IntegerLit(expr) => analyze_integer_lit_expr(ctx, expr, type_hint)?,
        ExprNode::RealLit(expr) => analyze_real_lit_expr(ctx, expr, type_hint)?,
        ExprNode::BooleanLit(expr) => analyze_boolean_lit_expr(ctx, expr, type_hint)?,
        ExprNode::StringLit(expr) => analyze_string_lit_expr(ctx, expr, type_hint)?,
        ExprNode::ArrayLit(expr) => analyze_array_lit_expr(ctx, expr, type_hint)?,
        ExprNode::Binary(expr) => analyze_binary_expr(ctx, expr, type_hint)?,
        ExprNode::Unary(expr) => analyze_unary_expr(ctx, expr, type_hint)?,
        ExprNode::Call(expr) => analyze_call_expr(ctx, expr, type_hint)?,
        ExprNode::Index(expr) => analyze_index_expr(ctx, expr, type_hint)?,
        ExprNode::Cast(expr) => analyze_cast_expr(ctx, expr, type_hint)?,
        ExprNode::Grouped(expr) => analyze_grouped_expr(ctx, expr, type_hint)?,
    };
    Ok(value)
}

fn analyze_ident_expr(ctx: &mut Context, token: &Token) -> Result<Expr, CompileError> {
    let name = token.value.clone();
    let symbol = ctx
        .get(name.as_ref())
        .ok_or(CompileError::UndefinedSymbol(UndefinedSymbol {
            name: token.clone(),
        }))?;
    Ok(Expr {
        position: token.position.clone(),
        is_assignable: true,
        result_type: symbol.get_type().clone(),
        kind: ExprKind::Ident(IdentExpr { name }),
    })
}

fn analyze_integer_lit_expr(
    ctx: &mut Context,
    token: &Token,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    let result_type = if let Some(hint) = type_hint {
        match hint {
            Type::Int(t) => Type::Int(t),
            _ => get_builtin_type(ctx, INT_TYPE).unwrap(),
        }
    } else {
        get_builtin_type(ctx, INT_TYPE).unwrap()
    };

    let value = token.value.parse::<u64>().expect("invalid integer literal");

    Ok(Expr {
        position: token.position.clone(),
        is_assignable: false,
        result_type,
        kind: ExprKind::Const(ConstExpr {
            value: Const::IntConst(value),
        }),
    })
}

fn analyze_real_lit_expr(
    ctx: &mut Context,
    token: &Token,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    let result_type = if let Some(hint) = type_hint {
        match hint {
            Type::Float(t) => Type::Float(t),
            _ => get_builtin_type(ctx, FLOAT32_TYPE).unwrap(),
        }
    } else {
        get_builtin_type(ctx, FLOAT32_TYPE).unwrap()
    };

    let value = token.value.parse::<f64>().expect("invalid float literal");

    Ok(Expr {
        position: token.position.clone(),
        is_assignable: false,
        result_type,
        kind: ExprKind::Const(ConstExpr {
            value: Const::FloatConst(value),
        }),
    })
}

fn analyze_boolean_lit_expr(
    ctx: &mut Context,
    token: &Token,
    _: Option<Type>,
) -> Result<Expr, CompileError> {
    let value = match token.kind {
        TokenKind::True => true,
        TokenKind::False => false,
        _ => unreachable!("boolean expression should be either true or false"),
    };

    let result_type = get_builtin_type(ctx, BOOL_TYPE).expect("bool type should be defined");

    Ok(Expr {
        position: token.position.clone(),
        is_assignable: false,
        result_type,
        kind: ExprKind::Const(ConstExpr {
            value: Const::BoolConst(value),
        }),
    })
}

fn analyze_string_lit_expr(
    ctx: &mut Context,
    token: &Token,
    _: Option<Type>,
) -> Result<Expr, CompileError> {
    let value: String = token
        .value
        .trim_start_matches('"')
        .trim_end_matches('"')
        .into();

    let result_type = get_builtin_type(ctx, STRING_TYPE).expect("string type should be defined");

    Ok(Expr {
        position: token.position.clone(),
        is_assignable: false,
        result_type,
        kind: ExprKind::Const(ConstExpr {
            value: Const::StringConst(value),
        }),
    })
}

fn analyze_array_lit_expr(
    ctx: &mut Context,
    expr_node: &ArrayLitNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_binary_expr(
    ctx: &mut Context,
    expr_node: &BinaryExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_unary_expr(
    ctx: &mut Context,
    expr_node: &UnaryExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_call_expr(
    ctx: &mut Context,
    expr_node: &CallExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_index_expr(
    ctx: &mut Context,
    expr_node: &IndexExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_cast_expr(
    ctx: &mut Context,
    expr_node: &CastExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_grouped_expr(
    ctx: &mut Context,
    expr_node: &GroupedExprNode,
    type_hint: Option<Type>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_type(ctx: &mut Context, type_node: &TypeExprNode) -> Result<Type, CompileError> {
    Ok(match type_node {
        TypeExprNode::Ident(type_name) => {
            if let Some(symbol_type) = ctx.get(&type_name.value).map(|s| s.get_type()) {
                if let Type::Type(t) = symbol_type {
                    return Ok((**t).clone());
                }
            }
            return Err(UndefinedType {
                name: type_name.clone(),
            }
            .into());
        }
        TypeExprNode::Array(ref array_type) => {
            Type::Array(Rc::new(analyze_array_type(ctx, array_type)?))
        }
    })
}

fn get_builtin_type(ctx: &mut Context, name: &str) -> Option<Type> {
    Some(match name {
        "int" => match ctx.arch {
            Architecture::IA32 => Type::Int(Rc::new(IntType {
                bits: 32,
                signed: true,
            })),
            Architecture::IA64 => Type::Int(Rc::new(IntType {
                bits: 64,
                signed: true,
            })),
        },
        "int32" => Type::Int(Rc::new(IntType {
            bits: 32,
            signed: true,
        })),
        "int64" => Type::Int(Rc::new(IntType {
            bits: 64,
            signed: true,
        })),
        "uint" => match ctx.arch {
            Architecture::IA32 => Type::Int(Rc::new(IntType {
                bits: 32,
                signed: false,
            })),
            Architecture::IA64 => Type::Int(Rc::new(IntType {
                bits: 64,
                signed: false,
            })),
        },
        "uint32" => Type::Int(Rc::new(IntType {
            bits: 32,
            signed: false,
        })),
        "uint64" => Type::Int(Rc::new(IntType {
            bits: 64,
            signed: false,
        })),
        "float32" => Type::Float(Rc::new(FloatType { bits: 32 })),
        "float64" => Type::Float(Rc::new(FloatType { bits: 64 })),
        "bool" => Type::Bool,
        "string" => Type::String,
        _ => return None,
    })
}

fn analyze_array_type(
    ctx: &mut Context,
    array_node: &ArrayTypeNode,
) -> Result<ArrayType, CompileError> {
    let length_type = Type::Int(Rc::new(IntType {
        bits: 32,
        signed: false,
    }));
    let length = analyze_expr(ctx, &array_node.length, Some(length_type.clone()))?;
    let Type::Int(ref _int_type) = length.result_type else {
        return Err(TypeMismatch{
            expected: length_type,
            got: length,
        }.into());
    };

    let element_type = analyze_type(ctx, &array_node.element_type)?;
    Ok(ArrayType {
        length,
        element_type,
    })
}

fn analyze_func_type(
    ctx: &mut Context,
    func_node: &FuncNode,
) -> Result<FunctionType, CompileError> {
    let mut parameters = vec![];
    for param_node in func_node.head.parameters.iter() {
        parameters.push(analyze_type(ctx, &param_node.typ)?);
    }

    let return_type = if let Some(ref return_type_node) = func_node.head.return_type {
        analyze_type(ctx, return_type_node)?
    } else {
        Type::Void
    };

    Ok(FunctionType {
        parameters,
        return_type,
    })
}

fn is_type_equal(a: &Type, b: &Type) -> bool {
    todo!();
}

enum Symbol {
    Builtin { name: Rc<String>, typ: Type },
    User { token: Token, typ: Type },
}

impl Symbol {
    fn get_type(&self) -> &Type {
        match self {
            Symbol::User { token: _, typ } => typ,
            Symbol::Builtin { name: _, typ } => typ,
        }
    }
}

struct Context {
    symbol_table: HashMap<Rc<String>, Vec<Symbol>>,
    scopes: Vec<HashSet<Rc<String>>>,
    loop_depth: i32,
    current_return_type: Type,

    arch: Architecture,
}

impl Context {
    fn new(target: Target) -> Self {
        Self {
            symbol_table: HashMap::new(),
            scopes: vec![],
            loop_depth: 0,
            current_return_type: Type::Void,
            arch: target.architecture,
        }
    }

    fn get(&self, name: &String) -> Option<&Symbol> {
        self.symbol_table.get(name)?.last()
    }

    fn add_user_symbol(&mut self, token: &Token, typ: Type) {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains(&token.value) {
            unreachable!("cannot redeclare symbol with the same name");
        }
        scope.insert(token.value.clone());
        self.symbol_table
            .entry(token.value.clone())
            .or_insert(vec![])
            .push(Symbol::User {
                token: token.clone(),
                typ: typ.clone(),
            });
    }

    fn add_builtin_symbol(&mut self, name: Rc<String>, typ: Type) {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains(&name) {
            unreachable!("cannot redeclare builtin with the same name");
        }
        scope.insert(name.clone());
        self.symbol_table
            .entry(name.clone())
            .or_insert(vec![])
            .push(Symbol::Builtin {
                name: name.clone(),
                typ,
            });
    }

    fn add_loop(&mut self) {
        self.loop_depth += 1;
    }

    fn pop_loop(&mut self) {
        self.loop_depth -= 1;
    }

    fn add_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        let Some(names) = self.scopes.pop() else {
            return;
        };
        for name in names.iter() {
            self.symbol_table.get_mut(name).map(|scopes| scopes.pop());
        }
    }
}
