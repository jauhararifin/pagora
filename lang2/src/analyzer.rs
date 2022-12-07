use crate::{
    ast::{
        ArrayLitNode, ArrayTypeNode, AssignStmtNode, BinaryExprNode, BlockStmtNode, CallExprNode,
        CastExprNode, ExprNode, FuncNode, GroupedExprNode, IfStmtNode, IndexExprNode, Item,
        ReturnStmtNode, RootNode, StmtNode, TypeExprNode, UnaryExprNode, VarNode, WhileStmtNode,
    },
    env::{Architecture, Target},
    errors::{
        CannotInferType, CompileError, InvalidBinaryOp, InvalidNumberOfArguments, InvalidUnaryOp,
        MultiErrors, NotAFunction, NotAssignable, TypeMismatch, UndefinedSymbol, UndefinedType,
    },
    semantic::{
        ArrayType, AssignStatement, BinaryExpr, BinaryOp, BlockStatement, Builtin, CallExpr,
        CallStatement, Const, ConstExpr, Expr, ExprKind, Function, FunctionType, IdentExpr,
        IfStatement, Program, Statement, Type, TypeInternal, UnaryExpr, UnaryOp, Variable,
        WhileStatement,
    },
    tokens::{Token, TokenKind},
};
use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    iter::zip,
    rc::Rc,
};

pub fn analyze(root: RootNode, target: Target, builtin: Builtin) -> Result<Program, CompileError> {
    let mut ctx = Context::new(target);
    ctx.add_scope();

    let mut errors = MultiErrors::new();

    populate_builtin_types(&mut ctx);
    for func in builtin.functions {
        ctx.add_builtin_symbol(
            func.name,
            Rc::new(Type {
                name: None,
                internal: TypeInternal::Function(func.typ),
            }),
        );
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
                ctx.add_user_symbol(
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
        Type::int(INT_TYPE.into(), arch_bit, true),
    );
    ctx.add_builtin_symbol(
        Rc::new(INT32_TYPE.into()),
        Type::int(INT32_TYPE.into(), 32, true),
    );
    ctx.add_builtin_symbol(
        Rc::new(INT64_TYPE.into()),
        Type::int(INT64_TYPE.into(), 64, true),
    );
    ctx.add_builtin_symbol(
        Rc::new(UINT_TYPE.into()),
        Type::int(UINT_TYPE.into(), 32, false),
    );
    ctx.add_builtin_symbol(
        Rc::new(UINT32_TYPE.into()),
        Type::int(UINT32_TYPE.into(), 32, false),
    );
    ctx.add_builtin_symbol(
        Rc::new(UINT64_TYPE.into()),
        Type::int(UINT64_TYPE.into(), 64, false),
    );
    ctx.add_builtin_symbol(
        Rc::new(FLOAT32_TYPE.into()),
        Type::float(FLOAT32_TYPE.into(), 32),
    );
    ctx.add_builtin_symbol(
        Rc::new(FLOAT64_TYPE.into()),
        Type::float(FLOAT64_TYPE.into(), 64),
    );
    ctx.add_builtin_symbol(Rc::new(BOOL_TYPE.into()), Type::bool(BOOL_TYPE.into()));
    ctx.add_builtin_symbol(
        Rc::new(STRING_TYPE.into()),
        Type::string(STRING_TYPE.into()),
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

    let func_type = &ctx
        .get(name.as_ref())
        .unwrap_or_else(|| panic!("function type for {} is not found", name))
        .typ;
    let func_type = match func_type.internal {
        TypeInternal::Function(ref func_type) => func_type.clone(),
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
        .map(|(param_type, param_node)| Symbol {
            token: Some(param_node.name.clone()),
            kind: SymbolKind::Variable,
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
        if let Some(ref token) = symbol.token {
            ctx.add_user_symbol(token, symbol.typ);
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
        let condition_type = get_builtin_type(ctx, BOOL_TYPE.into());
        let else_if_stmt = IfStatement {
            condition: analyze_expr(ctx, &else_if.condition, condition_type)?,
            body: Box::new(Statement::Block(body)),
            else_stmt: else_stmt.take(),
        };
        else_stmt = Some(Box::new(Statement::If(else_if_stmt)));
    }

    let body = analyze_block_statement(ctx, &stmt.body, vec![])?;
    let condition_type = get_builtin_type(ctx, BOOL_TYPE.into());
    Ok(IfStatement {
        condition: analyze_expr(ctx, &stmt.condition, condition_type)?,
        body: Box::new(Statement::Block(body)),
        else_stmt,
    })
}

fn analyze_while_statement(
    ctx: &mut Context,
    stmt: &WhileStmtNode,
) -> Result<WhileStatement, CompileError> {
    let condition_type = get_builtin_type(ctx, BOOL_TYPE.into());
    let condition = analyze_expr(ctx, &stmt.condition, condition_type)?;

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
    type_hint: Option<Rc<Type>>,
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
        result_type: symbol.typ.clone(),
        kind: ExprKind::Ident(IdentExpr { name }),
    })
}

fn analyze_integer_lit_expr(
    ctx: &mut Context,
    token: &Token,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    let result_type = if let Some(hint) = type_hint {
        match &hint.internal {
            TypeInternal::Int(_) => hint,
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
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    let result_type = if let Some(hint) = type_hint {
        match &hint.internal {
            TypeInternal::Float(_) => hint,
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
    _: Option<Rc<Type>>,
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
    _: Option<Rc<Type>>,
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
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    let element_type_hint = type_hint.map_or(None, |t| match t.internal {
        TypeInternal::Array(ref array_type) => Some(array_type.element_type.clone()),
        _ => None,
    });

    let mut values = vec![];
    for value_node in expr_node.elements.iter() {
        values.push(analyze_expr(ctx, value_node, element_type_hint.clone())?);
    }

    let element_type = if let Some(t) = values.first() {
        t.result_type.clone()
    } else if let Some(t) = element_type_hint {
        t.clone()
    } else {
        return Err(CompileError::CannotInferType(
            CannotInferType {
                position: expr_node.open_square.position.clone(),
            }
            .into(),
        ));
    };

    Ok(Expr {
        position: expr_node.open_square.position,
        is_assignable: false,
        result_type: Type::array(None, element_type),
        kind: ExprKind::Const(ConstExpr {
            value: Const::ArrayConst(values),
        }),
    })
}

lazy_static! {
    static ref INTEGER_BIN_OP: HashSet<BinaryOp> = HashSet::from([
        BinaryOp::Add,
        BinaryOp::Sub,
        BinaryOp::Mul,
        BinaryOp::Div,
        BinaryOp::Mod,
        BinaryOp::ShiftLeft,
        BinaryOp::ShiftRight,
        BinaryOp::BitAnd,
        BinaryOp::BitOr,
        BinaryOp::BitXor,
    ]);
    static ref REAL_BIN_OP: HashSet<BinaryOp> =
        HashSet::from([BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Div,]);
    static ref COMP_BIN_OP: HashSet<BinaryOp> = HashSet::from([
        BinaryOp::Gt,
        BinaryOp::GEq,
        BinaryOp::Lt,
        BinaryOp::LEq,
        BinaryOp::Eq,
        BinaryOp::NEq,
    ]);
    static ref BOOL_BIN_OP: HashSet<BinaryOp> = HashSet::from([BinaryOp::And, BinaryOp::Or]);
    static ref BINARY_OP_MAP: HashMap<TokenKind, BinaryOp> = HashMap::from([
        (TokenKind::Add, BinaryOp::Add),
        (TokenKind::Sub, BinaryOp::Sub),
        (TokenKind::Div, BinaryOp::Div),
        (TokenKind::Mul, BinaryOp::Mul),
        (TokenKind::Mod, BinaryOp::Mod),
        (TokenKind::And, BinaryOp::And),
        (TokenKind::Or, BinaryOp::Or),
        (TokenKind::BitAnd, BinaryOp::BitAnd),
        (TokenKind::BitOr, BinaryOp::BitOr),
        (TokenKind::BitXor, BinaryOp::BitXor),
        (TokenKind::Eq, BinaryOp::Eq),
        (TokenKind::NEq, BinaryOp::NEq),
        (TokenKind::Gt, BinaryOp::Gt),
        (TokenKind::GEq, BinaryOp::GEq),
        (TokenKind::Lt, BinaryOp::Lt),
        (TokenKind::LEq, BinaryOp::LEq),
        (TokenKind::ShiftLeft, BinaryOp::ShiftLeft),
        (TokenKind::ShiftRight, BinaryOp::ShiftRight),
    ]);
}

fn analyze_binary_expr(
    ctx: &mut Context,
    expr_node: &BinaryExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    // TODO: improve the type_hint based on the op token.
    let a = analyze_expr(ctx, &expr_node.a, type_hint.clone())?;
    let b = analyze_expr(ctx, &expr_node.b, Some(a.result_type.clone()))?;
    let op = BINARY_OP_MAP
        .get(&expr_node.op.kind)
        .expect("invalid binary operator");

    if !is_type_equal(&a.result_type, &b.result_type) {
        return Err(CompileError::InvalidBinaryOp(InvalidBinaryOp {
            a,
            op: op.clone(),
            b,
        }));
    }

    let typ = a.result_type.clone();
    let result_type = if INTEGER_BIN_OP.contains(op) && typ.is_int() {
        typ
    } else if REAL_BIN_OP.contains(op) && typ.is_float() {
        typ
    } else if COMP_BIN_OP.contains(op) && (typ.is_int() || typ.is_float() || typ.is_string()) {
        get_builtin_type(ctx, BOOL_TYPE).unwrap()
    } else if BOOL_BIN_OP.contains(op) && typ.is_bool() {
        get_builtin_type(ctx, BOOL_TYPE).unwrap()
    } else {
        return Err(CompileError::InvalidBinaryOp(InvalidBinaryOp {
            a,
            op: op.clone(),
            b,
        }));
    };

    Ok(Expr {
        position: a.position.clone(),
        is_assignable: false,
        result_type,
        kind: ExprKind::Binary(BinaryExpr {
            a: Box::new(a),
            op: op.clone(),
            b: Box::new(b),
        }),
    })
}

lazy_static! {
    static ref INTEGER_UNARY_OP: HashSet<UnaryOp> =
        HashSet::from([UnaryOp::Add, UnaryOp::Sub, UnaryOp::BitNot]);
    static ref REAL_UNARY_OP: HashSet<UnaryOp> = HashSet::from([UnaryOp::Add, UnaryOp::Sub]);
    static ref BOOL_UNARY_OP: HashSet<UnaryOp> = HashSet::from([UnaryOp::Not]);
    static ref UNARY_OP_MAP: HashMap<TokenKind, UnaryOp> = HashMap::from([
        (TokenKind::Add, UnaryOp::Add),
        (TokenKind::Sub, UnaryOp::Sub),
        (TokenKind::Not, UnaryOp::Not),
        (TokenKind::BitNot, UnaryOp::BitNot),
    ]);
}

fn analyze_unary_expr(
    ctx: &mut Context,
    expr_node: &UnaryExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    // TODO: improve the type_hint based on the op token.
    let value = analyze_expr(ctx, &expr_node.value, type_hint)?;
    let op = UNARY_OP_MAP
        .get(&expr_node.op.kind)
        .expect("invalid unary operator");

    let result_type = if INTEGER_UNARY_OP.contains(op) && value.result_type.is_int() {
        value.result_type.clone()
    } else if REAL_UNARY_OP.contains(op) && value.result_type.is_float() {
        value.result_type.clone()
    } else if BOOL_UNARY_OP.contains(op) && value.result_type.is_bool() {
        value.result_type.clone()
    } else {
        return Err(InvalidUnaryOp {
            op: op.clone(),
            value,
        }
        .into());
    };

    Ok(Expr {
        position: value.position.clone(),
        is_assignable: false,
        result_type,
        kind: ExprKind::Unary(UnaryExpr {
            op: op.clone(),
            value: Box::new(value),
        }),
    })
}

fn analyze_call_expr(
    ctx: &mut Context,
    expr_node: &CallExprNode,
    _: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    let callee = analyze_expr(ctx, &expr_node.target, None)?;

    let func_type = match callee.result_type.internal {
        TypeInternal::Function(ref func_type) => func_type,
        _ => return Err(NotAFunction { value: callee }.into()),
    };

    if func_type.parameters.len() != expr_node.arguments.len() {
        return Err(InvalidNumberOfArguments {
            position: callee.position.clone(),
            expected: func_type.parameters.len(),
            got: expr_node.arguments.len(),
        }
        .into());
    }

    let mut arguments = vec![];
    for (param, arg) in zip(func_type.parameters.iter(), expr_node.arguments.iter()) {
        let arg_expr = analyze_expr(ctx, &arg, Some(param.clone()))?;
        if !is_type_equal(&arg_expr.result_type, &param) {
            return Err(TypeMismatch {
                expected: param.clone(),
                got: arg_expr,
            }
            .into());
        }
        arguments.push(arg_expr);
    }

    Ok(Expr {
        position: callee.position.clone(),
        is_assignable: false,
        result_type: func_type.return_type.clone(),
        kind: ExprKind::Call(CallExpr {
            target: Box::new(callee),
            arguments,
        }),
    })
}

fn analyze_index_expr(
    ctx: &mut Context,
    expr_node: &IndexExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_cast_expr(
    ctx: &mut Context,
    expr_node: &CastExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_grouped_expr(
    ctx: &mut Context,
    expr_node: &GroupedExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr, CompileError> {
    analyze_expr(ctx, &expr_node.value, type_hint)
}

fn analyze_type(ctx: &mut Context, type_node: &TypeExprNode) -> Result<Rc<Type>, CompileError> {
    Ok(match type_node {
        TypeExprNode::Ident(type_name) => {
            if let Some(symbol) = ctx.get(&type_name.value) {
                if let SymbolKind::Type = symbol.kind {
                    return Ok(symbol.typ.clone());
                }
            }
            return Err(UndefinedType {
                name: type_name.clone(),
            }
            .into());
        }
        TypeExprNode::Array(ref array_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Array(analyze_array_type(ctx, array_type)?),
        }),
    })
}

// TODO: consider using lazy static instead of calling method like this.
fn get_builtin_type(ctx: &mut Context, name: &str) -> Option<Rc<Type>> {
    ctx.get(&String::from(name))
        .map(|symbol| symbol.typ.clone())
}

fn analyze_array_type(
    ctx: &mut Context,
    array_node: &ArrayTypeNode,
) -> Result<ArrayType, CompileError> {
    let element_type = analyze_type(ctx, &array_node.element_type)?;
    Ok(ArrayType { element_type })
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
        Type::tuple(vec![])
    };

    Ok(FunctionType {
        parameters,
        return_type,
    })
}

fn is_type_equal(a: &Type, b: &Type) -> bool {
    a == b
}

struct Symbol {
    token: Option<Token>,
    kind: SymbolKind,
    typ: Rc<Type>,
}

enum SymbolKind {
    Variable,
    Type,
}

struct Context {
    symbol_table: HashMap<Rc<String>, Vec<Symbol>>,
    scopes: Vec<HashSet<Rc<String>>>,
    loop_depth: i32,
    current_return_type: Rc<Type>,

    arch: Architecture,
}

impl Context {
    fn new(target: Target) -> Self {
        Self {
            symbol_table: HashMap::new(),
            scopes: vec![],
            loop_depth: 0,
            current_return_type: Type::tuple(vec![]),
            arch: target.architecture,
        }
    }

    fn get(&self, name: &String) -> Option<&Symbol> {
        self.symbol_table.get(name)?.last()
    }

    fn add_user_symbol(&mut self, token: &Token, typ: Rc<Type>) {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains(&token.value) {
            unreachable!("cannot redeclare symbol with the same name");
        }
        scope.insert(token.value.clone());
        self.symbol_table
            .entry(token.value.clone())
            .or_insert(vec![])
            .push(Symbol {
                token: Some(token.clone()),
                kind: SymbolKind::Variable,
                typ: typ.clone(),
            });
    }

    fn add_builtin_symbol(&mut self, name: Rc<String>, typ: Rc<Type>) {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains(&name) {
            unreachable!("cannot redeclare builtin with the same name");
        }
        scope.insert(name.clone());
        self.symbol_table
            .entry(name.clone())
            .or_insert(vec![])
            .push(Symbol {
                token: None,
                kind: SymbolKind::Variable,
                typ: typ.clone(),
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
