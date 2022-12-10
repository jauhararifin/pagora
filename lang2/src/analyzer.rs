use crate::{
    ast::{
        ArrayLitNode, ArrayTypeNode, AssignStmtNode, BinaryExprNode, BlockStmtNode, CallExprNode,
        CastExprNode, ExprNode, FuncNode, GroupedExprNode, IfStmtNode, IndexExprNode, Item,
        ReturnStmtNode, RootNode, StmtNode, StructNode, TupleTypeNode, TypeExprNode, UnaryExprNode,
        VarNode, VarStmtNode, WhileStmtNode,
    },
    builtin::{get_builtin, BOOL_TYPE, FLOAT32_TYPE, INT32_TYPE, STRING_TYPE},
    errors::{
        cannot_cast, cannot_infer_type, cannot_redeclare_symbool, invalid_binary_op,
        invalid_number_of_argument, invalid_unary_op, not_a_function, not_an_array, not_assignable,
        type_mismatch, undefined_symbol, undefined_type, CompileError, Result,
    },
    semantic::{
        ArrayType, AssignStatement, BinaryExpr, BinaryOp, BlockStatement, CallExpr, CallStatement,
        CastExpr, Const, ConstExpr, Expr, ExprKind, Function, FunctionType, IdentExpr, IfStatement,
        IndexExpr, Program, Statement, StructField, StructType, TupleType, Type, TypeInternal,
        UnaryExpr, UnaryOp, Variable, WhileStatement,
    },
    tokens::{Token, TokenKind},
};
use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    iter::zip,
    rc::Rc,
};

pub fn analyze(root: RootNode) -> Result<Program> {
    let builtin = get_builtin();
    let mut ctx = Context::new();
    ctx.add_scope();

    let mut errors = CompileError::new();

    // load all builtin types
    let types = builtin.types;
    for typ in types.iter() {
        ctx.add_builtin_symbol(
            Rc::new(typ.name.as_ref().unwrap().clone()),
            SymbolKind::Type,
            typ.clone(),
        );
    }

    // load all user typenames
    for item in root.items.iter() {
        let name = match item {
            Item::Struct(node) => &node.name,
            Item::Tuple(node) => &node.name,
            _ => continue,
        };
        if let Err(err) = ctx.add_typename(name) {
            errors.push(err);
        }
    }

    // load all user types
    for item in root.items.iter() {
        match item {
            Item::Struct(struct_node) => {
                let typ = match analyze_struct_type(&mut ctx, struct_node) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                };
                let typ = Rc::new(Type {
                    name: Some(struct_node.name.value.as_ref().clone()),
                    internal: TypeInternal::Struct(typ),
                });
                ctx.add_user_symbol(&struct_node.name, SymbolKind::Type, typ);
            }
            Item::Tuple(tuple_node) => {
                let typ = match analyze_tuple_type(&mut ctx, &tuple_node.typ) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                };
                let typ = Rc::new(Type {
                    name: Some(tuple_node.name.value.as_ref().clone()),
                    internal: TypeInternal::Tuple(typ),
                });
                ctx.add_user_symbol(&tuple_node.name, SymbolKind::Type, typ);
            }
            _ => continue,
        }
    }

    // load all builtin functions
    for func in builtin.functions {
        ctx.add_builtin_symbol(
            func.name,
            SymbolKind::Variable,
            Rc::new(Type {
                name: None,
                internal: TypeInternal::Function(func.typ),
            }),
        );
    }

    // extract func and var nodes
    let mut func_nodes = vec![];
    let mut var_nodes = vec![];
    for item in root.items {
        match item {
            Item::Func(func_node) => func_nodes.push(func_node),
            Item::Var(var_node) => var_nodes.push(var_node),
            _ => continue,
        }
    }

    // analyze user function types
    for func_node in func_nodes.iter() {
        match analyze_func_type(&mut ctx, func_node) {
            Ok(func_type) => {
                ctx.add_user_symbol(
                    &func_node.head.name,
                    SymbolKind::Variable,
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

    // analyze user variables
    let mut variables = Vec::<Variable>::new();
    for var_node in var_nodes.iter() {
        match analyze_variable(&mut ctx, var_node) {
            Ok(variable) => variables.push(variable),
            Err(err) => errors.push(err),
        }
    }

    // analyze user functions
    let mut functions = Vec::<Function>::new();
    for func_node in func_nodes {
        let Some(func_type) = &ctx.get_val(func_node.head.name.value.as_ref()) else {
            continue;
        };
        let func_type = match func_type.typ.internal {
            TypeInternal::Function(ref func_type) => func_type.clone(),
            _ => continue,
        };

        match analyze_function(&mut ctx, func_node, func_type) {
            Ok(func) => {
                functions.push(func);
            }
            Err(err) => {
                errors.push(err);
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(Program {
            types,
            variables,
            functions,
        })
    }
}

fn analyze_variable(ctx: &mut Context, var_node: &VarNode) -> Result<Variable> {
    analyze_var_stmt(ctx, &var_node.stmt)
}

fn analyze_function(
    ctx: &mut Context,
    func_node: FuncNode,
    func_type: FunctionType,
) -> Result<Function> {
    let name = func_node.head.name.value.clone();
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

fn analyze_statement(ctx: &mut Context, stmt: &StmtNode) -> Result<Statement> {
    Ok(match stmt {
        StmtNode::Block(stmt) => Statement::Block(analyze_block_statement(ctx, stmt, vec![])?),
        StmtNode::Var(stmt) => Statement::Var(analyze_var_stmt(ctx, stmt)?),
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
) -> Result<BlockStatement> {
    ctx.add_scope();

    for symbol in additional_symbols {
        if let Some(ref token) = symbol.token {
            ctx.add_user_symbol(token, SymbolKind::Variable, symbol.typ);
        }
    }

    let mut errors = CompileError::new();
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

fn analyze_var_stmt(ctx: &mut Context, stmt: &VarStmtNode) -> Result<Variable> {
    let name = stmt.name.value.clone();

    let value = if let Some(ref expr_node) = stmt.value {
        Some(analyze_expr(ctx, expr_node, None)?)
    } else {
        None
    };

    let typ = if let Some(ref type_node) = stmt.typ {
        analyze_type(ctx, type_node)?
    } else if let Some(ref value) = value {
        value.result_type.clone()
    } else {
        unreachable!("a variable should has type or value");
    };

    ctx.add_user_symbol(&stmt.name, SymbolKind::Variable, typ.clone());
    Ok(Variable { name, typ, value })
}

fn analyze_return_statement(ctx: &mut Context, stmt: &ReturnStmtNode) -> Result<Option<Expr>> {
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

fn analyze_if_statement(ctx: &mut Context, stmt: &IfStmtNode) -> Result<IfStatement> {
    let mut else_stmt = if let Some(ref s) = stmt.else_stmt {
        let body = analyze_block_statement(ctx, &s.body, vec![])?;
        Some(Box::new(Statement::Block(body)))
    } else {
        None
    };

    for else_if in stmt.else_ifs.iter().rev() {
        let body = analyze_block_statement(ctx, &else_if.body, vec![])?;
        let condition_type = get_builtin_type(ctx, BOOL_TYPE);
        let else_if_stmt = IfStatement {
            condition: analyze_expr(ctx, &else_if.condition, condition_type)?,
            body: Box::new(Statement::Block(body)),
            else_stmt: else_stmt.take(),
        };
        else_stmt = Some(Box::new(Statement::If(else_if_stmt)));
    }

    let body = analyze_block_statement(ctx, &stmt.body, vec![])?;
    let condition_type = get_builtin_type(ctx, BOOL_TYPE);
    Ok(IfStatement {
        condition: analyze_expr(ctx, &stmt.condition, condition_type)?,
        body: Box::new(Statement::Block(body)),
        else_stmt,
    })
}

fn analyze_while_statement(ctx: &mut Context, stmt: &WhileStmtNode) -> Result<WhileStatement> {
    let condition_type = get_builtin_type(ctx, BOOL_TYPE);
    let condition = analyze_expr(ctx, &stmt.condition, condition_type)?;

    ctx.add_loop();
    let body = analyze_block_statement(ctx, &stmt.body, vec![])?;
    ctx.pop_loop();

    Ok(WhileStatement { condition, body })
}

fn analyze_assign_statement(ctx: &mut Context, stmt: &AssignStmtNode) -> Result<AssignStatement> {
    let receiver = analyze_expr(ctx, &stmt.receiver, None)?;
    if !receiver.is_assignable {
        return Err(not_assignable(&receiver));
    }

    let value = analyze_expr(ctx, &stmt.value, Some(receiver.result_type.clone()))?;
    if !is_type_equal(&receiver.result_type, &value.result_type) {
        return Err(type_mismatch(&receiver.result_type, &value));
    }

    Ok(AssignStatement {
        receiver: Box::new(receiver),
        value: Box::new(value),
    })
}

fn analyze_call_statement(ctx: &mut Context, stmt: &CallExprNode) -> Result<CallStatement> {
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
) -> Result<Expr> {
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

fn analyze_ident_expr(ctx: &mut Context, token: &Token) -> Result<Expr> {
    let name = token.value.clone();
    let symbol = ctx.get_val(name.as_ref()).ok_or(undefined_symbol(&token))?;
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
) -> Result<Expr> {
    let result_type = if let Some(hint) = type_hint {
        match &hint.internal {
            TypeInternal::Int(_) => hint,
            _ => get_builtin_type(ctx, INT32_TYPE).unwrap(),
        }
    } else {
        get_builtin_type(ctx, INT32_TYPE).unwrap()
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
) -> Result<Expr> {
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

fn analyze_boolean_lit_expr(ctx: &mut Context, token: &Token, _: Option<Rc<Type>>) -> Result<Expr> {
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

fn analyze_string_lit_expr(ctx: &mut Context, token: &Token, _: Option<Rc<Type>>) -> Result<Expr> {
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
) -> Result<Expr> {
    let element_type_hint = type_hint.and_then(|t| match t.internal {
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
        return Err(cannot_infer_type(&expr_node.open_square.position));
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
) -> Result<Expr> {
    // TODO: improve the type_hint based on the op token.
    let a = analyze_expr(ctx, &expr_node.a, type_hint.clone())?;
    let b = analyze_expr(ctx, &expr_node.b, Some(a.result_type.clone()))?;
    let op = BINARY_OP_MAP
        .get(&expr_node.op.kind)
        .expect("invalid binary operator");

    if !is_type_equal(&a.result_type, &b.result_type) {
        return Err(invalid_binary_op(&a, &expr_node.op, &b));
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
        return Err(invalid_binary_op(&a, &expr_node.op, &b));
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
        (TokenKind::BitAnd, UnaryOp::Addr),
    ]);
}

fn analyze_unary_expr(
    ctx: &mut Context,
    expr_node: &UnaryExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr> {
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
    } else if let UnaryOp::Addr = op {
        Rc::new(Type {
            name: None,
            internal: TypeInternal::Pointer(value.result_type.clone()),
        })
    } else {
        return Err(invalid_unary_op(&expr_node.op, &value));
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
) -> Result<Expr> {
    let callee = analyze_expr(ctx, &expr_node.target, None)?;

    let func_type = match callee.result_type.internal {
        TypeInternal::Function(ref func_type) => func_type,
        _ => return Err(not_a_function(&callee)),
    };

    if func_type.parameters.len() != expr_node.arguments.len() {
        return Err(invalid_number_of_argument(
            &callee.position,
            func_type.parameters.len(),
            expr_node.arguments.len(),
        ));
    }

    let mut arguments = vec![];
    for (param, arg) in zip(func_type.parameters.iter(), expr_node.arguments.iter()) {
        let arg_expr = analyze_expr(ctx, &arg, Some(param.clone()))?;
        if !is_type_equal(&arg_expr.result_type, &param) {
            return Err(type_mismatch(param.as_ref(), &arg_expr));
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
    _: Option<Rc<Type>>,
) -> Result<Expr> {
    let target = analyze_expr(ctx, &expr_node.target, None)?;

    let array_type = match target.result_type.internal {
        TypeInternal::Array(ref array_type) => array_type,
        _ => return Err(not_an_array(&target)),
    };

    let int_type = get_builtin_type(ctx, INT32_TYPE);
    let index = analyze_expr(ctx, &expr_node.index, int_type)?;
    if !index.result_type.is_int() {
        return Err(type_mismatch(
            get_builtin_type(ctx, INT32_TYPE).unwrap().as_ref(),
            &index,
        ));
    }

    Ok(Expr {
        position: target.position.clone(),
        is_assignable: false,
        result_type: array_type.element_type.clone(),
        kind: ExprKind::Index(IndexExpr {
            target: Box::new(target),
            index: Box::new(index),
        }),
    })
}

fn analyze_cast_expr(
    ctx: &mut Context,
    expr_node: &CastExprNode,
    _: Option<Rc<Type>>,
) -> Result<Expr> {
    let value = analyze_expr(ctx, &expr_node.value, None)?;
    let target = analyze_type(ctx, &expr_node.target)?;

    let castable = (value.result_type.is_int() || value.result_type.is_float())
        && (target.is_int() || target.is_float());
    if !castable {
        return Err(cannot_cast(&value, &target));
    }

    Ok(Expr {
        position: value.position.clone(),
        is_assignable: false,
        result_type: target.clone(),
        kind: ExprKind::Cast(CastExpr {
            value: Box::new(value),
            target: target.clone(),
        }),
    })
}

fn analyze_grouped_expr(
    ctx: &mut Context,
    expr_node: &GroupedExprNode,
    type_hint: Option<Rc<Type>>,
) -> Result<Expr> {
    analyze_expr(ctx, &expr_node.value, type_hint)
}

fn analyze_type(ctx: &mut Context, type_node: &TypeExprNode) -> Result<Rc<Type>> {
    Ok(match type_node {
        TypeExprNode::Tuple(tuple_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Tuple(analyze_tuple_type(ctx, tuple_type)?),
        }),
        TypeExprNode::Ident(type_name) => {
            if let Some(symbol) = ctx.get_type(&type_name.value) {
                if let SymbolKind::Type = symbol.kind {
                    return Ok(symbol.typ.clone());
                }
            }
            return Err(undefined_type(type_name));
        }
        TypeExprNode::Array(ref array_type) => Rc::new(Type {
            name: None,
            internal: TypeInternal::Array(analyze_array_type(ctx, array_type)?),
        }),
        TypeExprNode::Pointer(type_node) => {
            if let TypeExprNode::Ident(type_name) = type_node.as_ref() {
                if !ctx.has_typename(&type_name.value) {
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
                analyze_type(ctx, type_node)?
            }
        }
    })
}

fn analyze_tuple_type(ctx: &mut Context, tuple_node: &TupleTypeNode) -> Result<TupleType> {
    let mut items = vec![];
    for item_node in tuple_node.fields.iter() {
        items.push(analyze_type(ctx, item_node)?);
    }

    Ok(TupleType { items })
}

// TODO: consider using lazy static instead of calling method like this.
fn get_builtin_type(ctx: &mut Context, name: &str) -> Option<Rc<Type>> {
    ctx.get_type(&String::from(name))
        .map(|symbol| symbol.typ.clone())
}

fn analyze_array_type(ctx: &mut Context, array_node: &ArrayTypeNode) -> Result<ArrayType> {
    let element_type = analyze_type(ctx, &array_node.element_type)?;
    Ok(ArrayType { element_type })
}

fn analyze_func_type(ctx: &mut Context, func_node: &FuncNode) -> Result<FunctionType> {
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

fn analyze_struct_type(ctx: &mut Context, struct_node: &StructNode) -> Result<StructType> {
    let mut fields = vec![];
    for field in struct_node.fields.iter() {
        let name = field.name.value.clone();
        let typ = analyze_type(ctx, &field.typ)?;
        fields.push(StructField { name, typ });
    }

    Ok(StructType { fields })
}

fn is_type_equal(a: &Type, b: &Type) -> bool {
    a == b
}

#[derive(Debug)]
struct Symbol {
    token: Option<Token>,
    kind: SymbolKind,
    typ: Rc<Type>,
}

#[derive(Debug)]
enum SymbolKind {
    Variable,
    Type,
}

struct Context {
    type_names: HashMap<Rc<String>, Token>,
    symbol_table: HashMap<Rc<String>, Vec<Symbol>>,
    scopes: Vec<HashSet<Rc<String>>>,
    loop_depth: i32,
    current_return_type: Rc<Type>,
}

impl Context {
    fn new() -> Self {
        Self {
            type_names: HashMap::new(),
            symbol_table: HashMap::new(),
            scopes: vec![],
            loop_depth: 0,
            current_return_type: Type::tuple(vec![]),
        }
    }

    fn has_typename(&self, name: &Rc<String>) -> bool {
        self.type_names.contains_key(name)
    }

    fn add_typename(&mut self, name: &Token) -> Result<()> {
        let entry = self.type_names.get(name.value.as_ref());
        if let Some(declared_at) = entry {
            return Err(cannot_redeclare_symbool(name, declared_at));
        }
        self.type_names.insert(name.value.clone(), name.clone());
        Ok(())
    }

    fn get_type(&self, name: &String) -> Option<&Symbol> {
        self.symbol_table
            .get(name)?
            .last()
            .and_then(|sym| match sym.kind {
                SymbolKind::Type => Some(sym),
                _ => None,
            })
    }

    fn get_val(&self, name: &String) -> Option<&Symbol> {
        self.symbol_table
            .get(name)?
            .last()
            .and_then(|sym| match sym.kind {
                SymbolKind::Variable => Some(sym),
                _ => None,
            })
    }

    fn add_user_symbol(&mut self, token: &Token, kind: SymbolKind, typ: Rc<Type>) {
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
                kind,
                typ: typ.clone(),
            });
    }

    fn add_builtin_symbol(&mut self, name: Rc<String>, kind: SymbolKind, typ: Rc<Type>) {
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
                kind,
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
