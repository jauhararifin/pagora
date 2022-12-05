use std::{
    collections::{HashMap, HashSet},
    iter::zip,
    rc::Rc,
};

use crate::{
    ast::{
        AssignStmtNode, BlockStmtNode, CallExprNode, ExprNode, FuncNode, IfStmtNode, Item,
        ReturnStmtNode, RootNode, StmtNode, TypeExprNode, VarNode, WhileStmtNode,
    },
    errors::{CompileError, MultiErrors, NotAssignable, TypeMismatch},
    semantic::{
        AssignStatement, BlockStatement, Builtin, CallExpr, CallStatement, Expr, Function,
        FunctionType, IfStatement, Program, Statement, Type, Variable, WhileStatement,
    },
    tokens::{Token, TokenKind},
};

pub fn analyze(root: RootNode, builtin: Builtin) -> Result<Program, CompileError> {
    let mut ctx = Context::new();
    ctx.add_scope();

    let mut errors = MultiErrors::new();

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
    for var_node in var_nodes {
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

struct Context {
    symbol_table: HashMap<Rc<String>, Vec<Symbol>>,
    scopes: Vec<HashSet<Rc<String>>>,
    loop_depth: i32,
    current_return_type: Type,
}

enum Symbol {
    Builtin { name: Rc<String>, typ: Type },
    User { token: Token, typ: Type },
}

impl Symbol {
    fn get_type(&self) -> &Type {
        match self {
            Symbol::User { token, typ } => typ,
            Symbol::Builtin { name, typ } => typ,
        }
    }
}

impl Context {
    fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            scopes: vec![],
            loop_depth: 0,
            current_return_type: Type::Void,
        }
    }

    fn get(&self, name: &String) -> Option<&Symbol> {
        self.symbol_table.get(name)?.last()
    }

    fn get_in_scope(&self, name: &String) -> Option<&Symbol> {
        if !self.scopes.last()?.contains(name) {
            None
        } else {
            self.get(name)
        }
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

fn analyze_variable(ctx: &mut Context, var_node: VarNode) -> Result<Variable, CompileError> {
    let name = var_node.name.value;

    let value = if let Some(expr_node) = var_node.value {
        Some(analyze_expr(ctx, expr_node)?)
    } else {
        None
    };

    let typ = if let Some(type_node) = var_node.typ {
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

    let body = if let Some(body) = func_node.body {
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

fn analyze_statement(ctx: &mut Context, stmt: StmtNode) -> Result<Statement, CompileError> {
    Ok(match stmt {
        StmtNode::Block(stmt) => Statement::Block(analyze_block_statement(ctx, stmt, vec![])?),
        StmtNode::Var(stmt) => Statement::Var(analyze_variable(ctx, stmt)?),
        StmtNode::Return(stmt) => Statement::Return(analyze_return_statement(ctx, stmt)?),
        StmtNode::Keyword(stmt) => analyze_keyword_statement(stmt),
        StmtNode::If(stmt) => Statement::If(analyze_if_statement(ctx, stmt)?),
        StmtNode::While(stmt) => Statement::While(analyze_while_statement(ctx, stmt)?),
        StmtNode::Assign(stmt) => Statement::Assign(analyze_assign_statement(ctx, stmt)?),
        StmtNode::Call(stmt) => Statement::Call(analyze_call_statement(ctx, stmt)?),
    })
}

fn analyze_block_statement(
    ctx: &mut Context,
    stmt: BlockStmtNode,
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
    for item in stmt.statements {
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
    stmt: ReturnStmtNode,
) -> Result<Option<Expr>, CompileError> {
    Ok(if let Some(value) = stmt.value {
        Some(analyze_expr(ctx, value)?)
    } else {
        None
    })
}

fn analyze_keyword_statement(keyword: Token) -> Statement {
    match keyword.kind {
        TokenKind::Continue => Statement::Continue,
        TokenKind::Break => Statement::Break,
        _ => unreachable!(
            "Keyword {:?} is not a valid statement keyword",
            keyword.kind
        ),
    }
}

fn analyze_if_statement(ctx: &mut Context, stmt: IfStmtNode) -> Result<IfStatement, CompileError> {
    let mut else_stmt = if let Some(s) = stmt.else_stmt {
        let body = analyze_block_statement(ctx, s.body, vec![])?;
        Some(Box::new(Statement::Block(body)))
    } else {
        None
    };

    for else_if in stmt.else_ifs.into_iter().rev() {
        let body = analyze_block_statement(ctx, else_if.body, vec![])?;
        let else_if_stmt = IfStatement {
            condition: analyze_expr(ctx, else_if.condition)?,
            body: Box::new(Statement::Block(body)),
            else_stmt: else_stmt.take(),
        };
        else_stmt = Some(Box::new(Statement::If(else_if_stmt)));
    }

    let body = analyze_block_statement(ctx, stmt.body, vec![])?;
    Ok(IfStatement {
        condition: analyze_expr(ctx, stmt.condition)?,
        body: Box::new(Statement::Block(body)),
        else_stmt,
    })
}

fn analyze_while_statement(
    ctx: &mut Context,
    stmt: WhileStmtNode,
) -> Result<WhileStatement, CompileError> {
    Ok(WhileStatement {
        condition: analyze_expr(ctx, stmt.condition)?,
        body: analyze_block_statement(ctx, stmt.body, vec![])?,
    })
}

fn analyze_assign_statement(
    ctx: &mut Context,
    stmt: AssignStmtNode,
) -> Result<AssignStatement, CompileError> {
    let receiver = analyze_expr(ctx, stmt.receiver)?;
    if !receiver.is_assignable {
        return Err(NotAssignable { receiver }.into());
    }

    let value = analyze_expr(ctx, stmt.value)?;
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
    stmt: CallExprNode,
) -> Result<CallStatement, CompileError> {
    let expr = analyze_call_expr(ctx, stmt)?;
    Ok(CallStatement { expr })
}

fn analyze_expr(ctx: &mut Context, expr_node: ExprNode) -> Result<Expr, CompileError> {
    todo!();
}

fn analyze_call_expr(ctx: &mut Context, expr_node: CallExprNode) -> Result<CallExpr, CompileError> {
    todo!();
}

fn analyze_type(ctx: &mut Context, type_node: TypeExprNode) -> Result<Type, CompileError> {
    todo!();
}

fn is_type_equal(a: &Type, b: &Type) -> bool {
    todo!();
}

fn analyze_func_type(
    ctx: &mut Context,
    func_node: &FuncNode,
) -> Result<FunctionType, CompileError> {
    todo!();
}
