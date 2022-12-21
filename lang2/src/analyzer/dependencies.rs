use super::scope::Scope;
use crate::{
    ast::{
        BlockStmtNode, CallExprNode, ExprNode, FuncNode, IfStmtNode, ItemNode, StmtNode,
        VarStmtNode, WhileStmtNode,
    },
    errors::{unexpected, CompileError, Result},
    semantic::IfStatement,
};
use std::rc::Rc;

fn get_item_dependencies(scope: &mut Scope, item: &ItemNode) -> Result<Vec<Rc<String>>> {
    match item {
        ItemNode::Import(_) => Ok(vec![]),
        ItemNode::Type(node) => get_expr_dependencies(scope, &node.type_expr),
        ItemNode::Var(node) => node
            .stmt
            .value
            .as_ref()
            .and_then(|expr| Some(get_expr_dependencies(scope, &expr)))
            .unwrap_or(Ok(vec![])),
        ItemNode::Func(node) => get_func_dependencies(scope, node),
    }
}

fn get_type_dependencies(scope: &Scope, expr_node: &ExprNode) -> Result<Vec<Rc<String>>> {
    match expr_node {
        ExprNode::Deref(_) => Ok(vec![]),
        ExprNode::Ident(node) => Ok(if scope.has_in_root(&node.value) {
            vec![node.value.clone()]
        } else {
            vec![]
        }),
        ExprNode::Selection(node) => get_type_dependencies(scope, &node.value),
        ExprNode::Grouped(node) => get_type_dependencies(scope, &node.value),
        ExprNode::Array(node) => get_type_dependencies(scope, &node.element_type),
        ExprNode::Struct(node) => {
            let mut result = vec![];
            let mut errors = CompileError::new();
            for field in node.fields.iter() {
                match get_type_dependencies(scope, &field.type_expr) {
                    Ok(deps) => result.extend_from_slice(&deps),
                    Err(err) => errors.push(err),
                }
            }
            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(result)
            }
        }
        _ => Err(unexpected(
            "TYPE",
            "EXPR",
            match expr_node {
                ExprNode::IntegerLit(node) => node.position.clone(),
                ExprNode::RealLit(node) => node.position.clone(),
                ExprNode::BooleanLit(node) => node.position.clone(),
                ExprNode::StringLit(node) => node.position.clone(),
                ExprNode::CompositeLit(node) => node.open_block.position.clone(),
                ExprNode::KeyValue(node) => node.key.position.clone(),
                ExprNode::Binary(node) => node.op.position.clone(),
                ExprNode::Addr(node) => node.ampersand.position.clone(),
                ExprNode::Deref(node) => node.asterisk.position.clone(),
                ExprNode::Unary(node) => node.op.position.clone(),
                ExprNode::Call(node) => node.open_brac.position.clone(),
                ExprNode::Index(node) => node.open_square.position.clone(),
                ExprNode::Cast(node) => node.as_tok.position.clone(),
                ExprNode::Selection(node) => node.dot.position.clone(),
                ExprNode::Grouped(node) => node.open_brac.position.clone(),
                _ => unreachable!(),
            },
        )),
    }
}

fn get_expr_dependencies(scope: &Scope, expr_node: &ExprNode) -> Result<Vec<Rc<String>>> {
    match expr_node {
        ExprNode::Ident(node) => Ok(if scope.has_in_root(&node.value) {
            vec![node.value.clone()]
        } else {
            vec![]
        }),
        ExprNode::IntegerLit(_)
        | ExprNode::RealLit(_)
        | ExprNode::BooleanLit(_)
        | ExprNode::StringLit(_) => Ok(vec![]),
        ExprNode::CompositeLit(node) => get_exprs_dependencies(scope, node.values.iter()),
        ExprNode::KeyValue(node) => get_expr_dependencies(scope, &node.value),
        ExprNode::Binary(node) => {
            get_exprs_dependencies(scope, [node.a.as_ref(), node.b.as_ref()].into_iter())
        }
        ExprNode::Addr(node) => get_expr_dependencies(scope, &node.value),
        ExprNode::Deref(node) => get_expr_dependencies(scope, &node.value),
        ExprNode::Unary(node) => get_expr_dependencies(scope, &node.value),
        ExprNode::Call(node) => {
            let mut result = vec![];
            let mut errors = CompileError::new();
            match get_expr_dependencies(scope, &node.target) {
                Ok(deps) => result.extend_from_slice(&deps),
                Err(err) => errors.push(err),
            }
            for arg in node.arguments.iter() {
                match get_type_dependencies(scope, &arg) {
                    Ok(deps) => result.extend_from_slice(&deps),
                    Err(err) => errors.push(err),
                }
            }
            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(result)
            }
        }
        ExprNode::Index(node) => get_exprs_dependencies(
            scope,
            [node.target.as_ref(), node.index.as_ref()].into_iter(),
        ),
        ExprNode::Cast(node) => {
            let mut result = vec![];
            let mut errors = CompileError::new();
            match get_expr_dependencies(scope, &node.target) {
                Ok(deps) => result.extend_from_slice(&deps),
                Err(err) => errors.push(err),
            }
            match get_type_dependencies(scope, &node.value) {
                Ok(deps) => result.extend_from_slice(&deps),
                Err(err) => errors.push(err),
            }
            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(result)
            }
        }
        ExprNode::Selection(node) => get_expr_dependencies(scope, &node.value),
        ExprNode::Grouped(node) => get_expr_dependencies(scope, &node.value),
        _ => Err(unexpected(
            "TYPE",
            "EXPR",
            match expr_node {
                ExprNode::Array(node) => node.open_square.position.clone(),
                ExprNode::Struct(node) => node.open_block.position.clone(),
                _ => unreachable!(),
            },
        )),
    }
}

fn get_exprs_dependencies<'a>(
    scope: &Scope,
    exprs: impl Iterator<Item = &'a ExprNode>,
) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();
    for expr in exprs {
        match get_type_dependencies(scope, &expr) {
            Ok(deps) => result.extend_from_slice(&deps),
            Err(err) => errors.push(err),
        }
    }
    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}

fn get_func_dependencies(scope: &mut Scope, func_node: &FuncNode) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();

    for param in func_node.head.parameters.iter() {
        match get_type_dependencies(scope, &param.type_expr) {
            Ok(deps) => result.extend_from_slice(&deps),
            Err(err) => errors.push(err),
        }
    }

    if let Some(return_type) = &func_node.head.return_type {
        match get_type_dependencies(scope, return_type) {
            Ok(deps) => result.extend_from_slice(&deps),
            Err(err) => errors.push(err),
        }
    }

    if let Some(ref body) = func_node.body {
        let param_names = func_node
            .head
            .parameters
            .iter()
            .map(|param| param.name.value.clone());
        match get_block_stmt_dependencies(scope, body, param_names) {
            Ok(deps) => result.extend_from_slice(&deps),
            Err(err) => errors.push(err),
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}

fn get_stmt_dependencies(scope: &mut Scope, stmt: &StmtNode) -> Result<Vec<Rc<String>>> {
    match stmt {
        StmtNode::Block(node) => get_block_stmt_dependencies(scope, node, vec![].into_iter()),
        StmtNode::Var(node) => get_var_stmt_dependencies(scope, node),
        StmtNode::Return(node) => match node.value {
            Some(ref value) => get_expr_dependencies(scope, value),
            None => Ok(vec![]),
        },
        StmtNode::Keyword(_) => Ok(vec![]),
        StmtNode::If(node) => get_if_stmt_dependencies(scope, node),
        StmtNode::While(node) => get_while_stmt_dependencies(scope, node),
        StmtNode::Assign(node) => get_expr_dependencies(scope, &node.value),
        StmtNode::Call(node) => get_call_stmt_dependencies(scope, node),
    }
}

fn get_block_stmt_dependencies(
    scope: &mut Scope,
    block_stmt: &BlockStmtNode,
    additional_names: impl Iterator<Item = Rc<String>>,
) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();

    scope.add_scope();

    for name in additional_names {
        if let Err(err) = scope.add_name(name) {
            errors.push(err);
        }
    }

    for stmt in block_stmt.statements.iter() {
        match get_stmt_dependencies(scope, stmt) {
            Ok(deps) => result.extend_from_slice(&deps),
            Err(err) => errors.push(err),
        }
    }

    scope.pop_scope();

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}

fn get_var_stmt_dependencies(scope: &mut Scope, var_stmt: &VarStmtNode) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();

    if let Some(ref expr) = var_stmt.value {
        match get_expr_dependencies(scope, expr) {
            Ok(deps) => result.extend_from_slice(&deps),
            Err(err) => errors.push(err),
        }
    }

    scope.add_name(var_stmt.name.value.clone());

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}

fn get_if_stmt_dependencies(scope: &mut Scope, if_stmt: &IfStmtNode) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();

    match get_expr_dependencies(scope, &if_stmt.condition) {
        Ok(deps) => result.extend_from_slice(&deps),
        Err(err) => errors.push(err),
    }

    match get_block_stmt_dependencies(scope, &if_stmt.body, vec![].into_iter()) {
        Ok(deps) => result.extend_from_slice(&deps),
        Err(err) => errors.push(err),
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}

fn get_while_stmt_dependencies(
    scope: &mut Scope,
    while_stmt: &WhileStmtNode,
) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();

    match get_expr_dependencies(scope, &while_stmt.condition) {
        Ok(deps) => result.extend_from_slice(&deps),
        Err(err) => errors.push(err),
    }

    match get_block_stmt_dependencies(scope, &while_stmt.body, vec![].into_iter()) {
        Ok(deps) => result.extend_from_slice(&deps),
        Err(err) => errors.push(err),
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}

fn get_call_stmt_dependencies(
    scope: &mut Scope,
    call_stmt: &CallExprNode,
) -> Result<Vec<Rc<String>>> {
    let mut result = vec![];
    let mut errors = CompileError::new();

    match get_expr_dependencies(scope, &call_stmt.target) {
        Ok(deps) => result.extend_from_slice(&deps),
        Err(err) => errors.push(err),
    }

    match get_exprs_dependencies(scope, call_stmt.arguments.iter()) {
        Ok(deps) => result.extend_from_slice(&deps),
        Err(err) => errors.push(err),
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}
