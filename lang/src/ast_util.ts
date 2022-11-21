import {
  AssignStatementNode,
  BlockStatementNode,
  ExprNode,
  ExprNodeKind,
  FunctionNode,
  IfStatementNode,
  MainNode,
  ParamNode,
  ParamsNode,
  ReturnStatementNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
  VarNode,
  VarStatementNode,
  WhileStatementNode,
} from './ast'

export function encodeAst(root: RootNode): any {
  const funcs = root.functions.map(encodeFunctionDeclNode)
  const vars = root.variables.map(encodeVarNode)
  if (root.main !== undefined) {
    return funcs.concat(vars).concat(encodeMainDeclNode(root.main))
  }
  return funcs.concat(vars)
}

function encodeFunctionDeclNode(node: FunctionNode): any[] {
  return [
    node.function.encode(),
    node.name.encode(),
    node.openBrac.encode(),
    encodeParamsNode(node.params),
    node.closeBrac.encode(),
    node.arrow?.encode(),
    node.returnType != null ? encodeTypeExprNode(node.returnType) : undefined,
    node.body !== undefined ? encodeBlockStatementNode(node.body) : undefined,
  ]
}

function encodeMainDeclNode(node: MainNode): any[] {
  return encodeBlockStatementNode(node.body)
}

function encodeVarNode(node: VarNode): any[] {
  return [
    node.var.encode(),
    node.name.encode(),
    node.colon?.encode(),
    node.type != null ? encodeTypeExprNode(node.type) : undefined,
    node.assign?.encode(),
    node.value != null ? encodeExprNode(node.value) : undefined,
  ]
}

function encodeParamsNode(node: ParamsNode): any[] {
  return node.params.map(encodeParamGroup)
}

function encodeParamGroup(node: ParamNode): any[] {
  return [
    node.name.encode(),
    node.colon.encode(),
    encodeTypeExprNode(node.type),
  ]
}

function encodeTypeExprNode(node: TypeExprNode): any {
  switch (node.kind) {
    case TypeExprNodeKind.IDENT:
      return node.type.encode()
    case TypeExprNodeKind.ARRAY:
      return [
        node.array.encode(),
        node.openSquare.encode(),
        node.dimension.values.flatMap(encodeExprNode),
        node.closeSquare.encode(),
        node.of.encode(),
        encodeTypeExprNode(node.elementType),
      ]
  }
}

function encodeStatementNode(node: StatementNode): any {
  switch (node.kind) {
    case StatementNodeKind.VAR:
      return encodeVarStatement(node)
    case StatementNodeKind.ASSIGN:
      return encodeAssignStatement(node)
    case StatementNodeKind.RETURN:
      return encodeReturnStatement(node)
    case StatementNodeKind.KEYWORD:
      return node.keyword.kind
    case StatementNodeKind.IF:
      return encodeIfStatement(node)
    case StatementNodeKind.WHILE:
      return encodeWhileStatement(node)
    case StatementNodeKind.BLOCK:
      return encodeBlockStatementNode(node)
    case StatementNodeKind.EXPR:
      return encodeExprNode(node.expr)
  }
}

function encodeVarStatement(node: VarStatementNode): any[] {
  return encodeVarNode(node.variable)
}

function encodeAssignStatement(node: AssignStatementNode): any[] {
  return [
    encodeExprNode(node.receiver),
    node.assign.encode(),
    encodeExprNode(node.value),
  ]
}

function encodeReturnStatement(node: ReturnStatementNode): any[] {
  return [
    node.return.encode(),
    node.value != null ? encodeExprNode(node.value) : undefined,
  ]
}

function encodeIfStatement(node: IfStatementNode): any[] {
  return [
    node.if.encode(),
    encodeExprNode(node.condition),
    node.then.encode(),
    encodeStatementNode(node.body),
  ]
}

function encodeWhileStatement(node: WhileStatementNode): any[] {
  return [
    node.while.encode(),
    encodeExprNode(node.condition),
    node.do.encode(),
    encodeStatementNode(node.body),
  ]
}

function encodeBlockStatementNode(node: BlockStatementNode): any[] {
  return [
    node.begin.encode(),
    node.statements.map(encodeStatementNode),
    node.end.encode(),
  ]
}

function encodeExprNode(node: ExprNode): any {
  switch (node.kind) {
    case ExprNodeKind.IDENT:
      return node.name.encode()
    case ExprNodeKind.INTEGER_LIT:
      return node.value.encode()
    case ExprNodeKind.BOOLEAN_LIT:
      return node.value.encode()
    case ExprNodeKind.BINARY:
      return [encodeExprNode(node.a), node.op.encode(), encodeExprNode(node.b)]
    case ExprNodeKind.UNARY:
      return [node.op.encode(), encodeExprNode(node.value)]
    case ExprNodeKind.CALL:
      return [
        encodeExprNode(node.callee),
        node.openBrac.encode(),
        node.arguments.values.flatMap(encodeExprNode),
        node.closeBrac.encode(),
      ]
    case ExprNodeKind.ARRAY_INDEX:
      return [
        encodeExprNode(node.array),
        node.openSquare.encode(),
        node.index.values.flatMap(encodeExprNode),
        node.closeSquare.encode(),
      ]
    case ExprNodeKind.CAST:
      return [
        encodeExprNode(node.source),
        node.as.encode(),
        encodeTypeExprNode(node.target),
      ]
    case ExprNodeKind.GROUPED:
      return encodeExprNode(node.value)
  }
}
