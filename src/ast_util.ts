import {
  AssignStatementNode,
  BlockStatementNode,
  DeclKind,
  DeclNode,
  ExprNode,
  ExprNodeKind,
  FunctionDeclNode,
  IfStatementNode,
  MainDeclNode,
  ParamGroup,
  ParamsNode,
  ReturnStatementNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
  VariableDeclNode,
  VarNode,
  VarStatementNode,
  WhileStatementNode,
} from './ast'

export function encodeAst(root: RootNode): any[] {
  return root.declarations.map(encodeDeclNode)
}

function encodeDeclNode(node: DeclNode): any[] {
  switch (node.kind) {
    case DeclKind.FUNCTION:
      return encodeFunctionDeclNode(node)
    case DeclKind.MAIN:
      return encodeMainDeclNode(node)
    case DeclKind.VARIABLE:
      return encodeVariableDeclNode(node)
  }
}

function encodeFunctionDeclNode(node: FunctionDeclNode): any[] {
  return [
    node.function.repr(),
    node.name.repr(),
    node.openBrac.repr(),
    encodeParamsNode(node.params),
    node.closeBrac.repr(),
    node.arrow?.repr(),
    node.returnType != null ? encodeTypeExprNode(node.returnType) : undefined,
    encodeBlockStatementNode(node.body),
  ]
}

function encodeMainDeclNode(node: MainDeclNode): any[] {
  return encodeBlockStatementNode(node.body)
}

function encodeVariableDeclNode(node: VariableDeclNode): any[] {
  return encodeVarNode(node.variable)
}

function encodeVarNode(node: VarNode): any[] {
  return [
    node.var.repr(),
    node.name.repr(),
    node.colon?.repr(),
    node.type != null ? encodeTypeExprNode(node.type) : undefined,
    node.assign?.repr(),
    node.value != null ? encodeExprNode(node.value) : undefined,
  ]
}

function encodeParamsNode(node: ParamsNode): any[] {
  return node.params.map(encodeParamGroup)
}

function encodeParamGroup(node: ParamGroup): any[] {
  return [node.name.repr(), node.colon.repr(), encodeTypeExprNode(node.type)]
}

function encodeTypeExprNode(node: TypeExprNode): any {
  switch (node.kind) {
    case TypeExprNodeKind.PRIMITIVE:
      return node.type.repr()
    case TypeExprNodeKind.ARRAY:
      return [
        node.array.repr(),
        node.openSquare.repr(),
        node.dimension.values.flatMap(encodeExprNode),
        node.closeSquare.repr(),
        node.of.repr(),
        encodeTypeExprNode(node.type),
      ]
  }
}

function encodeStatementNode(node: StatementNode): any[] {
  switch (node.kind) {
    case StatementNodeKind.VAR:
      return encodeVarStatement(node)
    case StatementNodeKind.ASSIGN:
      return encodeAssignStatement(node)
    case StatementNodeKind.RETURN:
      return encodeReturnStatement(node)
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
    node.assign.repr(),
    encodeExprNode(node.value),
  ]
}

function encodeReturnStatement(node: ReturnStatementNode): any[] {
  return [
    node.return.repr(),
    node.value != null ? encodeExprNode(node.value) : undefined,
  ]
}

function encodeIfStatement(node: IfStatementNode): any[] {
  return [
    node.if.repr(),
    encodeExprNode(node.condition),
    node.then.repr(),
    encodeStatementNode(node.body),
  ]
}

function encodeWhileStatement(node: WhileStatementNode): any[] {
  return [
    node.while.repr(),
    encodeExprNode(node.condition),
    node.do.repr(),
    encodeStatementNode(node.body),
  ]
}

function encodeBlockStatementNode(node: BlockStatementNode): any[] {
  return [
    node.begin.repr(),
    node.statements.map(encodeStatementNode),
    node.end.repr(),
  ]
}

function encodeExprNode(node: ExprNode): any {
  switch (node.kind) {
    case ExprNodeKind.IDENT:
      return node.name.repr()
    case ExprNodeKind.INTEGER_LIT:
      return node.value.repr()
    case ExprNodeKind.BOOLEAN_LIT:
      return node.value.repr()
    case ExprNodeKind.BINARY:
      return [encodeExprNode(node.a), node.op.repr(), encodeExprNode(node.b)]
    case ExprNodeKind.UNARY:
      return [node.op.repr(), encodeExprNode(node.value)]
    case ExprNodeKind.CALL:
      return [
        encodeExprNode(node.callee),
        node.openBrac.repr(),
        node.arguments.values.flatMap(encodeExprNode),
        node.closeBrac.repr(),
      ]
    case ExprNodeKind.ARRAY_INDEX:
      return [
        encodeExprNode(node.array),
        node.openSquare.repr(),
        node.index.values.flatMap(encodeExprNode),
        node.closeSquare.repr(),
      ]
    case ExprNodeKind.CAST:
      return [
        encodeExprNode(node.source),
        node.as.repr(),
        encodeTypeExprNode(node.target),
      ]
    case ExprNodeKind.GROUPED:
      return encodeExprNode(node.value)
  }
}
