import {
  AssignStatement,
  BinaryExpr,
  BlockStatement,
  BooleanLitExpr,
  CallExpr,
  CastExpr,
  Expr,
  ExprKind,
  ExprStatement,
  Function,
  IdentExpr,
  IfStatement,
  IndexExpr,
  IntegerLitExpr,
  Program,
  ReturnStatement,
  Statement,
  StatementKind,
  Type,
  TypeKind,
  UnaryExpr,
  VarStatement,
  Variable,
  WhileStatement,
} from './semantic'

// encodeProgram encodes analyzed program so that it can be represented in more human readable format
// this is useful for testing purpose.
export function encodeProgram(program: Program): any {
  const functions = program.functions.map(encodeFunction)
  const globals = program.globals.map(encodeVariable)
  const body = encodeBlockStmt(program.main)
  return [...functions, ...globals, ['main', body]]
}

function encodeFunction(func: Function): any {
  return [
    'func',
    func.name,
    func.arguments.map((arg, i) => [arg, encodeType(func.type.arguments[i])]),
    encodeType(func.type.return),
    func.body != null ? encodeBlockStmt(func.body) : undefined,
  ]
}

function encodeType(type: Type): any {
  const primitiveKind = [
    TypeKind.INTEGER,
    TypeKind.REAL,
    TypeKind.BOOLEAN,
    TypeKind.STRING,
    TypeKind.BYTE,
    TypeKind.VOID,
  ]
  if (primitiveKind.includes(type.kind)) {
    return type.kind.toString()
  }

  if (type.kind === TypeKind.ARRAY) {
    return [
      'array',
      type.dimension.map((d) => d.toString()),
      encodeType(type.elementType),
    ]
  }

  if (type.kind === TypeKind.FUNCTION) {
    return [
      'func',
      type.arguments.map((arg) => encodeType(arg)),
      encodeType(type.return),
    ]
  }
}

function encodeStmt(stmt: Statement): any {
  switch (stmt.kind) {
    case StatementKind.BLOCK:
      return encodeBlockStmt(stmt)
    case StatementKind.VAR:
      return encodeVarStmt(stmt)
    case StatementKind.ASSIGN:
      return encodeAssignStmt(stmt)
    case StatementKind.EXPR:
      return encodeExprStmt(stmt)
    case StatementKind.IF:
      return encodeIfStmt(stmt)
    case StatementKind.WHILE:
      return encodeWhileStmt(stmt)
    case StatementKind.RETURN:
      return encodeReturnStmt(stmt)
  }
}

function encodeBlockStmt(stmt: BlockStatement): any {
  return stmt.body.map((stmt) => encodeStmt(stmt))
}

function encodeVarStmt(stmt: VarStatement): any {
  return encodeVariable(stmt.variable)
}

function encodeVariable(variable: Variable): any {
  return [
    'var',
    variable.name,
    encodeType(variable.type),
    variable.value !== undefined ? encodeExpr(variable.value) : undefined,
  ]
}

function encodeAssignStmt(stmt: AssignStatement): any {
  return ['assign', encodeExpr(stmt.target), encodeExpr(stmt.value)]
}

function encodeExprStmt(stmt: ExprStatement): any {
  return encodeExpr(stmt.value)
}

function encodeIfStmt(stmt: IfStatement): any {
  return [
    'if',
    encodeExpr(stmt.condition),
    encodeStmt(stmt.body),
    stmt.else !== undefined ? encodeStmt(stmt.else) : undefined,
  ]
}

function encodeWhileStmt(stmt: WhileStatement): any {
  return ['while', encodeExpr(stmt.condition), encodeStmt(stmt.body)]
}

function encodeReturnStmt(stmt: ReturnStatement): any {
  return [
    'return',
    stmt.value !== undefined ? encodeExpr(stmt.value) : undefined,
  ]
}

function encodeExpr(expr: Expr): any {
  switch (expr.kind) {
    case ExprKind.IDENT:
      return encodeIdentExpr(expr)
    case ExprKind.INTEGER_LIT:
      return encodeIntegerLitExpr(expr)
    case ExprKind.BOOLEAN_LIT:
      return encodeBooleanLitExpr(expr)
    case ExprKind.BINARY:
      return encodeBinaryExpr(expr)
    case ExprKind.UNARY:
      return encodeUnaryExpr(expr)
    case ExprKind.CALL:
      return encodeCallExpr(expr)
    case ExprKind.INDEX:
      return encodeArrayIndexExpr(expr)
    case ExprKind.CAST:
      return encodeCastExpr(expr)
  }
}

function encodeIdentExpr(expr: IdentExpr): any {
  return ['ident', expr.ident]
}

function encodeIntegerLitExpr(expr: IntegerLitExpr): any {
  return expr.value.toString()
}

function encodeBooleanLitExpr(expr: BooleanLitExpr): any {
  return expr.value
}

function encodeBinaryExpr(expr: BinaryExpr): any {
  return [expr.op, encodeExpr(expr.a), encodeExpr(expr.b)]
}

function encodeUnaryExpr(expr: UnaryExpr): any {
  return [expr.op, encodeExpr(expr.value)]
}

function encodeCallExpr(expr: CallExpr): any {
  return ['call', encodeExpr(expr.function), expr.arguments.map(encodeExpr)]
}

function encodeArrayIndexExpr(expr: IndexExpr): any {
  return ['index', encodeExpr(expr.array), expr.indices.map(encodeExpr)]
}

function encodeCastExpr(expr: CastExpr): any {
  return ['cast', encodeExpr(expr.source), encodeType(expr.type)]
}
