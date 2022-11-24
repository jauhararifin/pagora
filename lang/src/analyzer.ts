import { apis, BuiltinAPIs } from './api'
import {
  ArrayIndexExprNode,
  ArrayLitExprNode,
  ArrayTypeNode,
  AssignStatementNode,
  BinaryExprNode,
  BlockStatementNode,
  BooleanLitExprNode,
  CallExprNode,
  CastExprNode,
  ExprNode,
  ExprNodeKind,
  ExprStatementNode,
  FunctionNode,
  IdentExprNode,
  IfStatementNode,
  IntegerLitExprNode,
  KeywordStatementNode,
  RealLitExprNode,
  ReturnStatementNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  StringLitExprNode,
  TypeExprNode,
  TypeExprNodeKind,
  UnaryExprNode,
  VarNode,
  VarStatementNode,
  WhileStatementNode,
} from './ast'
import {
  BuiltinRedeclared,
  CompileError,
  InvalidBinaryOperator,
  InvalidUnaryOperator,
  MissingMain,
  MissingReturnValue,
  MultiCompileError,
  MultipleDeclaration,
  NotAConstant,
  NotAssignable,
  NotInALoop,
  TypeMismatch,
  UndefinedSymbol,
  WrongNumberOfArgument,
  WrongNumberOfIndex,
} from './errors'
import {
  ArrayLitExpr,
  ArrayType,
  AssignStatement,
  BinaryExpr,
  BinaryOp,
  BlockStatement,
  Boolean,
  BooleanLitExpr,
  Byte,
  CallExpr,
  CastExpr,
  Expr,
  ExprKind,
  ExprStatement,
  Function,
  FunctionType,
  IdentExpr,
  IfStatement,
  IndexExpr,
  Integer,
  IntegerLitExpr,
  Program,
  Real,
  RealLitExpr,
  ReturnStatement,
  Statement,
  StatementKind,
  String,
  StringLitExpr,
  Type,
  TypeKind,
  UnaryExpr,
  UnaryOp,
  Variable,
  VarStatement,
  Void,
  WhileStatement,
} from './semantic'
import { Token, TokenKind } from './tokens'

export function analyze(ast: RootNode, builtins: BuiltinAPIs = apis): Program {
  const ctx = new Context()
  ctx.addScope()

  const errors: CompileError[] = []

  for (const func of builtins.functions) {
    ctx.addBuiltinSymbol(func.name, func.type)
  }

  for (const func of ast.functions) {
    const typeResult = analyzeFuncType(ctx, func)
    if (typeResult.err !== undefined) {
      errors.push(typeResult.err)
      continue
    }

    const result = ctx.addSymbol(func.name, typeResult.ok)
    if (result.err !== undefined) {
      errors.push(result.err)
    }
  }

  const variables: Variable[] = []

  for (const variable of ast.variables) {
    const varResult = analyzeVariable(ctx, variable)
    if (varResult.err !== undefined) {
      errors.push(varResult.err)
      continue
    }

    const result = ctx.addSymbol(variable.name, varResult.ok.type)
    if (result.err !== undefined) {
      errors.push(result.err)
      continue
    }

    variables.push(varResult.ok)
  }

  const functions: Function[] = []

  for (const func of ast.functions) {
    const funcResult = analyzeFunction(ctx, func)
    if (funcResult.err !== undefined) {
      errors.push(funcResult.err)
      continue
    }
    functions.push(funcResult.ok)
  }

  let main: BlockStatement | undefined
  if (ast.main === undefined) {
    errors.push(new MissingMain())
  } else {
    const result = analyzeBlockStatement(ctx, ast.main.body)
    if (result.err !== undefined) {
      errors.push(result.err)
    } else {
      main = result.ok
    }
  }

  if (errors.length > 0) {
    throw new MultiCompileError(errors)
  }

  return { globals: variables, functions, main: main! }
}

class Context {
  scopes: Scope[] = []
  loopDepth: number = 0
  currentReturnType: Type = { kind: TypeKind.VOID }

  get(name: string): Symbol | undefined {
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      if (name in this.scopes[i]) {
        return this.scopes[i][name]
      }
    }
  }

  getInScope(name: string): Symbol | undefined {
    if (name in this.scopes[this.scopes.length - 1]) {
      return this.scopes[this.scopes.length - 1][name]
    }
    return undefined
  }

  addSymbol(token: Token, type: Type): Result<void> {
    const sym = this.scopes[this.scopes.length - 1][token.value]
    if (sym !== undefined) {
      if (sym.isBuiltin) {
        return err(new BuiltinRedeclared(token))
      } else {
        return err(new MultipleDeclaration(sym.name, token))
      }
    }

    this.scopes[this.scopes.length - 1][token.value] = {
      name: token,
      isBuiltin: false,
      type,
    }
    return ok(undefined)
  }

  addBuiltinSymbol(name: string, type: Type): void {
    if (name in this.scopes[this.scopes.length - 1]) {
      throw new Error(`Builtin ${name} is redeclared`)
    }

    this.scopes[this.scopes.length - 1][name] = { name, isBuiltin: true, type }
  }

  addScope(): void {
    this.scopes.push({})
  }

  popScope(): void {
    this.scopes.pop()
  }
}

interface Scope {
  [symbolName: string]: Symbol
}

type Symbol = UserSymbol | BuiltinSymbol

interface UserSymbol {
  name: Token
  isBuiltin: false
  type: Type
}

interface BuiltinSymbol {
  name: string
  isBuiltin: true
  type: Type
}

type Result<T> =
  | { ok: T; err: undefined }
  | { ok: undefined; err: CompileError }

function ok<T>(value: T): Result<T> {
  return { ok: value, err: undefined }
}

function err<T>(err: CompileError): Result<T> {
  return { ok: undefined, err }
}

function analyzeVariable(
  ctx: Context,
  variable: VarNode,
  constantOnly: boolean = false
): Result<Variable> {
  assertTokenKind(variable.name, TokenKind.IDENTIFIER)

  const name = variable.name.value

  assert(
    variable.type !== undefined || variable.value !== undefined,
    'variable declaration should have type or value expression'
  )

  let value: Expr | undefined
  if (variable.value !== undefined) {
    const exprResult = analyzeExpr(ctx, variable.value)
    if (exprResult.err !== undefined) {
      return err(exprResult.err)
    }
    value = exprResult.ok
  }

  let type = value?.type
  if (type === undefined) {
    const typeResult = analyzeType(ctx, variable.type!)
    if (typeResult.err !== undefined) {
      return err(typeResult.err)
    }
    type = typeResult.ok
  }

  assert(
    type !== undefined,
    'variable declaration should have type or value expression'
  )

  if (value !== undefined) {
    if (!typeEqual(value.type, type)) {
      return err(new TypeMismatch(value, type))
    }

    if (constantOnly && !value.isConstexpr) {
      return err(new NotAConstant(value))
    }
  }

  return ok({ name, type, value })
}

function analyzeFunction(ctx: Context, func: FunctionNode): Result<Function> {
  assertTokenKind(func.name, TokenKind.IDENTIFIER)

  const name = func.name.value

  const symbol = ctx.get(name)
  if (symbol === undefined) {
    throw new Error(`function type for "${name}" is not found`)
  }
  if (symbol.type.kind !== TypeKind.FUNCTION) {
    throw new Error(`symbol ${name} is not a function`)
  }

  ctx.currentReturnType = symbol.type.return
  if (symbol.type.arguments.length !== func.params.params.length) {
    throw new Error(`unmatched parameters length in function ${name}`)
  }

  const params: Symbol[] = symbol.type.arguments.map((type, i) => ({
    name: func.params.params[i].name,
    isBuiltin: false,
    type,
  }))
  const paramNames: string[] = func.params.params.map((v) => v.name.value)

  let body: BlockStatement | undefined
  if (func.body !== undefined) {
    const result = analyzeBlockStatement(ctx, func.body, params)
    if (result.err !== undefined) {
      return err(result.err)
    }
    body = result.ok
  }

  return ok({
    name,
    type: symbol.type,
    arguments: paramNames,
    body,
  })
}

function analyzeStatement(
  ctx: Context,
  stmt: StatementNode
): Result<Statement> {
  switch (stmt.kind) {
    case StatementNodeKind.BLOCK:
      return analyzeBlockStatement(ctx, stmt)
    case StatementNodeKind.VAR:
      return analyzeVarStatement(ctx, stmt)
    case StatementNodeKind.ASSIGN:
      return analyzeAssignStatement(ctx, stmt)
    case StatementNodeKind.EXPR:
      return analyzeExprStatement(ctx, stmt)
    case StatementNodeKind.IF:
      return analyzeIfStatement(ctx, stmt)
    case StatementNodeKind.WHILE:
      return analyzeWhileStatement(ctx, stmt)
    case StatementNodeKind.RETURN:
      return analyzeReturnStatement(ctx, stmt)
    case StatementNodeKind.KEYWORD:
      return analyzeKeywordStatement(ctx, stmt)
    default:
      throw new Error(`unrecognized statement kind`)
  }
}

function analyzeBlockStatement(
  ctx: Context,
  stmt: BlockStatementNode,
  additionalSymbols: Symbol[] = []
): Result<BlockStatement> {
  ctx.addScope()

  for (const symbol of additionalSymbols) {
    if (symbol.isBuiltin) {
      throw new Error(`Cannot add builtin symbol "${symbol.name}"`)
    }
    ctx.addSymbol(symbol.name, symbol.type)
  }

  const errors: CompileError[] = []
  const statements: Statement[] = []
  for (const item of stmt.statements) {
    const result = analyzeStatement(ctx, item)
    if (result.err !== undefined) {
      errors.push(result.err)
      continue
    }
    statements.push(result.ok)
  }

  ctx.popScope()

  if (errors.length > 0) {
    return err(new MultiCompileError(errors))
  }

  return ok({ kind: StatementKind.BLOCK, body: statements })
}

function analyzeVarStatement(
  ctx: Context,
  stmt: VarStatementNode
): Result<VarStatement> {
  const result = analyzeVariable(ctx, stmt.variable, false)
  if (result.err !== undefined) {
    return err(result.err)
  }
  return ok({ kind: StatementKind.VAR, variable: result.ok })
}

function analyzeAssignStatement(
  ctx: Context,
  stmt: AssignStatementNode
): Result<AssignStatement> {
  const receiverResult = analyzeExpr(ctx, stmt.receiver)
  if (receiverResult.err !== undefined) {
    return err(receiverResult.err)
  }
  const receiver = receiverResult.ok

  if (!receiver.isAssignable) {
    return err(new NotAssignable(receiver))
  }

  const valueResult = analyzeExpr(ctx, stmt.value)
  if (valueResult.err !== undefined) {
    return err(valueResult.err)
  }
  const value = valueResult.ok

  if (!typeEqual(value.type, receiver.type)) {
    return err(new TypeMismatch(value, receiver.type))
  }

  return ok({
    kind: StatementKind.ASSIGN,
    target: receiver,
    value,
  })
}

function analyzeExprStatement(
  ctx: Context,
  stmt: ExprStatementNode
): Result<ExprStatement> {
  const result = analyzeExpr(ctx, stmt.expr)
  if (result.err !== undefined) {
    return err(result.err)
  }
  return ok({ kind: StatementKind.EXPR, value: result.ok })
}

function analyzeIfStatement(
  ctx: Context,
  stmt: IfStatementNode
): Result<IfStatement> {
  const conditionResult = analyzeExpr(ctx, stmt.condition)
  if (conditionResult.err !== undefined) {
    return err(conditionResult.err)
  }
  const condition = conditionResult.ok

  if (!typeEqual(condition.type, Boolean)) {
    return err(new TypeMismatch(condition, Boolean))
  }

  const bodyResult = analyzeStatement(ctx, stmt.body)
  if (bodyResult.err !== undefined) {
    return err(bodyResult.err)
  }
  const body = bodyResult.ok

  let elseStmt: Statement | undefined
  if (stmt.elseBody !== undefined) {
    const result = analyzeStatement(ctx, stmt.elseBody)
    if (result.err !== undefined) {
      return err(result.err)
    }
    elseStmt = result.ok
  }

  return ok({
    kind: StatementKind.IF,
    condition,
    body,
    else: elseStmt,
  })
}

function analyzeWhileStatement(
  ctx: Context,
  stmt: WhileStatementNode
): Result<WhileStatement> {
  const conditionResult = analyzeExpr(ctx, stmt.condition)
  if (conditionResult.err !== undefined) {
    return err(conditionResult.err)
  }
  const condition = conditionResult.ok

  if (!typeEqual(condition.type, Boolean)) {
    return err(new TypeMismatch(condition, Boolean))
  }

  ctx.loopDepth++
  const bodyResult = analyzeStatement(ctx, stmt.body)
  if (bodyResult.err !== undefined) {
    ctx.loopDepth--
    return err(bodyResult.err)
  }
  const body = bodyResult.ok
  ctx.loopDepth--

  return ok({
    kind: StatementKind.WHILE,
    condition,
    body,
  })
}

function analyzeReturnStatement(
  ctx: Context,
  stmt: ReturnStatementNode
): Result<ReturnStatement> {
  if (stmt.value != null) {
    const valueResult = analyzeExpr(ctx, stmt.value)
    if (valueResult.err !== undefined) {
      return err(valueResult.err)
    }
    const value = valueResult.ok

    if (!typeEqual(value.type, ctx.currentReturnType)) {
      return err(new TypeMismatch(value, ctx.currentReturnType))
    }
    return ok({ kind: StatementKind.RETURN, value })
  } else {
    if (ctx.currentReturnType.kind !== TypeKind.VOID) {
      return err(new MissingReturnValue(stmt.return))
    }
    return ok({ kind: StatementKind.RETURN })
  }
}

function analyzeKeywordStatement(
  ctx: Context,
  stmt: KeywordStatementNode
): Result<Statement> {
  const keyword = stmt.keyword.kind

  let kind: StatementKind | undefined
  if (keyword === TokenKind.CONTINUE) {
    kind = StatementKind.CONTINUE
  } else if (keyword === TokenKind.BREAK) {
    kind = StatementKind.BREAK
  } else {
    throw new Error(`keyword statement "${keyword}" is not supported yet`)
  }

  if (ctx.loopDepth === 0) {
    return err(new NotInALoop(stmt.keyword))
  }
  return ok({ kind })
}

function analyzeExpr(ctx: Context, expr: ExprNode): Result<Expr> {
  switch (expr.kind) {
    case ExprNodeKind.IDENT:
      return analyzeIdentExpr(ctx, expr)
    case ExprNodeKind.INTEGER_LIT:
      return analyzeIntegerLitExpr(expr)
    case ExprNodeKind.BOOLEAN_LIT:
      return analyzeBooleanLitExpr(expr)
    case ExprNodeKind.ARRAY_LIT:
      return analyzeArrayLitExpr(ctx, expr)
    case ExprNodeKind.STRING_LIT:
      return analyzeStringLitExpr(expr)
    case ExprNodeKind.REAL_LIT:
      return analyzeRealLitExpr(expr)
    case ExprNodeKind.BINARY:
      return analyzeBinaryExpr(ctx, expr)
    case ExprNodeKind.UNARY:
      return analyzeUnaryExpr(ctx, expr)
    case ExprNodeKind.CALL:
      return analyzeCallExpr(ctx, expr)
    case ExprNodeKind.ARRAY_INDEX:
      return analyzeArrayIndexExpr(ctx, expr)
    case ExprNodeKind.CAST:
      return analyzeCastExpr(ctx, expr)
    case ExprNodeKind.GROUPED:
      return analyzeExpr(ctx, expr.value)
    default:
      throw new Error(`unrecognized expr kind`)
  }
}

function analyzeIdentExpr(
  ctx: Context,
  expr: IdentExprNode
): Result<IdentExpr> {
  assertTokenKind(expr.name, TokenKind.IDENTIFIER)

  const name = expr.name.value
  const symbol = ctx.get(name)
  if (symbol === undefined) {
    return err(new UndefinedSymbol(expr.name))
  }

  return ok({
    kind: ExprKind.IDENT,
    type: symbol.type,
    isConstexpr: false,
    constValue: undefined,
    isAssignable: true,
    position: expr.name.position,
    ident: name,
  })
}

function analyzeIntegerLitExpr(
  expr: IntegerLitExprNode
): Result<IntegerLitExpr> {
  assertTokenKind(expr.value, TokenKind.INTEGER_LITERAL)

  const value = BigInt.asIntN(64, BigInt(expr.value.value))

  return ok({
    kind: ExprKind.INTEGER_LIT,
    type: Integer,
    isConstexpr: true,
    constValue: value,
    isAssignable: false,
    position: expr.value.position,
    value,
  })
}

function analyzeBooleanLitExpr(
  expr: BooleanLitExprNode
): Result<BooleanLitExpr> {
  let value: boolean | undefined
  if (expr.value.kind === TokenKind.TRUE) {
    value = true
  } else if (expr.value.kind === TokenKind.FALSE) {
    value = false
  } else {
    throw new Error('boolean expression should have true or false value')
  }

  return ok({
    kind: ExprKind.BOOLEAN_LIT,
    type: Boolean,
    isConstexpr: true,
    constValue: value,
    isAssignable: false,
    position: expr.value.position,
    value,
  })
}

function analyzeArrayLitExpr(
  ctx: Context,
  expr: ArrayLitExprNode
): Result<ArrayLitExpr> {
  const values: Expr[] = []
  let isConstexpr = true
  const constValue: Expr[] = []

  for (let i = 0; i < expr.value.values.length; i++) {
    const valNode = expr.value.values[i]

    const result = analyzeExpr(ctx, valNode)
    if (result.err !== undefined) {
      return err(result.err)
    }
    const value = result.ok

    isConstexpr = isConstexpr && value.isConstexpr
    constValue.push(value.constValue)
    values.push(value)
  }

  for (let i = 1; i < values.length; i++) {
    if (!typeEqual(values[i].type, values[0].type)) {
      return err(new TypeMismatch(values[i], values[0].type))
    }
  }

  let elementType = values[0].type
  let dimension = [BigInt.asIntN(64, BigInt(values.length))]
  if (values[0].type.kind === TypeKind.ARRAY) {
    elementType = values[0].type.type
    dimension = [dimension[0], ...values[0].type.dimension]
  }

  const typ: Type = {
    kind: TypeKind.ARRAY,
    dimension,
    type: elementType,
  }

  return ok({
    kind: ExprKind.ARRAY_LIT,
    isConstexpr,
    constValue: isConstexpr ? constValue : undefined,
    isAssignable: false,
    position: expr.openSquare.position,
    type: typ,
    values,
  })
}

function analyzeStringLitExpr(expr: StringLitExprNode): Result<StringLitExpr> {
  const trimQuote = expr.value.value.slice(1, expr.value.value.length - 1)
  return ok({
    kind: ExprKind.STRING_LIT,
    isConstexpr: true,
    constValue: trimQuote,
    isAssignable: false,
    type: { kind: TypeKind.STRING },
    position: expr.value.position,
    value: trimQuote,
  })
}

function analyzeRealLitExpr(expr: RealLitExprNode): Result<RealLitExpr> {
  assertTokenKind(expr.value, TokenKind.REAL_LITERAL)

  const value = Number(expr.value)

  return ok({
    kind: ExprKind.REAL_LIT,
    type: Integer,
    isConstexpr: true,
    constValue: value,
    isAssignable: false,
    position: expr.value.position,
    value,
  })
}

const integerBinOp = new Set([
  TokenKind.PLUS,
  TokenKind.MINUS,
  TokenKind.MULTIPLY,
  TokenKind.DIV,
  TokenKind.MOD,
  TokenKind.SHIFT_LEFT,
  TokenKind.SHIFT_RIGHT,
  TokenKind.BIT_AND,
  TokenKind.BIT_OR,
  TokenKind.BIT_XOR,
])
const realBinOp = new Set([
  TokenKind.PLUS,
  TokenKind.MINUS,
  TokenKind.MULTIPLY,
  TokenKind.DIV,
])
const compBinOp = new Set([
  TokenKind.GREATER_THAN,
  TokenKind.GREATER_THAN_EQUAL,
  TokenKind.LESS_THAN,
  TokenKind.LESS_THAN_EQUAL,
  TokenKind.EQUAL,
  TokenKind.NOT_EQUAL,
])
const boolBinOp = new Set([TokenKind.AND, TokenKind.OR])

const binopMap: { [s: string]: BinaryOp } = {
  [TokenKind.PLUS]: BinaryOp.PLUS,
  [TokenKind.MINUS]: BinaryOp.MINUS,
  [TokenKind.DIV]: BinaryOp.DIV,
  [TokenKind.MULTIPLY]: BinaryOp.MUL,
  [TokenKind.MOD]: BinaryOp.MOD,
  [TokenKind.AND]: BinaryOp.AND,
  [TokenKind.OR]: BinaryOp.OR,
  [TokenKind.BIT_AND]: BinaryOp.BIT_AND,
  [TokenKind.BIT_OR]: BinaryOp.BIT_OR,
  [TokenKind.BIT_XOR]: BinaryOp.BIT_XOR,
  [TokenKind.EQUAL]: BinaryOp.EQUAL,
  [TokenKind.NOT_EQUAL]: BinaryOp.NOT_EQUAL,
  [TokenKind.GREATER_THAN]: BinaryOp.GREATER_THAN,
  [TokenKind.GREATER_THAN_EQUAL]: BinaryOp.GREATER_THAN_EQUAL,
  [TokenKind.LESS_THAN]: BinaryOp.LESS_THAN,
  [TokenKind.LESS_THAN_EQUAL]: BinaryOp.LESS_THAN_EQUAL,
  [TokenKind.SHIFT_LEFT]: BinaryOp.SHIFT_LEFT,
  [TokenKind.SHIFT_RIGHT]: BinaryOp.SHIFT_RIGHT,
}

function analyzeBinaryExpr(
  ctx: Context,
  expr: BinaryExprNode
): Result<BinaryExpr> {
  const aResult = analyzeExpr(ctx, expr.a)
  if (aResult.err !== undefined) {
    return err(aResult.err)
  }
  const bResult = analyzeExpr(ctx, expr.b)
  if (bResult.err !== undefined) {
    return err(bResult.err)
  }

  const a = aResult.ok
  const b = bResult.ok
  const op = expr.op

  let resultType: Type | undefined

  if (
    integerBinOp.has(op.kind) &&
    typeEqual(a.type, Integer) &&
    typeEqual(b.type, Integer)
  ) {
    resultType = Integer
  } else if (
    realBinOp.has(op.kind) &&
    typeEqual(a.type, Real) &&
    typeEqual(b.type, Real)
  ) {
    resultType = Real
  } else if (
    compBinOp.has(op.kind) &&
    ((typeEqual(a.type, Real) && typeEqual(b.type, Real)) ||
      (typeEqual(a.type, Integer) && typeEqual(b.type, Integer)))
  ) {
    resultType = a.type
  } else if (
    boolBinOp.has(op.kind) &&
    typeEqual(a.type, Boolean) &&
    typeEqual(b.type, Boolean)
  ) {
    resultType = Boolean
  } else {
    return err(new InvalidBinaryOperator(a, op, b))
  }

  return ok({
    kind: ExprKind.BINARY,
    isConstexpr: false,
    constValue: undefined,
    isAssignable: false,
    type: resultType,
    position: a.position,
    a,
    b,
    op: binopMap[op.kind]!,
  })
}

const integerUnaryOp = new Set([
  TokenKind.PLUS,
  TokenKind.MINUS,
  TokenKind.BIT_NOT,
])
const realUnaryOp = new Set([TokenKind.PLUS, TokenKind.MINUS])
const boolUnaryOp = new Set([TokenKind.NOT])

const unaryOpMap: { [s: string]: UnaryOp } = {
  [TokenKind.PLUS]: UnaryOp.PLUS,
  [TokenKind.MINUS]: UnaryOp.MINUS,
  [TokenKind.NOT]: UnaryOp.NOT,
  [TokenKind.BIT_NOT]: UnaryOp.BIT_NOT,
}

function analyzeUnaryExpr(
  ctx: Context,
  expr: UnaryExprNode
): Result<UnaryExpr> {
  const result = analyzeExpr(ctx, expr.value)
  if (result.err !== undefined) {
    return err(result.err)
  }
  const value = result.ok

  if (!(expr.op.kind in unaryOpMap)) {
    throw new Error(`invalid unary operator ${expr.op.kind}`)
  }

  const op = expr.op

  let resultType: Type | undefined
  if (integerUnaryOp.has(op.kind) && typeEqual(value.type, Integer)) {
    resultType = Integer
  } else if (realUnaryOp.has(op.kind) && typeEqual(value.type, Real)) {
    resultType = Real
  } else if (boolUnaryOp.has(op.kind) && typeEqual(value.type, Boolean)) {
    resultType = Boolean
  } else {
    return err(new InvalidUnaryOperator(value, op))
  }

  return ok({
    kind: ExprKind.UNARY,
    isConstexpr: false,
    constValue: undefined,
    isAssignable: false,
    type: resultType,
    position: expr.op.position,
    value,
    op: unaryOpMap[op.kind],
  })
}

function analyzeCallExpr(ctx: Context, expr: CallExprNode): Result<CallExpr> {
  const calleeResult = analyzeExpr(ctx, expr.callee)
  if (calleeResult.err !== undefined) {
    return err(calleeResult.err)
  }
  const callee = calleeResult.ok

  if (callee.type.kind !== TypeKind.FUNCTION) {
    return err(new TypeMismatch(callee, TypeKind.FUNCTION))
  }

  if (callee.type.arguments.length !== expr.arguments.values.length) {
    return err(new WrongNumberOfArgument(expr, callee.type.arguments.length))
  }

  const args: Expr[] = []
  for (let i = 0; i < callee.type.arguments.length; i++) {
    const result = analyzeExpr(ctx, expr.arguments.values[i])
    if (result.err !== undefined) {
      return err(result.err)
    }
    const arg = result.ok

    const paramType = callee.type.arguments[i]

    if (!typeEqual(arg.type, paramType)) {
      return err(new TypeMismatch(arg, paramType))
    }

    args.push(arg)
  }

  return ok({
    kind: ExprKind.CALL,
    isConstexpr: false,
    constValue: undefined,
    isAssignable: false,
    type: callee.type.return,
    position: callee.position,
    function: callee,
    arguments: args,
  })
}

function analyzeArrayIndexExpr(
  ctx: Context,
  expr: ArrayIndexExprNode
): Result<IndexExpr> {
  const arrayResult = analyzeExpr(ctx, expr.array)
  if (arrayResult.err !== undefined) {
    return err(arrayResult.err)
  }
  const array = arrayResult.ok

  if (array.type.kind !== TypeKind.ARRAY) {
    return err(new TypeMismatch(array, TypeKind.ARRAY))
  }

  if (array.type.dimension.length !== expr.index.values.length) {
    return err(new WrongNumberOfIndex(expr, array.type.dimension.length))
  }

  const indices: Expr[] = []
  for (let i = 0; i < array.type.dimension.length; i++) {
    const result = analyzeExpr(ctx, expr.index.values[i])
    if (result.err !== undefined) {
      return err(result.err)
    }
    const index = result.ok

    if (index.type.kind !== TypeKind.INTEGER) {
      throw new TypeMismatch(index, TypeKind.INTEGER)
    }

    indices.push(index)
  }

  return ok({
    kind: ExprKind.INDEX,
    isConstexpr: false,
    constValue: undefined,
    isAssignable: true,
    type: array.type.type,
    position: array.position,
    array,
    indices,
  })
}

function analyzeCastExpr(ctx: Context, expr: CastExprNode): Result<CastExpr> {
  const valueResult = analyzeExpr(ctx, expr.source)
  if (valueResult.err !== undefined) {
    return err(valueResult.err)
  }
  const value = valueResult.ok

  const targetResult = analyzeType(ctx, expr.target)
  if (targetResult.err !== undefined) {
    return err(targetResult.err)
  }
  const target = targetResult.ok

  const castable =
    (value.type.kind === TypeKind.INTEGER && target.kind === TypeKind.REAL) ||
    (value.type.kind === TypeKind.REAL && target.kind === TypeKind.INTEGER)

  if (!castable) {
    return err(new TypeMismatch(value, target))
  }

  return ok({
    kind: ExprKind.CAST,
    isConstexpr: false,
    constValue: undefined,
    isAssignable: false,
    position: value.position,
    source: value,
    type: target,
  })
}

function analyzeType(ctx: Context, expr: TypeExprNode): Result<Type> {
  if (expr.kind === TypeExprNodeKind.IDENT) {
    switch (expr.type.kind) {
      case TokenKind.INTEGER:
        return ok(Integer)
      case TokenKind.BOOLEAN:
        return ok(Boolean)
      case TokenKind.BYTE:
        return ok(Byte)
      case TokenKind.REAL:
        return ok(Real)
      case TokenKind.STRING:
        return ok(String)
      default:
        throw new Error(`unrecognized type ${expr.kind}`)
    }
  } else if (expr.kind === TypeExprNodeKind.ARRAY) {
    return analyzeArrayType(ctx, expr)
  } else {
    throw new Error(`unsupported type kind found`)
  }
}

function analyzeFuncType(
  ctx: Context,
  func: FunctionNode
): Result<FunctionType> {
  const args: Type[] = []
  for (const param of func.params.params) {
    const typeResult = analyzeType(ctx, param.type)
    if (typeResult.err !== undefined) {
      return err(typeResult.err)
    }
    args.push(typeResult.ok)
  }

  let returnType: Type = Void
  if (func.returnType !== undefined) {
    const result = analyzeType(ctx, func.returnType)
    if (result.err !== undefined) {
      return err(result.err)
    }
    returnType = result.ok
  }

  return ok({ kind: TypeKind.FUNCTION, arguments: args, return: returnType })
}

function analyzeArrayType(
  ctx: Context,
  array: ArrayTypeNode
): Result<ArrayType> {
  const dimension: Expr[] = []
  for (const dim of array.dimension.values) {
    const result = analyzeExpr(ctx, dim)
    if (result.err !== undefined) {
      return err(result.err)
    }
    dimension.push(result.ok)
  }

  const dimensionNum = []
  for (const dim of dimension) {
    if (!dim.isConstexpr) {
      return err(new NotAConstant(dim))
    }
    if (!typeEqual(dim.type, Integer)) {
      return err(new TypeMismatch(dim, Integer))
    }

    dimensionNum.push(dim.constValue as bigint)
  }

  const result = analyzeType(ctx, array.elementType)
  if (result.err !== undefined) {
    return err(result.err)
  }
  const elementType = result.ok

  return ok({
    kind: TypeKind.ARRAY,
    dimension: dimensionNum,
    type: elementType,
  })
}

function typeEqual(a: Type, b: Type): boolean {
  if (a.kind !== b.kind) return false

  if (a.kind === TypeKind.ARRAY) {
    const bType = b as ArrayType

    if (a.dimension.length !== bType.dimension.length) {
      return false
    }

    for (let i = 0; i < a.dimension.length; i++)
      if (a.dimension[i] !== bType.dimension[i]) {
        return false
      }

    return typeEqual(a.type, bType.type)
  } else if (a.kind === TypeKind.FUNCTION) {
    const bType = b as FunctionType
    return (
      typeEqual(a.return, bType.return) &&
      a.arguments.every((t, i) => typeEqual(t, bType.arguments[i]))
    )
  } else {
    return a.kind === b.kind
  }
}

function assertTokenKind(
  token: Token,
  kind: TokenKind,
  msg: string = ''
): void {
  if (msg.length === 0) {
    msg = `expected token '${token.value}' to be ${kind}, but it is a ${token.kind}`
  }
  assert(token.kind === kind)
}

function assert(
  v: boolean,
  msg: string = 'assertion failed'
): asserts v is true {
  if (!v) {
    throw new Error(msg)
  }
}
