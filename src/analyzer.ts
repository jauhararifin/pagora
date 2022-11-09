import {
  Argument,
  ArrayType,
  AssignStatement,
  BinaryExpr,
  BinaryOp,
  BlockStatement,
  Boolean,
  BooleanLitExpr,
  CallExpr,
  CastExpr,
  Char,
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
  ReturnStatement,
  Statement,
  StatementKind,
  Type,
  TypeKind,
  UnaryExpr,
  UnaryOp,
  VarStatement,
  Variable,
  Void,
  WhileStatement
} from './semantic'
import {
  ArrayIndexExprNode,
  ArrayTypeNode,
  AssignStatementNode,
  BinaryExprNode,
  BlockStatementNode,
  BooleanLitExprNode,
  CallExprNode,
  CastExprNode,
  DeclKind,
  ExprNode,
  ExprNodeKind,
  ExprStatementNode,
  FunctionDeclNode,
  IdentExprNode,
  IfStatementNode,
  IntegerLitExprNode,
  ReturnStatementNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
  UnaryExprNode,
  VarNode,
  VarStatementNode,
  WhileStatementNode
} from './ast'
import { Error as CompileError, ErrorKind, Result } from './errors'
import { Token, TokenKind } from './tokens'

export function analyze (ast: RootNode): Result<Program> {
  return new Analyzer().analyze(ast)
}

class Analyzer {
  functions: Function[] = []
  globals: Variable[] = []
  errors: CompileError[] = []

  symbolTable: Array<{ [key: string]: [Token, Type] }> = []
  currentReturnType: Type = { kind: TypeKind.VOID }

  // TODO: skip the whole process if the number errors are too many
  analyze (ast: RootNode): Result<Program> {
    // TODO: improve the language to support struct, tuple and type definition
    // This requires an additional step to load all the type names beforehand.
    // Although, at this phase, we don't need it yet.
    this.symbolTable = [{}]

    let main: BlockStatement | undefined
    for (const declaration of ast.declarations) {
      if (declaration.kind === DeclKind.FUNCTION) {
        this.analyzeFunction(declaration)
      } else if (declaration.kind === DeclKind.VARIABLE) {
        const variable = this.analyzeVariable(declaration.variable)
        if (variable !== undefined) {
          this.globals.push(variable)
        }
      } else if (declaration.kind === DeclKind.MAIN) {
        this.addScope()
        const stmt = this.analyzeBlockStatement(declaration.body)
        this.removeScope()
        if (main !== undefined) {
          this.emitError({ kind: ErrorKind.DUPLICATED_MAIN })
        } else {
          main = stmt
        }
      }
    }

    if (main === undefined) {
      this.emitError({ kind: ErrorKind.MISSING_MAIN })
      return { errors: this.errors }
    }

    return {
      value: { functions: this.functions, globals: this.globals, main },
      errors: this.errors
    }
  }

  private analyzeFunction (functionDecl: FunctionDeclNode): void {
    this.assertTokenKind(functionDecl.name, TokenKind.IDENTIFIER)

    const name = functionDecl.name.value

    const symbol = this.getSymbol(name)
    if (symbol !== undefined) {
      const [declaredAt] = symbol
      this.emitError({ kind: ErrorKind.MULTIPLE_DECLARATION, declaredAt, redeclaredAt: functionDecl.name })

      // TODO: consider keep analyze the function body for better warning message
      return
    }

    const type = this.analyzeFunctionType(functionDecl)
    if (type === undefined) return

    this.currentReturnType = (type.return != null) ? type.return : Void

    this.assert(type.arguments.length === functionDecl.params.params.length)
    const args: Argument[] = functionDecl.params.params.map((p, i): Argument => ({
      name: p.name.value,
      type: type.arguments[i]
    }))

    this.addScope()
    for (let i = 0; i < args.length; i++) {
      const arg = args[i]
      this.addSymbol(arg.name, functionDecl.params.params[i].name, arg.type)
    }
    const body = this.analyzeBlockStatement(functionDecl.body)
    this.removeScope()

    this.functions.push({ name, type, arguments: args, body })
    this.addSymbol(name, functionDecl.name, type)
  }

  private analyzeStatement (statement: StatementNode): Statement | undefined {
    switch (statement.kind) {
      case StatementNodeKind.BLOCK:
        return this.analyzeBlockStatement(statement)
      case StatementNodeKind.VAR:
        return this.analyzeVarStatement(statement)
      case StatementNodeKind.ASSIGN:
        return this.analyzeAssignStatement(statement)
      case StatementNodeKind.EXPR:
        return this.analyzeExprStatement(statement)
      case StatementNodeKind.IF:
        return this.analyzeIfStatement(statement)
      case StatementNodeKind.WHILE:
        return this.analyzeWhileStatement(statement)
      case StatementNodeKind.RETURN:
        return this.analyzeReturnStatement(statement)
    }
  }

  private analyzeBlockStatement (blockStatement: BlockStatementNode): BlockStatement {
    return {
      kind: StatementKind.BLOCK,
      body: blockStatement
        .statements
        .map((s) => this.analyzeStatement(s))
        .filter((s): s is Statement => s !== undefined)
    }
  }

  private analyzeVarStatement (stmt: VarStatementNode): VarStatement | undefined {
    const variable = this.analyzeVariable(stmt.variable)
    if (variable == null) return undefined

    return {
      kind: StatementKind.VAR,
      variable
    }
  }

  private analyzeVariable (variable: VarNode): Variable | undefined {
    this.assertTokenKind(variable.name, TokenKind.IDENTIFIER)

    const name = variable.name.value
    const symbol = this.getSymbol(name)
    if (symbol !== undefined) {
      const [declaredAt] = symbol
      this.emitError({ kind: ErrorKind.MULTIPLE_DECLARATION, declaredAt, redeclaredAt: variable.name })
      return
    }

    if (variable.type === undefined && variable.value === undefined) {
      throw new Error('variable declaration should have type or value expression')
    }

    const value: Expr | undefined = (variable.value != null) ? this.analyzeExpr(variable.value) : undefined

    const type = variable.type != null ? this.analyzeType(variable.type) : value?.type
    if (type == null) {
      throw new Error('variable declaration should have type or value expression')
    }

    if (value != null) {
      if (!this.valueIsA(value.type, type)) {
        this.emitError({ kind: ErrorKind.TYPE_MISMATCH, source: value, targetType: type })
        return
      }

      if (!value.isConstexpr) {
        this.emitError({ kind: ErrorKind.NOT_A_CONSTANT, value })
        return
      }
    }

    this.addSymbol(name, variable.name, type)
    return { name, type, value }
  }

  private analyzeAssignStatement (stmt: AssignStatementNode): AssignStatement | undefined {
    const receiver = this.analyzeExpr(stmt.receiver)
    if (receiver === undefined) return

    if (!receiver.isAssignable) {
      this.emitError({ kind: ErrorKind.CANNOT_ASSIGN, expr: stmt.receiver, receiver: receiver.type })
      return
    }

    const value = this.analyzeExpr(stmt.value)
    if (value === undefined) return

    if (!this.valueIsA(value.type, receiver.type)) {
      this.emitError({ kind: ErrorKind.TYPE_MISMATCH, targetType: receiver.type, source: value })
      return
    }

    return {
      kind: StatementKind.ASSIGN,
      target: receiver,
      value
    }
  }

  private analyzeExprStatement (stmt: ExprStatementNode): ExprStatement | undefined {
    const value = this.analyzeExpr(stmt.expr)
    if (value === undefined) return

    return { kind: StatementKind.EXPR, value }
  }

  private analyzeIfStatement (stmt: IfStatementNode): IfStatement | undefined {
    const condition = this.analyzeExpr(stmt.condition)
    if (condition === undefined) return

    if (!this.valueIsA(condition.type, Boolean)) {
      this.emitError({ kind: ErrorKind.TYPE_MISMATCH, targetType: Boolean, source: condition })
      return
    }

    const body = this.analyzeStatement(stmt.body)
    if (body === undefined) return

    const elseStmt = (stmt.else != null) ? this.analyzeStatement(stmt.else) : undefined

    return {
      kind: StatementKind.IF,
      condition,
      body,
      else: elseStmt
    }
  }

  private analyzeWhileStatement (stmt: WhileStatementNode): WhileStatement | undefined {
    const condition = this.analyzeExpr(stmt.condition)
    if (condition === undefined) return

    if (!this.valueIsA(condition.type, Boolean)) {
      this.emitError({ kind: ErrorKind.TYPE_MISMATCH, targetType: Boolean, source: condition })
      return
    }

    const body = this.analyzeStatement(stmt.body)
    if (body === undefined) return

    return {
      kind: StatementKind.WHILE,
      condition,
      body
    }
  }

  private analyzeReturnStatement (stmt: ReturnStatementNode): ReturnStatement | undefined {
    if (stmt.value != null) {
      const value = this.analyzeExpr(stmt.value)
      if (value === undefined) return
      if (!this.valueIsA(value.type, this.currentReturnType)) {
        this.emitError({ kind: ErrorKind.TYPE_MISMATCH, source: value, targetType: this.currentReturnType })
        return
      }
      return { kind: StatementKind.RETURN, value }
    } else {
      if (this.currentReturnType.kind !== TypeKind.VOID) {
        this.emitError({ kind: ErrorKind.FUNCTION_IS_VOID, return: stmt.return })
        return
      }
      return { kind: StatementKind.RETURN }
    }
  }

  private analyzeType (node: TypeExprNode): Type | undefined {
    switch (node.kind) {
      case TypeExprNodeKind.PRIMITIVE:
        switch (node.type.kind) {
          case TokenKind.INTEGER:
            return Integer
          case TokenKind.BOOLEAN:
            return Boolean
          case TokenKind.CHAR:
            return Char
          case TokenKind.REAL:
            return Real
          default:
            throw new Error(`unrecognized type ${node.kind}`)
        }
      case TypeExprNodeKind.ARRAY:
        return this.analyzeArrayType(node)
    }
  }

  private analyzeArrayType (node: ArrayTypeNode): ArrayType | undefined {
    const dimension = node.dimension.values.map(n => this.analyzeExpr(n))

    const dimensionNum = []
    for (let i = 0; i < dimension.length; i++) {
      const dim = dimension[i]
      if (dim === undefined) return
      if (!dim.isConstexpr) {
        this.emitError({ kind: ErrorKind.NOT_A_CONSTANT, value: dim })
        return
      }
      if (!this.valueIsA(dim.type, Integer)) {
        this.emitError({ kind: ErrorKind.TYPE_MISMATCH, source: dim, targetType: Integer })
        return
      }

      dimensionNum.push(dim.constValue as BigInt)
    }

    const elementType = this.analyzeType(node.type)
    if (elementType === undefined) return

    return { kind: TypeKind.ARRAY, dimension: dimensionNum, type: elementType }
  }

  private analyzeFunctionType (node: FunctionDeclNode): FunctionType | undefined {
    const args: Type[] = []
    for (const param of node.params.params) {
      const type = this.analyzeType(param.type)
      if (type === undefined) return
      args.push(type)
    }

    const voidType: Type = { kind: TypeKind.VOID }
    const returnType = (node.returnType !== undefined) ? this.analyzeType(node.returnType) : voidType
    if (returnType === undefined) return

    return { kind: TypeKind.FUNCTION, arguments: args, return: returnType }
  }

  private analyzeExpr (node: ExprNode): Expr | undefined {
    switch (node.kind) {
      case ExprNodeKind.IDENT:
        return this.analyzeIdentExpr(node)
      case ExprNodeKind.INTEGER_LIT:
        return this.analyzeIntegerLitExpr(node)
      case ExprNodeKind.BOOLEAN_LIT:
        return this.analyzeBooleanLitExpr(node)
      case ExprNodeKind.BINARY:
        return this.analyzeBinaryExpr(node)
      case ExprNodeKind.UNARY:
        return this.analyzeUnaryExpr(node)
      case ExprNodeKind.CALL:
        return this.analyzeCallExpr(node)
      case ExprNodeKind.ARRAY_INDEX:
        return this.analyzeArrayIndexExpr(node)
      case ExprNodeKind.CAST:
        return this.analyzeCastExpr(node)
      case ExprNodeKind.GROUPED:
        return this.analyzeExpr(node.value)
    }
  }

  private analyzeIdentExpr (expr: IdentExprNode): IdentExpr | undefined {
    this.assertTokenKind(expr.name, TokenKind.IDENTIFIER)

    const name = expr.name.value
    const symbol = this.getSymbol(name)
    if (symbol === undefined) {
      this.emitError({ kind: ErrorKind.UNDEFINED, name: expr.name })
      return
    }

    const [, refType] = symbol

    return {
      kind: ExprKind.IDENT,
      type: refType,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: true,
      ident: name
    }
  }

  private analyzeIntegerLitExpr (expr: IntegerLitExprNode): IntegerLitExpr | undefined {
    this.assertTokenKind(expr.value, TokenKind.INTEGER_LITERAL)

    const value = BigInt(expr.value.value)

    // TODO: IMPORTANT: add bound check here. We should only support 64bit integer in this language.

    return {
      kind: ExprKind.INTEGER_LIT,
      type: Integer,
      isConstexpr: true,
      constValue: value,
      isAssignable: false,
      value
    }
  }

  private analyzeBooleanLitExpr (expr: BooleanLitExprNode): BooleanLitExpr | undefined {
    let value: boolean | undefined
    if (expr.value.kind === TokenKind.TRUE) {
      value = true
    } else if (expr.value.kind === TokenKind.FALSE) {
      value = false
    }

    if (value === undefined) {
      throw new Error('boolean expression should have true or false value')
    }

    // TODO: IMPORTANT: add bound check here. We should only support 64bit integer in this language.

    return {
      kind: ExprKind.BOOLEAN_LIT,
      type: Boolean,
      isConstexpr: true,
      constValue: value,
      isAssignable: false,
      value
    }
  }

  private analyzeBinaryExpr (expr: BinaryExprNode): BinaryExpr | undefined {
    const a = this.analyzeExpr(expr.a)
    if (a === undefined) return

    const b = this.analyzeExpr(expr.b)
    if (b === undefined) return

    const operatorMap: {
      [K in TokenKind]?: {
        acceptedTypes: Array<[Type, Type, Type]>
        op: BinaryOp
      }
    } = {
      [TokenKind.PLUS]: {
        acceptedTypes: [[Integer, Integer, Integer], [Real, Real, Real]],
        op: BinaryOp.PLUS
      },
      [TokenKind.MINUS]: {
        acceptedTypes: [[Integer, Integer, Integer], [Real, Real, Real]],
        op: BinaryOp.MINUS
      },
      [TokenKind.MULTIPLY]: {
        acceptedTypes: [[Integer, Integer, Integer], [Real, Real, Real]],
        op: BinaryOp.MUL
      },
      [TokenKind.DIV]: {
        acceptedTypes: [[Integer, Integer, Integer], [Real, Real, Real]],
        op: BinaryOp.DIV
      },
      [TokenKind.GREATER_THAN]: {
        acceptedTypes: [[Integer, Integer, Boolean], [Real, Real, Boolean]],
        op: BinaryOp.GREATER_THAN
      },
      [TokenKind.GREATER_THAN_EQUAL]: {
        acceptedTypes: [[Integer, Integer, Boolean], [Real, Real, Boolean]],
        op: BinaryOp.GREATER_THAN_EQUAL
      },
      [TokenKind.LESS_THAN]: {
        acceptedTypes: [[Integer, Integer, Boolean], [Real, Real, Boolean]],
        op: BinaryOp.LESS_THAN
      },
      [TokenKind.LESS_THAN_EQUAL]: {
        acceptedTypes: [[Integer, Integer, Boolean], [Real, Real, Boolean]],
        op: BinaryOp.LESS_THAN_EQUAL
      },
      [TokenKind.EQUAL]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
          [Boolean, Boolean, Boolean]
        ],
        op: BinaryOp.EQUAL
      },
      [TokenKind.NOT_EQUAL]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
          [Boolean, Boolean, Boolean]
        ],
        op: BinaryOp.NOT_EQUAL
      },
      [TokenKind.AND]: {
        acceptedTypes: [[Boolean, Boolean, Boolean]],
        op: BinaryOp.AND
      },
      [TokenKind.OR]: {
        acceptedTypes: [[Boolean, Boolean, Boolean]],
        op: BinaryOp.AND
      },
      [TokenKind.BIT_AND]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.BIT_AND
      },
      [TokenKind.BIT_OR]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.BIT_OR
      },
      [TokenKind.BIT_XOR]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.BIT_XOR
      },
      [TokenKind.SHIFT_LEFT]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.SHIFT_LEFT
      },
      [TokenKind.SHIFT_RIGHT]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.SHIFT_RIGHT
      }
    }

    if (!(expr.op.kind in operatorMap)) {
      throw new Error(`invalid binary operator ${expr.op.kind}`)
    }

    const spec = operatorMap[expr.op.kind]
    if (spec === undefined) return

    const { acceptedTypes, op } = spec

    const result = acceptedTypes.find(([aType, bType, rType]) => a.type === aType && b.type === bType)
    if (result === undefined) {
      this.emitError({ kind: ErrorKind.INVALID_BINARY_OP, a, b, op: expr.op })
      return
    }
    const [,,resultType] = result

    return {
      kind: ExprKind.BINARY,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      type: resultType,
      a,
      b,
      op
    }
  }

  private analyzeUnaryExpr (expr: UnaryExprNode): UnaryExpr | undefined {
    const value = this.analyzeExpr(expr.value)
    if (value === undefined) return

    const operatorMap: {
      [K in TokenKind]?: {
        acceptedTypes: Array<[Type, Type]>
        op: UnaryOp
      }
    } = {
      [TokenKind.PLUS]: {
        acceptedTypes: [[Integer, Integer], [Real, Real]],
        op: UnaryOp.PLUS
      },
      [TokenKind.MINUS]: {
        acceptedTypes: [[Integer, Integer], [Real, Real]],
        op: UnaryOp.MINUS
      },
      [TokenKind.NOT]: {
        acceptedTypes: [[Boolean, Boolean]],
        op: UnaryOp.NOT
      },
      [TokenKind.BIT_NOT]: {
        acceptedTypes: [[Integer, Integer]],
        op: UnaryOp.BIT_NOT
      }
    }

    if (!(expr.op.kind in operatorMap)) {
      throw new Error(`invalid unary operator ${expr.op.kind}`)
    }

    const spec = operatorMap[expr.op.kind]
    if (spec === undefined) return

    const { acceptedTypes, op } = spec

    const result = acceptedTypes.find(([vType, rType]) => value.type === vType)
    if (result === undefined) {
      this.emitError({ kind: ErrorKind.INVALID_UNARY_OP, value, op: expr.op })
      return
    }
    const [,resultType] = result

    return {
      kind: ExprKind.UNARY,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      type: resultType,
      value,
      op
    }
  }

  private analyzeCallExpr (expr: CallExprNode): CallExpr | undefined {
    const callee = this.analyzeExpr(expr.callee)
    if (callee === undefined) return

    if (callee.type.kind !== TypeKind.FUNCTION) {
      this.emitError({
        kind: ErrorKind.TYPE_MISMATCH,
        source: callee,
        targetType: TypeKind.FUNCTION
      })
      return
    }

    if (callee.type.arguments.length !== expr.arguments.values.length) {
      this.emitError({
        kind: ErrorKind.WRONG_NUMBER_OF_ARGUMENT,
        expected: callee.type.arguments.length,
        got: expr.arguments.values.length
      })
      return
    }

    const args: Expr[] = []
    for (let i = 0; i < callee.type.arguments.length; i++) {
      const arg = this.analyzeExpr(expr.arguments.values[i])
      if (arg === undefined) return

      const paramType = callee.type.arguments[i]

      if (!this.valueIsCastable(arg.type, paramType)) {
        this.emitError({
          kind: ErrorKind.TYPE_MISMATCH,
          source: arg,
          targetType: paramType
        })
        return
      }

      args.push(arg)
    }

    return {
      kind: ExprKind.CALL,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      type: callee.type.return,
      function: callee,
      arguments: args
    }
  }

  private analyzeArrayIndexExpr (expr: ArrayIndexExprNode): IndexExpr | undefined {
    const array = this.analyzeExpr(expr.array)
    if (array === undefined) return

    if (array.type.kind !== TypeKind.ARRAY) {
      this.emitError({
        kind: ErrorKind.TYPE_MISMATCH,
        source: array,
        targetType: TypeKind.ARRAY
      })
      return
    }

    if (array.type.dimension.length !== expr.index.values.length) {
      this.emitError({
        kind: ErrorKind.WRONG_NUMBER_OF_INDEX,
        expected: array.type.dimension.length,
        got: expr.index.values.length
      })
      return
    }

    const indices: Expr[] = []
    for (let i = 0; i < array.type.dimension.length; i++) {
      const index = this.analyzeExpr(expr.index.values[i])
      if (index === undefined) return

      if (index.type.kind !== TypeKind.INTEGER) {
        this.emitError({
          kind: ErrorKind.TYPE_MISMATCH,
          source: index,
          targetType: TypeKind.INTEGER
        })
        return
      }

      indices.push(index)
    }

    return {
      kind: ExprKind.INDEX,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: true,
      type: array.type.type,
      array,
      indices
    }
  }

  private analyzeCastExpr (expr: CastExprNode): CastExpr | undefined {
    const value = this.analyzeExpr(expr.source)
    if (value === undefined) return

    const target = this.analyzeType(expr.target)
    if (target === undefined) return

    if (!this.valueIsCastable(value.type, target)) {
      this.emitError({
        kind: ErrorKind.TYPE_MISMATCH,
        source: value,
        targetType: target
      })
      return
    }

    return {
      kind: ExprKind.CAST,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      source: value,
      type: target
    }
  }

  private valueIsA (value: Type, target: Type): boolean {
    if (value.kind !== target.kind) return false

    if (value.kind === TypeKind.ARRAY) {
      const targetType = target as ArrayType
      return value.dimension === targetType.dimension && this.valueIsA(value.type, targetType.type)
    }

    if (value.kind === TypeKind.FUNCTION) {
      const targetType = target as FunctionType
      return value.return === targetType.return &&
      value.arguments.every((t, i) => this.valueIsA(t, targetType.arguments[i]))
    }

    return value.kind === target.kind
  }

  private valueIsCastable (value: Type, target: Type): boolean {
    // TODO: implement cast check
    return this.valueIsA(value, target)
  }

  private getSymbol (name: string): [Token, Type] | undefined {
    for (let i = this.symbolTable.length - 1; i >= 0; i--) {
      if (name in this.symbolTable[i]) {
        return this.symbolTable[i][name]
      }
    }
  }

  private addSymbol (name: string, token: Token, type: Type): void {
    this.symbolTable[this.symbolTable.length - 1][name] = [token, type]
  }

  private addScope (): void {
    this.symbolTable.push({})
  }

  private removeScope (): void {
    this.symbolTable.pop()
  }

  private emitError (error: CompileError): void {
    this.errors.push(error)
  }

  private assertTokenKind (token: Token, kind: TokenKind, msg: string = ''): void {
    if (msg.length === 0) {
      msg = `expected token '${token.value}' to be ${kind}, but it is a ${token.kind}`
    }
    this.assert(token.kind === kind)
  }

  private assert (v: boolean, msg: string = 'assertion failed'): void {
    if (!v) {
      throw new Error(msg)
    }
  }
}
