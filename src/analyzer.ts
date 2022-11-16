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
  WhileStatement,
  Byte,
  ArrayLitExpr,
  StringLitExpr,
} from './semantic'
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
  StringLitExprNode,
  TypeExprNode,
  TypeExprNodeKind,
  UnaryExprNode,
  VarNode,
  VarStatementNode,
  WhileStatementNode,
} from './ast'
import {
  CompileError,
  CompileErrorItem,
  DuplicatedMain,
  InvalidBinaryOperator,
  InvalidUnaryOperator,
  MissingMain,
  MissingReturnValue,
  MultipleDeclaration,
  NotAConstant,
  NotAssignable,
  TypeMismatch,
  UndefinedSymbol,
  WrongNumberOfArgument,
  WrongNumberOfIndex,
} from './errors'
import { Token, TokenKind } from './tokens'

export function analyze(ast: RootNode): Program {
  return new Analyzer().analyze(ast)
}

// TODO: add checking in the function body. Make sure that a function with return type always return.
// TODO: add native function.
// TODO: support any type
class Analyzer {
  functions: Function[] = []
  globals: Variable[] = []
  errors: CompileErrorItem[] = []

  symbolTable: Array<{ [key: string]: [Token, Type] }> = []
  currentReturnType: Type = { kind: TypeKind.VOID }

  // TODO: skip the whole process if the number errors are too many
  analyze(ast: RootNode): Program {
    // TODO: improve the language to support struct, tuple and type definition
    // This requires an additional step to load all the type names beforehand.
    // Although, at this phase, we don't need it yet.
    this.symbolTable = [{}]

    let main: BlockStatement | undefined
    let mainToken: Token | undefined

    for (const declaration of ast.declarations) {
      try {
        if (declaration.kind === DeclKind.FUNCTION) {
          this.analyzeFunction(declaration)
        } else if (declaration.kind === DeclKind.VARIABLE) {
          const variable = this.analyzeVariable(declaration.variable, false)
          if (variable !== undefined) {
            this.globals.push(variable)
          }
        } else if (declaration.kind === DeclKind.MAIN) {
          this.addScope()
          const stmt = this.analyzeBlockStatement(declaration.body)
          this.removeScope()
          if (main !== undefined) {
            this.emitError(
              new DuplicatedMain(mainToken!, declaration.body.begin)
            )
          } else {
            main = stmt
            mainToken = declaration.body.begin
          }
        }
      } catch (e) {
        this.emitError(e as CompileErrorItem)
      }
    }

    if (main === undefined) {
      this.emitError(new MissingMain())
      throw new CompileError(this.errors)
    }

    if (this.errors.length > 0) {
      throw new CompileError(this.errors)
    }

    return { functions: this.functions, globals: this.globals, main }
  }

  private analyzeFunction(functionDecl: FunctionDeclNode): void {
    this.assertTokenKind(functionDecl.name, TokenKind.IDENTIFIER)

    const name = functionDecl.name.value

    const symbol = this.getSymbol(name)
    if (symbol !== undefined) {
      const [declaredAt] = symbol
      this.emitError(new MultipleDeclaration(declaredAt, functionDecl.name))

      // TODO: consider keep analyze the function body for better warning message
      return
    }

    const type = this.analyzeFunctionType(functionDecl)
    if (type === undefined) return

    this.currentReturnType = type.return != null ? type.return : Void

    this.assert(type.arguments.length === functionDecl.params.params.length)
    const args: Argument[] = functionDecl.params.params.map(
      (p, i): Argument => ({
        name: p.name.value,
        type: type.arguments[i],
      })
    )

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

  private analyzeStatement(statement: StatementNode): Statement {
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
      case StatementNodeKind.CONTINUE:
      case StatementNodeKind.BREAK:
        throw new Error('not implemented yet')
    }
  }

  private analyzeBlockStatement(
    blockStatement: BlockStatementNode
  ): BlockStatement {
    const statements: Statement[] = []
    for (const stmt of blockStatement.statements) {
      try {
        statements.push(this.analyzeStatement(stmt))
      } catch (e) {
        this.emitError(e as CompileErrorItem)
      }
    }

    return {
      kind: StatementKind.BLOCK,
      body: statements,
    }
  }

  private analyzeVarStatement(stmt: VarStatementNode): VarStatement {
    const variable = this.analyzeVariable(stmt.variable, true)

    return {
      kind: StatementKind.VAR,
      variable,
    }
  }

  private analyzeVariable(
    variable: VarNode,
    allowNonConstant: boolean
  ): Variable {
    this.assertTokenKind(variable.name, TokenKind.IDENTIFIER)

    const name = variable.name.value
    const symbol = this.getSymbol(name)
    if (symbol !== undefined) {
      const [declaredAt] = symbol
      throw new MultipleDeclaration(declaredAt, variable.name)
    }

    if (variable.type === undefined && variable.value === undefined) {
      throw new Error(
        'variable declaration should have type or value expression'
      )
    }

    const value: Expr | undefined =
      variable.value != null ? this.analyzeExpr(variable.value) : undefined

    const type =
      variable.type != null ? this.analyzeType(variable.type) : value?.type
    if (type == null) {
      throw new Error(
        'variable declaration should have type or value expression'
      )
    }

    if (value != null) {
      if (!this.valueIsA(value.type, type)) {
        throw new TypeMismatch(value, type)
      }

      if (!allowNonConstant && !value.isConstexpr) {
        throw new NotAConstant(value)
      }
    }

    this.addSymbol(name, variable.name, type)
    return { name, type, value }
  }

  private analyzeAssignStatement(stmt: AssignStatementNode): AssignStatement {
    const receiver = this.analyzeExpr(stmt.receiver)

    if (!receiver.isAssignable) {
      throw new NotAssignable(receiver)
    }

    const value = this.analyzeExpr(stmt.value)

    if (!this.valueIsA(value.type, receiver.type)) {
      throw new TypeMismatch(value, receiver.type)
    }

    return {
      kind: StatementKind.ASSIGN,
      target: receiver,
      value,
    }
  }

  private analyzeExprStatement(stmt: ExprStatementNode): ExprStatement {
    const value = this.analyzeExpr(stmt.expr)
    return { kind: StatementKind.EXPR, value }
  }

  private analyzeIfStatement(stmt: IfStatementNode): IfStatement {
    const condition = this.analyzeExpr(stmt.condition)
    if (!this.valueIsA(condition.type, Boolean)) {
      throw new TypeMismatch(condition, Boolean)
    }

    const body = this.analyzeStatement(stmt.body)

    const elseStmt =
      stmt.elseBody != null ? this.analyzeStatement(stmt.elseBody) : undefined

    return {
      kind: StatementKind.IF,
      condition,
      body,
      else: elseStmt,
    }
  }

  private analyzeWhileStatement(stmt: WhileStatementNode): WhileStatement {
    const condition = this.analyzeExpr(stmt.condition)

    if (!this.valueIsA(condition.type, Boolean)) {
      throw new TypeMismatch(condition, Boolean)
    }

    const body = this.analyzeStatement(stmt.body)

    return {
      kind: StatementKind.WHILE,
      condition,
      body,
    }
  }

  private analyzeReturnStatement(stmt: ReturnStatementNode): ReturnStatement {
    if (stmt.value != null) {
      const value = this.analyzeExpr(stmt.value)
      if (!this.valueIsA(value.type, this.currentReturnType)) {
        throw new TypeMismatch(value, this.currentReturnType)
      }
      return { kind: StatementKind.RETURN, value }
    } else {
      if (this.currentReturnType.kind !== TypeKind.VOID) {
        throw new MissingReturnValue(stmt.return)
      }
      return { kind: StatementKind.RETURN }
    }
  }

  private analyzeType(node: TypeExprNode): Type {
    switch (node.kind) {
      case TypeExprNodeKind.PRIMITIVE:
        switch (node.type.kind) {
          case TokenKind.INTEGER:
            return Integer
          case TokenKind.BOOLEAN:
            return Boolean
          case TokenKind.BYTE:
            return Byte
          case TokenKind.REAL:
            return Real
          default:
            throw new Error(`unrecognized type ${node.kind}`)
        }
      case TypeExprNodeKind.ARRAY:
        return this.analyzeArrayType(node)
    }
  }

  private analyzeArrayType(node: ArrayTypeNode): ArrayType {
    const dimension = node.dimension.values.map((n) => this.analyzeExpr(n))

    const dimensionNum = []
    for (const dim of dimension) {
      if (!dim.isConstexpr) {
        throw new NotAConstant(dim)
      }
      if (!this.valueIsA(dim.type, Integer)) {
        throw new TypeMismatch(dim, Integer)
      }

      dimensionNum.push(dim.constValue as bigint)
    }

    const elementType = this.analyzeType(node.type)

    return {
      kind: TypeKind.ARRAY,
      dimension: dimensionNum,
      type: elementType,
    }
  }

  private analyzeFunctionType(node: FunctionDeclNode): FunctionType {
    const args: Type[] = []
    for (const param of node.params.params) {
      const type = this.analyzeType(param.type)
      args.push(type)
    }

    const voidType: Type = { kind: TypeKind.VOID }
    const returnType =
      node.returnType !== undefined
        ? this.analyzeType(node.returnType)
        : voidType

    return { kind: TypeKind.FUNCTION, arguments: args, return: returnType }
  }

  private analyzeExpr(node: ExprNode): Expr {
    switch (node.kind) {
      case ExprNodeKind.IDENT:
        return this.analyzeIdentExpr(node)
      case ExprNodeKind.INTEGER_LIT:
        return this.analyzeIntegerLitExpr(node)
      case ExprNodeKind.BOOLEAN_LIT:
        return this.analyzeBooleanLitExpr(node)
      case ExprNodeKind.ARRAY_LIT:
        return this.analyzeArrayLitExpr(node)
      case ExprNodeKind.STRING_LIT:
        return this.analyzeStringLitExpr(node)
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

  private analyzeIdentExpr(expr: IdentExprNode): IdentExpr {
    this.assertTokenKind(expr.name, TokenKind.IDENTIFIER)

    const name = expr.name.value
    const symbol = this.getSymbol(name)
    if (symbol === undefined) {
      throw new UndefinedSymbol(expr.name)
    }

    const [, refType] = symbol

    return {
      kind: ExprKind.IDENT,
      type: refType,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: true,
      position: expr.name.position,
      ident: name,
    }
  }

  private analyzeIntegerLitExpr(expr: IntegerLitExprNode): IntegerLitExpr {
    this.assertTokenKind(expr.value, TokenKind.INTEGER_LITERAL)

    const value = BigInt(expr.value.value)

    // TODO: IMPORTANT: add bound check here. We should only support 64bit integer in this language.

    return {
      kind: ExprKind.INTEGER_LIT,
      type: Integer,
      isConstexpr: true,
      constValue: value,
      isAssignable: false,
      position: expr.value.position,
      value,
    }
  }

  private analyzeBooleanLitExpr(expr: BooleanLitExprNode): BooleanLitExpr {
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
      position: expr.value.position,
      value,
    }
  }

  private analyzeArrayLitExpr(expr: ArrayLitExprNode): ArrayLitExpr {
    const values: Expr[] = []
    let isConstexpr = true
    const constValue: Expr[] = []

    for (let i = 0; i < expr.value.values.length; i++) {
      const valNode = expr.value.values[i]
      const value = this.analyzeExpr(valNode)

      isConstexpr = isConstexpr && value.isConstexpr
      constValue.push(value.constValue)
      values.push(value)
    }

    for (let i = 1; i < values.length; i++) {
      if (!this.valueIsA(values[i].type, values[0].type)) {
        throw new TypeMismatch(values[i], values[0].type)
      }
    }

    let elementType = values[0].type
    let dimension = [BigInt(values.length)]
    if (values[0].type.kind === TypeKind.ARRAY) {
      elementType = values[0].type.type
      dimension = [dimension[0], ...values[0].type.dimension]
    }

    const typ: Type = {
      kind: TypeKind.ARRAY,
      dimension,
      type: elementType,
    }

    return {
      kind: ExprKind.ARRAY_LIT,
      isConstexpr,
      constValue: isConstexpr ? constValue : undefined,
      isAssignable: false,
      position: expr.openSquare.position,
      type: typ,
      values,
    }
  }

  private analyzeStringLitExpr(expr: StringLitExprNode): StringLitExpr {
    throw new Error('not yet implemented')
  }

  private analyzeBinaryExpr(expr: BinaryExprNode): BinaryExpr {
    const a = this.analyzeExpr(expr.a)
    const b = this.analyzeExpr(expr.b)

    const operatorMap: {
      [K in TokenKind]?: {
        acceptedTypes: Array<[Type, Type, Type]>
        op: BinaryOp
      }
    } = {
      [TokenKind.PLUS]: {
        acceptedTypes: [
          [Integer, Integer, Integer],
          [Real, Real, Real],
        ],
        op: BinaryOp.PLUS,
      },
      [TokenKind.MINUS]: {
        acceptedTypes: [
          [Integer, Integer, Integer],
          [Real, Real, Real],
        ],
        op: BinaryOp.MINUS,
      },
      [TokenKind.MULTIPLY]: {
        acceptedTypes: [
          [Integer, Integer, Integer],
          [Real, Real, Real],
        ],
        op: BinaryOp.MUL,
      },
      [TokenKind.DIV]: {
        acceptedTypes: [
          [Integer, Integer, Integer],
          [Real, Real, Real],
        ],
        op: BinaryOp.DIV,
      },
      [TokenKind.GREATER_THAN]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
        ],
        op: BinaryOp.GREATER_THAN,
      },
      [TokenKind.GREATER_THAN_EQUAL]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
        ],
        op: BinaryOp.GREATER_THAN_EQUAL,
      },
      [TokenKind.LESS_THAN]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
        ],
        op: BinaryOp.LESS_THAN,
      },
      [TokenKind.LESS_THAN_EQUAL]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
        ],
        op: BinaryOp.LESS_THAN_EQUAL,
      },
      [TokenKind.EQUAL]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
          [Boolean, Boolean, Boolean],
        ],
        op: BinaryOp.EQUAL,
      },
      [TokenKind.NOT_EQUAL]: {
        acceptedTypes: [
          [Integer, Integer, Boolean],
          [Real, Real, Boolean],
          [Boolean, Boolean, Boolean],
        ],
        op: BinaryOp.NOT_EQUAL,
      },
      [TokenKind.AND]: {
        acceptedTypes: [[Boolean, Boolean, Boolean]],
        op: BinaryOp.AND,
      },
      [TokenKind.OR]: {
        acceptedTypes: [[Boolean, Boolean, Boolean]],
        op: BinaryOp.AND,
      },
      [TokenKind.BIT_AND]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.BIT_AND,
      },
      [TokenKind.BIT_OR]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.BIT_OR,
      },
      [TokenKind.BIT_XOR]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.BIT_XOR,
      },
      [TokenKind.SHIFT_LEFT]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.SHIFT_LEFT,
      },
      [TokenKind.SHIFT_RIGHT]: {
        acceptedTypes: [[Integer, Integer, Integer]],
        op: BinaryOp.SHIFT_RIGHT,
      },
    }

    if (!(expr.op.kind in operatorMap)) {
      throw new Error(`invalid binary operator ${expr.op.kind}`)
    }

    const spec = operatorMap[expr.op.kind]
    if (spec === undefined) {
      throw new Error('failed analyze binary operator')
    }

    const { acceptedTypes, op } = spec

    const result = acceptedTypes.find(
      ([aType, bType, rType]) => a.type === aType && b.type === bType
    )
    if (result === undefined) {
      throw new InvalidBinaryOperator(a, op, b)
    }
    const [, , resultType] = result

    return {
      kind: ExprKind.BINARY,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      type: resultType,
      position: a.position,
      a,
      b,
      op,
    }
  }

  private analyzeUnaryExpr(expr: UnaryExprNode): UnaryExpr {
    const value = this.analyzeExpr(expr.value)

    const operatorMap: {
      [K in TokenKind]?: {
        acceptedTypes: Array<[Type, Type]>
        op: UnaryOp
      }
    } = {
      [TokenKind.PLUS]: {
        acceptedTypes: [
          [Integer, Integer],
          [Real, Real],
        ],
        op: UnaryOp.PLUS,
      },
      [TokenKind.MINUS]: {
        acceptedTypes: [
          [Integer, Integer],
          [Real, Real],
        ],
        op: UnaryOp.MINUS,
      },
      [TokenKind.NOT]: {
        acceptedTypes: [[Boolean, Boolean]],
        op: UnaryOp.NOT,
      },
      [TokenKind.BIT_NOT]: {
        acceptedTypes: [[Integer, Integer]],
        op: UnaryOp.BIT_NOT,
      },
    }

    if (!(expr.op.kind in operatorMap)) {
      throw new Error(`invalid unary operator ${expr.op.kind}`)
    }

    const spec = operatorMap[expr.op.kind]
    if (spec === undefined) {
      throw new Error('cannot analyze unary operator')
    }

    const { acceptedTypes, op } = spec

    const result = acceptedTypes.find(([vType, rType]) => value.type === vType)
    if (result === undefined) {
      throw new InvalidUnaryOperator(value, expr.op)
    }
    const [, resultType] = result

    return {
      kind: ExprKind.UNARY,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      type: resultType,
      position: expr.op.position,
      value,
      op,
    }
  }

  private analyzeCallExpr(expr: CallExprNode): CallExpr {
    const callee = this.analyzeExpr(expr.callee)
    if (callee.type.kind !== TypeKind.FUNCTION) {
      throw new TypeMismatch(callee, TypeKind.FUNCTION)
    }

    if (callee.type.arguments.length !== expr.arguments.values.length) {
      throw new WrongNumberOfArgument(expr, callee.type.arguments.length)
    }

    const args: Expr[] = []
    for (let i = 0; i < callee.type.arguments.length; i++) {
      const arg = this.analyzeExpr(expr.arguments.values[i])
      const paramType = callee.type.arguments[i]

      if (!this.valueIsCastable(arg.type, paramType)) {
        throw new TypeMismatch(arg, paramType)
      }

      args.push(arg)
    }

    return {
      kind: ExprKind.CALL,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      type: callee.type.return,
      position: callee.position,
      function: callee,
      arguments: args,
    }
  }

  private analyzeArrayIndexExpr(expr: ArrayIndexExprNode): IndexExpr {
    const array = this.analyzeExpr(expr.array)
    if (array.type.kind !== TypeKind.ARRAY) {
      throw new TypeMismatch(array, TypeKind.ARRAY)
    }

    if (array.type.dimension.length !== expr.index.values.length) {
      throw new WrongNumberOfIndex(expr, array.type.dimension.length)
    }

    const indices: Expr[] = []
    for (let i = 0; i < array.type.dimension.length; i++) {
      const index = this.analyzeExpr(expr.index.values[i])
      if (index.type.kind !== TypeKind.INTEGER) {
        throw new TypeMismatch(index, TypeKind.INTEGER)
      }

      indices.push(index)
    }

    return {
      kind: ExprKind.INDEX,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: true,
      type: array.type.type,
      position: array.position,
      array,
      indices,
    }
  }

  private analyzeCastExpr(expr: CastExprNode): CastExpr {
    const value = this.analyzeExpr(expr.source)
    const target = this.analyzeType(expr.target)
    if (!this.valueIsCastable(value.type, target)) {
      throw new TypeMismatch(value, target)
    }

    return {
      kind: ExprKind.CAST,
      isConstexpr: false,
      constValue: undefined,
      isAssignable: false,
      position: value.position,
      source: value,
      type: target,
    }
  }

  private valueIsA(value: Type, target: Type): boolean {
    if (value.kind !== target.kind) return false

    if (value.kind === TypeKind.ARRAY) {
      const targetType = target as ArrayType
      return (
        value.dimension === targetType.dimension &&
        this.valueIsA(value.type, targetType.type)
      )
    }

    if (value.kind === TypeKind.FUNCTION) {
      const targetType = target as FunctionType
      return (
        value.return === targetType.return &&
        value.arguments.every((t, i) =>
          this.valueIsA(t, targetType.arguments[i])
        )
      )
    }

    return value.kind === target.kind
  }

  private valueIsCastable(value: Type, target: Type): boolean {
    // TODO: implement cast check
    return this.valueIsA(value, target)
  }

  private getSymbol(name: string): [Token, Type] | undefined {
    for (let i = this.symbolTable.length - 1; i >= 0; i--) {
      if (name in this.symbolTable[i]) {
        return this.symbolTable[i][name]
      }
    }
  }

  private addSymbol(name: string, token: Token, type: Type): void {
    this.symbolTable[this.symbolTable.length - 1][name] = [token, type]
  }

  private addScope(): void {
    this.symbolTable.push({})
  }

  private removeScope(): void {
    this.symbolTable.pop()
  }

  private emitError(error: CompileErrorItem): void {
    this.errors.push(error)
  }

  private assertTokenKind(
    token: Token,
    kind: TokenKind,
    msg: string = ''
  ): void {
    if (msg.length === 0) {
      msg = `expected token '${token.value}' to be ${kind}, but it is a ${token.kind}`
    }
    this.assert(token.kind === kind)
  }

  private assert(v: boolean, msg: string = 'assertion failed'): void {
    if (!v) {
      throw new Error(msg)
    }
  }
}
