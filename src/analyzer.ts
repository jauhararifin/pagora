import {
  Argument,
  ArrayType,
  AssignStatement,
  BlockStatement,
  Boolean,
  Char,
  Expr,
  ExprStatement,
  Function,
  FunctionType,
  IfStatement,
  Integer,
  Program,
  Real,
  ReturnStatement,
  Statement,
  StatementKind,
  Type,
  TypeKind,
  VarStatement,
  Variable,
  Void,
  WhileStatement
} from './semantic'
import {
  ArrayTypeNode,
  AssignStatementNode,
  BlockStatementNode,
  DeclKind,
  ExprNode,
  ExprStatementNode,
  FunctionDeclNode,
  IfStatementNode,
  ReturnStatementNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
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
  currentReturnType: Type = { kind: TypeKind.Void }

  // TODO: skip the whole process if the number errors are too many
  analyze (ast: RootNode): Result<Program> {
    // TODO: improve the language to support struct, tuple and type definition
    // This requires an additional step to load all the type names beforehand.
    // Although, at this phase, we don't need it yet.
    this.symbolTable = [{}]

    for (const declaration of ast.declarations) {
      switch (declaration.kind) {
        case DeclKind.FUNCTION:
          this.analyzeFunction(declaration)
          break
        case DeclKind.VARIABLE:
          this.analyzeVariable(declaration.variable)
          break
        case DeclKind.MAIN:
          this.analyzeBlockStatement(declaration.body)
      }
    }

    return {
      value: { functions: this.functions, globals: this.globals },
      errors: this.errors
    }
  }

  private analyzeFunction (functionDecl: FunctionDeclNode): void {
    this.assertTokenKind(functionDecl.name, TokenKind.Identifier)

    const name = functionDecl.name.value

    const symbolTable = this.getCurrentSymbolTable()
    if (name in symbolTable) {
      const [declaredAt] = symbolTable[name]
      this.emitError({ kind: ErrorKind.MultipleDeclaration, declaredAt, redeclaredAt: functionDecl.name })

      // TODO: consider keep analyze the function body for better warning message
      return
    }

    const type = this.analyzeFunctionType(functionDecl)
    if (type === undefined) return

    this.currentReturnType = type

    this.assert(type.arguments.length === functionDecl.params.params.length)
    const args: Argument[] = functionDecl.params.params.map((p, i): Argument => ({
      name: p.name.value,
      type: type.arguments[i]
    }))

    const body = this.analyzeBlockStatement(functionDecl.body)

    this.functions.push({ name, type, arguments: args, body })
    symbolTable[name] = [functionDecl.name, type]
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
      kind: StatementKind.Block,
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
      kind: StatementKind.Var,
      variable
    }
  }

  private analyzeVariable (variable: VarNode): Variable | undefined {
    this.assertTokenKind(variable.name, TokenKind.Identifier)

    const name = variable.name.value
    const symbolTable = this.getCurrentSymbolTable()
    if (name in symbolTable) {
      const [declaredAt] = symbolTable[name]
      this.emitError({ kind: ErrorKind.MultipleDeclaration, declaredAt, redeclaredAt: variable.name })
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
        this.emitError({ kind: ErrorKind.TypeMismatch, source: value.type, target: type })
        return
      }

      if (!value.isConstexpr) {
        this.emitError({ kind: ErrorKind.NotAConstant, value })
        return
      }
    }

    this.globals.push({ name, type, value })
    symbolTable[name] = [variable.name, type]
  }

  private analyzeAssignStatement (stmt: AssignStatementNode): AssignStatement | undefined {
    const receiver = this.analyzeExpr(stmt.receiver)
    if (receiver === undefined) return

    if (!receiver.isAssignable) {
      this.emitError({ kind: ErrorKind.CannotAssign, expr: stmt.receiver, receiver: receiver.type })
      return
    }

    const value = this.analyzeExpr(stmt.value)
    if (value === undefined) return

    if (!this.valueIsA(value.type, receiver.type)) {
      this.emitError({ kind: ErrorKind.TypeMismatch, target: receiver.type, source: value.type })
      return
    }

    return {
      kind: StatementKind.Assign,
      target: receiver,
      value
    }
  }

  private analyzeExprStatement (stmt: ExprStatementNode): ExprStatement | undefined {
    const value = this.analyzeExpr(stmt.expr)
    if (value === undefined) return

    return { kind: StatementKind.Expr, value }
  }

  private analyzeIfStatement (stmt: IfStatementNode): IfStatement | undefined {
    const condition = this.analyzeExpr(stmt.condition)
    if (condition === undefined) return

    if (!this.valueIsA(condition.type, Boolean)) {
      this.emitError({ kind: ErrorKind.TypeMismatch, target: Boolean, source: condition.type })
      return
    }

    const body = this.analyzeStatement(stmt.body)
    if (body === undefined) return

    const elseStmt = (stmt.else != null) ? this.analyzeStatement(stmt.else) : undefined

    return {
      kind: StatementKind.If,
      condition,
      body,
      else: elseStmt
    }
  }

  private analyzeWhileStatement (stmt: WhileStatementNode): WhileStatement | undefined {
    const condition = this.analyzeExpr(stmt.condition)
    if (condition === undefined) return

    const boolType: Type = { kind: TypeKind.Boolean }
    if (!this.valueIsA(condition.type, boolType)) {
      this.emitError({ kind: ErrorKind.TypeMismatch, target: boolType, source: condition.type })
      return
    }

    const body = this.analyzeStatement(stmt.body)
    if (body === undefined) return

    return {
      kind: StatementKind.While,
      condition,
      body
    }
  }

  private analyzeReturnStatement (stmt: ReturnStatementNode): ReturnStatement | undefined {
    if (stmt.value != null) {
      const value = this.analyzeExpr(stmt.value)
      if (value === undefined) return
      if (!this.valueIsA(value.type, this.currentReturnType)) {
        this.emitError({ kind: ErrorKind.TypeMismatch, source: value.type, target: this.currentReturnType })
        return
      }
      return { kind: StatementKind.Return, value }
    } else {
      if (this.currentReturnType.kind !== TypeKind.Void) {
        this.emitError({ kind: ErrorKind.TypeMismatch, source: Void, target: this.currentReturnType })
        return
      }
      return { kind: StatementKind.Return }
    }
  }

  private analyzeType (node: TypeExprNode): Type | undefined {
    switch (node.kind) {
      case TypeExprNodeKind.PRIMITIVE:
        switch (node.type.kind) {
          case TokenKind.Integer:
            return Integer
          case TokenKind.Boolean:
            return Boolean
          case TokenKind.Char:
            return Char
          case TokenKind.Real:
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
        this.emitError({ kind: ErrorKind.NotAConstant, value: dim })
        return
      }
      if (!this.valueIsA(dim.type, Integer)) {
        this.emitError({ kind: ErrorKind.TypeMismatch, source: dim.type, target: Integer })
        return
      }

      dimensionNum.push(dim.constValue as BigInt)
    }

    const elementType = this.analyzeType(node.type)
    if (elementType === undefined) return

    return { kind: TypeKind.Array, dimension: dimensionNum, type: elementType }
  }

  private analyzeFunctionType (node: FunctionDeclNode): FunctionType | undefined {
    const args: Type[] = []
    for (const param of node.params.params) {
      const type = this.analyzeType(param.type)
      if (type === undefined) return
      args.push(type)
    }

    const voidType: Type = { kind: TypeKind.Void }
    const returnType = (node.returnType !== undefined) ? this.analyzeType(node.returnType) : voidType

    return { kind: TypeKind.Function, arguments: args, return: returnType }
  }

  private analyzeExpr (node: ExprNode): Expr | undefined {
    throw new Error('not implemented yet')
  }

  private valueIsA (value: Type, target: Type): boolean {
    if (value.kind !== target.kind) return false

    if (value.kind === TypeKind.Array) {
      const targetType = target as ArrayType
      return value.dimension === targetType.dimension && this.valueIsA(value.type, targetType.type)
    }

    if (value.kind === TypeKind.Function) {
      const targetType = target as FunctionType
      return value.return === targetType.return &&
      value.arguments.every((t, i) => this.valueIsA(t, targetType.arguments[i]))
    }

    throw new Error('unreachable')
  }

  private getCurrentSymbolTable (): { [name: string]: [Token, Type] } {
    return this.symbolTable[this.symbolTable.length - 1]
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
