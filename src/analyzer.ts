import {
  Argument,
  ArrayType,
  BlockStatement,
  Expr,
  Function,
  FunctionType,
  Program,
  Statement,
  Type,
  TypeKind,
  Variable
} from './semantic'
import {
  BlockStatementNode,
  DeclKind,
  ExprNode,
  FunctionDeclNode,
  RootNode,
  StatementNode,
  TypeExprNode,
  VariableDeclNode
} from './ast'
import { Error as CompileError, ErrorKind, Result } from './errors'
import { Token, TokenKind } from './tokens'

export function analyze (ast: RootNode): Result<Program> {
  return new Analyzer().analyze(ast)
}

class Analyzer {
  globalSymbolNames: { [key: string]: Token }

  functions: Function[]
  globals: Variable[]
  errors: CompileError[]

  currentReturnType: Type | undefined

  constructor () {
    this.globalSymbolNames = {}

    this.functions = []
    this.globals = []
    this.errors = []
  }

  // TODO: skip the whole process if the number errors are too many
  analyze (ast: RootNode): Result<Program> {
    // TODO: improve the language to support struct, tuple and type definition
    // This requires an additional step to load all the type names beforehand.
    // Although, at this phase, we don't need it yet.

    for (const declaration of ast.declarations) {
      switch (declaration.kind) {
        case DeclKind.FUNCTION:
          this.analyzeFunction(declaration)
          break
        case DeclKind.VARIABLE:
          this.analyzeVariable(declaration)
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
    if (name in this.globalSymbolNames) {
      const declaredAt = this.globalSymbolNames[name]
      this.emitError({ kind: ErrorKind.MultipleDeclaration, declaredAt, redeclaredAt: functionDecl.name })

      // TODO: consider keep analyze the function body for better warning message
      return
    }

    const type = this.analyzeFunctionType(functionDecl)
    this.currentReturnType = type

    this.assert(type.arguments.length === functionDecl.params.params.length)
    const args: Argument[] = functionDecl.params.params.map((p, i): Argument => ({
      name: p.name.value,
      type: type.arguments[i]
    }))

    const body = this.analyzeBlockStatement(functionDecl.body)
    this.functions.push({ name, type, arguments: args, body })
  }

  private analyzeVariable (varDecl: VariableDeclNode): void {
    this.assertTokenKind(varDecl.name, TokenKind.Identifier)

    const name = varDecl.name.value
    if (name in this.globalSymbolNames) {
      const declaredAt = this.globalSymbolNames[name]
      this.emitError({ kind: ErrorKind.MultipleDeclaration, declaredAt, redeclaredAt: varDecl.name })
      return
    }

    if (varDecl.type === undefined && varDecl.value === undefined) {
      throw new Error('variable declaration should have type or value expression')
    }

    const value: Expr | undefined = (varDecl.value != null) ? this.analyzeExpr(varDecl.value) : undefined

    const type = varDecl.type != null ? this.analyzeType(varDecl.type) : value?.type
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
  }

  private analyzeStatement (statement: StatementNode): Statement {
    throw new Error('not implemented yet')
  }

  private analyzeBlockStatement (blockStatement: BlockStatementNode): BlockStatement {
    throw new Error('not implemented yet')
  }

  private analyzeType (node: TypeExprNode): Type {
    throw new Error('not implemented yet')
  }

  private analyzeFunctionType (node: FunctionDeclNode): FunctionType {
    const args: Type[] = []
    for (const param of node.params.params) {
      const type = this.analyzeType(param.type)
      args.push(type)
    }

    const returnType = (node.returnType !== undefined) ? this.analyzeType(node.returnType) : undefined

    return { kind: TypeKind.Function, arguments: args, return: returnType }
  }

  private analyzeExpr (node: ExprNode): Expr {
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
