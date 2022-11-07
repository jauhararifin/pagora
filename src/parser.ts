import {
  ArrayTypeNode,
  BlockStatementNode,
  CommaSeparatedExpr,
  DeclKind,
  DeclNode,
  ExprNode,
  ExprNodeKind,
  FunctionDeclNode,
  MainDeclNode,
  ParamGroup,
  ParamsNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
  VarStatementNode,
  VariableDeclNode
} from './ast'
import { Error, ErrorKind, Result } from './errors'
import { PrimitiveTypes, Token, TokenKind } from './tokens'

export function parse (tokens: Token[]): Result<RootNode> {
  return new Parser(tokens).parse()
}

// TODO: improve the recursive descent parser to use non-recursive style to avoid stack overflow.
// TODO: skip the whole process if the number errors are too many
class Parser {
  tokens: Token[]
  index: number
  errors: Error[]

  constructor (tokens: Token[]) {
    this.tokens = tokens
    this.index = 0
    this.errors = []
  }

  parse (): Result<RootNode> {
    const root = this.parseRoot()
    return { value: root, errors: this.errors }
  }

  private parseRoot (): RootNode {
    const declarations: DeclNode[] = []
    while (this.peek().kind !== TokenKind.EOF) {
      const token = this.expectEither([TokenKind.Var, TokenKind.Function, TokenKind.Begin], false)
      if (token == null) continue

      let decl: DeclNode | undefined
      switch (token.kind) {
        case TokenKind.Var:
          decl = this.parseVariableDecl()
          break
        case TokenKind.Function:
          decl = this.parseFunctionDecl()
          break
        case TokenKind.Begin:
          decl = this.parseMainDecl()
          break
      }

      if (decl != null) declarations.push(decl)
    }

    return { declarations }
  }

  private parseVariableDecl (): VariableDeclNode | undefined {
    const stmt = this.parseVarStatement()
    if (stmt === undefined) return undefined

    return { ...stmt, kind: DeclKind.VARIABLE }
  }

  private parseFunctionDecl (): FunctionDeclNode | undefined {
    const functionToken = this.expectEither([TokenKind.Function])
    if (functionToken === undefined) return undefined

    const name = this.expectEither([TokenKind.Identifier])
    if (name === undefined) return undefined

    const openBrac = this.expectEither([TokenKind.OpenBrac])
    if (openBrac === undefined) return undefined

    const params = this.parseParams()
    if (params === undefined) return undefined

    const closeBrac = this.expectEither([TokenKind.CloseBrac])
    if (closeBrac === undefined) return undefined

    const arrow = this.consumeIfMatch([TokenKind.Arrow])
    let returnType: TypeExprNode | undefined
    if (arrow != null) {
      returnType = this.parseTypeExpr()
      if (returnType === undefined) return undefined
    }

    const body = this.parseBlockStatement()
    if (body === undefined) return undefined

    return {
      kind: DeclKind.FUNCTION,
      function: functionToken,
      name,
      openBrac,
      params,
      closeBrac,
      arrow,
      returnType,
      body
    }
  }

  // TODO: figure out if `parseParams` can be abstracted away to reduce code duplication in parse comma separated expr.
  private parseParams (): ParamsNode | undefined {
    const params: ParamGroup[] = []
    const commas: Token[] = []

    const token = this.peek()
    if (token.kind === TokenKind.CloseBrac) {
      return { params, commas }
    }

    while (true) {
      const name = this.expectEither([TokenKind.Identifier])
      if (name === undefined) return undefined

      const colon = this.expectEither([TokenKind.Colon])
      if (colon === undefined) return undefined

      const type = this.parseTypeExpr()
      if (type === undefined) return undefined
      params.push({ name, colon, type })

      const next = this.peek()
      if (next.kind !== TokenKind.Comma) break
    }

    return { params, commas }
  }

  private parseMainDecl (): MainDeclNode | undefined {
    const body = this.parseBlockStatement()
    if (body === undefined) return undefined
    return { kind: DeclKind.MAIN, body }
  }

  private parseStatement (emitError: boolean = false): StatementNode | undefined {
    const token = this.peek()
    switch (token.kind) {
      case TokenKind.Begin:
        return this.parseBlockStatement()
      case TokenKind.If:
        return this.parseIfStatement()
      case TokenKind.While:
        return this.parseWhileStatement()
      case TokenKind.Var:
        return this.parseVarStatement()
      case TokenKind.Return:
        return this.parseReturnStatement()
      case TokenKind.End:
      case TokenKind.Semicolon:
      case TokenKind.PhantomSemicolon:
        return undefined
      default:
        return this.parseAssignStatement()
    }
  }

  private parseBlockStatement (): BlockStatementNode | undefined {
    const begin = this.expectEither([TokenKind.Begin])
    if (begin === undefined) return undefined

    const statements: StatementNode[] = []
    while (true) {
      const stmt = this.parseStatement()
      if (stmt === undefined) break
      statements.push(stmt)
    }

    const end = this.expectEither([TokenKind.End])
    if (end === undefined) return undefined

    return { kind: StatementNodeKind.BLOCK, begin, statements, end }
  }

  private parseIfStatement (): StatementNode | undefined {
    const ifToken = this.expectEither([TokenKind.If])
    if (ifToken === undefined) return undefined

    const condition = this.parseExpr()
    if (condition === undefined) return undefined

    const thenToken = this.expectEither([TokenKind.Then])
    if (thenToken === undefined) return undefined

    const body = this.parseStatement()
    if (body === undefined) return undefined

    const elseToken = this.consumeIfMatch([TokenKind.Else])
    if (elseToken === undefined) {
      return { kind: StatementNodeKind.IF, if: ifToken, condition, then: thenToken, body }
    }

    const elseBody = this.parseStatement()
    if (elseBody === undefined) return undefined

    return { kind: StatementNodeKind.IF, if: ifToken, condition, then: thenToken, body, else: elseBody }
  }

  private parseWhileStatement (): StatementNode | undefined {
    const whileToken = this.expectEither([TokenKind.While])
    if (whileToken === undefined) return undefined

    const condition = this.parseExpr()
    if (condition === undefined) return undefined

    const doToken = this.expectEither([TokenKind.Do])
    if (doToken === undefined) return undefined

    const body = this.parseStatement()
    if (body === undefined) return undefined

    return { kind: StatementNodeKind.WHILE, while: whileToken, condition, do: doToken, body }
  }

  private parseReturnStatement (): StatementNode | undefined {
    const returnToken = this.expectEither([TokenKind.Return])
    if (returnToken === undefined) return undefined

    const value = this.parseExpr()
    if (value === undefined) return undefined

    return { kind: StatementNodeKind.RETURN, return: returnToken, value }
  }

  private parseAssignStatement (): StatementNode | undefined {
    const receiver = this.parseExpr()
    if (receiver === undefined) return undefined

    const assign = this.consumeIfMatch([TokenKind.Assign])
    if (assign === undefined) {
      return { kind: StatementNodeKind.EXPR, expr: receiver }
    }

    const value = this.parseExpr()
    if (value === undefined) return undefined

    this.consumeIfMatch([TokenKind.Semicolon, TokenKind.PhantomSemicolon])

    return { kind: StatementNodeKind.ASSIGN, receiver, assign, value }
  }

  private parseVarStatement (): VarStatementNode | undefined {
    const varToken = this.expectEither([TokenKind.Var])
    if (varToken === undefined) return undefined

    const varName = this.expectEither([TokenKind.Identifier])
    if (varName === undefined) return undefined

    const next = this.expectEither([TokenKind.Colon, TokenKind.Assign])
    if (next === undefined) return undefined

    if (next.kind === TokenKind.Colon) {
      const typeExpr = this.parseTypeExpr()
      if (typeExpr === undefined) return undefined

      const assignToken = this.consumeIfMatch([TokenKind.Assign])
      if (assignToken === undefined) {
        return {
          kind: StatementNodeKind.VAR,
          variable: {
            var: varToken,
            name: varName,
            colon: next,
            type: typeExpr
          }
        }
      } else {
        const valueExpr = this.parseExpr()
        if (valueExpr === undefined) return undefined
        return {
          kind: StatementNodeKind.VAR,
          variable: {
            var: varToken,
            name: varName,
            colon: next,
            type: typeExpr,
            assign: assignToken,
            value: valueExpr
          }
        }
      }
    } else {
      const valueExpr = this.parseExpr()
      if (valueExpr === undefined) return undefined
      return {
        kind: StatementNodeKind.VAR,
        variable: {
          var: varToken,
          name: varName,
          assign: next,
          value: valueExpr
        }
      }
    }
  }

  private parseTypeExpr (): TypeExprNode | undefined {
    const token = this.expectEither([...PrimitiveTypes, TokenKind.Array])
    if (token === undefined) return undefined

    if (PrimitiveTypes.includes(token.kind)) {
      return { kind: TypeExprNodeKind.PRIMITIVE, type: token }
    }

    this.prev()
    return this.parseArrayTypeExpr()
  }

  private parseArrayTypeExpr (): ArrayTypeNode | undefined {
    const arrayToken = this.expectEither([TokenKind.Array])
    if (arrayToken === undefined) return undefined

    const openSquare = this.expectEither([TokenKind.OpenSquare])
    if (openSquare === undefined) return undefined

    const commaSeparatedSize = this.parseCommaSeparatedExpr()
    if (commaSeparatedSize === undefined) return undefined

    const closeSquare = this.expectEither([TokenKind.CloseSquare])
    if (closeSquare === undefined) return undefined

    const ofToken = this.expectEither([TokenKind.Of])
    if (ofToken === undefined) return undefined

    const elementType = this.parseTypeExpr()
    if (elementType === undefined) return undefined

    return {
      kind: TypeExprNodeKind.ARRAY,
      array: arrayToken,
      openSquare,
      dimension: commaSeparatedSize,
      closeSquare,
      of: ofToken,
      type: elementType
    }
  }

  private parseCommaSeparatedExpr (): CommaSeparatedExpr | undefined {
    const exprs: ExprNode[] = []
    const commas: Token[] = []
    while (true) {
      const expr = this.parseExpr()
      if (expr === undefined) {
        break
      }
      exprs.push(expr)

      const commaTok = this.consumeIfMatch([TokenKind.Comma])
      if (commaTok === undefined) {
        break
      }
      commas.push(commaTok)
    }

    return { values: exprs, commas }
  }

  private parseExpr (): ExprNode | undefined {
    return this.parseBinaryExpr(TokenKind.Or)
  }

  private parseBinaryExpr (op: TokenKind): ExprNode | undefined {
    const operatorPrecedences = [
      TokenKind.Or,
      TokenKind.And,
      TokenKind.BitOr,
      TokenKind.BitXor,
      TokenKind.BitAnd,
      TokenKind.Equal,
      TokenKind.NotEqual,
      TokenKind.LessThan,
      TokenKind.LessThanEqual,
      TokenKind.GreaterThan,
      TokenKind.GreaterThanEqual,
      TokenKind.ShiftLeft,
      TokenKind.ShiftRight,
      TokenKind.Plus,
      TokenKind.Minus,
      TokenKind.Multiply,
      TokenKind.Div,
      TokenKind.Mod
    ]

    const i = operatorPrecedences.indexOf(op)
    const aExpr: ExprNode | undefined = i === operatorPrecedences.length - 1
      ? this.parseArrayIndexExp()
      : this.parseBinaryExpr(operatorPrecedences[i + 1])
    if (aExpr === undefined) return undefined

    const opToken = this.consumeIfMatch([op])
    if (opToken === undefined) return aExpr

    const bExpr = this.parseExpr()
    if (bExpr === undefined) return undefined

    return {
      kind: ExprNodeKind.BINARY,
      a: aExpr,
      op: opToken,
      b: bExpr
    }
  }

  private parseArrayIndexExp (): ExprNode | undefined {
    const arraySource = this.parseCastExpression()
    if (arraySource === undefined) return undefined

    const openSquare = this.consumeIfMatch([TokenKind.OpenSquare])
    if (openSquare === undefined) return arraySource

    const index = this.parseCommaSeparatedExpr()
    if (index == null) return undefined

    const closeSquare = this.expectEither([TokenKind.CloseBrac])
    if (closeSquare == null) return undefined

    return { kind: ExprNodeKind.ARRAY_INDEX, array: arraySource, openSquare, index, closeSquare }
  }

  private parseCastExpression (): ExprNode | undefined {
    const source = this.parseUnaryExpr()
    if (source == null) return undefined

    const asToken = this.consumeIfMatch([TokenKind.As])
    if (asToken == null) return source

    const target = this.parseTypeExpr()
    if (target == null) return undefined

    return { kind: ExprNodeKind.CAST, source, as: asToken, target }
  }

  private parseUnaryExpr (): ExprNode | undefined {
    const op = this.consumeIfMatch([
      TokenKind.BitNot,
      TokenKind.Minus,
      TokenKind.Plus,
      TokenKind.Not
    ])
    if (op == null) return this.parseCallExpr()

    const value = this.parseCallExpr()
    if (value == null) return undefined

    return { kind: ExprNodeKind.UNARY, op, value }
  }

  private parseCallExpr (): ExprNode | undefined {
    const callee = this.parsePrimaryExpr()
    if (callee == null) return undefined

    const openBrac = this.consumeIfMatch([TokenKind.OpenBrac])
    if (openBrac == null) return callee

    const args = this.parseCommaSeparatedExpr()
    if (args == null) return undefined

    const closeBrac = this.expectEither([TokenKind.CloseBrac])
    if (closeBrac == null) return undefined

    return { kind: ExprNodeKind.CALL, callee, openBrac, arguments: args, closeBrac }
  }

  private parsePrimaryExpr (): ExprNode | undefined {
    const token = this.peek()
    if (token.kind === TokenKind.OpenBrac) {
      const openBrac = this.next()
      const value = this.parseExpr()
      if (value == null) return undefined
      const closeBrac = this.expectEither([TokenKind.CloseBrac])
      if (closeBrac == null) return undefined
      return { kind: ExprNodeKind.GROUPED, openBrac, value, closeBrac }
    } else if (token.kind === TokenKind.IntegerLiteral) {
      const value = this.next()
      return { kind: ExprNodeKind.INTEGER_LIT, value }
    } else if (token.kind === TokenKind.True || token.kind === TokenKind.False) {
      const value = this.next()
      return { kind: ExprNodeKind.BOOLEAN_LIT, value }
    } else if (token.kind === TokenKind.Identifier) {
      const name = this.next()
      return { kind: ExprNodeKind.IDENT, name }
    } else {
      const token = this.next()
      this.emitError({ kind: ErrorKind.UnexpectedTokenForExpr, found: token })
      return undefined
    }
  }

  private consumeIfMatch (expectedKinds: TokenKind[]): Token | undefined {
    const token = this.peek()
    if (expectedKinds.includes(token.kind)) {
      this.next()
      return token
    }

    return undefined
  }

  private expectEither (expectedKinds: TokenKind[], consume: boolean = true): Token | undefined {
    let token = this.peek()
    if (consume) this.next()

    if (expectedKinds.includes(token.kind)) {
      return token
    }

    this.emitError({ kind: ErrorKind.UnexpectedToken, expected: expectedKinds, found: token })

    // TODO: improve error reporting logic. Research about this more.
    // I think this can be more celever by checking the current the parsing context.
    const stopKind = [TokenKind.PhantomSemicolon, TokenKind.Semicolon, TokenKind.EOF]
    while (!stopKind.includes(token.kind)) {
      token = this.next()
    }

    return undefined
  }

  private next (): Token {
    if (this.index >= this.tokens.length) {
      return {
        kind: TokenKind.EOF,
        position: this.tokens[this.tokens.length - 1].position,
        value: ''
      }
    }
    return this.tokens[this.index++]
  }

  private prev (): void {
    if (this.index === 0) {
      return
    }
    this.index--
  }

  private peek (): Token {
    if (this.index >= this.tokens.length) {
      return {
        kind: TokenKind.EOF,
        position: this.tokens[this.tokens.length - 1].position,
        value: ''
      }
    }
    return this.tokens[this.index]
  }

  private emitError (error: Error): void {
    this.errors.push(error)
  }
}
