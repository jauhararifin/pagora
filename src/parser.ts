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
      const token = this.expectEither([TokenKind.VAR, TokenKind.FUNCTION, TokenKind.BEGIN], false)
      if (token == null) {
        this.next()
        continue
      }

      let decl: DeclNode | undefined
      switch (token.kind) {
        case TokenKind.VAR:
          decl = this.parseVariableDecl()
          break
        case TokenKind.FUNCTION:
          decl = this.parseFunctionDecl()
          break
        case TokenKind.BEGIN:
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
    const functionToken = this.expectEither([TokenKind.FUNCTION])
    if (functionToken === undefined) return undefined

    const name = this.expectEither([TokenKind.IDENTIFIER])
    if (name === undefined) return undefined

    const openBrac = this.expectEither([TokenKind.OPEN_BRAC])
    if (openBrac === undefined) return undefined

    const params = this.parseParams()
    if (params === undefined) return undefined

    const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])
    if (closeBrac === undefined) return undefined

    const arrow = this.consumeIfMatch([TokenKind.ARROW])
    let returnType: TypeExprNode | undefined
    if (arrow != null) {
      returnType = this.parseTypeExpr()
      if (returnType === undefined) return undefined
    }

    this.consumeIfMatch([TokenKind.PHANTOM_SEMICOLON])

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
    if (token.kind === TokenKind.CLOSE_BRAC) {
      return { params, commas }
    }

    while (true) {
      const name = this.expectEither([TokenKind.IDENTIFIER])
      if (name === undefined) return undefined

      const colon = this.expectEither([TokenKind.COLON])
      if (colon === undefined) return undefined

      const type = this.parseTypeExpr()
      if (type === undefined) return undefined
      params.push({ name, colon, type })

      const next = this.peek()
      if (next.kind !== TokenKind.COMMA) break
      else this.next()
    }

    return { params, commas }
  }

  private parseMainDecl (): MainDeclNode | undefined {
    const body = this.parseBlockStatement()
    if (body === undefined) return undefined
    return { kind: DeclKind.MAIN, body }
  }

  private parseStatement (): StatementNode | undefined {
    while (true) {
      const token = this.peek()
      switch (token.kind) {
        case TokenKind.BEGIN:
          return this.parseBlockStatement()
        case TokenKind.IF:
          return this.parseIfStatement()
        case TokenKind.WHILE:
          return this.parseWhileStatement()
        case TokenKind.VAR:
          return this.parseVarStatement()
        case TokenKind.RETURN:
          return this.parseReturnStatement()
        case TokenKind.END:
          return undefined
        case TokenKind.SEMICOLON:
        case TokenKind.PHANTOM_SEMICOLON:
          this.next()
          break
        default:
          return this.parseAssignStatement()
      }
    }
  }

  private parseBlockStatement (): BlockStatementNode | undefined {
    const begin = this.expectEither([TokenKind.BEGIN])
    if (begin === undefined) return undefined

    const statements: StatementNode[] = []
    while (true) {
      const stmt = this.parseStatement()
      if (stmt === undefined) break
      statements.push(stmt)
    }

    const end = this.expectEither([TokenKind.END])
    if (end === undefined) return undefined

    return { kind: StatementNodeKind.BLOCK, begin, statements, end }
  }

  private parseIfStatement (): StatementNode | undefined {
    const ifToken = this.expectEither([TokenKind.IF])
    if (ifToken === undefined) return undefined

    const condition = this.parseExpr()
    if (condition === undefined) return undefined

    const thenToken = this.expectEither([TokenKind.THEN])
    if (thenToken === undefined) return undefined

    const body = this.parseStatement()
    if (body === undefined) return undefined

    const elseToken = this.consumeIfMatch([TokenKind.ELSE])
    if (elseToken === undefined) {
      return { kind: StatementNodeKind.IF, if: ifToken, condition, then: thenToken, body }
    }

    const elseBody = this.parseStatement()
    if (elseBody === undefined) return undefined

    return { kind: StatementNodeKind.IF, if: ifToken, condition, then: thenToken, body, else: elseBody }
  }

  private parseWhileStatement (): StatementNode | undefined {
    const whileToken = this.expectEither([TokenKind.WHILE])
    if (whileToken === undefined) return undefined

    const condition = this.parseExpr()
    if (condition === undefined) return undefined

    const doToken = this.expectEither([TokenKind.DO])
    if (doToken === undefined) return undefined

    const body = this.parseStatement()
    if (body === undefined) return undefined

    return { kind: StatementNodeKind.WHILE, while: whileToken, condition, do: doToken, body }
  }

  private parseReturnStatement (): StatementNode | undefined {
    const returnToken = this.expectEither([TokenKind.RETURN])
    if (returnToken === undefined) return undefined

    const value = this.parseExpr()
    if (value === undefined) return undefined

    return { kind: StatementNodeKind.RETURN, return: returnToken, value }
  }

  private parseAssignStatement (): StatementNode | undefined {
    const receiver = this.parseExpr()
    if (receiver === undefined) return undefined

    const assign = this.consumeIfMatch([TokenKind.ASSIGN])
    if (assign === undefined) {
      return { kind: StatementNodeKind.EXPR, expr: receiver }
    }

    const value = this.parseExpr()
    if (value === undefined) return undefined

    this.consumeIfMatch([TokenKind.SEMICOLON, TokenKind.PHANTOM_SEMICOLON])

    return { kind: StatementNodeKind.ASSIGN, receiver, assign, value }
  }

  private parseVarStatement (): VarStatementNode | undefined {
    const varToken = this.expectEither([TokenKind.VAR])
    if (varToken === undefined) return undefined

    const varName = this.expectEither([TokenKind.IDENTIFIER])
    if (varName === undefined) return undefined

    const next = this.expectEither([TokenKind.COLON, TokenKind.ASSIGN])
    if (next === undefined) return undefined

    if (next.kind === TokenKind.COLON) {
      const typeExpr = this.parseTypeExpr()
      if (typeExpr === undefined) return undefined

      const assignToken = this.consumeIfMatch([TokenKind.ASSIGN])
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
    const token = this.expectEither([...PrimitiveTypes, TokenKind.ARRAY])
    if (token === undefined) return undefined

    if (PrimitiveTypes.includes(token.kind)) {
      return { kind: TypeExprNodeKind.PRIMITIVE, type: token }
    }

    this.prev()
    return this.parseArrayTypeExpr()
  }

  private parseArrayTypeExpr (): ArrayTypeNode | undefined {
    const arrayToken = this.expectEither([TokenKind.ARRAY])
    if (arrayToken === undefined) return undefined

    const openSquare = this.expectEither([TokenKind.OPEN_SQUARE])
    if (openSquare === undefined) return undefined

    const commaSeparatedSize = this.parseCommaSeparatedExpr()
    if (commaSeparatedSize === undefined) return undefined

    const closeSquare = this.expectEither([TokenKind.CLOSE_SQUARE])
    if (closeSquare === undefined) return undefined

    const ofToken = this.expectEither([TokenKind.OF])
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

      const commaTok = this.consumeIfMatch([TokenKind.COMMA])
      if (commaTok === undefined) {
        break
      }
      commas.push(commaTok)
    }

    return { values: exprs, commas }
  }

  private parseExpr (): ExprNode | undefined {
    return this.parseBinaryExpr(TokenKind.OR)
  }

  private parseBinaryExpr (op: TokenKind): ExprNode | undefined {
    const operatorPrecedences = [
      TokenKind.OR,
      TokenKind.AND,
      TokenKind.BIT_OR,
      TokenKind.BIT_XOR,
      TokenKind.BIT_AND,
      TokenKind.EQUAL,
      TokenKind.NOT_EQUAL,
      TokenKind.LESS_THAN,
      TokenKind.LESS_THAN_EQUAL,
      TokenKind.GREATER_THAN,
      TokenKind.GREATER_THAN_EQUAL,
      TokenKind.SHIFT_LEFT,
      TokenKind.SHIFT_RIGHT,
      TokenKind.PLUS,
      TokenKind.MINUS,
      TokenKind.MULTIPLY,
      TokenKind.DIV,
      TokenKind.MOD
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

    const openSquare = this.consumeIfMatch([TokenKind.OPEN_SQUARE])
    if (openSquare === undefined) return arraySource

    const index = this.parseCommaSeparatedExpr()
    if (index == null) return undefined

    const closeSquare = this.expectEither([TokenKind.CLOSE_BRAC])
    if (closeSquare == null) return undefined

    return { kind: ExprNodeKind.ARRAY_INDEX, array: arraySource, openSquare, index, closeSquare }
  }

  private parseCastExpression (): ExprNode | undefined {
    const source = this.parseUnaryExpr()
    if (source == null) return undefined

    const asToken = this.consumeIfMatch([TokenKind.AS])
    if (asToken == null) return source

    const target = this.parseTypeExpr()
    if (target == null) return undefined

    return { kind: ExprNodeKind.CAST, source, as: asToken, target }
  }

  private parseUnaryExpr (): ExprNode | undefined {
    const op = this.consumeIfMatch([
      TokenKind.BIT_NOT,
      TokenKind.MINUS,
      TokenKind.PLUS,
      TokenKind.NOT
    ])
    if (op == null) return this.parseCallExpr()

    const value = this.parseCallExpr()
    if (value == null) return undefined

    return { kind: ExprNodeKind.UNARY, op, value }
  }

  private parseCallExpr (): ExprNode | undefined {
    const callee = this.parsePrimaryExpr()
    if (callee == null) return undefined

    const openBrac = this.consumeIfMatch([TokenKind.OPEN_BRAC])
    if (openBrac == null) return callee

    const args = this.parseCommaSeparatedExpr()
    if (args == null) return undefined

    const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])
    if (closeBrac == null) return undefined

    return { kind: ExprNodeKind.CALL, callee, openBrac, arguments: args, closeBrac }
  }

  private parsePrimaryExpr (): ExprNode | undefined {
    const token = this.peek()
    if (token.kind === TokenKind.OPEN_BRAC) {
      const openBrac = this.next()
      const value = this.parseExpr()
      if (value == null) return undefined
      const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])
      if (closeBrac == null) return undefined
      return { kind: ExprNodeKind.GROUPED, openBrac, value, closeBrac }
    } else if (token.kind === TokenKind.INTEGER_LITERAL) {
      const value = this.next()
      return { kind: ExprNodeKind.INTEGER_LIT, value }
    } else if (token.kind === TokenKind.TRUE || token.kind === TokenKind.FALSE) {
      const value = this.next()
      return { kind: ExprNodeKind.BOOLEAN_LIT, value }
    } else if (token.kind === TokenKind.IDENTIFIER) {
      const name = this.next()
      return { kind: ExprNodeKind.IDENT, name }
    } else {
      const token = this.next()
      this.emitError({ kind: ErrorKind.UNEXPECTED_TOKEN_FOR_EXPR, found: token })
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

    this.emitError({ kind: ErrorKind.UNEXPECTED_TOKEN, expected: expectedKinds, found: token })

    // TODO: improve error reporting logic. Research about this more.
    // I think this can be more celever by checking the current the parsing context.
    const stopKind = [TokenKind.PHANTOM_SEMICOLON, TokenKind.SEMICOLON, TokenKind.EOF]
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
