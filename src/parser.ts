import {
  ArrayTypeNode,
  CommaSeparatedExpr,
  DeclKind,
  DeclNode,
  ExprKind,
  ExprNode,
  RootNode,
  StatementKind,
  TypeExprNode,
  TypeKind,
  VariableDeclNode,
  VarStatementNode
} from './ast'
import { PrimitiveTypes, Token, TokenKind } from './tokens'
import { Result, Error, ErrorKind } from './errors'

export function parse (tokens: Token[]): Result<RootNode> {
  return new Parser(tokens).parse()
}

// TODO: improve the recursive descent parser to use non-recursive style to avoid stack overflow.
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

      let decl: DeclNode | null = null
      switch (token.kind) {
        case TokenKind.Var:
          decl = this.parseVariableDecl()
          break
        case TokenKind.Function:
          break
        case TokenKind.Begin:
          break
      }

      if (decl != null) declarations.push(decl)
    }

    return { declarations }
  }

  private parseVariableDecl (): VariableDeclNode | null {
    const stmt = this.parseVariable()
    if (stmt == null) return null

    return { ...stmt, kind: DeclKind.VARIABLE }
  }

  private parseVariable (): VarStatementNode | null {
    const varToken = this.expectEither([TokenKind.Var])
    if (varToken == null) return null

    const varName = this.expectEither([TokenKind.Identifier])
    if (varName == null) return null

    const next = this.expectEither([TokenKind.Colon, TokenKind.Assign])
    if (next == null) return null

    if (next.kind === TokenKind.Colon) {
      const typeExpr = this.parseTypeExpr()
      if (typeExpr == null) return null

      const assignToken = this.consumeIfMatch([TokenKind.Assign])
      if (assignToken == null) {
        return {
          kind: StatementKind.VAR,
          var: varToken,
          name: varName,
          colon: next,
          type: typeExpr
        }
      } else {
        const valueExpr = this.parseExpr()
        if (valueExpr == null) return null
        return {
          kind: StatementKind.VAR,
          var: varToken,
          name: varName,
          colon: next,
          type: typeExpr,
          assign: assignToken,
          value: valueExpr
        }
      }
    } else {
      const valueExpr = this.parseExpr()
      if (valueExpr == null) return null
      return {
        kind: StatementKind.VAR,
        var: varToken,
        name: varName,
        assign: next,
        value: valueExpr
      }
    }
  }

  private parseTypeExpr (): TypeExprNode | null {
    const token = this.expectEither([...PrimitiveTypes, TokenKind.Array])
    if (token == null) return null

    if (PrimitiveTypes.includes(token.kind)) {
      return { kind: TypeKind.PRIMITIVE, type: token }
    }

    this.prev()
    return this.parseArrayTypeExpr()
  }

  private parseArrayTypeExpr (): ArrayTypeNode | null {
    const arrayToken = this.expectEither([TokenKind.Array])
    if (arrayToken == null) return null

    const openSquare = this.expectEither([TokenKind.OpenSquare])
    if (openSquare == null) return null

    const commaSeparatedSize = this.parseCommaSeparatedExpr()
    if (commaSeparatedSize == null) return null

    const closeSquare = this.expectEither([TokenKind.CloseSquare])
    if (closeSquare == null) return null

    const ofToken = this.expectEither([TokenKind.Of])
    if (ofToken == null) return null

    const elementType = this.parseTypeExpr()
    if (elementType == null) return null

    return {
      kind: TypeKind.ARRAY,
      array: arrayToken,
      openSquare,
      dimension: commaSeparatedSize,
      closeSquare,
      of: ofToken,
      type: elementType
    }
  }

  private parseCommaSeparatedExpr (): CommaSeparatedExpr | null {
    const exprs: ExprNode[] = []
    const commas: Token[] = []
    while (true) {
      const expr = this.parseExpr()
      if (expr == null) {
        break
      }
      exprs.push(expr)

      const commaTok = this.consumeIfMatch([TokenKind.Comma])
      if (commaTok == null) {
        break
      }
      commas.push(commaTok)
    }

    return { values: exprs, commas }
  }

  private parseExpr (): ExprNode | null {
    return this.parseBinaryExpr(TokenKind.Or)
  }

  private parseBinaryExpr (op: TokenKind): ExprNode | null {
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
    const aExpr: ExprNode | null = i === operatorPrecedences.length - 1
      ? this.parseArrayIndexExp()
      : this.parseBinaryExpr(operatorPrecedences[i + 1])
    if (aExpr == null) return null

    const opToken = this.consumeIfMatch([op])
    if (opToken == null) return aExpr

    const bExpr = this.parseExpr()
    if (bExpr == null) return null

    return {
      kind: ExprKind.BINARY,
      a: aExpr,
      op: opToken,
      b: bExpr
    }
  }

  private parseArrayIndexExp (): ExprNode | null {
    const arraySource = this.parseCastExpression()
    if (arraySource == null) return null

    const openSquare = this.consumeIfMatch([TokenKind.OpenSquare])
    if (openSquare == null) return arraySource

    const index = this.parseCommaSeparatedExpr()
    if (index == null) return null

    const closeSquare = this.expectEither([TokenKind.CloseBrac])
    if (closeSquare == null) return null

    return { kind: ExprKind.ARRAY_INDEX, array: arraySource, openSquare, index, closeSquare }
  }

  private parseCastExpression (): ExprNode | null {
    const source = this.parseUnaryExpr()
    if (source == null) return null

    const asToken = this.consumeIfMatch([TokenKind.As])
    if (asToken == null) return source

    const target = this.parseTypeExpr()
    if (target == null) return null

    return { kind: ExprKind.CAST, source, as: asToken, target }
  }

  private parseUnaryExpr (): ExprNode | null {
    const op = this.consumeIfMatch([
      TokenKind.BitNot,
      TokenKind.Minus,
      TokenKind.Plus,
      TokenKind.Not
    ])
    if (op == null) return this.parseCallExpr()

    const value = this.parseCallExpr()
    if (value == null) return null

    return { kind: ExprKind.UNARY, op, value }
  }

  private parseCallExpr (): ExprNode | null {
    const callee = this.parsePrimaryExpr()
    if (callee == null) return null

    const openBrac = this.consumeIfMatch([TokenKind.OpenBrac])
    if (openBrac == null) return callee

    const args = this.parseCommaSeparatedExpr()
    if (args == null) return null

    const closeBrac = this.expectEither([TokenKind.CloseBrac])
    if (closeBrac == null) return null

    return { kind: ExprKind.CALL, callee, openBrac, arguments: args, closeBrac }
  }

  private parsePrimaryExpr (): ExprNode | null {
    const token = this.peek()
    if (token.kind === TokenKind.OpenBrac) {
      const openBrac = this.next()
      const value = this.parseExpr()
      if (value == null) return null
      const closeBrac = this.expectEither([TokenKind.CloseBrac])
      if (closeBrac == null) return null
      return { kind: ExprKind.GROUPED, openBrac, value, closeBrac }
    } else if (token.kind === TokenKind.IntegerLiteral) {
      const value = this.next()
      return { kind: ExprKind.INTEGER_LIT, value }
    } else if (token.kind === TokenKind.True || token.kind === TokenKind.False) {
      const value = this.next()
      return { kind: ExprKind.BOOLEAN_LIT, value }
    } else if (token.kind === TokenKind.Identifier) {
      const name = this.next()
      return { kind: ExprKind.IDENT, name }
    } else {
      this.emitError({ kind: ErrorKind.UnexpectedExpr, found: token })
      return null
    }
  }

  private consumeIfMatch (expectedKinds: TokenKind[]): Token | null {
    const token = this.peek()
    if (expectedKinds.includes(token.kind)) {
      this.next()
      return token
    }

    return null
  }

  private expectEither (expectedKinds: TokenKind[], consume: boolean = true): Token | null {
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

    return null
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
