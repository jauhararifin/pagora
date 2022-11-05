import {
  ArrayTypeNode,
  CommaSeparatedExpr,
  DeclKind,
  DeclNode,
  ExprKind,
  ExprNode,
  IntegerLitExprNode,
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
    const varToken = this.expectEither([TokenKind.Var])
    if (varToken == null) return null

    const varName = this.expectEither([TokenKind.Identifier])
    if (varName == null) return null

    const colonToken = this.expectEither([TokenKind.Colon])
    if (colonToken == null) return null

    const typeExpr = this.parseTypeExpr()
    if (typeExpr == null) return null

    return {
      kind: DeclKind.VARIABLE,
      var: varToken,
      name: varName,
      colon: colonToken,
      type: typeExpr
    }
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
    const token = this.peek()
    switch (token.kind) {
      case TokenKind.IntegerLiteral:
        return this.parseIntegerLiteral()
      default:
        return null
    }
  }

  private parseIntegerLiteral (): IntegerLitExprNode | null {
    const token = this.expectEither([TokenKind.IntegerLiteral])
    if (token == null) return null

    return {
      kind: ExprKind.INTEGER_LIT,
      value: token
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
