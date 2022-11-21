import {
  ArrayTypeNode,
  BlockStatementNode,
  CommaSeparatedExpr,
  ExprNode,
  ExprNodeKind,
  ParamsNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
  VarStatementNode,
  FunctionNode,
  VarNode,
  MainNode,
  ParamNode,
} from './ast'
import {
  CompileError,
  CompileErrorItem,
  DuplicatedMain,
  UnexpectedToken,
} from './errors'
import { PrimitiveTypes, Token, TokenKind } from './tokens'

export function parse(tokens: Token[]): RootNode {
  return new Parser(tokens).parse()
}

// TODO: improve the recursive descent parser to use non-recursive style to avoid stack overflow.
// TODO: skip the whole process if the number errors are too many
class Parser {
  tokens: Token[]
  index: number
  errors: CompileErrorItem[]

  constructor(tokens: Token[]) {
    this.tokens = tokens.filter((tok) => tok.kind !== TokenKind.COMMENT)
    this.index = 0
    this.errors = []
  }

  parse(): RootNode {
    const root = this.parseRoot()

    if (this.errors.length > 0) {
      throw new CompileError(this.errors)
    }

    return root
  }

  private parseRoot(): RootNode {
    const variables: VarNode[] = []
    const functions: FunctionNode[] = []
    let main: MainNode | undefined

    while (this.peek().kind !== TokenKind.EOF) {
      const token = this.expectEither(
        [TokenKind.VAR, TokenKind.FUNCTION, TokenKind.BEGIN],
        false
      )
      try {
        switch (token.kind) {
          case TokenKind.VAR:
            variables.push(this.parseVarNode())
            break
          case TokenKind.FUNCTION:
            functions.push(this.parseFunctionDecl())
            break
          case TokenKind.BEGIN: {
            const body = this.parseBlockStatement()
            if (main !== undefined) {
              throw new DuplicatedMain(main.body.begin, token)
            }
            main = { body }
            break
          }
          default:
            throw new Error('unreachabe')
        }
      } catch (e) {
        this.errors.push(e as CompileErrorItem)
        if (this.tooManyErrors()) break
      }
    }

    return { variables, functions, main }
  }

  private parseFunctionDecl(): FunctionNode {
    const functionToken = this.expectEither([TokenKind.FUNCTION])
    const name = this.expectEither([TokenKind.IDENTIFIER])
    const openBrac = this.expectEither([TokenKind.OPEN_BRAC])
    const params = this.parseParams()
    const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])

    const arrow = this.consumeIfMatch([TokenKind.ARROW])
    let returnType: TypeExprNode | undefined
    if (arrow != null) {
      returnType = this.parseTypeExpr()
    }

    this.expectEither([TokenKind.SEMICOLON])
    const body = this.parseBlockStatement()

    return {
      function: functionToken,
      name,
      openBrac,
      params,
      closeBrac,
      arrow,
      returnType,
      body,
    }
  }

  // TODO: figure out if `parseParams` can be abstracted away to reduce code duplication in parse comma separated expr.
  private parseParams(): ParamsNode {
    const params: ParamNode[] = []
    const commas: Token[] = []

    const token = this.peek()
    if (token.kind === TokenKind.CLOSE_BRAC) {
      return { params, commas }
    }

    while (true) {
      const name = this.expectEither([TokenKind.IDENTIFIER])
      const colon = this.expectEither([TokenKind.COLON])
      const type = this.parseTypeExpr()

      params.push({ name, colon, type })

      const next = this.peek()
      if (next.kind !== TokenKind.COMMA) break
      else this.next()
    }

    return { params, commas }
  }

  // TODO: fix the way statement parsing is designed. It feels weird that parseStatement returns undefined when
  // found an `END` token. This should be handled by the upper layer instead.
  private parseStatement(): StatementNode | undefined {
    while (true) {
      const token = this.peek()
      if (token.kind !== TokenKind.SEMICOLON) {
        break
      }
      this.next()
    }

    const token = this.peek()
    let result: StatementNode | undefined
    switch (token.kind) {
      case TokenKind.BEGIN:
        result = this.parseBlockStatement()
        break
      case TokenKind.IF:
        result = this.parseIfStatement()
        break
      case TokenKind.WHILE:
        result = this.parseWhileStatement()
        break
      case TokenKind.VAR:
        result = this.parseVarStatement()
        break
      case TokenKind.RETURN:
        result = this.parseReturnStatement()
        break
      case TokenKind.CONTINUE:
      case TokenKind.BREAK:
        result = this.parseKeywordStatement()
        break
      case TokenKind.END:
        result = undefined
        break
      default:
        result = this.parseAssignStatement()
        break
    }

    this.consumeIfMatch([TokenKind.SEMICOLON])
    return result
  }

  private parseBlockStatement(): BlockStatementNode {
    const begin = this.expectEither([TokenKind.BEGIN])
    const statements: StatementNode[] = []
    while (this.hasNext()) {
      try {
        const stmt = this.parseStatement()
        if (stmt === undefined) break
        statements.push(stmt)
      } catch (e) {
        this.errors.push(e as CompileErrorItem)
        if (this.tooManyErrors()) break
      }
    }

    const end = this.expectEither([TokenKind.END])

    return { kind: StatementNodeKind.BLOCK, begin, statements, end }
  }

  private parseIfStatement(): StatementNode {
    const ifToken = this.expectEither([TokenKind.IF])
    const condition = this.parseExpr()
    const thenToken = this.expectEither([TokenKind.THEN])
    const body = this.parseStatement()
    if (body === undefined) {
      throw new UnexpectedToken('statement', this.peek())
    }
    const elseToken = this.consumeIfMatch([TokenKind.ELSE])
    if (elseToken === undefined) {
      return {
        kind: StatementNodeKind.IF,
        if: ifToken,
        condition,
        then: thenToken,
        body,
      }
    }

    const elseBody = this.parseStatement()

    return {
      kind: StatementNodeKind.IF,
      if: ifToken,
      condition,
      then: thenToken,
      body,
      else: elseToken,
      elseBody,
    }
  }

  private parseWhileStatement(): StatementNode {
    const whileToken = this.expectEither([TokenKind.WHILE])
    const condition = this.parseExpr()
    const doToken = this.expectEither([TokenKind.DO])
    const body = this.parseStatement()
    if (body === undefined) {
      throw new UnexpectedToken('statement', this.peek())
    }

    return {
      kind: StatementNodeKind.WHILE,
      while: whileToken,
      condition,
      do: doToken,
      body,
    }
  }

  private parseReturnStatement(): StatementNode {
    const ret = this.expectEither([TokenKind.RETURN])
    if (this.peek().kind === TokenKind.SEMICOLON) {
      return {
        kind: StatementNodeKind.RETURN,
        return: ret,
      }
    }

    return {
      kind: StatementNodeKind.RETURN,
      return: ret,
      value: this.parseExpr(),
    }
  }

  private parseKeywordStatement(): StatementNode {
    return {
      kind: StatementNodeKind.KEYWORD,
      keyword: this.expectEither([TokenKind.CONTINUE, TokenKind.BREAK]),
    }
  }

  private parseAssignStatement(): StatementNode {
    const receiver = this.parseExpr()
    const assign = this.consumeIfMatch([TokenKind.ASSIGN])
    if (assign === undefined) {
      return { kind: StatementNodeKind.EXPR, expr: receiver }
    }

    const value = this.parseExpr()
    this.expectEither([TokenKind.SEMICOLON])
    return { kind: StatementNodeKind.ASSIGN, receiver, assign, value }
  }

  private parseVarStatement(): VarStatementNode {
    return {
      kind: StatementNodeKind.VAR,
      variable: this.parseVarNode(),
    }
  }

  private parseVarNode(): VarNode {
    const varToken = this.expectEither([TokenKind.VAR])
    const varName = this.expectEither([TokenKind.IDENTIFIER])
    const next = this.expectEither([TokenKind.COLON, TokenKind.ASSIGN])
    if (next.kind === TokenKind.COLON) {
      const typeExpr = this.parseTypeExpr()

      const assignToken = this.consumeIfMatch([TokenKind.ASSIGN])
      if (assignToken === undefined) {
        this.expectEither([TokenKind.SEMICOLON])
        return {
          var: varToken,
          name: varName,
          colon: next,
          type: typeExpr,
        }
      } else {
        const valueExpr = this.parseExpr()
        this.expectEither([TokenKind.SEMICOLON])
        return {
          var: varToken,
          name: varName,
          colon: next,
          type: typeExpr,
          assign: assignToken,
          value: valueExpr,
        }
      }
    } else {
      const valueExpr = this.parseExpr()
      this.expectEither([TokenKind.SEMICOLON])
      return {
        var: varToken,
        name: varName,
        assign: next,
        value: valueExpr,
      }
    }
  }

  private parseTypeExpr(): TypeExprNode {
    const token = this.expectEither([...PrimitiveTypes, TokenKind.ARRAY])

    if (PrimitiveTypes.includes(token.kind)) {
      return { kind: TypeExprNodeKind.IDENT, type: token }
    }

    this.prev()
    return this.parseArrayTypeExpr()
  }

  private parseArrayTypeExpr(): ArrayTypeNode {
    const arrayToken = this.expectEither([TokenKind.ARRAY])
    const openSquare = this.expectEither([TokenKind.OPEN_SQUARE])
    const commaSeparatedSize = this.parseCommaSeparatedExpr()
    const closeSquare = this.expectEither([TokenKind.CLOSE_SQUARE])
    const ofToken = this.expectEither([TokenKind.OF])
    const elementType = this.parseTypeExpr()

    return {
      kind: TypeExprNodeKind.ARRAY,
      array: arrayToken,
      openSquare,
      dimension: commaSeparatedSize,
      closeSquare,
      of: ofToken,
      elementType,
    }
  }

  private parseCommaSeparatedExpr(): CommaSeparatedExpr {
    const exprs: ExprNode[] = []
    const commas: Token[] = []
    while (true) {
      const expr = this.parseExpr()
      exprs.push(expr)

      const commaTok = this.consumeIfMatch([TokenKind.COMMA])
      if (commaTok === undefined) {
        break
      }
      commas.push(commaTok)
    }

    return { values: exprs, commas }
  }

  private parseExpr(): ExprNode {
    const result = this.parseBinaryExpr(TokenKind.OR)
    return result
  }

  private parseBinaryExpr(op: TokenKind): ExprNode {
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
      TokenKind.MOD,
    ]

    const i = operatorPrecedences.indexOf(op)
    const aExpr: ExprNode =
      i === operatorPrecedences.length - 1
        ? this.parseArrayIndexExp()
        : this.parseBinaryExpr(operatorPrecedences[i + 1])

    let result: ExprNode = aExpr
    while (true) {
      const opToken = this.consumeIfMatch([op])
      if (opToken === undefined) {
        return result
      }

      const bExpr: ExprNode =
        i === operatorPrecedences.length - 1
          ? this.parseArrayIndexExp()
          : this.parseBinaryExpr(operatorPrecedences[i + 1])

      result = {
        kind: ExprNodeKind.BINARY,
        a: result,
        op: opToken,
        b: bExpr,
      }
    }
  }

  private parseArrayIndexExp(): ExprNode {
    const arraySource = this.parseCastExpression()
    const openSquare = this.consumeIfMatch([TokenKind.OPEN_SQUARE])
    if (openSquare === undefined) return arraySource

    const index = this.parseCommaSeparatedExpr()
    const closeSquare = this.expectEither([TokenKind.CLOSE_SQUARE])

    return {
      kind: ExprNodeKind.ARRAY_INDEX,
      array: arraySource,
      openSquare,
      index,
      closeSquare,
    }
  }

  private parseCastExpression(): ExprNode {
    const source = this.parseUnaryExpr()

    const asToken = this.consumeIfMatch([TokenKind.AS])
    if (asToken == null) return source

    const target = this.parseTypeExpr()

    return { kind: ExprNodeKind.CAST, source, as: asToken, target }
  }

  private parseUnaryExpr(): ExprNode {
    const op = this.consumeIfMatch([
      TokenKind.BIT_NOT,
      TokenKind.MINUS,
      TokenKind.PLUS,
      TokenKind.NOT,
    ])
    if (op == null) return this.parseCallExpr()

    const value = this.parseCallExpr()

    return { kind: ExprNodeKind.UNARY, op, value }
  }

  private parseCallExpr(): ExprNode {
    const callee = this.parsePrimaryExpr()
    const openBrac = this.consumeIfMatch([TokenKind.OPEN_BRAC])
    if (openBrac == null) return callee

    if (this.peek().kind === TokenKind.CLOSE_BRAC) {
      const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])
      return {
        kind: ExprNodeKind.CALL,
        callee,
        openBrac,
        arguments: { values: [], commas: [] },
        closeBrac,
      }
    }

    const args = this.parseCommaSeparatedExpr()
    const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])

    return {
      kind: ExprNodeKind.CALL,
      callee,
      openBrac,
      arguments: args,
      closeBrac,
    }
  }

  private parsePrimaryExpr(): ExprNode {
    const token = this.peek()
    if (token.kind === TokenKind.OPEN_BRAC) {
      const openBrac = this.next()
      const value = this.parseExpr()
      const closeBrac = this.expectEither([TokenKind.CLOSE_BRAC])
      return { kind: ExprNodeKind.GROUPED, openBrac, value, closeBrac }
    } else if (token.kind === TokenKind.OPEN_SQUARE) {
      const openSquare = this.next()
      const value = this.parseCommaSeparatedExpr()
      const closeSquare = this.expectEither([TokenKind.CLOSE_SQUARE])
      return { kind: ExprNodeKind.ARRAY_LIT, openSquare, value, closeSquare }
    } else if (token.kind === TokenKind.INTEGER_LITERAL) {
      const value = this.next()
      return { kind: ExprNodeKind.INTEGER_LIT, value }
    } else if (
      token.kind === TokenKind.TRUE ||
      token.kind === TokenKind.FALSE
    ) {
      const value = this.next()
      return { kind: ExprNodeKind.BOOLEAN_LIT, value }
    } else if (token.kind === TokenKind.IDENTIFIER) {
      const name = this.next()
      return { kind: ExprNodeKind.IDENT, name }
    } else if (token.kind === TokenKind.STRING_LITERAL) {
      const value = this.next()
      return { kind: ExprNodeKind.STRING_LIT, value }
    } else {
      const token = this.next()
      throw new UnexpectedToken('Expression', token)
    }
  }

  private consumeIfMatch(expectedKinds: TokenKind[]): Token | undefined {
    const token = this.peek()
    if (expectedKinds.includes(token.kind)) {
      this.next()
      return token
    }

    return undefined
  }

  private expectEither(
    expectedKinds: TokenKind[],
    consume: boolean = true
  ): Token {
    let token = this.peek()
    if (consume) this.next()

    if (expectedKinds.includes(token.kind)) {
      return token
    }
    const got = token

    // TODO: improve error reporting logic. Research about this more.
    // I think this can be more celever by checking the current the parsing context.
    const stopKind = [TokenKind.SEMICOLON, TokenKind.EOF]
    while (!stopKind.includes(token.kind)) {
      token = this.next()
    }

    throw new UnexpectedToken(expectedKinds, got)
  }

  private next(): Token {
    if (this.index >= this.tokens.length) {
      return new Token(
        TokenKind.EOF,
        '',
        this.tokens[this.tokens.length - 1].position
      )
    }
    return this.tokens[this.index++]
  }

  private prev(): void {
    if (this.index === 0) {
      return
    }
    this.index--
  }

  private peek(): Token {
    if (this.index >= this.tokens.length) {
      return new Token(
        TokenKind.EOF,
        '',
        this.tokens[this.tokens.length - 1].position
      )
    }
    return this.tokens[this.index]
  }

  private hasNext(): boolean {
    return this.peek().kind !== TokenKind.EOF
  }

  private tooManyErrors(): boolean {
    return this.errors.length > 15
  }
}
