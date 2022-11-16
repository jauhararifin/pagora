import { CompileError, CompileErrorItem, UnexpectedCharacter } from './errors'
import { Position, Token, TokenKind } from './tokens'

export function tokenize(sourceCode: string): Token[] {
  return new Lexer(sourceCode).scan()
}

class CharPos {
  c: string
  pos: Position

  constructor(c: string, pos: Position) {
    this.c = c
    this.pos = pos
  }

  isEnd(): boolean {
    return this.c === ''
  }
}

class Lexer {
  private readonly sourceCode: CharPos[]
  private index: number
  private readonly tokens: Token[]
  private readonly errors: CompileErrorItem[]

  constructor(sourceCode: string) {
    this.sourceCode = []
    this.index = 0
    this.tokens = []
    this.errors = []

    let line = 1
    let col = 0
    const codes = Array.from(sourceCode)
    for (const c of codes) {
      col++
      this.sourceCode.push(new CharPos(c, { line, col }))

      if (c === '\n') {
        line++
        col = 0
      }
    }
  }

  // TODO: skip the whole process if the number errors are too many
  scan(): Token[] {
    while (true) {
      this.advance()
      const c = this.peek()
      if (c.isEnd()) {
        break
      }

      if (
        c.c === '_' ||
        (c.c >= 'a' && c.c <= 'z') ||
        (c.c >= 'A' && c.c <= 'A')
      ) {
        this.scanWord()
      } else if (c.c === '"' || c.c === '`') {
        this.scanString()
      } else if (c.c >= '0' && c.c <= '9') {
        this.scanNumberLiteral()
      } else if ("'!%&|^~(){}[]*+-:<>,=;".includes(c.c)) {
        this.scanSymbol()
      } else if (c.c === '/') {
        this.next()
        const next = this.peek()
        if (next.c === '/') {
          this.back()
          this.scanComment(c.pos)
        } else {
          this.emitToken(new Token(TokenKind.DIV, '/', c.pos))
        }
      } else {
        this.emitError(new UnexpectedCharacter(c.c, c.pos))
        this.next()
      }
    }

    if (this.errors.length > 0) {
      throw new CompileError(this.errors)
    } else {
      return this.tokens
    }
  }

  private scanWord(): void {
    const position = this.peek().pos

    const word = this.consumeWhile(
      (c) =>
        c === '_' ||
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9')
    )

    const map: { [key: string]: TokenKind } = {
      var: TokenKind.VAR,
      type: TokenKind.TYPE,
      struct: TokenKind.STRUCT,
      as: TokenKind.AS,
      function: TokenKind.FUNCTION,
      begin: TokenKind.BEGIN,
      end: TokenKind.END,
      array: TokenKind.ARRAY,
      of: TokenKind.OF,
      integer: TokenKind.INTEGER,
      boolean: TokenKind.BOOLEAN,
      byte: TokenKind.BYTE,
      real: TokenKind.REAL,
      and: TokenKind.AND,
      not: TokenKind.NOT,
      or: TokenKind.OR,
      if: TokenKind.IF,
      then: TokenKind.THEN,
      else: TokenKind.ELSE,
      while: TokenKind.WHILE,
      do: TokenKind.DO,
      continue: TokenKind.CONTINUE,
      break: TokenKind.BREAK,
      return: TokenKind.RETURN,
    }

    const kind = word in map ? map[word] : TokenKind.IDENTIFIER
    this.emitToken(new Token(kind, word, position))
  }

  private scanString(): void {
    const c = this.peek()
    const position = c.pos
    const openingQuote = c.c

    const backslashes: { [key: string]: string } = {
      n: '\n',
      r: '\r',
      t: '\t',
      '\\': '\\',
      0: '\0',
      '"': '"',
      "'": "'",
      '`': '`',
    }

    let afterBackslash = false
    let value = ''

    this.next()
    while (true) {
      const c = this.peek()
      this.next()
      if (c.c === '\n') {
        // TODO: improve the error message by differentiating "unexpected character" error for newline and
        // general error.
        this.emitError(new UnexpectedCharacter(c.c, c.pos))
      }

      if (afterBackslash) {
        if (c.c in backslashes) {
          value += backslashes[c.c]
        } else {
          this.emitError(new UnexpectedCharacter(c.c, c.pos))
        }
        afterBackslash = false
      } else if (c.c === '\\') {
        afterBackslash = true
      } else if (c.c === openingQuote) {
        break
      } else {
        value += c.c
      }
    }

    this.emitToken(new Token(TokenKind.STRING_LITERAL, value, position))
  }

  private scanNumberLiteral(): void {
    // TODO: improve number literal. Support:
    // - hexadecimal, binary, octa
    // - floating point
    // - exponent expression
    const isDigit = (c: string): boolean => (c >= '0' && c <= '9') || c === '_'
    const c = this.peek()
    const value = this.consumeWhile(isDigit)

    const dot = this.peek()
    if (dot.c === '.') {
      const decimals = this.consumeWhile(isDigit)
      const realValue = value + '.' + decimals
      this.emitToken(new Token(TokenKind.REAL_LITERAL, realValue, c.pos))
      return
    }

    this.emitToken(new Token(TokenKind.INTEGER_LITERAL, value, c.pos))
  }

  private scanSymbol(): void {
    const first = this.peek()
    this.next()
    const second = this.peek()

    const symbolMap: Array<[string[], TokenKind]> = [
      [['!', '='], TokenKind.NOT_EQUAL],
      [['!'], TokenKind.NOT],
      [['='], TokenKind.EQUAL],
      [['*'], TokenKind.MULTIPLY],
      [['+'], TokenKind.PLUS],
      [['-', '>'], TokenKind.ARROW],
      [['-'], TokenKind.MINUS],
      [['/'], TokenKind.DIV],
      [[':', '='], TokenKind.ASSIGN],
      [[':'], TokenKind.COLON],
      [[';'], TokenKind.SEMICOLON],
      [['<', '='], TokenKind.LESS_THAN_EQUAL],
      [['<'], TokenKind.LESS_THAN],
      [['>', '='], TokenKind.GREATER_THAN_EQUAL],
      [['>'], TokenKind.GREATER_THAN],
      [['['], TokenKind.OPEN_SQUARE],
      [[']'], TokenKind.CLOSE_SQUARE],
      [['('], TokenKind.OPEN_BRAC],
      [[')'], TokenKind.CLOSE_BRAC],
      [[','], TokenKind.COMMA],
    ]

    const key = [first.c, second.c]
    for (const item of symbolMap) {
      let matched = true
      let value = ''
      for (let i = 0; i < item[0].length; i++) {
        value += key[i]
        if (item[0][i] !== key[i]) {
          matched = false
          break
        }
      }

      if (matched) {
        this.emitToken(new Token(item[1], value, first.pos))
        if (item[0].length > 1) {
          this.next()
        }
        return
      }
    }

    this.emitError(new UnexpectedCharacter(first.c, first.pos))
  }

  private scanComment(position: Position): void {
    const value = this.consumeWhile((c) => c !== '\n')
    this.emitToken(new Token(TokenKind.COMMENT, value, position))
  }

  private advance(): CharPos {
    while (true) {
      const c = this.peek()
      if (c.c === ' ' || c.c === '\t' || c.c === '\r') {
        this.next()
      } else if (c.c === '\n') {
        this.next()
      } else {
        return c
      }
    }
  }

  private peek(): CharPos {
    if (this.index >= this.sourceCode.length) {
      return new CharPos('', { line: 0, col: 0 })
    }

    return this.sourceCode[this.index]
  }

  private next(): void {
    if (this.index < this.sourceCode.length) {
      this.index++
    }
  }

  private back(): void {
    if (this.index > 0) {
      this.index--
    }
  }

  private emitToken(token: Token): void {
    this.tokens.push(token)
  }

  private emitError(error: CompileErrorItem): void {
    this.errors.push(error)
  }

  private consumeWhile(f: (c: string) => boolean): string {
    let value = ''
    while (true) {
      const c = this.peek()
      if (c.isEnd() || !f(c.c)) {
        break
      }
      value += c.c
      this.next()
    }
    return value
  }
}
