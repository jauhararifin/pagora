import { Token, TokenKind, Position } from './tokens'

export interface Error {
  position: Position
  message: string
}

export interface Result {
  tokens: Token[]
  errors: Error[]
}

export function tokenize (sourceCode: string): Result {
  return new Lexer(sourceCode).scan()
}

class CharPos {
  c: string
  pos: Position

  constructor (c: string, pos: Position) {
    this.c = c
    this.pos = pos
  }

  isEnd (): boolean {
    return this.c === ''
  }
}

class Lexer {
  private readonly sourceCode: CharPos[]
  private index: number
  private readonly tokens: Token[]
  private readonly errors: Error[]

  constructor (sourceCode: string) {
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

  scan (): Result {
    while (true) {
      this.advance()
      const c = this.peek()
      if (c.isEnd()) { break }

      if (c.c === '_' || (c.c >= 'a' && c.c <= 'z') || (c.c >= 'A' && c.c <= 'A')) {
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
          this.emitToken({
            value: '/',
            position: c.pos,
            kind: TokenKind.Div
          })
        }
      } else {
        this.emitError({ position: c.pos, message: `unrecognized character '${c.c}'` })
        this.next()
      }
    }

    return { tokens: this.tokens, errors: this.errors }
  }

  private scanWord (): void {
    const position = this.peek().pos

    const word = this.consumeWhile((c) => c === '_' ||
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'A') ||
      (c >= '0' && c <= '9')
    )

    const map: { [key: string]: TokenKind } = {
      var: TokenKind.Var,
      type: TokenKind.Type,
      struct: TokenKind.Struct,
      as: TokenKind.As,
      function: TokenKind.Function,
      begin: TokenKind.Begin,
      end: TokenKind.End,
      array: TokenKind.Array,
      of: TokenKind.Of,
      integer: TokenKind.Integer,
      char: TokenKind.Char,
      and: TokenKind.And,
      not: TokenKind.Not,
      or: TokenKind.Or,
      if: TokenKind.If,
      then: TokenKind.Then,
      else: TokenKind.Else,
      while: TokenKind.While,
      for: TokenKind.For,
      do: TokenKind.Do,
      continue: TokenKind.Continue,
      break: TokenKind.Break,
      return: TokenKind.Real
    }

    const kind = word in map ? map[word] : TokenKind.Identifier
    this.emitToken({ value: word, position, kind })
  }

  private scanString (): void {
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
      '`': '`'
    }

    let afterBackslash = false
    let value = ''

    this.next()
    while (true) {
      const c = this.peek()
      this.next()
      if (c.c === '\n') {
        this.emitError({ position: c.pos, message: 'unexpected newline on string literal' })
      }

      if (afterBackslash) {
        if (c.c in backslashes) {
          value += backslashes[c.c]
        } else {
          this.emitError({ position, message: `unexpected ${c.c} character` })
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

    this.emitToken({
      value,
      position,
      kind: TokenKind.StringLiteral
    })
  }

  private scanNumberLiteral (): void {
    // TODO: improve number literal. Support:
    // - hexadecimal, binary, octa
    // - floating point
    // - exponent expression
    const c = this.peek()
    const value = this.consumeWhile((c) => (c >= '0' && c <= '9') || c === '_')
    this.emitToken({
      value,
      position: c.pos,
      kind: TokenKind.IntegerLiteral
    })
  }

  private scanSymbol (): void {
    const first = this.peek()
    this.next()
    const second = this.peek()

    const symbolMap: Array<[string[], TokenKind]> = [
      [['!', '='], TokenKind.NotEqual],
      [['!'], TokenKind.Not],
      [['='], TokenKind.Equal],
      [['*'], TokenKind.Multiply],
      [['+'], TokenKind.Plus],
      [['->'], TokenKind.Arrow],
      [['-'], TokenKind.Minus],
      [['/'], TokenKind.Div],
      [[':', '='], TokenKind.Assign],
      [[':'], TokenKind.Colon],
      [[';'], TokenKind.Semicolon],
      [['<', '='], TokenKind.LessThanEqual],
      [['<'], TokenKind.LessThan],
      [['>', '='], TokenKind.GreaterThanEqual],
      [['>'], TokenKind.GreaterThan],
      [['['], TokenKind.OpenSquare],
      [[']'], TokenKind.CloseSquare],
      [['('], TokenKind.OpenBrac],
      [[')'], TokenKind.CloseBrac]
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
        this.emitToken({
          value,
          position: first.pos,
          kind: item[1]
        })
        if (item[0].length > 1) { this.next() }
        return
      }
    }

    this.emitError({ position: first.pos, message: 'unexpected symbol' })
  }

  private scanComment (position: Position): void {
    const value = this.consumeWhile((c) => c !== '\n')
    this.emitToken({
      value,
      position,
      kind: TokenKind.Comment
    })
  }

  private advance (): CharPos {
    while (true) {
      const c = this.peek()
      if (c.c === ' ' || c.c === '\t' || c.c === '\r') {
        this.next()
      } else if (c.c === '\n') {
        if (this.tokens.length > 0) {
          const lastToken = this.tokens[this.tokens.length - 1]
          const needPhantomToken = [
            TokenKind.Break,
            TokenKind.Continue,
            TokenKind.IntegerLiteral,
            TokenKind.RealLiteral,
            TokenKind.StringLiteral,
            TokenKind.Break,
            TokenKind.Continue,
            TokenKind.Return,
            TokenKind.CloseBrac,
            TokenKind.CloseSquare,
            TokenKind.Integer,
            TokenKind.Char,
            TokenKind.Real
          ]
          if (needPhantomToken.includes(lastToken.kind)) {
            this.emitToken({ value: ';', position: c.pos, kind: TokenKind.PhantomSemicolon })
          }
        }
        this.next()
      } else {
        return c
      }
    }
  }

  private peek (): CharPos {
    if (this.index >= this.sourceCode.length) {
      return new CharPos('', { line: 0, col: 0 })
    }

    return this.sourceCode[this.index]
  }

  private next (): void {
    if (this.index < this.sourceCode.length) { this.index++ }
  }

  private back (): void {
    if (this.index > 0) { this.index-- }
  }

  private emitToken (token: Token): void {
    this.tokens.push(token)
  }

  private emitError (error: Error): void {
    this.errors.push(error)
  }

  private consumeWhile (f: (c: string) => boolean): string {
    let value = ''
    while (true) {
      const c = this.peek()
      if (c.isEnd() || !f(c.c)) { break }
      value += c.c
      this.next()
    }
    return value
  }
}
