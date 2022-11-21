import { MAX_ERRORS } from './config'
import {
  CompileError,
  CompileErrorItem,
  MissingClosingQuote,
  TooManyErrors,
  UnexpectedCharacter,
} from './errors'
import { Position, Token, TokenKind } from './tokens'

export function scan(sourceCode: string): Token[] {
  const codes = new SourceCode(sourceCode)
  const tokens: Token[] = []
  const errors: CompileErrorItem[] = []

  while (!codes.empty()) {
    if (skipWhitespaces(codes)) continue
    if (updateResult(scanWord(codes), tokens, errors)) continue
    if (updateResult(scanString(codes), tokens, errors)) continue
    if (updateResult(scanNumber(codes), tokens, errors)) continue
    if (updateResult(scanComment(codes), tokens, errors)) continue
    if (updateResult(scanSymbol(codes), tokens, errors)) continue

    if (errors.length < MAX_ERRORS) {
      errors.push(new UnexpectedCharacter(codes.char(), codes.pos()))
      if (errors.length === MAX_ERRORS) {
        errors.push(new TooManyErrors())
        break
      }
    }

    codes.advance(1)
  }

  if (errors.length > 0) {
    throw new CompileError(errors)
  }

  return tokens
}

class SourceCode {
  characters: CharPos[] = []
  index: number = 0

  constructor(source: string) {
    const characters = []

    let line = 1
    let col = 0
    const codes = Array.from(source)

    for (const c of codes) {
      col++
      characters.push({ c, pos: { line, col } })

      if (c === '\n') {
        line++
        col = 0
      }
    }

    this.characters = characters
    this.index = 0
  }

  char(num: number = 1): string {
    if (num === 1) return this.characters[this.index]?.c ?? ''

    return this.characters
      .slice(this.index, this.index + num)
      .map((v) => v.c)
      .join('')
  }

  pos(): Position {
    if (this.characters.length === 0) {
      return { line: 0, col: 0 }
    }
    return (
      this.characters[this.index]?.pos ??
      this.characters[this.characters.length - 1].pos
    )
  }

  advance(n: number): void {
    this.index += n
  }

  empty(): boolean {
    return this.index >= this.characters.length
  }

  length(): number {
    return this.characters.length
  }
}

interface CharPos {
  c: string
  pos: Position
}

function updateResult(
  scanResult: ScanResult,
  tokens: Token[],
  errors: Error[]
): boolean {
  const { consumed, token, errors: scanErrors } = scanResult
  if (token !== undefined) {
    tokens.push(token)
  }

  if (errors.length + scanErrors.length > MAX_ERRORS) {
    errors.push(...scanErrors.slice(0, MAX_ERRORS - errors.length))
    errors.push(new TooManyErrors())
  } else {
    errors.push(...scanErrors)
  }

  return consumed
}

const whitespaces = new Set([' ', '\t', '\r', '\n'])

function skipWhitespaces(sourceCode: SourceCode): boolean {
  return consumeChars(sourceCode, whitespaces).length > 0
}

interface ScanResult {
  consumed: boolean
  token?: Token
  errors: CompileErrorItem[]
}

const SCANNED_NOTHING: ScanResult = { consumed: false, errors: [] }

const wordPrefix = new Set(
  '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
)
const wordChars = new Set(
  '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
)

const wordToToken: { [key: string]: TokenKind } = {
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
  string: TokenKind.STRING,
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
  true: TokenKind.TRUE,
  false: TokenKind.FALSE,
}

function scanWord(sourceCode: SourceCode): ScanResult {
  if (sourceCode.empty()) {
    return SCANNED_NOTHING
  }
  const c = sourceCode.char()
  if (!wordPrefix.has(c)) {
    return SCANNED_NOTHING
  }

  const position = sourceCode.pos()
  const word = consumeChars(sourceCode, wordChars)
  const kind = wordToToken[word] ?? TokenKind.IDENTIFIER
  const token = new Token(kind, word, position)

  return {
    consumed: true,
    token,
    errors: [],
  }
}

function consumeChars(sourceCode: SourceCode, chars: Set<string>): string {
  let value = ''
  for (; !sourceCode.empty(); sourceCode.advance(1)) {
    const c = sourceCode.char()
    if (!chars.has(c)) {
      break
    }
    value += c
  }
  return value
}

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

function scanString(sourceCode: SourceCode): ScanResult {
  if (sourceCode.empty()) {
    return SCANNED_NOTHING
  }
  if (sourceCode.char() !== '"') {
    return SCANNED_NOTHING
  }

  const position = sourceCode.pos()
  const openingQuote = sourceCode.char()
  sourceCode.advance(1)

  let afterBackslash = false
  let closed = false
  let value = openingQuote
  const errors: CompileErrorItem[] = []

  for (; !closed && !sourceCode.empty(); sourceCode.advance(1)) {
    const c = sourceCode.char()
    const pos = sourceCode.pos()

    if (c === '\n') {
      errors.push(new MissingClosingQuote(pos))
      break
    }

    if (afterBackslash) {
      if (c in backslashes) {
        value += backslashes[c]
      } else {
        errors.push(new UnexpectedCharacter(c, pos))
      }
      afterBackslash = false
    } else if (c === '\\') {
      afterBackslash = true
    } else if (c === openingQuote) {
      value += c
      closed = true
    } else {
      value += c
    }
  }

  if (!closed) {
    errors.push(new MissingClosingQuote(sourceCode.pos()))
  }

  return {
    consumed: true,
    token: new Token(TokenKind.STRING_LITERAL, value, position),
    errors,
  }
}

const digit = new Set('0123456789')

function scanNumber(sourceCode: SourceCode): ScanResult {
  // TODO: improve number literal. Support:
  // - hexadecimal, binary, octa
  // - floating point
  // - exponent expression
  if (sourceCode.empty()) {
    return SCANNED_NOTHING
  }
  if (!digit.has(sourceCode.char())) {
    return SCANNED_NOTHING
  }

  const pos = sourceCode.pos()
  const value = consumeChars(sourceCode, digit)

  if (sourceCode.char() === '.') {
    sourceCode.advance(1)
    const decimals = consumeChars(sourceCode, digit)
    const realValue = value + '.' + decimals
    return {
      consumed: true,
      token: new Token(TokenKind.REAL_LITERAL, realValue, pos),
      errors: [],
    }
  }

  return {
    consumed: true,
    token: new Token(TokenKind.INTEGER_LITERAL, value, pos),
    errors: [],
  }
}

const symbolMap: Array<[string, TokenKind]> = [
  ['!=', TokenKind.NOT_EQUAL],
  ['!', TokenKind.NOT],
  ['=', TokenKind.EQUAL],
  ['*', TokenKind.MULTIPLY],
  ['+', TokenKind.PLUS],
  ['->', TokenKind.ARROW],
  ['-', TokenKind.MINUS],
  ['/', TokenKind.DIV],
  [':=', TokenKind.ASSIGN],
  [':', TokenKind.COLON],
  [';', TokenKind.SEMICOLON],
  ['<=', TokenKind.LESS_THAN_EQUAL],
  ['<', TokenKind.LESS_THAN],
  ['>=', TokenKind.GREATER_THAN_EQUAL],
  ['>', TokenKind.GREATER_THAN],
  ['[', TokenKind.OPEN_SQUARE],
  [']', TokenKind.CLOSE_SQUARE],
  ['(', TokenKind.OPEN_BRAC],
  [')', TokenKind.CLOSE_BRAC],
  [',', TokenKind.COMMA],
  ['%', TokenKind.MOD],
]

function scanSymbol(sourceCode: SourceCode): ScanResult {
  if (sourceCode.empty()) {
    return SCANNED_NOTHING
  }

  const pos = sourceCode.pos()
  let sym = ''
  let value = ''
  let op: TokenKind | undefined

  while (!sourceCode.empty()) {
    sym += sourceCode.char()

    let match = false
    for (const [key, value] of symbolMap) {
      if (key.startsWith(sym)) {
        match = true
        op = value
      }
    }
    if (!match) break

    value += sourceCode.char()
    sourceCode.advance(1)
  }

  if (op === undefined) return SCANNED_NOTHING

  return { consumed: true, token: new Token(op, value, pos), errors: [] }
}

function scanComment(sourceCode: SourceCode): ScanResult {
  if (sourceCode.char(2) !== '//') {
    return SCANNED_NOTHING
  }

  const pos = sourceCode.pos()
  let value = ''

  for (; !sourceCode.empty(); sourceCode.advance(1)) {
    if (sourceCode.char() === '\n') break
    value += sourceCode.char()
  }

  return {
    consumed: true,
    token: new Token(TokenKind.COMMENT, value, pos),
    errors: [],
  }
}
