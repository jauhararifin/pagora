export class Token {
  kind: TokenKind
  value: string
  position: Position

  constructor(kind: TokenKind, value: string, pos: Position) {
    this.kind = kind
    this.value = value
    this.position = pos
  }

  repr(): string {
    return (
      this.kind.toString() +
      (!KEYWORD.has(this.kind) ? `(${this.value.toString()})` : '')
    )
  }
}

export class Position {
  line: number
  col: number

  constructor(line: number, col: number) {
    this.line = line
    this.col = col
  }

  toString(): string {
    return `${this.line}:${this.col}`
  }
}

export enum TokenKind {
  EOF = 'EOF',
  INVALID = 'INVALID',
  COMMENT = 'COMMENT',

  VAR = 'VAR',
  TYPE = 'TYPE',
  AS = 'AS',
  IDENTIFIER = 'IDENTIFIER',
  FUNCTION = 'FUNCTION',
  COMMA = 'COMMA',
  COLON = 'COLON',
  ARROW = 'ARROW',
  SEMICOLON = 'SEMICOLON',

  BEGIN = 'BEGIN',
  END = 'END',

  OPEN_SQUARE = 'OPEN_SQUARE',
  CLOSE_SQUARE = 'CLOSE_SQUARE',
  OPEN_BRAC = 'OPEN_BRAC',
  CLOSE_BRAC = 'CLOSE_BRAC',

  INTEGER_LITERAL = 'INTEGER_LITERAL',
  REAL_LITERAL = 'REAL_LITERAL',
  STRING_LITERAL = 'STRING_LITERAL',
  TRUE = 'TRUE',
  FALSE = 'FALSE',

  ASSIGN = 'ASSIGN',

  PLUS = 'PLUS',
  MINUS = 'MINUS',
  MULTIPLY = 'MULTIPLY',
  DIV = 'DIV',
  MOD = 'MOD',
  BIT_OR = 'BIT_OR',
  BIT_AND = 'BIT_AND',
  BIT_XOR = 'BIT_XOR',
  BIT_NOT = 'BIT_NOT',
  SHIFT_LEFT = 'SHIFT_LEFT',
  SHIFT_RIGHT = 'SHIFT_RIGHT',

  AND = 'AND',
  NOT = 'NOT',
  OR = 'OR',

  EQUAL = 'EQUAL',
  NOT_EQUAL = 'NOT_EQUAL',
  GREATER_THAN = 'GREATER_THAN',
  GREATER_THAN_EQUAL = 'GREATER_THAN_EQUAL',
  LESS_THAN = 'LESS_THAN',
  LESS_THAN_EQUAL = 'LESS_THAN_EQUAL',

  IF = 'IF',
  THEN = 'THEN',
  ELSE = 'ELSE',
  WHILE = 'WHILE',
  DO = 'DO',

  CONTINUE = 'CONTINUE',
  BREAK = 'BREAK',
  RETURN = 'RETURN',

  INTEGER = 'INTEGER',
  STRING = 'STRING',
  BYTE = 'BYTE',
  REAL = 'REAL',
  BOOLEAN = 'BOOLEAN',
  ARRAY = 'ARRAY',
  OF = 'OF',
  STRUCT = 'STRUCT',
}

export const PrimitiveTypes = [
  TokenKind.INTEGER,
  TokenKind.BYTE,
  TokenKind.REAL,
  TokenKind.BOOLEAN,
  TokenKind.STRING,
]

const KEYWORD = new Set([
  TokenKind.EOF,
  TokenKind.INVALID,
  TokenKind.VAR,
  TokenKind.TYPE,
  TokenKind.STRUCT,
  TokenKind.AS,
  TokenKind.FUNCTION,
  TokenKind.COMMA,
  TokenKind.COLON,
  TokenKind.ARROW,
  TokenKind.SEMICOLON,
  TokenKind.BEGIN,
  TokenKind.END,
  TokenKind.ARRAY,
  TokenKind.OF,
  TokenKind.OPEN_SQUARE,
  TokenKind.CLOSE_SQUARE,
  TokenKind.OPEN_BRAC,
  TokenKind.CLOSE_BRAC,
  TokenKind.TRUE,
  TokenKind.FALSE,
  TokenKind.ASSIGN,
  TokenKind.PLUS,
  TokenKind.MINUS,
  TokenKind.MULTIPLY,
  TokenKind.DIV,
  TokenKind.MOD,
  TokenKind.BIT_OR,
  TokenKind.BIT_AND,
  TokenKind.BIT_XOR,
  TokenKind.BIT_NOT,
  TokenKind.SHIFT_LEFT,
  TokenKind.SHIFT_RIGHT,
  TokenKind.AND,
  TokenKind.NOT,
  TokenKind.OR,
  TokenKind.EQUAL,
  TokenKind.NOT_EQUAL,
  TokenKind.GREATER_THAN,
  TokenKind.GREATER_THAN_EQUAL,
  TokenKind.LESS_THAN,
  TokenKind.LESS_THAN_EQUAL,
  TokenKind.IF,
  TokenKind.THEN,
  TokenKind.ELSE,
  TokenKind.WHILE,
  TokenKind.DO,
  TokenKind.CONTINUE,
  TokenKind.BREAK,
  TokenKind.RETURN,
  TokenKind.INTEGER,
  TokenKind.BYTE,
  TokenKind.REAL,
  TokenKind.BOOLEAN,
])
