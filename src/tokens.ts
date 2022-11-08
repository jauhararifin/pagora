export interface Token {
  value: string
  position: Position
  kind: TokenKind
}

export interface Position {
  line: number
  col: number
}

export enum TokenKind {
  EOF = 'EOF',
  PHANTOM_SEMICOLON = 'PHANTOM_SEMICOLON',
  INVALID = 'INVALID',
  COMMENT = 'COMMENT',

  VAR = 'VAR',
  TYPE = 'TYPE',
  STRUCT = 'STRUCT',
  AS = 'AS',
  IDENTIFIER = 'IDENTIFIER',
  FUNCTION = 'FUNCTION',
  COMMA = 'COMMA',
  COLON = 'COLON',
  ARROW = 'ARROW',
  SEMICOLON = 'SEMICOLON',

  BEGIN = 'BEGIN',
  END = 'END',

  ARRAY = 'ARRAY',
  OF = 'OF',

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
  FOR = 'FOR',
  DO = 'DO',

  CONTINUE = 'CONTINUE',
  BREAK = 'BREAK',
  RETURN = 'RETURN',

  INTEGER = 'INTEGER',
  CHAR = 'CHAR',
  REAL = 'REAL',
  BOOLEAN = 'BOOLEAN',
  // TODO: change "CHAR" to "BYTE" and support string literal as array of bytes.
}

export const PrimitiveTypes = [
  TokenKind.INTEGER,
  TokenKind.CHAR,
  TokenKind.REAL,
  TokenKind.BOOLEAN
]
