import { Position, Token, TokenKind } from './tokens'

export type Error = UnexpectedCharacter | UnexpectedToken | UnexpectedEOF | UnexpectedExpr

export enum ErrorKind {
  // for lexer phase
  UnexpectedCharacter = 'UnexpectedCharacter',

  // for parsing phase
  UnexpectedToken = 'UnexpectedToken',
  UnexpectedEOF = 'UnexpectedEOF',
  UnexpectedExpr = 'UnexpectedExpr',
}

export interface UnexpectedCharacter {
  kind: ErrorKind.UnexpectedCharacter
  char: string
  position: Position
}

export interface UnexpectedToken {
  kind: ErrorKind.UnexpectedToken
  expected: TokenKind[]
  found: Token
}

export interface UnexpectedEOF {
  kind: ErrorKind.UnexpectedEOF
  expected: TokenKind[]
}

export interface UnexpectedExpr {
  kind: ErrorKind.UnexpectedExpr
  found: Token
}

export interface Result<T> {
  value: T
  errors: Error[]
}
