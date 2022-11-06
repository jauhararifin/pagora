import { Position, Token, TokenKind } from './tokens'

export type Error = UnexpectedCharacter
| UnexpectedToken
| UnexpectedEOF
| UnexpectedTokenForExpr
| UnexpectedTokenForStatment

export enum ErrorKind {
  // for lexer phase
  UnexpectedCharacter = 'UnexpectedCharacter',

  // for parsing phase
  UnexpectedToken = 'UnexpectedToken',
  UnexpectedEOF = 'UnexpectedEOF',
  UnexpectedTokenForExpr = 'UnexpectedTokenForExpr',
  UnexpectedTokenForStatment = 'UnexpectedTokenForStatment',
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

export interface UnexpectedTokenForExpr {
  kind: ErrorKind.UnexpectedTokenForExpr
  found: Token
}

export interface UnexpectedTokenForStatment {
  kind: ErrorKind.UnexpectedTokenForStatment
  found: Token
}

export interface Result<T> {
  value?: T
  errors: Error[]
}
