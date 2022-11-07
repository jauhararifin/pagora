import { Expr, Type } from './semantic'
import { Position, Token, TokenKind } from './tokens'
import { ExprNode } from './ast'

export type Error = UnexpectedCharacter
| UnexpectedToken
| UnexpectedEOF
| UnexpectedTokenForExpr
| UnexpectedTokenForStatment
| MultipleDeclaration
| TypeMismatch
| NotAConstant
| CannotAssign
| Undefined

export enum ErrorKind {
  // for lexer phase
  UnexpectedCharacter = 'UnexpectedCharacter',

  // for parsing phase
  UnexpectedToken = 'UnexpectedToken',
  UnexpectedEOF = 'UnexpectedEOF',
  UnexpectedTokenForExpr = 'UnexpectedTokenForExpr',
  UnexpectedTokenForStatment = 'UnexpectedTokenForStatment',

  // for analyzing phase
  MultipleDeclaration = 'MultipleDeclaration',
  TypeMismatch = 'TypeMismatch',
  NotAConstant = 'NotAConstant',
  CannotAssign = 'CannotAssign',
  Undefined = 'Undefined'
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

export interface MultipleDeclaration {
  kind: ErrorKind.MultipleDeclaration
  declaredAt: Token
  redeclaredAt: Token
}

export interface TypeMismatch {
  kind: ErrorKind.TypeMismatch
  source: Type
  target: Type
}

export interface NotAConstant {
  kind: ErrorKind.NotAConstant
  value: Expr
}

export interface CannotAssign {
  kind: ErrorKind.CannotAssign
  expr: ExprNode
  receiver: Type
}

export interface Undefined {
  kind: ErrorKind.Undefined
  name: Token
}

export interface Result<T> {
  value?: T
  errors: Error[]
}
