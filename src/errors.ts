import { Expr, Type, TypeKind } from './semantic'
import { Position, Token, TokenKind } from './tokens'
import { ExprNode } from './ast'

// TODO: currently, I feel the way the error is oraganized is very messy. The namings are not good, doesn't have error
// message, and some of the error are quite duplicated. Need to refactor this.

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
| InvalidBinaryOperator
| InvalidUnaryOperator
| FunctionIsVoid
| MissingMain
| DuplicatedMain
| WrongNumberOfArgument
| WrongNumberOfIndex

export enum ErrorKind {
  // for lexer phase
  UNEXPECTED_CHARACTER = 'UNEXPECTED_CHARACTER',

  // for parsing phase
  UNEXPECTED_TOKEN = 'UNEXPECTED_TOKEN',
  UNEXPECTED_EOF = 'UNEXPECTED_EOF',
  UNEXPECTED_TOKEN_FOR_EXPR = 'UNEXPECTED_TOKEN_FOR_EXPR',
  UNEXPECTED_TOKEN_FOR_STATMENT = 'UNEXPECTED_TOKEN_FOR_STATMENT',

  // for analyzing phase
  MULTIPLE_DECLARATION = 'MULTIPLE_DECLARATION',
  TYPE_MISMATCH = 'TYPE_MISMATCH',
  FUNCTION_IS_VOID = 'FUNCTION_IS_VOID',
  NOT_A_CONSTANT = 'NOT_A_CONSTANT',
  CANNOT_ASSIGN = 'CANNOT_ASSIGN',
  UNDEFINED = 'UNDEFINED',
  INVALID_BINARY_OP = 'INVALID_BINARY_OP',
  INVALID_UNARY_OP = 'INVALID_UNARY_OP',
  MISSING_MAIN = 'MISSING_MAIN',
  DUPLICATED_MAIN = 'DUPLICATED_MAIN',
  WRONG_NUMBER_OF_ARGUMENT = 'WRONG_NUMBER_OF_ARGUMENT',
  WRONG_NUMBER_OF_INDEX = 'WRONG_NUMBER_OF_INDEX'
}

export interface UnexpectedCharacter {
  kind: ErrorKind.UNEXPECTED_CHARACTER
  char: string
  position: Position
}

export interface UnexpectedToken {
  kind: ErrorKind.UNEXPECTED_TOKEN
  expected: TokenKind[]
  found: Token
}

export interface UnexpectedEOF {
  kind: ErrorKind.UNEXPECTED_EOF
  expected: TokenKind[]
}

export interface UnexpectedTokenForExpr {
  kind: ErrorKind.UNEXPECTED_TOKEN_FOR_EXPR
  found: Token
}

export interface UnexpectedTokenForStatment {
  kind: ErrorKind.UNEXPECTED_TOKEN_FOR_STATMENT
  found: Token
}

export interface MultipleDeclaration {
  kind: ErrorKind.MULTIPLE_DECLARATION
  declaredAt: Token
  redeclaredAt: Token
}

export interface TypeMismatch {
  kind: ErrorKind.TYPE_MISMATCH
  source: Expr
  targetType: Type | TypeKind
}

export interface FunctionIsVoid {
  kind: ErrorKind.FUNCTION_IS_VOID
  return: Token
}

export interface NotAConstant {
  kind: ErrorKind.NOT_A_CONSTANT
  value: Expr
}

export interface CannotAssign {
  kind: ErrorKind.CANNOT_ASSIGN
  expr: ExprNode
  receiver: Type
}

export interface Undefined {
  kind: ErrorKind.UNDEFINED
  name: Token
}

export interface InvalidBinaryOperator {
  kind: ErrorKind.INVALID_BINARY_OP
  a: Expr
  op: Token
  b: Expr
}

export interface InvalidUnaryOperator {
  kind: ErrorKind.INVALID_UNARY_OP
  op: Token
  value: Expr
}

export interface MissingMain {
  kind: ErrorKind.MISSING_MAIN
}

export interface DuplicatedMain {
  // TODO: add position of the first main
  kind: ErrorKind.DUPLICATED_MAIN
}

export interface WrongNumberOfArgument {
  kind: ErrorKind.WRONG_NUMBER_OF_ARGUMENT
  expected: number
  got: number
}

export interface WrongNumberOfIndex {
  kind: ErrorKind.WRONG_NUMBER_OF_INDEX
  expected: number
  got: number
}

export interface Result<T> {
  value?: T
  errors: Error[]
}
