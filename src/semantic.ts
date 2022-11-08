export interface Program {
  globals: Variable[]
  functions: Function[]
  main: BlockStatement
}

export interface Function {
  name: string
  type: FunctionType
  arguments: Argument[]
  body: BlockStatement
}

export interface Argument {
  name: string
  type: Type
}

export type Type = PrimitiveType | ArrayType | FunctionType

export enum TypeKind {
  VOID = 'VOID',
  INTEGER = 'INTEGER',
  REAL = 'REAL',
  BOOLEAN = 'BOOLEAN',
  STRING = 'STRING',
  CHAR = 'CHAR',
  ARRAY = 'ARRAY',
  FUNCTION = 'FUNCTION',
}

export interface PrimitiveType {
  kind: TypeKind.INTEGER | TypeKind.REAL | TypeKind.BOOLEAN | TypeKind.STRING | TypeKind.CHAR | TypeKind.VOID
}

export const Integer: Type = { kind: TypeKind.INTEGER }
export const Real: Type = { kind: TypeKind.REAL }
export const Boolean: Type = { kind: TypeKind.BOOLEAN }
export const String: Type = { kind: TypeKind.STRING }
export const Char: Type = { kind: TypeKind.CHAR }
export const Void: Type = { kind: TypeKind.VOID }

export interface ArrayType {
  kind: TypeKind.ARRAY
  dimension: BigInt[]
  type: Type
}

export interface FunctionType {
  kind: TypeKind.FUNCTION
  arguments: Type[]
  return?: Type
}

export type Statement = BlockStatement
| VarStatement
| IfStatement
| WhileStatement
| AssignStatement
| ExprStatement
| ReturnStatement

export enum StatementKind {
  BLOCK = 'BLOCK',
  VAR = 'VAR',
  IF = 'IF',
  WHILE = 'WHILE',
  ASSIGN = 'ASSIGN',
  EXPR = 'EXPR',
  RETURN = 'RETURN',
}

export interface BlockStatement {
  kind: StatementKind.BLOCK
  body: Statement[]
}

export interface VarStatement {
  kind: StatementKind.VAR
  variable: Variable
}

export interface Variable {
  name: string
  type: Type
  value?: Expr
}

export interface IfStatement {
  kind: StatementKind.IF
  condition: Expr
  body: Statement
  else?: Statement
}

export interface WhileStatement {
  kind: StatementKind.WHILE
  condition: Expr
  body: Statement
}

export interface AssignStatement {
  kind: StatementKind.ASSIGN
  target: Expr
  value: Expr
}

export interface ExprStatement {
  kind: StatementKind.EXPR
  value: Expr
}

export interface ReturnStatement {
  kind: StatementKind.RETURN
  value?: Expr
}

export interface Expr {
  kind: ExprKind
  isConstexpr: boolean
  constValue: any
  isAssignable: boolean
  type: Type
}

export enum ExprKind {
  BINARY = 'BINARY',
  UNARY = 'UNARY',
  INDEX = 'INDEX',
  CAST = 'CAST',
  CALL = 'CALL',
  INTEGER_LIT = 'INTEGER_LIT',
  CHAR_LIT = 'CHAR_LIT',
  BOOLEAN_LIT = 'BOOLEAN_LIT',
  IDENT = 'IDENT',
}

export interface BinaryExpr extends Expr {
  kind: ExprKind.BINARY
  a: Expr
  op: BinaryOp
  b: Expr
}

export enum BinaryOp {
  PLUS = 'PLUS',
  MINUS = 'MINUS',
  DIV = 'DIV',
  MUL = 'MUL',
  AND = 'AND',
  OR = 'OR',
  BIT_AND = 'BIT_AND',
  BIT_OR = 'BIT_OR',
  BIT_XOR = 'BIT_XOR',
  EQUAL = 'EQUAL',
  NOT_EQUAL = 'NOT_EQUAL',
  GREATER_THAN = 'GREATER_THAN',
  GREATER_THAN_EQUAL = 'GREATER_THAN_EQUAL',
  LESS_THAN = 'LESS_THAN',
  LESS_THAN_EQUAL = 'LESS_THAN_EQUAL',
  SHIFT_LEFT = 'SHIFT_LEFT',
  SHIFT_RIGHT = 'SHIFT_RIGHT',
}

export interface UnaryExpr extends Expr {
  kind: ExprKind.UNARY
  op: UnaryOp
  value: Expr
}

export enum UnaryOp {
  Plus, Minus, Not, BitNot
}

export interface IndexExpr extends Expr {
  kind: ExprKind.INDEX
  array: Expr
  index: Expr
}

export interface CastExpr extends Expr {
  kind: ExprKind.CAST
  source: Expr
  type: Type
}

export interface CallExpr extends Expr {
  kind: ExprKind.CALL
  function: Expr
  arguments: Expr[]
}

export interface IntegerLitExpr extends Expr {
  kind: ExprKind.INTEGER_LIT
  value: BigInt
}

export interface CharLitExpr extends Expr {
  kind: ExprKind.CHAR_LIT
  value: string
}

export interface BooleanLitExpr extends Expr {
  kind: ExprKind.BOOLEAN_LIT
  value: boolean
}

export interface IdentExpr extends Expr {
  kind: ExprKind.IDENT
  ident: string
}
