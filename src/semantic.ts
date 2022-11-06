export interface Program {
  globals: Variable[]
  functions: Function[]
}

export interface Variable {
  name: string
  type: Type
}

export interface Function {
  name: string
  arguments: Argument[]
  returnType: Type
  body: Statement
}

export interface Argument {
  name: string
  type: Type
}

export type Type = PrimitiveType | ArrayType

export enum TypeKind {
  Integer,
  Real,
  Boolean,
  String,
  Char,
  Array,
}

export interface PrimitiveType {
  kind: TypeKind.Integer | TypeKind.Real | TypeKind.Boolean | TypeKind.String | TypeKind.Char
}

export interface ArrayType {
  dimension: number[]
  type: TypeKind
}

export type Statement = BlockStatement
| IfStatement
| WhileStatement
| AssignStatement
| ExprStatement
| ReturnStatement

export enum StatementKind {
  Block,
  If,
  While,
  Assign,
  Expr,
  Return,
}

export interface BlockStatement {
  kind: StatementKind.Block
  body: Statement[]
}

export interface IfStatement {
  kind: StatementKind.If
  condition: Expr
  body: Statement
  else?: Statement
}

export interface WhileStatement {
  kind: StatementKind.While
  condition: Expr
  body: Statement
}

export interface AssignStatement {
  kind: StatementKind.Assign
  target: Expr
  value: Expr
}

export interface ExprStatement {
  kind: StatementKind.Expr
  value: Expr
}

export interface ReturnStatement {
  kind: StatementKind.Return
  value?: Expr
}

export type Expr = BinaryExpr
| UnaryExpr
| IndexExpr
| CastExpr
| CallExpr
| IntegerLitExpr
| CharLitExpr
| BooleanLitExpr
| IdentExpr

export enum ExprKind {
  Binary,
  Unary,
  Index,
  Cast,
  Call,
  IntegerLit,
  CharLit,
  BooleanLit,
  Ident,
}

export interface BinaryExpr {
  kind: ExprKind.Binary
  a: Expr
  op: BinaryOp
  b: Expr
}

export enum BinaryOp {
  Plus, Minus, Div, Mul, And, Or, BitAnd, BitOr, BitXor
}

export interface UnaryExpr {
  kind: ExprKind.Unary
  op: UnaryOp
  value: Expr
}

export enum UnaryOp {
  Plus, Minus, Not, BitNot
}

export interface IndexExpr {
  kind: ExprKind.Index
  array: Expr
  index: Expr
}

export interface CastExpr {
  kind: ExprKind.Cast
  source: Expr
  type: Type
}

export interface CallExpr {
  kind: ExprKind.Call
  function: Expr
  arguments: Expr[]
}

export interface IntegerLitExpr {
  kind: ExprKind.IntegerLit
  value: BigInt
}

export interface CharLitExpr {
  kind: ExprKind.CharLit
  value: string
}

export interface BooleanLitExpr {
  kind: ExprKind.BooleanLit
  value: boolean
}

export interface IdentExpr {
  kind: ExprKind.Ident
  ident: string
}
