export interface Program {
  globals: Variable[]
  functions: Function[]
}

export interface Variable {
  name: string
  type: Type
  value?: Expr
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
  Integer,
  Real,
  Boolean,
  String,
  Char,
  Array,
  Function,
}

export interface PrimitiveType {
  kind: TypeKind.Integer | TypeKind.Real | TypeKind.Boolean | TypeKind.String | TypeKind.Char
}

export interface ArrayType {
  kind: TypeKind.Array
  dimension: number[]
  type: Type
}

export interface FunctionType {
  kind: TypeKind.Function
  arguments: Type[]
  return?: Type
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

export interface Expr {
  kind: ExprKind
  isConstexpr: true
  type: Type
}

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

export interface IndexExpr extends Expr {
  kind: ExprKind.Index
  array: Expr
  index: Expr
}

export interface CastExpr extends Expr {
  kind: ExprKind.Cast
  source: Expr
  type: Type
}

export interface CallExpr extends Expr {
  kind: ExprKind.Call
  function: Expr
  arguments: Expr[]
}

export interface IntegerLitExpr extends Expr {
  kind: ExprKind.IntegerLit
  value: BigInt
}

export interface CharLitExpr extends Expr {
  kind: ExprKind.CharLit
  value: string
}

export interface BooleanLitExpr extends Expr {
  kind: ExprKind.BooleanLit
  value: boolean
}

export interface IdentExpr extends Expr {
  kind: ExprKind.Ident
  ident: string
}
