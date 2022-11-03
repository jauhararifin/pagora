import { Token } from './tokens'

export interface RootNode {
  declarations: DeclNode[]
}

export interface DeclNode {
  kind: DeclKind
}

export enum DeclKind {
  FUNCTION,
  VARIABLE,
}

// TODO: support native function with empty body
export interface FunctionDeclNode {
  kind: DeclKind.FUNCTION
  function: Token
  name: Token
  openBrace: Token
  params: ParamsNode
  closeBrace: Token
  colon: Token
  returnType?: TypeExprNode
  body: StatementNode
}

export interface ParamsNode {
  params: ParamGroup[]
  commas: Token[]
}

export interface ParamGroup {
  names: Token[]
  colon: Token
  type: TypeExprNode
}

export interface TypeExprNode {
  kind: TypeKind
}

export enum TypeKind {
  PRIMITIVE,
  ARRAY,
  STRUCT,
  TUPLE,
}

export interface PrimitiveTypeNode {
  kind: TypeKind.PRIMITIVE
  type: Token
}

export interface ArrayTypeNode {
  kind: TypeKind.ARRAY
  openSquare: Token
  dimension: CommaSeparatedExpr // capture the commas position
  closeSquare: Token
  of: Token
  type: TypeExprNode
}

export interface StatementNode {
  kind: StatementKind
}

export enum StatementKind {
  VAR,
  ASSIGN,
  RETURN,
  IF,
  WHILE,
  BLOCK,
  EXPR,
}

export interface VarStatementNode {
  kind: StatementKind.VAR
  var: Token
  name: Token
  type: TypeExprNode
  assign?: Token
  value?: ExprNode
}

export interface AssignStatementNode {
  kind: StatementKind.ASSIGN
  name: Token
  assign: Token
  value: ExprNode
}

export interface ReturnStatementNode {
  kind: StatementKind.RETURN
  return: Token
  value?: ExprNode
}

export interface IfStatementNode {
  kind: StatementKind.IF
  if: Token
  condition: ExprNode
  body: StatementNode
}

export interface WhileStatementNode {
  kind: StatementKind.WHILE
  while: Token
  condition: ExprNode
  body: StatementNode
}

export interface BlockStatementNode {
  kind: StatementKind.BLOCK
  begin: Token
  statements: StatementNode[]
  end: Token
}

export interface ExprStatementNode {
  kind: StatementKind.EXPR
  expr: ExprNode
}

export interface ExprNode {
  kind: ExprKind
}

export enum ExprKind {
  IDENT,
  INTEGER_LIT,
  FLOAT_LIT,
  BOOLEAN_LIT,
  ARRAY_LIT,
  TUPLE_LIT,
  BINARY,
  UNARY,
  CALL,
  ARRAY_INDEX,
  TUPLE_INDEX,
  STRUCT_INDEX,
  CAST,
}

export interface IdentExprNode {
  kind: ExprKind.IDENT
  name: Token
}

export interface IntegerLitExprNode {
  kind: ExprKind.IDENT
  value: Token
}

export interface BooleanLitExprNode {
  kind: ExprKind.IDENT
  value: Token
}

export interface BinaryExprNode {
  kind: ExprKind.BINARY
  a: ExprNode
  op: Token
  b: ExprNode
}

export interface UnaryExprNode {
  kind: ExprKind.UNARY
  op: Token
  value: ExprNode
}

export interface CallExprNode {
  kind: ExprKind.CALL
  callee: ExprKind
  openBrac: Token
  arguments: CommaSeparatedExpr
  closeBrac: Token
}

export interface CommaSeparatedExpr {
  values: ExprNode[]
  commas: Token[]
}

export interface ArrayIndexExprNode {
  kind: ExprKind.ARRAY_INDEX
  array: ExprNode
  openSquare: Token
  index: CommaSeparatedExpr
  closeSquare: Token
}

export interface TupleIndexExprNode {
  kind: ExprKind.TUPLE_INDEX
  tuple: ExprNode
  dot: Token
  index: ExprNode
}

export interface StructIndexExprNode {
  kind: ExprKind.STRUCT_INDEX
  struct: ExprNode
  dot: Token
  index: Token
}

export interface CastExprNode {
  kind: ExprKind.CAST
  source: ExprNode
  as: Token
  target: TypeExprNode
}
