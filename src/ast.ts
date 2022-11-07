import { Token } from './tokens'

export interface RootNode {
  declarations: DeclNode[]
}

export type DeclNode = FunctionDeclNode | VariableDeclNode | MainDeclNode

export enum DeclKind {
  FUNCTION,
  VARIABLE,
  MAIN,
}

// TODO: support native function with empty body
export interface FunctionDeclNode {
  kind: DeclKind.FUNCTION
  function: Token
  name: Token
  openBrac: Token
  params: ParamsNode
  closeBrac: Token
  arrow?: Token
  returnType?: TypeExprNode
  body: BlockStatementNode
}

export interface VariableDeclNode {
  kind: DeclKind.VARIABLE
  variable: VarNode
}

export interface VarNode {
  var: Token
  name: Token
  colon?: Token
  type?: TypeExprNode
  assign?: Token
  value?: ExprNode
}

export interface MainDeclNode {
  kind: DeclKind.MAIN
  body: BlockStatementNode
}

export interface ParamsNode {
  params: ParamGroup[]
  commas: Token[]
}

export interface ParamGroup {
  name: Token // TODO: improve this, introduce multiple args grouping like golang.
  colon: Token
  type: TypeExprNode
}

export type TypeExprNode = PrimitiveTypeNode | ArrayTypeNode

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
  array: Token
  openSquare: Token
  dimension: CommaSeparatedExpr // capture the commas position
  closeSquare: Token
  of: Token
  type: TypeExprNode
}

export type StatementNode = VarStatementNode |
AssignStatementNode |
ReturnStatementNode |
IfStatementNode |
WhileStatementNode |
BlockStatementNode |
ExprStatementNode

export enum StatementNodeKind {
  VAR,
  ASSIGN,
  RETURN,
  IF,
  WHILE,
  BLOCK,
  EXPR,
}

export interface VarStatementNode {
  kind: StatementNodeKind.VAR
  variable: VarNode
}

export interface AssignStatementNode {
  kind: StatementNodeKind.ASSIGN
  receiver: ExprNode
  assign: Token
  value: ExprNode
}

export interface ReturnStatementNode {
  kind: StatementNodeKind.RETURN
  return: Token
  value?: ExprNode
}

export interface IfStatementNode {
  kind: StatementNodeKind.IF
  if: Token
  condition: ExprNode
  then: Token
  body: StatementNode
  else?: StatementNode
}

export interface WhileStatementNode {
  kind: StatementNodeKind.WHILE
  while: Token
  condition: ExprNode
  do: Token
  body: StatementNode
}

export interface BlockStatementNode {
  kind: StatementNodeKind.BLOCK
  begin: Token
  statements: StatementNode[]
  end: Token
}

export interface ExprStatementNode {
  kind: StatementNodeKind.EXPR
  expr: ExprNode
}

export type ExprNode =
IdentExprNode |
IntegerLitExprNode |
BooleanLitExprNode |
BinaryExprNode |
UnaryExprNode |
CallExprNode |
ArrayIndexExprNode |
CastExprNode |
GroupedExprNode

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
  GROUPED,
}

export interface IdentExprNode {
  kind: ExprKind.IDENT
  name: Token
}

export interface IntegerLitExprNode {
  kind: ExprKind.INTEGER_LIT
  value: Token
}

export interface BooleanLitExprNode {
  kind: ExprKind.BOOLEAN_LIT
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
  callee: ExprNode
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

export interface GroupedExprNode {
  kind: ExprKind.GROUPED
  openBrac: Token
  value: ExprNode
  closeBrac: Token
}
