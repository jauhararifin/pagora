import { Token } from './tokens'

export interface RootNode {
  declarations: DeclNode[]
}

export type DeclNode = FunctionDeclNode | VariableDeclNode | MainDeclNode

export enum DeclKind {
  FUNCTION = 'FUNCTION',
  VARIABLE = 'VARIABLE',
  MAIN = 'MAIN',
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

export enum TypeExprNodeKind {
  PRIMITIVE = 'PRIMITIVE',
  ARRAY = 'ARRAY',
  STRUCT = 'STRUCT',
  TUPLE = 'TUPLE',
}

export interface PrimitiveTypeNode {
  kind: TypeExprNodeKind.PRIMITIVE
  type: Token
}

export interface ArrayTypeNode {
  kind: TypeExprNodeKind.ARRAY
  array: Token
  openSquare: Token
  dimension: CommaSeparatedExpr // capture the commas position
  closeSquare: Token
  of: Token
  type: TypeExprNode
}

export type StatementNode =
  | VarStatementNode
  | AssignStatementNode
  | ReturnStatementNode
  | ContinueStatementNode
  | BreakStatementNode
  | IfStatementNode
  | WhileStatementNode
  | BlockStatementNode
  | ExprStatementNode

export enum StatementNodeKind {
  VAR = 'VAR',
  ASSIGN = 'ASSIGN',
  RETURN = 'RETURN',
  IF = 'IF',
  WHILE = 'WHILE',
  CONTINUE = 'CONTINUE',
  BREAK = 'BREAK',
  BLOCK = 'BLOCK',
  EXPR = 'EXPR',
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

export interface ContinueStatementNode {
  kind: StatementNodeKind.CONTINUE
  continue: Token
}

export interface BreakStatementNode {
  kind: StatementNodeKind.BREAK
  break: Token
}

export interface IfStatementNode {
  kind: StatementNodeKind.IF
  if: Token
  condition: ExprNode
  then: Token
  body: StatementNode
  else?: Token
  elseBody?: StatementNode
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
  | IdentExprNode
  | IntegerLitExprNode
  | BooleanLitExprNode
  | ArrayLitExprNode
  | StringLitExprNode
  | BinaryExprNode
  | UnaryExprNode
  | CallExprNode
  | ArrayIndexExprNode
  | CastExprNode
  | GroupedExprNode

export enum ExprNodeKind {
  IDENT = 'IDENT',
  INTEGER_LIT = 'INTEGER_LIT',
  FLOAT_LIT = 'FLOAT_LIT',
  BOOLEAN_LIT = 'BOOLEAN_LIT',
  ARRAY_LIT = 'ARRAY_LIT',
  TUPLE_LIT = 'TUPLE_LIT',
  STRING_LIT = 'STRING_LIT',
  BINARY = 'BINARY',
  UNARY = 'UNARY',
  CALL = 'CALL',
  ARRAY_INDEX = 'ARRAY_INDEX',
  TUPLE_INDEX = 'TUPLE_INDEX',
  STRUCT_INDEX = 'STRUCT_INDEX',
  CAST = 'CAST',
  GROUPED = 'GROUPED',
}

export interface IdentExprNode {
  kind: ExprNodeKind.IDENT
  name: Token
}

export interface IntegerLitExprNode {
  kind: ExprNodeKind.INTEGER_LIT
  value: Token
}

export interface BooleanLitExprNode {
  kind: ExprNodeKind.BOOLEAN_LIT
  value: Token
}

export interface StringLitExprNode {
  kind: ExprNodeKind.STRING_LIT
  value: Token
}

export interface ArrayLitExprNode {
  kind: ExprNodeKind.ARRAY_LIT
  openSquare: Token
  value: CommaSeparatedExpr
  closeSquare: Token
}

export interface BinaryExprNode {
  kind: ExprNodeKind.BINARY
  a: ExprNode
  op: Token
  b: ExprNode
}

export interface UnaryExprNode {
  kind: ExprNodeKind.UNARY
  op: Token
  value: ExprNode
}

export interface CallExprNode {
  kind: ExprNodeKind.CALL
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
  kind: ExprNodeKind.ARRAY_INDEX
  array: ExprNode
  openSquare: Token
  index: CommaSeparatedExpr
  closeSquare: Token
}

// TODO: currently it's not supported yet.
export interface TupleIndexExprNode {
  kind: ExprNodeKind.TUPLE_INDEX
  tuple: ExprNode
  dot: Token
  index: ExprNode
}

// TODO: currently it's not supported yet.
export interface StructIndexExprNode {
  kind: ExprNodeKind.STRUCT_INDEX
  struct: ExprNode
  dot: Token
  index: Token
}

export interface CastExprNode {
  kind: ExprNodeKind.CAST
  source: ExprNode
  as: Token
  target: TypeExprNode
}

export interface GroupedExprNode {
  kind: ExprNodeKind.GROUPED
  openBrac: Token
  value: ExprNode
  closeBrac: Token
}
