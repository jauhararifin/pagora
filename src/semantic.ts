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

export enum StatementKind {
  Block,
  If,
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

export type Expr = null
