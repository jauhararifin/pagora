export type Value =
  | BooleanValue
  | IntegerValue
  | RealValue
  | StringValue
  | ArrayValue
  | FuncValue
  | VoidValue

export enum ValueKind {
  BOOLEAN = 'BOOLEAN',
  INTEGER = 'INTEGER',
  REAL = 'REAL',
  STRING = 'STRING',
  ARRAY = 'ARRAY',
  FUNC = 'FUNC',
  VOID = 'VOID',
}

export interface BooleanValue {
  kind: ValueKind.BOOLEAN
  value: boolean
}

export interface IntegerValue {
  kind: ValueKind.INTEGER
  value: bigint
}

export interface RealValue {
  kind: ValueKind.REAL
  value: number
}

export interface StringValue {
  kind: ValueKind.STRING
  value: string
}

export interface ArrayValue {
  kind: ValueKind.ARRAY
  value: Value[]
}

export interface FuncValue {
  kind: ValueKind.FUNC
  value: (args: Value[]) => Value
}

export interface VoidValue {
  kind: ValueKind.VOID
  value: any
}
