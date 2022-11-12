// reference: https://webassembly.github.io/spec/core/syntax/index.html

// Types

export type NumType = 'i32' | 'i64' | 'f32' | 'f64'

export type RefType = 'funcref' | 'externref'

export type ValType = NumType | RefType

export type ResultType = ValType[]

export type FunctionType = [ResultType, '->', ResultType]

export interface Limits {min: number, max?: number}

export type MemType = Limits

export type TableType = [Limits, RefType]

export type GlobalType = [Mut, ValType]

export type Mut = 'const' | 'var'

// Values

export type I64 = BigInt

export type Name = string

// Instructions

export type Instr = NumericInstr | ReferenceInstr | ParametricInstr | VariableInstr | TableInstr | MemoryInstr
| ControlInstr

export type NumericInstr = ['i64.const', I64]
// unary
| ['i64.clz'] | ['i64.ctz'] | ['i64.popcnt']
// binary
| ['i64.add'] | ['i64.sub'] | ['i64.mul'] | ['i64.div_u'] | ['i64.div_s'] | ['i64.rem_u'] | ['i64.rem_s'] | ['i64.and']
| ['i64.or'] | ['i64.xor'] | ['i64.shl'] | ['i64.shl_u'] | ['i64.shl_s'] | ['i64.rotl'] | ['i64.rotr']
// test
| ['i64.eqz']
// comparison
| ['i64.eq'] | ['i64.ne'] | ['i64.lt_u'] | ['i64.lt_s'] | ['i64.gt_u'] | ['i64.gt_s'] | ['i64.le_u'] | ['i64.le_s']
| ['i64.ge_u'] | ['i64.ge_s']

export type ReferenceInstr = ['ref.null', RefType] | ['ref.is_null'] | ['ref.func', FuncIdx]

export type ParametricInstr = ['drop']

export type VariableInstr = ['local.get', LocalIdx] | ['local.set', LocalIdx] | ['local.tee', LocalIdx]
| ['global.get', GlobalIdx] | ['global.set', GlobalIdx]

export type TableInstr = ['table.get', TableIdx] | ['table.set', TableIdx] | ['table.size', TableIdx]
| ['table.grow', TableIdx] | ['table.fill', TableIdx] | ['table.copy', TableIdx, TableIdx]
| ['table.init', TableIdx, ElemIdx] | ['table.drop', ElemIdx]

export type MemoryInstr = ['i64.load', MemArg] | ['i64.store', MemArg]
| ['memory.size'] | ['memory.grow'] | ['memory.fill'] | ['memory.copy'] | ['memory.init', DataIdx]

export interface MemArg {offset: number, align: number}

export type ControlInstr = ['nop']
| ['unreachable']
| ['block', BlockType, Instr[], 'end']
| ['loop', BlockType, Instr[], 'end']
| ['if', BlockType, Instr[], 'else', Instr[], 'end']
| ['br', LabelIdx]
| ['br_if', LabelIdx]
| ['br_table', LabelIdx[], LabelIdx]
| ['return']
| ['call', FuncIdx]
| ['call_indirect', TableIdx, TypeIdx]

export type BlockType = TypeIdx | ValType | undefined

export type Expr = [Instr[], 'end']

// Modules

export interface Module {
  types: FunctionType[]
  funcs: Func[]
  tables: Table[]
  globals: Global[]
  mems: Mem[]
  exports: Export[]
}

export type TypeIdx = number
export type FuncIdx = number
export type TableIdx = number
export type MemIdx = number
export type GlobalIdx = number
export type ElemIdx = number
export type DataIdx = number
export type LocalIdx = number
export type LabelIdx = number

export interface Func {
  type: TypeIdx
  locals: ValType[]
  body: Expr
}

export interface Table {type: TableType}

export interface Mem {type: MemType}

export interface Global {type: GlobalType, init: Expr}

export interface Export {
  name: Name
  desc: ExportDesc
}

export type ExportDesc = ['func', FuncIdx] | ['table', TableIdx] | ['mem', MemIdx] | ['global', GlobalIdx]
