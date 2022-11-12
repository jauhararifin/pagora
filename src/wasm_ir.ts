// WASM. Reference: https://webassembly.github.io/spec/core/syntax/index.html

// Values. Reference: https://webassembly.github.io/spec/core/syntax/values.html

// Reference: https://webassembly.github.io/spec/core/syntax/values.html#bytes
export type Byte = number

// Reference: https://webassembly.github.io/spec/core/syntax/values.html#integers
export type I64 = bigint
export type U32 = number

// Reference: https://webassembly.github.io/spec/core/syntax/values.html#names
export type Name = string

// Types. Reference: https://webassembly.github.io/spec/core/syntax/types.html

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#number-types
export type NumType = 'i32' | 'i64' | 'f32' | 'f64'

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#reference-types
export type RefType = 'funcref' | 'externref'

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#value-types
export type ValType = NumType | RefType

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#result-types
export type ResultType = [ValType[]]

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#function-types
export type FunctionType = [ResultType, '->', ResultType]

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#limits
export interface Limits {min: U32, max?: U32}

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#memory-types
export type MemType = Limits

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#table-types
export type TableType = [Limits, RefType]

// Reference: https://webassembly.github.io/spec/core/syntax/types.html#global-types
export type GlobalType = [Mut, ValType]
export type Mut = 'const' | 'var'

// Instructions. Reference: https://webassembly.github.io/spec/core/syntax/instructions.html
export type Instr = NumericInstr | ReferenceInstr | ParametricInstr | VariableInstr | TableInstr | MemoryInstr
| ControlInstr

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#numeric-instructions
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

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#reference-instructions
export type ReferenceInstr = ['ref.null', RefType] | ['ref.is_null'] | ['ref.func', FuncIdx]

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#parametric-instructions
export type ParametricInstr = ['drop']

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#variable-instructions
export type VariableInstr = ['local.get', LocalIdx] | ['local.set', LocalIdx] | ['local.tee', LocalIdx]
| ['global.get', GlobalIdx] | ['global.set', GlobalIdx]

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#table-instructions
export type TableInstr = ['table.get', TableIdx] | ['table.set', TableIdx] | ['table.size', TableIdx]
| ['table.grow', TableIdx] | ['table.fill', TableIdx] | ['table.copy', TableIdx, TableIdx]
| ['table.init', TableIdx, ElemIdx] | ['table.drop', ElemIdx]

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#memory-instructions
export type MemoryInstr = ['i64.load', MemArg] | ['i64.store', MemArg]
| ['memory.size'] | ['memory.grow'] | ['memory.fill'] | ['memory.copy'] | ['memory.init', DataIdx]
export interface MemArg {offset: U32, align: U32}

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
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

// Reference: https://webassembly.github.io/spec/core/syntax/instructions.html#expressions
export type Expr = [Instr[], 'end']

// Modules. Reference: https://webassembly.github.io/spec/core/syntax/modules.html
export interface Module {
  types: FunctionType[]
  funcs: Func[]
  tables: Table[]
  globals: Global[]
  mems: Mem[]
  exports: Export[]
}

// Reference: https://webassembly.github.io/spec/core/syntax/modules.html#indices
export type TypeIdx = U32
export type FuncIdx = U32
export type TableIdx = U32
export type MemIdx = U32
export type GlobalIdx = U32
export type ElemIdx = U32
export type DataIdx = U32
export type LocalIdx = U32
export type LabelIdx = U32

// Reference: https://webassembly.github.io/spec/core/syntax/modules.html#functions
export interface Func {
  type: TypeIdx
  locals: ValType[]
  body: Expr
}

// Reference: https://webassembly.github.io/spec/core/syntax/modules.html#tables
export interface Table {type: TableType}

// Reference: https://webassembly.github.io/spec/core/syntax/modules.html#memories
export interface Mem {type: MemType}

// Reference: https://webassembly.github.io/spec/core/syntax/modules.html#globals
export interface Global {type: GlobalType, init: Expr}

// Reference: https://webassembly.github.io/spec/core/syntax/modules.html#exports
export interface Export {
  name: Name
  desc: ExportDesc
}
export type ExportDesc = ['func', FuncIdx] | ['table', TableIdx] | ['mem', MemIdx] | ['global', GlobalIdx]
