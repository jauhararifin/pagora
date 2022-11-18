import {
  BlockType,
  Export,
  ExportDesc,
  Expr,
  Func,
  FunctionType,
  Instr,
  MemArg,
  Module,
  NumType,
  RefType,
  ResultType,
  ValType,
} from './wasm'

// Reference: https://webassembly.github.io/spec/core/binary/conventions.html#vectors
function encodeVec<T>(content: T[], encoder: (v: T) => number[]): number[] {
  return [...encodeU32(content.length), ...content.flatMap(encoder)]
}

// Integers. Reference: https://webassembly.github.io/spec/core/binary/values.html#integers

function encodeU32(n: number): number[] {
  if (n === 0) return [0]

  const result: number[] = []
  while (n > 0) {
    result.push((n & 0b01111111) | 0b10000000)
    n = n >> 7
  }
  result[result.length - 1] &= 0b01111111
  return result.reverse()
}

function encodeI64(n: bigint): number[] {
  const zero = BigInt(0)
  if (n === zero) return [0]

  const result: number[] = []
  while (n > zero) {
    result.push(Number((n & BigInt(0b01111111)) | BigInt(0b10000000)))
    n = n / BigInt(128)
  }
  result[result.length - 1] &= 0b01111111
  return result.reverse()
}

// Types. Reference: https://webassembly.github.io/spec/core/binary/types.html#

// Reference: https://webassembly.github.io/spec/core/binary/types.html#number-types
function encodeNumType(t: NumType): number[] {
  switch (t) {
    case 'i32':
      return [0x7f]
    case 'i64':
      return [0x7e]
    case 'f32':
      return [0x7d]
    case 'f64':
      return [0x7c]
  }
}

// Reference: https://webassembly.github.io/spec/core/binary/types.html#reference-types
function encodeRefType(t: RefType): number[] {
  switch (t) {
    case 'funcref':
      return [0x70]
    case 'externref':
      return [0x6f]
  }
}

// Reference: https://webassembly.github.io/spec/core/binary/types.html#value-types
function encodeValType(t: ValType): number[] {
  switch (t) {
    case 'i32':
    case 'i64':
    case 'f32':
    case 'f64':
      return encodeNumType(t)
    case 'funcref':
    case 'externref':
      return encodeRefType(t)
  }
}

// Reference: https://webassembly.github.io/spec/core/binary/types.html#result-types
function encodeResultType(type: ResultType): number[] {
  return [...encodeU32(type[0].length), ...type[0].flatMap(encodeValType)]
}

// Reference: https://webassembly.github.io/spec/core/binary/types.html#function-types
function encodeFuncType(type: FunctionType): number[] {
  return [0x60, ...encodeResultType(type[0]), ...encodeResultType(type[2])]
}

// Instruction. Reference: https://webassembly.github.io/spec/core/binary/instructions.html
function encodeInstr(instr: Instr): number[] {
  const [op, arg1, arg2, , arg4] = instr
  switch (op) {
    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
    case 'unreachable':
      return [0x00]
    case 'nop':
      return [0x01]
    case 'block':
      return [
        0x02,
        ...encodeBlockType(arg1),
        ...arg2.flatMap(encodeInstr),
        0x0b,
      ]
    case 'loop':
      return [
        0x03,
        ...encodeBlockType(arg1),
        ...arg2.flatMap(encodeInstr),
        0x0b,
      ]
    case 'if':
      return [
        0x04,
        ...encodeBlockType(arg1),
        ...arg2.flatMap(encodeInstr),
        ...(arg4.length > 0 ? [0x05, ...arg4.flatMap(encodeInstr)] : []),
        0x0b,
      ]
    case 'br':
      return [0x0c, ...encodeU32(arg1)]
    case 'br_if':
      return [0x0d, ...encodeU32(arg1)]
    case 'br_table':
      return [0x0e, ...encodeVec(arg1, encodeU32), ...encodeU32(arg2)]
    case 'return':
      return [0x0f]
    case 'call':
      return [0x10, ...encodeU32(arg1)]
    case 'call_indirect':
      return [0x11, ...encodeU32(arg2), ...encodeU32(arg1)]

    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#reference-instructions
    case 'ref.null':
      return [0xd0, ...encodeRefType(arg1)]
    case 'ref.is_null':
      return [0xd1]
    case 'ref.func':
      return [0xd2, ...encodeU32(arg1)]

    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#parametric-instructions
    case 'drop':
      return [0x1a]

    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#variable-instructions
    case 'local.get':
      return [0x20, ...encodeU32(arg1)]
    case 'local.set':
      return [0x21, ...encodeU32(arg1)]
    case 'local.tee':
      return [0x22, ...encodeU32(arg1)]
    case 'global.get':
      return [0x23, ...encodeU32(arg1)]
    case 'global.set':
      return [0x24, ...encodeU32(arg1)]

    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#table-instructions
    case 'table.get':
      return [0x25, ...encodeU32(arg1)]
    case 'table.set':
      return [0x26, ...encodeU32(arg1)]
    case 'table.init':
      return [0xfc, 12, ...encodeU32(arg2), ...encodeU32(arg1)]
    case 'table.copy':
      return [0xfc, 14, ...encodeU32(arg1), ...encodeU32(arg2)]
    case 'table.grow':
      return [0xfc, 15, ...encodeU32(arg1)]
    case 'table.size':
      return [0xfc, 16, ...encodeU32(arg1)]
    case 'table.fill':
      return [0xfc, 17, ...encodeU32(arg1)]

    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#memory-instructions
    case 'i64.load':
      return [0x29, ...encodeMemarg(arg1)]
    case 'i64.store':
      return [0x37, ...encodeMemarg(arg1)]
    case 'memory.size':
      return [0x3f, 0x00]
    case 'memory.grow':
      return [0x40, 0x00]
    case 'memory.fill':
      return [0xfc, 11, 0x00]
    case 'memory.copy':
      return [0xfc, 10, 0x00, 0x00]
    case 'memory.init':
      return [0xfc, 8, ...encodeU32(arg1), 0x00]

    // reference: https://webassembly.github.io/spec/core/binary/instructions.html#numeric-instructions
    case 'i64.const':
      return [0x42, ...encodeI64(arg1)]
    case 'i64.clz':
      return [0x79]
    case 'i64.ctz':
      return [0x7a]
    case 'i64.popcnt':
      return [0x7b]
    case 'i64.add':
      return [0x7c]
    case 'i64.sub':
      return [0x7d]
    case 'i64.mul':
      return [0x7e]
    case 'i64.div_s':
      return [0x7f]
    case 'i64.div_u':
      return [0x80]
    case 'i64.rem_s':
      return [0x81]
    case 'i64.rem_u':
      return [0x82]
    case 'i64.and':
      return [0x83]
    case 'i64.or':
      return [0x84]
    case 'i64.xor':
      return [0x85]
    case 'i64.shl':
      return [0x86]
    case 'i64.shl_s':
      return [0x87]
    case 'i64.shl_u':
      return [0x88]
    case 'i64.rotl':
      return [0x89]
    case 'i64.rotr':
      return [0x90]
    case 'i64.eqz':
      return [0x50]
    case 'i64.eq':
      return [0x51]
    case 'i64.ne':
      return [0x52]
    case 'i64.lt_s':
      return [0x53]
    case 'i64.lt_u':
      return [0x54]
    case 'i64.gt_s':
      return [0x55]
    case 'i64.gt_u':
      return [0x56]
    case 'i64.le_s':
      return [0x57]
    case 'i64.le_u':
      return [0x58]
    case 'i64.ge_s':
      return [0x59]
    case 'i64.ge_u':
      return [0x5a]
  }

  throw new Error(`unknown instruction ${op}`)
}

// Reference: https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
function encodeBlockType(blockType: BlockType): number[] {
  if (blockType === undefined) return [0x40]

  switch (blockType) {
    case 'i32':
    case 'i64':
    case 'f32':
    case 'f64':
      return encodeValType(blockType)
    case 'funcref':
    case 'externref':
      return encodeRefType(blockType)
  }

  // TODO: implement s33
  return []
}

// Reference: https://webassembly.github.io/spec/core/binary/instructions.html#memory-instructions
function encodeMemarg(memarg: MemArg): number[] {
  return [...encodeU32(memarg.align), ...encodeU32(memarg.offset)]
}

// Reference: https://webassembly.github.io/spec/core/binary/instructions.html#expressions
function encodeExpr(expr: Expr): number[] {
  return [...expr[0].flatMap(encodeInstr), 0x0b]
}

// Modules. Reference: https://webassembly.github.io/spec/core/binary/modules.html#sections

// Reference: https://webassembly.github.io/spec/core/binary/modules.html#sections
function encodeSection(id: number, content: number[]): number[] {
  return [id, ...encodeU32(content.length), ...content]
}

// Reference: https://webassembly.github.io/spec/core/binary/modules.html#type-section
function encodeTypeSec(types: FunctionType[]): number[] {
  return encodeSection(0x01, encodeVec(types, encodeFuncType))
}

// Reference: https://webassembly.github.io/spec/core/binary/modules.html#function-section
function encodeFuncSec(funcs: Func[]): number[] {
  return encodeSection(
    0x03,
    encodeVec(
      funcs.map((f) => f.type),
      encodeU32
    )
  )
}

// Reference: https://webassembly.github.io/spec/core/binary/modules.html#export-section
function encodeExportSec(exports: Export[]): number[] {
  return encodeSection(0x07, encodeVec(exports, encodeExport))
}

function encodeExport(exp: Export): number[] {
  return [...encodeName(exp.name), ...encodeExportDesc(exp.desc)]
}

function encodeName(name: string): number[] {
  // TODO: check if this is correct way to do it.
  const v = Array.from(new TextEncoder().encode(name))
  return [...encodeU32(v.length), ...v]
}

function encodeExportDesc(desc: ExportDesc): number[] {
  const [t, id] = desc
  const c = encodeU32(id)
  switch (t) {
    case 'func':
      return [0x00, ...c]
    case 'table':
      return [0x01, ...c]
    case 'mem':
      return [0x02, ...c]
    case 'global':
      return [0x03, ...c]
  }
}

// Reference: https://webassembly.github.io/spec/core/binary/modules.html#code-section
function encodeCodeSec(funcs: Func[]): number[] {
  return encodeSection(0x0a, encodeVec(funcs, encodeCode))
}

function encodeCode(func: Func): number[] {
  const f = encodeFunc(func)
  return [...encodeU32(f.length), ...f]
}

function encodeFunc(func: Func): number[] {
  // TODO(jauhararifin): technically we can combine multiple locals with the same type together to reduce the binary
  // size. But, let's make it simple for now.
  return [...encodeVec(func.locals, encodeLocal), ...encodeExpr(func.body)]
}

function encodeLocal(local: ValType): number[] {
  return [...encodeU32(1), ...encodeValType(local)]
}

// Reference: https://webassembly.github.io/spec/core/binary/modules.html#binary-module
export function encodeModule(mod: Module): Uint8Array {
  return new Uint8Array([
    0x00,
    0x61,
    0x73,
    0x6d,
    0x01,
    0x00,
    0x00,
    0x00,
    ...encodeTypeSec(mod.types),
    ...encodeFuncSec(mod.funcs),
    ...encodeExportSec(mod.exports),
    ...encodeCodeSec(mod.funcs),
  ])
}
