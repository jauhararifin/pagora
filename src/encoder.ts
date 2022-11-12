import { FunctionType, Module, ResultType, ValType } from './wasm_ir'

export function encodeModule (mod: Module): Uint8Array {
  return new Uint8Array([
    0x00, 0x61, 0x73, 0x6d,
    0x01, 0x00, 0x00, 0x00,
    ...encodeFuncTypeSec(mod.types)
  ])
}

function encodeFuncTypeSec (types: FunctionType[]): number[] {
  const funcTypes = [
    ...encodeU32(types.length),
    ...types.flatMap(encodeFuncType)
  ]
  return [
    0x01,
    ...encodeU32(funcTypes.length),
    ...funcTypes
  ]
}

function encodeFuncType (type: FunctionType): number[] {
  return [
    0x60,
    ...encodeResultType(type[0]),
    ...encodeResultType(type[2])
  ]
}

function encodeResultType (type: ResultType): number[] {
  return [
    ...encodeU32(type.length),
    ...type.flatMap(encodeValType)
  ]
}

function encodeValType (t: ValType): number[] {
  switch (t) {
    case 'i32':
      return [0x7f]
    case 'i64':
      return [0x7e]
    case 'f32':
      return [0x7d]
    case 'f64':
      return [0x7c]
    case 'funcref':
      return [0x70]
    case 'externref':
      return [0x6f]
  }
}

function encodeU32 (n: number): number[] {
  if (n === 0) return [0]

  const result: number[] = []
  while (n > 0) {
    result.push((n & 0b01111111) | (0b10000000))
    n = n >> 7
  }
  result[result.length - 1] &= 0b01111111
  return result.reverse()
}
