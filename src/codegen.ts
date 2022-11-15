import { Func, FunctionType, Global, Instr, Module } from './wasm'
import { Program, Type, TypeKind } from './semantic'

export function generateWasmModule(program: Program): Module {
  return new CodeGenerator().generate(program)
}

class CodeGenerator {
  globals: Global[]
  globalNameToIdx: { [key: string]: number }
  functions: Func[]
  functionNameToIdx: { [key: string]: number }
  funcTypes: FunctionType[]

  initStatements: Instr[]

  constructor() {
    this.globals = []
    this.globalNameToIdx = {}
    this.functions = []
    this.functionNameToIdx = {}
    this.funcTypes = []
    this.initStatements = []
  }

  generate(program: Program): Module {
    this.init()
    this.generateGlobals(program)
    return {
      types: this.funcTypes,
      funcs: this.functions,
      tables: [],
      globals: this.globals,
      mems: [],
      exports: [],
    }
  }

  init(): void {
    this.addGlobal('@lastAllocated', {
      type: ['var', 'i32'],
      init: [[['i32.const', 0]], 'end'],
    })
    const funcType = this.addFuncType([[['i32']], '->', [['i32']]])
    this.addFunction('@allocate', {
      type: funcType,
      locals: ['i32'],
      body: [[['local.get', 0]], 'end'],
    })
  }

  addFuncType(t: FunctionType): number {
    const i = this.funcTypes.length
    this.funcTypes.push(t)
    return i
  }

  addGlobal(name: string, global: Global): number {
    if (name in this.globalNameToIdx) {
      throw new Error(`Global ${name} is already defined`)
    }

    const index = this.globals.length
    this.globalNameToIdx[name] = index
    this.globals.push(global)
    return index
  }

  addFunction(name: string, f: Func): number {
    if (name in this.functionNameToIdx) {
      throw new Error(`Function ${name} is already defined`)
    }

    const index = this.functions.length
    this.functionNameToIdx[name] = index
    this.functions.push(f)
    return index
  }

  getFunction(name: string): number {
    if (!(name in this.functionNameToIdx)) {
      throw new Error(`Function ${name} is not defined`)
    }
    return this.functionNameToIdx[name]
  }

  generateGlobals(program: Program): void {
    const allocateFuncId = this.getFunction('@allocate')

    for (const global of program.globals) {
      switch (global.type.kind) {
        case TypeKind.INTEGER: {
          this.globals.push({
            type: ['var', 'i64'],
            init: [[['i64.const', BigInt(0)]], 'end'],
          })
          break
        }

        case TypeKind.ARRAY: {
          const varIndex = this.globals.push({
            type: ['var', 'i32'],
            init: [[['i32.const', 0]], 'end'],
          })

          const size = this.getTypeSize(global.type)
          this.initStatements.push(['i32.const', size])
          this.initStatements.push(['call', allocateFuncId])
          this.initStatements.push(['global.set', varIndex])
          break
        }

        default: {
          throw new Error(
            'Only arrays and integers are supported for global. For now.'
          )
        }
      }
    }
  }

  getTypeSize(typ: Type): number {
    switch (typ.kind) {
      case TypeKind.INTEGER:
        return 8
      case TypeKind.REAL:
        return 8
      case TypeKind.BOOLEAN:
        return 1
      case TypeKind.BYTE:
        return 1
      case TypeKind.ARRAY:
        return (
          Number(typ.dimension.reduce((p, v) => p * v)) *
          this.getTypeSize(typ.type)
        )
      default:
        throw new Error('Other types are not yet implemented')
    }
  }
}
