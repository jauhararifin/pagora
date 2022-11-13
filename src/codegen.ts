// import { Global, Module } from './wasm_ir'
// import { Program, Variable } from './semantic'

// export function generateWasmModule (program: Program): Module {
//   return {
//     types: [],
//     funcs: [],
//     tables: [],
//     globals: [],
//     mems: [],
//     exports: []
//   }
// }

// class CodeGenerator {
//   globals: Global[]
//   globalNameToIdx: { [key: string]: number }

//   constructor () {
//     this.globals = []
//     this.globalNameToIdx = {}
//   }

//   generate (program: Program): Module {
//     for (const global of program.globals) {
//       global.value?.kind
//     }

//     return {
//       types: [],
//       funcs: [],
//       tables: [],
//       globals: this.globals,
//       mems: [],
//       exports: []
//     }
//   }
// }
