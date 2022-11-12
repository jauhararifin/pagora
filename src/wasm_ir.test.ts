import { Module } from './wasm_ir'
import { encodeModule } from './encoder'

const simpleProgram: Module = {
  types: [
    [['i64', 'i64'], '->', ['i64']]
  ],
  funcs: [
    {
      type: 0,
      locals: [],
      body: [[
        ['local.get', 0],
        ['local.get', 1],
        ['i64.add']
      ], 'end']
    }
  ],
  tables: [],
  globals: [],
  mems: [],
  exports: [
    {
      name: 'addTwo',
      desc: ['func', 0]
    }
  ]
}

const simpleProgramBin = [
  0x00, 0x61, 0x73, 0x6d, // magic number
  0x01, 0x00, 0x00, 0x00, // version

  0x01, // section type = func type
  0x07, // section size
  0x01, // there is 1 function type
  0x60, // func
  0x02, // with 2 input
  0x7e, // i64
  0x7e, // i64
  0x01, // with 1 output
  0x7e, // i64

  0x03, // section type = func
  0x02, // section size
  0x01, // there are 2 functions
  0x00, // func 0 type is in idx 0

  0x0a, // section type = code
  0x09, // size of the section
  0x01, // there is 1 function
  0x07, // function size
  0x00, // there is 0 locals
  0x20, 0x00, // local.get 0
  0x20, 0x01, // local.get 1
  0x7c, // i64.add
  0x0b // end
]

describe('encode', () => {
  it('jauhar', () => {
    const encoded = encodeModule(simpleProgram)
    expect(encoded).toStrictEqual(new Uint8Array(simpleProgramBin))
  })
})
