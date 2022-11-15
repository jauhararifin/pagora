import { analyze } from './analyzer'
import { parse } from './parser'
import { tokenize } from './lexer'
import { generateWasmModule } from './codegen'

const program1Source = `
function aplusb(a: integer, b: integer) -> integer
begin
    return a + b;
end

begin
    var a: integer := 10;
    var b: integer := 20;
    aplusb(a, b);
end
`

const program1 = {
  types: [[[['i32']], '->', [['i32']]]],
  funcs: [
    {
      type: 0,
      locals: ['i32'],
      body: [[['local.get', 0]], 'end'],
    },
  ],
  globals: [{ init: [[['i32.const', 0]], 'end'], type: ['var', 'i32'] }],
  mems: [],
  tables: [],
  exports: [],
}

interface Testcase {
  name: string
  sourceCode: string
  expectedResult?: any
}

describe('analyzer test', () => {
  const testcases: Testcase[] = [
    {
      name: 'program 1',
      sourceCode: program1Source,
      expectedResult: program1,
    },
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      const tokens = tokenize(testcase.sourceCode)
      const ast = parse(tokens)
      const program = analyze(ast)
      const module = generateWasmModule(program)
      expect(module).toStrictEqual(testcase.expectedResult)
    })
  }
})
