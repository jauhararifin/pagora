import { Error, ErrorKind } from './errors'
import { analyze } from './analyzer'
import { encodeProgram } from './semantic_util'
import { parse } from './parser'
import { tokenize } from './lexer'

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

const program1 = [
  ['func', 'aplusb', [['a', 'INTEGER'], ['b', 'INTEGER']], 'INTEGER', [
    ['return', ['PLUS', ['ident', 'a'], ['ident', 'b']]]
  ]],
  ['main', [
    ['var', 'a', 'INTEGER', '10'],
    ['var', 'b', 'INTEGER', '20'],
    ['call', ['ident', 'aplusb'], [['ident', 'a'], ['ident', 'b']]]
  ]]
]

const program2Source = `
begin
  var n := 10

  var i := 0
  while i < n do
  begin
    var j := 0
    while j < n-i-1 do
    begin
      j := j + 1
    end

    j := 0
    while j < i*2+1 do
    begin
      j := j + 1
    end

    i := i + 1
  end
end
`

const program2 = [
  ['main', [
    ['var', 'n', 'INTEGER', '10'],
    ['var', 'i', 'INTEGER', '0'],
    ['while', ['LESS_THAN', ['ident', 'i'], ['ident', 'n']], [
      ['var', 'j', 'INTEGER', '0'],
      ['while', ['LESS_THAN', ['ident', 'j'], ['MINUS', ['ident', 'n'], ['MINUS', ['ident', 'i'], '1']]], [
        ['assign', ['ident', 'j'], ['PLUS', ['ident', 'j'], '1']]
      ]],
      ['assign', ['ident', 'j'], '0'],
      ['while', ['LESS_THAN', ['ident', 'j'], ['PLUS', ['MUL', ['ident', 'i'], '2'], '1']], [
        ['assign', ['ident', 'j'], ['PLUS', ['ident', 'j'], '1']]
      ]],
      ['assign', ['ident', 'i'], ['PLUS', ['ident', 'i'], '1']]
    ]]
  ]]
]

const program3Source = `
var some_array: array [10,10] of integer
begin
  some_array[10,10] := 2
end
`

const program3 = [
  ['var', 'some_array', ['array', ['10', '10'], 'INTEGER'], undefined],
  ['main', [
    ['assign', ['index', ['ident', 'some_array'], ['10', '10']], '2']
  ]]
]

const arrayDimMismatchSource = `
var some_array: array [10,10,10] of integer
begin
  some_array[0,0] := 2
end
`

const arrayDimMismatchError: Error[] = [{
  kind: ErrorKind.WRONG_NUMBER_OF_INDEX,
  expected: 3,
  got: 2
}]

interface Testcase {
  name: string
  sourceCode: string
  expectedProgram?: any
  expectedErrors: Error[]
}

describe('analyzer test', () => {
  const testcases: Testcase[] = [{
    name: 'program 1',
    sourceCode: program1Source,
    expectedProgram: program1,
    expectedErrors: []
  }, {
    name: 'program 2',
    sourceCode: program2Source,
    expectedProgram: program2,
    expectedErrors: []
  }, {
    name: 'program 3',
    sourceCode: program3Source,
    expectedProgram: program3,
    expectedErrors: []
  }, {
    name: 'array dimension mismatch',
    sourceCode: arrayDimMismatchSource,
    expectedErrors: arrayDimMismatchError
  }]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      const { value: tokens, errors: scanErrors } = tokenize(testcase.sourceCode)
      if (scanErrors.length > 0) {
        expect(scanErrors).toStrictEqual(testcase.expectedErrors)
        return
      }

      if (tokens == null) fail()

      const { value: ast, errors: parseErrors } = parse(tokens)
      if (parseErrors.length > 0) {
        expect(parseErrors).toStrictEqual(testcase.expectedErrors)
        return
      }

      if (ast == null) fail()

      const { value: program, errors } = analyze(ast)

      expect(errors).toStrictEqual(testcase.expectedErrors)

      if (testcase.expectedProgram !== undefined) {
        if (program === undefined) {
          fail()
        }
        expect(encodeProgram(program)).toStrictEqual(testcase.expectedProgram)
      }
    })
  }
})
