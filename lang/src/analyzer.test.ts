import { MultiCompileError } from './errors'
import { analyze } from './analyzer'
import { encodeProgram } from './semantic_util'
import { parse } from './parser'
import { Program } from './semantic'
import { scan } from './scanner'

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
  [
    'func',
    'aplusb',
    [
      ['a', 'INTEGER'],
      ['b', 'INTEGER'],
    ],
    'INTEGER',
    [['return', ['PLUS', ['ident', 'a'], ['ident', 'b']]]],
  ],
  [
    'main',
    [
      ['var', 'a', 'INTEGER', '10'],
      ['var', 'b', 'INTEGER', '20'],
      [
        'call',
        ['ident', 'aplusb'],
        [
          ['ident', 'a'],
          ['ident', 'b'],
        ],
      ],
    ],
  ],
]

const program2Source = `
begin
  var n := 10;

  var i := 0;
  while i < n do
  begin
    var j := 0;
    while j < n-i-1 do
    begin
      j := j + 1;
    end

    j := 0;
    while j < i*2+1 do
    begin
      j := j + 1;
    end

    i := i + 1;
  end
end
`

const program2 = [
  [
    'main',
    [
      ['var', 'n', 'INTEGER', '10'],
      ['var', 'i', 'INTEGER', '0'],
      [
        'while',
        ['LESS_THAN', ['ident', 'i'], ['ident', 'n']],
        [
          ['var', 'j', 'INTEGER', '0'],
          [
            'while',
            [
              'LESS_THAN',
              ['ident', 'j'],
              ['MINUS', ['MINUS', ['ident', 'n'], ['ident', 'i']], '1'],
            ],
            [['assign', ['ident', 'j'], ['PLUS', ['ident', 'j'], '1']]],
          ],
          ['assign', ['ident', 'j'], '0'],
          [
            'while',
            [
              'LESS_THAN',
              ['ident', 'j'],
              ['PLUS', ['MUL', ['ident', 'i'], '2'], '1'],
            ],
            [['assign', ['ident', 'j'], ['PLUS', ['ident', 'j'], '1']]],
          ],
          ['assign', ['ident', 'i'], ['PLUS', ['ident', 'i'], '1']],
        ],
      ],
    ],
  ],
]

const program3Source = `
var some_array: array [10,10] of integer;
begin
  some_array[10,10] := 2;
end
`

const program3 = [
  ['var', 'some_array', ['array', ['10', '10'], 'INTEGER'], undefined],
  ['main', [['assign', ['index', ['ident', 'some_array'], ['10', '10']], '2']]],
]

const program4Source = `
begin
  var y := 0;
  var x := 0;
  while x > 0 do
  begin
    var y := 1;
  end
end
`

const program4 = [
  [
    'main',
    [
      ['var', 'y', 'INTEGER', '0'],
      ['var', 'x', 'INTEGER', '0'],
      [
        'while',
        ['GREATER_THAN', ['ident', 'x'], '0'],
        [['var', 'y', 'INTEGER', '1']],
      ],
    ],
  ],
]

const arrayDimMismatchSource = `
var some_array: array [10,10,10] of integer;
begin
  some_array[0,0] := 2;
end
`

const arrayDimMismatchError: string[] = [
  'Error at 4:13: Wrong number of index. Expected 3, got 2',
]

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
    {
      name: 'program 2',
      sourceCode: program2Source,
      expectedResult: program2,
    },
    {
      name: 'program 3',
      sourceCode: program3Source,
      expectedResult: program3,
    },
    {
      name: 'program 4',
      sourceCode: program4Source,
      expectedResult: program4,
    },
    {
      name: 'array dimension mismatch',
      sourceCode: arrayDimMismatchSource,
      expectedResult: arrayDimMismatchError,
    },
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      let program: Program
      try {
        const tokens = scan(testcase.sourceCode)
        const ast = parse(tokens)
        program = analyze(ast, {})
      } catch (e) {
        if (e instanceof MultiCompileError) {
          for (let i = 0; i < e.errors.length; i++) {
            expect(e.errors[i].message).toStrictEqual(
              testcase.expectedResult[i]
            )
          }
          return
        } else {
          throw e
        }
      }

      expect(encodeProgram(program)).toStrictEqual(testcase.expectedResult)
    })
  }
})
