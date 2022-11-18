import { parse } from './parser'
import { tokenize } from './lexer'
import { encodeAst } from './ast_util'
import { CompileError, CompileErrorItem } from './errors'

interface Testcase {
  name: string
  sourceCode: string
  expectedResult: any | CompileErrorItem[]
}

describe('tokenize test', () => {
  const testcases: Testcase[] = [
    {
      name: 'global variable without value',
      sourceCode: 'var some_var: integer;',
      expectedResult: [
        [
          'VAR',
          'IDENTIFIER(some_var)',
          'COLON',
          'INTEGER',
          undefined,
          undefined,
        ],
      ],
    },
    {
      name: 'various primitive type',
      sourceCode: `
        var some_var: real;
        var some_var: boolean;
        var some_var: byte;
        var some_var: integer;
      `,
      expectedResult: [
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'REAL', undefined, undefined],
        [
          'VAR',
          'IDENTIFIER(some_var)',
          'COLON',
          'BOOLEAN',
          undefined,
          undefined,
        ],
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'BYTE', undefined, undefined],
        [
          'VAR',
          'IDENTIFIER(some_var)',
          'COLON',
          'INTEGER',
          undefined,
          undefined,
        ],
      ],
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer;',
      expectedResult: [
        [
          'VAR',
          'IDENTIFIER(some_var)',
          'COLON',
          [
            'ARRAY',
            'OPEN_SQUARE',
            ['INTEGER_LITERAL(10)', 'INTEGER_LITERAL(20)'],
            'CLOSE_SQUARE',
            'OF',
            'INTEGER',
          ],
          undefined,
          undefined,
        ],
      ],
    },
    {
      name: 'plus and multiply precedence',
      sourceCode: 'begin 1 + 2 * 3 + 4 / 5; end',
      expectedResult: [
        [
          'BEGIN',
          [
            [
              'INTEGER_LITERAL(1)',
              'PLUS',
              [
                ['INTEGER_LITERAL(2)', 'MULTIPLY', 'INTEGER_LITERAL(3)'],
                'PLUS',
                ['INTEGER_LITERAL(4)', 'DIV', 'INTEGER_LITERAL(5)'],
              ],
            ],
          ],
          'END',
        ],
      ],
    },
    {
      name: 'plus and multiply precedence 2',
      sourceCode: 'begin i*2+1; end',
      expectedResult: [
        [
          'BEGIN',
          [
            [
              ['IDENTIFIER(i)', 'MULTIPLY', 'INTEGER_LITERAL(2)'],
              'PLUS',
              'INTEGER_LITERAL(1)',
            ],
          ],
          'END',
        ],
      ],
    },
    {
      name: 'empty function call',
      sourceCode: 'begin something(); end',
      expectedResult: [
        [
          'BEGIN',
          [['IDENTIFIER(something)', 'OPEN_BRAC', [], 'CLOSE_BRAC']],
          'END',
        ],
      ],
    },
    {
      name: 'return boolean',
      sourceCode: 'begin return false; end',
      expectedResult: [['BEGIN', [['RETURN', 'FALSE']], 'END']],
    },
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      try {
        const tokens = tokenize(testcase.sourceCode)
        const root = parse(tokens)
        expect(encodeAst(root)).toStrictEqual(testcase.expectedResult)
      } catch (e) {
        const err = e as CompileError
        expect(err.errors).toStrictEqual(testcase.expectedResult)
      }
    })
  }
})
