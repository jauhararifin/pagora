import { parse, parseStatement, TokenIterator } from './parser'
import { encodeAst, encodeStatementNode } from './ast_util'
import { CompileError, MultiCompileError } from './errors'
import { RootNode } from './ast'
import { scan } from './scanner'

interface Testcase {
  name: string
  sourceCode: string
  expectedResult: any | CompileError[]
}

describe('parsing test', () => {
  const testcases: Testcase[] = [
    {
      name: 'global variable without value',
      sourceCode: 'var some_var: integer;',
      // prettier-ignore
      expectedResult: [
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'INTEGER', undefined, undefined],
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
      // prettier-ignore
      expectedResult: [
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'REAL', undefined, undefined],
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'BOOLEAN', undefined, undefined],
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'BYTE', undefined, undefined],
        ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'INTEGER', undefined, undefined],
      ],
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer;',
      // prettier-ignore
      expectedResult: [
        ['VAR', 'IDENTIFIER(some_var)', 'COLON',
          ['ARRAY', 'OPEN_SQUARE', ['INTEGER_LITERAL(10)', 'INTEGER_LITERAL(20)'], 'CLOSE_SQUARE', 'OF','INTEGER'],
          undefined,
          undefined,
        ],
      ],
    },
    {
      name: 'plus and multiply precedence',
      sourceCode: 'begin 1 + 2 * 3 + 4 / 5; end',
      // prettier-ignore
      expectedResult: [
        ['BEGIN', [
          [
            ['INTEGER_LITERAL(1)', 'PLUS', ['INTEGER_LITERAL(2)', 'MULTIPLY', 'INTEGER_LITERAL(3)']],
            'PLUS',
            ['INTEGER_LITERAL(4)', 'DIV', 'INTEGER_LITERAL(5)'],
          ],
        ], 'END'],
      ],
    },
    {
      name: 'plus and multiply precedence 2',
      sourceCode: 'begin i*2+1; end',
      // prettier-ignore
      expectedResult: [
        ['BEGIN', [
          [
            ['IDENTIFIER(i)', 'MULTIPLY', 'INTEGER_LITERAL(2)'],
            'PLUS',
            'INTEGER_LITERAL(1)',
          ],
        ], 'END'],
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
    {
      name: 'while cond expr',
      sourceCode: 'begin while j < n-i-1 do output(" "); end',
      expectedResult: [
        [
          'BEGIN',
          [
            [
              'WHILE',
              [
                'IDENTIFIER(j)',
                'LESS_THAN',
                [
                  ['IDENTIFIER(n)', 'MINUS', 'IDENTIFIER(i)'],
                  'MINUS',
                  'INTEGER_LITERAL(1)',
                ],
              ],
              'DO',
              ['IDENTIFIER(output)', 'OPEN_BRAC', [undefined], 'CLOSE_BRAC'],
            ],
          ],
          'END',
        ],
      ],
    },
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      let root: RootNode
      try {
        const tokens = scan(testcase.sourceCode)
        root = parse(tokens)
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

      expect(encodeAst(root)).toStrictEqual(testcase.expectedResult)
    })
  }
})

describe('parse statement', () => {
  it('if statement', () => {
    const sourceCode = `if a then b(); else c();`
    const expectedAst = [
      'IF',
      'IDENTIFIER(a)',
      'THEN',
      ['IDENTIFIER(b)', 'OPEN_BRAC', [], 'CLOSE_BRAC'],
      'ELSE',
      ['IDENTIFIER(c)', 'OPEN_BRAC', [], 'CLOSE_BRAC'],
    ]
    const tokens = scan(sourceCode)
    const [statementNode, error] = parseStatement(new TokenIterator(tokens))
    expect(error).toBeUndefined()
    expect(encodeStatementNode(statementNode!)).toStrictEqual(expectedAst)
  })

  it('while statement', () => {
    const sourceCode = `
    begin
      var y := 0; 
      while a do begin 
        var y := 1;
        var x := 1;
      end
    end
    `

    // prettier-ignore
    const expectedAst = [
      'BEGIN',[
        ['VAR', 'IDENTIFIER(y)', undefined, undefined, 'ASSIGN', 'INTEGER_LITERAL(0)'],
        ['WHILE', 'IDENTIFIER(a)', 'DO', [
            'BEGIN', [
              ['VAR', 'IDENTIFIER(y)', undefined, undefined, 'ASSIGN', 'INTEGER_LITERAL(1)'],
              ['VAR', 'IDENTIFIER(x)', undefined, undefined, 'ASSIGN', 'INTEGER_LITERAL(1)'],
            ], 'END',
        ]],
      ], 'END',
    ]

    const tokens = scan(sourceCode)
    const [statementNode, error] = parseStatement(new TokenIterator(tokens))
    expect(error).toBeUndefined()
    expect(encodeStatementNode(statementNode!)).toStrictEqual(expectedAst)
  })
})
