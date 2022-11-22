import { CompileError, MultiCompileError } from './errors'
import { scan } from './scanner'

interface Testcase {
  name: string
  sourceCode: string
  expectedResult: string[] | CompileError[]
}

const simpleWhileLoop = `
var i := 0;
while i < 10 do
begin
    output(i);
end
`

const expectedSimpleWhileLoop = [
  'VAR',
  'IDENTIFIER(i)',
  'ASSIGN',
  'INTEGER_LITERAL(0)',
  'SEMICOLON',
  'WHILE',
  'IDENTIFIER(i)',
  'LESS_THAN',
  'INTEGER_LITERAL(10)',
  'DO',
  'BEGIN',
  'IDENTIFIER(output)',
  'OPEN_BRAC',
  'IDENTIFIER(i)',
  'CLOSE_BRAC',
  'SEMICOLON',
  'END',
]

describe('tokenize test', () => {
  const testcases: Testcase[] = [
    {
      name: 'if token',
      sourceCode: 'if',
      expectedResult: ['IF'],
    },
    {
      name: 'simple if with expression',
      sourceCode: 'if a < b',
      expectedResult: ['IF', 'IDENTIFIER(a)', 'LESS_THAN', 'IDENTIFIER(b)'],
    },
    {
      name: 'zero number literal',
      sourceCode: '0;',
      expectedResult: ['INTEGER_LITERAL(0)', 'SEMICOLON'],
    },
    {
      name: 'function call',
      sourceCode: 'some_function(some_var)',
      expectedResult: [
        'IDENTIFIER(some_function)',
        'OPEN_BRAC',
        'IDENTIFIER(some_var)',
        'CLOSE_BRAC',
      ],
    },
    {
      name: 'simple while loop',
      sourceCode: simpleWhileLoop,
      expectedResult: expectedSimpleWhileLoop,
    },
    {
      name: 'comment and div',
      sourceCode: 'a / b // this is a comment',
      expectedResult: [
        'IDENTIFIER(a)',
        'DIV',
        'IDENTIFIER(b)',
        'COMMENT(// this is a comment)',
      ],
    },
    {
      name: 'scan symbols',
      sourceCode: '>!a',
      expectedResult: ['GREATER_THAN', 'NOT', 'IDENTIFIER(a)'],
    },
    {
      name: 'scan invalid symbol',
      sourceCode: '@',
      expectedResult: [`Error at 1:1: Unexpected character "@"`],
    },
    {
      name: 'number literal',
      sourceCode: '123',
      expectedResult: ['INTEGER_LITERAL(123)'],
    },
    {
      name: 'negative number literal',
      sourceCode: '-123',
      expectedResult: ['MINUS', 'INTEGER_LITERAL(123)'],
    },
    {
      name: 'variable decl',
      sourceCode: 'var some_var: integer',
      expectedResult: ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'INTEGER'],
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer',
      expectedResult: [
        'VAR',
        'IDENTIFIER(some_var)',
        'COLON',
        'ARRAY',
        'OPEN_SQUARE',
        'INTEGER_LITERAL(10)',
        'COMMA',
        'INTEGER_LITERAL(20)',
        'CLOSE_SQUARE',
        'OF',
        'INTEGER',
      ],
    },
    {
      name: 'incomplete program',
      sourceCode: 'begin output("hello end',
      expectedResult: [
        `Error at 1:23: Missing closing quote in string literal`,
      ],
    },
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      try {
        const tokens = scan(testcase.sourceCode)
        const actualTokens = tokens?.map((tok) => tok.encode())
        expect(actualTokens).toStrictEqual(testcase.expectedResult)
      } catch (e) {
        if (e instanceof MultiCompileError) {
          expect(e.errors.flatMap((v) => v.message)).toStrictEqual(
            testcase.expectedResult
          )
        } else {
          throw e
        }
      }
    })
  }
})
