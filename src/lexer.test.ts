import { Error, ErrorKind } from './errors'
import { tokenize } from './lexer'

interface Testcase {
  name: string
  sourceCode: string
  expectedTokens: string[]
  expectedErrors: Error[]
}

const simpleWhileLoop = `
var i := 0;
while i < 10 do
begin
    output(i);
end
`

const expectedSimpleWhileLoop = [
  'VAR', 'IDENTIFIER(i)', 'ASSIGN', 'INTEGER_LITERAL(0)', 'SEMICOLON',
  'WHILE', 'IDENTIFIER(i)', 'LESS_THAN', 'INTEGER_LITERAL(10)', 'DO',
  'BEGIN',
  'IDENTIFIER(output)', 'OPEN_BRAC', 'IDENTIFIER(i)', 'CLOSE_BRAC', 'SEMICOLON',
  'END'
]

describe('tokenize test', () => {
  const testcases: Testcase[] = [
    {
      name: 'if token',
      sourceCode: 'if',
      expectedTokens: ['IF'],
      expectedErrors: []
    },
    {
      name: 'simple if with expression',
      sourceCode: 'if a < b',
      expectedTokens: ['IF', 'IDENTIFIER(a)', 'LESS_THAN', 'IDENTIFIER(b)'],
      expectedErrors: []
    },
    {
      name: 'zero number literal',
      sourceCode: '0;',
      expectedTokens: ['INTEGER_LITERAL(0)', 'SEMICOLON'],
      expectedErrors: []
    },
    {
      name: 'function call',
      sourceCode: 'some_function(some_var)',
      expectedTokens: ['IDENTIFIER(some_function)', 'OPEN_BRAC', 'IDENTIFIER(some_var)', 'CLOSE_BRAC'],
      expectedErrors: []
    },
    {
      name: 'simple for loop',
      sourceCode: simpleWhileLoop,
      expectedTokens: expectedSimpleWhileLoop,
      expectedErrors: []
    },
    {
      name: 'comment and div',
      sourceCode: 'a / b // this is a comment',
      expectedTokens: ['IDENTIFIER(a)', 'DIV', 'IDENTIFIER(b)', 'COMMENT(// this is a comment)'],
      expectedErrors: []
    },
    {
      name: 'scan symbols',
      sourceCode: '>!a',
      expectedTokens: ['GREATER_THAN', 'NOT', 'IDENTIFIER(a)'],
      expectedErrors: []
    },
    {
      name: 'scan invalid symbol',
      sourceCode: '@',
      expectedTokens: [],
      expectedErrors: [{ kind: ErrorKind.UNEXPECTED_CHARACTER, position: { line: 1, col: 1 }, char: '@' }]
    },
    {
      name: 'number literal',
      sourceCode: '123',
      expectedTokens: ['INTEGER_LITERAL(123)'],
      expectedErrors: []
    },
    {
      name: 'negative number literal',
      sourceCode: '-123',
      expectedTokens: ['MINUS', 'INTEGER_LITERAL(123)'],
      expectedErrors: []
    },
    {
      name: 'variable decl',
      sourceCode: 'var some_var: integer',
      expectedTokens: ['VAR', 'IDENTIFIER(some_var)', 'COLON', 'INTEGER'],
      expectedErrors: []
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer',
      expectedTokens: [
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
        'INTEGER'
      ],
      expectedErrors: []
    }
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      const { value: tokens, errors } = tokenize(testcase.sourceCode)
      const actualTokens = tokens?.map(tok => tok.repr())
      expect(actualTokens).toStrictEqual(testcase.expectedTokens)
      expect(errors).toStrictEqual(testcase.expectedErrors)
    })
  }
})
