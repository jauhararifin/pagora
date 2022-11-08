import { Error, ErrorKind } from './errors'
import { Token, TokenKind } from './tokens'
import { tokenize } from './lexer'

interface Testcase {
  name: string
  sourceCode: string
  expectedTokens: Token[]
  expectedErrors: Error[]
}

const simpleForLoop = `
for var i: integer := 0; i < n; i := i + 1 do
begin
    output(i)
end
`

const expectedSimpleForLoop = [
  { kind: TokenKind.FOR, value: 'for', position: { line: 2, col: 1 } },
  { kind: TokenKind.VAR, value: 'var', position: { line: 2, col: 5 } },
  { kind: TokenKind.IDENTIFIER, value: 'i', position: { line: 2, col: 9 } },
  { kind: TokenKind.COLON, value: ':', position: { line: 2, col: 10 } },
  { kind: TokenKind.INTEGER, value: 'integer', position: { line: 2, col: 12 } },
  { kind: TokenKind.ASSIGN, value: ':=', position: { line: 2, col: 20 } },
  { kind: TokenKind.INTEGER_LITERAL, value: '0', position: { line: 2, col: 23 } },
  { kind: TokenKind.SEMICOLON, value: ';', position: { line: 2, col: 24 } },
  { kind: TokenKind.IDENTIFIER, value: 'i', position: { line: 2, col: 26 } },
  { kind: TokenKind.LESS_THAN, value: '<', position: { line: 2, col: 28 } },
  { kind: TokenKind.IDENTIFIER, value: 'n', position: { line: 2, col: 30 } },
  { kind: TokenKind.SEMICOLON, value: ';', position: { line: 2, col: 31 } },
  { kind: TokenKind.IDENTIFIER, value: 'i', position: { line: 2, col: 33 } },
  { kind: TokenKind.ASSIGN, value: ':=', position: { line: 2, col: 35 } },
  { kind: TokenKind.IDENTIFIER, value: 'i', position: { line: 2, col: 38 } },
  { kind: TokenKind.PLUS, value: '+', position: { line: 2, col: 40 } },
  { kind: TokenKind.INTEGER_LITERAL, value: '1', position: { line: 2, col: 42 } },
  { kind: TokenKind.DO, value: 'do', position: { line: 2, col: 44 } },
  { kind: TokenKind.BEGIN, value: 'begin', position: { line: 3, col: 1 } },
  { kind: TokenKind.IDENTIFIER, value: 'output', position: { line: 4, col: 5 } },
  { kind: TokenKind.OPEN_BRAC, value: '(', position: { line: 4, col: 11 } },
  { kind: TokenKind.IDENTIFIER, value: 'i', position: { line: 4, col: 12 } },
  { kind: TokenKind.CLOSE_BRAC, value: ')', position: { line: 4, col: 13 } },
  { kind: TokenKind.PHANTOM_SEMICOLON, value: ';', position: { line: 4, col: 14 } },
  { kind: TokenKind.END, value: 'end', position: { line: 5, col: 1 } }
]

describe('tokenize test', () => {
  const testcases: Testcase[] = [
    {
      name: 'if token',
      sourceCode: 'if',
      expectedTokens: [{ kind: TokenKind.IF, value: 'if', position: { line: 1, col: 1 } }],
      expectedErrors: []
    },
    {
      name: 'simple if with expression',
      sourceCode: 'if a < b',
      expectedTokens: [
        { kind: TokenKind.IF, value: 'if', position: { line: 1, col: 1 } },
        { kind: TokenKind.IDENTIFIER, value: 'a', position: { line: 1, col: 4 } },
        { kind: TokenKind.LESS_THAN, value: '<', position: { line: 1, col: 6 } },
        { kind: TokenKind.IDENTIFIER, value: 'b', position: { line: 1, col: 8 } }
      ],
      expectedErrors: []
    },
    {
      name: 'zero number literal',
      sourceCode: '0;',
      expectedTokens: [
        { kind: TokenKind.INTEGER_LITERAL, value: '0', position: { line: 1, col: 1 } },
        { kind: TokenKind.SEMICOLON, value: ';', position: { line: 1, col: 2 } }
      ],
      expectedErrors: []
    },
    {
      name: 'function call',
      sourceCode: 'some_function(some_var)',
      expectedTokens: [
        { kind: TokenKind.IDENTIFIER, value: 'some_function', position: { line: 1, col: 1 } },
        { kind: TokenKind.OPEN_BRAC, value: '(', position: { line: 1, col: 14 } },
        { kind: TokenKind.IDENTIFIER, value: 'some_var', position: { line: 1, col: 15 } },
        { kind: TokenKind.CLOSE_BRAC, value: ')', position: { line: 1, col: 23 } }
      ],
      expectedErrors: []
    },
    {
      name: 'simple for loop',
      sourceCode: simpleForLoop,
      expectedTokens: expectedSimpleForLoop,
      expectedErrors: []
    },
    {
      name: 'comment and div',
      sourceCode: 'a / b // this is a comment',
      expectedTokens: [
        { kind: TokenKind.IDENTIFIER, value: 'a', position: { line: 1, col: 1 } },
        { kind: TokenKind.DIV, value: '/', position: { line: 1, col: 3 } },
        { kind: TokenKind.IDENTIFIER, value: 'b', position: { line: 1, col: 5 } },
        { kind: TokenKind.COMMENT, value: '// this is a comment', position: { line: 1, col: 7 } }
      ],
      expectedErrors: []
    },
    {
      name: 'scan symbols',
      sourceCode: '>!a',
      expectedTokens: [
        { kind: TokenKind.GREATER_THAN, value: '>', position: { line: 1, col: 1 } },
        { kind: TokenKind.NOT, value: '!', position: { line: 1, col: 2 } },
        { kind: TokenKind.IDENTIFIER, value: 'a', position: { line: 1, col: 3 } }
      ],
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
      expectedTokens: [{ kind: TokenKind.INTEGER_LITERAL, value: '123', position: { line: 1, col: 1 } }],
      expectedErrors: []
    },
    {
      name: 'negative number literal',
      sourceCode: '-123',
      expectedTokens: [
        { kind: TokenKind.MINUS, value: '-', position: { line: 1, col: 1 } },
        { kind: TokenKind.INTEGER_LITERAL, value: '123', position: { line: 1, col: 2 } }
      ],
      expectedErrors: []
    },
    {
      name: 'variable decl',
      sourceCode: 'var some_var: integer',
      expectedTokens: [
        { kind: TokenKind.VAR, value: 'var', position: { line: 1, col: 1 } },
        { kind: TokenKind.IDENTIFIER, value: 'some_var', position: { line: 1, col: 5 } },
        { kind: TokenKind.COLON, value: ':', position: { line: 1, col: 13 } },
        { kind: TokenKind.INTEGER, value: 'integer', position: { line: 1, col: 15 } }
      ],
      expectedErrors: []
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer',
      expectedTokens: [
        { kind: TokenKind.VAR, value: 'var', position: { line: 1, col: 1 } },
        { kind: TokenKind.IDENTIFIER, value: 'some_var', position: { line: 1, col: 5 } },
        { kind: TokenKind.COLON, value: ':', position: { line: 1, col: 13 } },
        { kind: TokenKind.ARRAY, value: 'array', position: { line: 1, col: 15 } },
        { kind: TokenKind.OPEN_SQUARE, value: '[', position: { line: 1, col: 20 } },
        { kind: TokenKind.INTEGER_LITERAL, value: '10', position: { line: 1, col: 21 } },
        { kind: TokenKind.COMMA, value: ',', position: { line: 1, col: 23 } },
        { kind: TokenKind.INTEGER_LITERAL, value: '20', position: { line: 1, col: 24 } },
        { kind: TokenKind.CLOSE_SQUARE, value: ']', position: { line: 1, col: 26 } },
        { kind: TokenKind.OF, value: 'of', position: { line: 1, col: 28 } },
        { kind: TokenKind.INTEGER, value: 'integer', position: { line: 1, col: 31 } }
      ],
      expectedErrors: []
    }
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      const { value: tokens, errors } = tokenize(testcase.sourceCode)
      expect(tokens).toStrictEqual(testcase.expectedTokens)
      expect(errors).toStrictEqual(testcase.expectedErrors)
    })
  }
})
