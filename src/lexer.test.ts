import { Error, ErrorKind } from './errors'
import { tokenize } from './lexer'
import { Token, TokenKind } from './tokens'

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
  { kind: TokenKind.For, value: 'for', position: { line: 2, col: 1 } },
  { kind: TokenKind.Var, value: 'var', position: { line: 2, col: 5 } },
  { kind: TokenKind.Identifier, value: 'i', position: { line: 2, col: 9 } },
  { kind: TokenKind.Colon, value: ':', position: { line: 2, col: 10 } },
  { kind: TokenKind.Integer, value: 'integer', position: { line: 2, col: 12 } },
  { kind: TokenKind.Assign, value: ':=', position: { line: 2, col: 20 } },
  { kind: TokenKind.IntegerLiteral, value: '0', position: { line: 2, col: 23 } },
  { kind: TokenKind.Semicolon, value: ';', position: { line: 2, col: 24 } },
  { kind: TokenKind.Identifier, value: 'i', position: { line: 2, col: 26 } },
  { kind: TokenKind.LessThan, value: '<', position: { line: 2, col: 28 } },
  { kind: TokenKind.Identifier, value: 'n', position: { line: 2, col: 30 } },
  { kind: TokenKind.Semicolon, value: ';', position: { line: 2, col: 31 } },
  { kind: TokenKind.Identifier, value: 'i', position: { line: 2, col: 33 } },
  { kind: TokenKind.Assign, value: ':=', position: { line: 2, col: 35 } },
  { kind: TokenKind.Identifier, value: 'i', position: { line: 2, col: 38 } },
  { kind: TokenKind.Plus, value: '+', position: { line: 2, col: 40 } },
  { kind: TokenKind.IntegerLiteral, value: '1', position: { line: 2, col: 42 } },
  { kind: TokenKind.Do, value: 'do', position: { line: 2, col: 44 } },
  { kind: TokenKind.Begin, value: 'begin', position: { line: 3, col: 1 } },
  { kind: TokenKind.Identifier, value: 'output', position: { line: 4, col: 5 } },
  { kind: TokenKind.OpenBrac, value: '(', position: { line: 4, col: 11 } },
  { kind: TokenKind.Identifier, value: 'i', position: { line: 4, col: 12 } },
  { kind: TokenKind.CloseBrac, value: ')', position: { line: 4, col: 13 } },
  { kind: TokenKind.PhantomSemicolon, value: ';', position: { line: 4, col: 14 } },
  { kind: TokenKind.End, value: 'end', position: { line: 5, col: 1 } }
]

describe('tokenize test', () => {
  const testcases: Testcase[] = [
    {
      name: 'if token',
      sourceCode: 'if',
      expectedTokens: [{ kind: TokenKind.If, value: 'if', position: { line: 1, col: 1 } }],
      expectedErrors: []
    },
    {
      name: 'simple if with expression',
      sourceCode: 'if a < b',
      expectedTokens: [
        { kind: TokenKind.If, value: 'if', position: { line: 1, col: 1 } },
        { kind: TokenKind.Identifier, value: 'a', position: { line: 1, col: 4 } },
        { kind: TokenKind.LessThan, value: '<', position: { line: 1, col: 6 } },
        { kind: TokenKind.Identifier, value: 'b', position: { line: 1, col: 8 } }
      ],
      expectedErrors: []
    },
    {
      name: 'zero number literal',
      sourceCode: '0;',
      expectedTokens: [
        { kind: TokenKind.IntegerLiteral, value: '0', position: { line: 1, col: 1 } },
        { kind: TokenKind.Semicolon, value: ';', position: { line: 1, col: 2 } }
      ],
      expectedErrors: []
    },
    {
      name: 'function call',
      sourceCode: 'some_function(some_var)',
      expectedTokens: [
        { kind: TokenKind.Identifier, value: 'some_function', position: { line: 1, col: 1 } },
        { kind: TokenKind.OpenBrac, value: '(', position: { line: 1, col: 14 } },
        { kind: TokenKind.Identifier, value: 'some_var', position: { line: 1, col: 15 } },
        { kind: TokenKind.CloseBrac, value: ')', position: { line: 1, col: 23 } }
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
        { kind: TokenKind.Identifier, value: 'a', position: { line: 1, col: 1 } },
        { kind: TokenKind.Div, value: '/', position: { line: 1, col: 3 } },
        { kind: TokenKind.Identifier, value: 'b', position: { line: 1, col: 5 } },
        { kind: TokenKind.Comment, value: '// this is a comment', position: { line: 1, col: 7 } }
      ],
      expectedErrors: []
    },
    {
      name: 'scan symbols',
      sourceCode: '>!a',
      expectedTokens: [
        { kind: TokenKind.GreaterThan, value: '>', position: { line: 1, col: 1 } },
        { kind: TokenKind.Not, value: '!', position: { line: 1, col: 2 } },
        { kind: TokenKind.Identifier, value: 'a', position: { line: 1, col: 3 } }
      ],
      expectedErrors: []
    },
    {
      name: 'scan invalid symbol',
      sourceCode: '@',
      expectedTokens: [],
      expectedErrors: [{ kind: ErrorKind.UnexpectedCharacter, position: { line: 1, col: 1 }, char: '@' }]
    },
    {
      name: 'number literal',
      sourceCode: '123',
      expectedTokens: [{ kind: TokenKind.IntegerLiteral, value: '123', position: { line: 1, col: 1 } }],
      expectedErrors: []
    },
    {
      name: 'negative number literal',
      sourceCode: '-123',
      expectedTokens: [
        { kind: TokenKind.Minus, value: '-', position: { line: 1, col: 1 } },
        { kind: TokenKind.IntegerLiteral, value: '123', position: { line: 1, col: 2 } }
      ],
      expectedErrors: []
    },
    {
      name: 'variable decl',
      sourceCode: 'var some_var: integer',
      expectedTokens: [
        { kind: TokenKind.Var, value: 'var', position: { line: 1, col: 1 } },
        { kind: TokenKind.Identifier, value: 'some_var', position: { line: 1, col: 5 } },
        { kind: TokenKind.Colon, value: ':', position: { line: 1, col: 13 } },
        { kind: TokenKind.Integer, value: 'integer', position: { line: 1, col: 15 } }
      ],
      expectedErrors: []
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer',
      expectedTokens: [
        { kind: TokenKind.Var, value: 'var', position: { line: 1, col: 1 } },
        { kind: TokenKind.Identifier, value: 'some_var', position: { line: 1, col: 5 } },
        { kind: TokenKind.Colon, value: ':', position: { line: 1, col: 13 } },
        { kind: TokenKind.Array, value: 'array', position: { line: 1, col: 15 } },
        { kind: TokenKind.OpenSquare, value: '[', position: { line: 1, col: 20 } },
        { kind: TokenKind.IntegerLiteral, value: '10', position: { line: 1, col: 21 } },
        { kind: TokenKind.Comma, value: ',', position: { line: 1, col: 23 } },
        { kind: TokenKind.IntegerLiteral, value: '20', position: { line: 1, col: 24 } },
        { kind: TokenKind.CloseSquare, value: ']', position: { line: 1, col: 26 } },
        { kind: TokenKind.Of, value: 'of', position: { line: 1, col: 28 } },
        { kind: TokenKind.Integer, value: 'integer', position: { line: 1, col: 31 } }
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
