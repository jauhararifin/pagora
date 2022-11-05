import { DeclKind, ExprKind, RootNode, TypeKind } from './ast'
import { tokenize } from './lexer'
import { parse } from './parser'
import { TokenKind } from './tokens'

interface Testcase {
  name: string
  sourceCode: string
  expectedAST?: RootNode
  expectedErrors: Error[]
}

describe('tokenize test', () => {
  const testcases: Testcase[] = [
    {
      name: 'global variable without value',
      sourceCode: 'var some_var: integer',
      expectedAST: {
        declarations: [{
          kind: DeclKind.VARIABLE,
          var: { kind: TokenKind.Var, position: { line: 1, col: 1 }, value: 'var' },
          name: { kind: TokenKind.Identifier, position: { line: 1, col: 5 }, value: 'some_var' },
          colon: { kind: TokenKind.Colon, position: { line: 1, col: 13 }, value: ':' },
          type: {
            kind: TypeKind.PRIMITIVE,
            type: { kind: TokenKind.Integer, position: { line: 1, col: 15 }, value: 'integer' }
          }
        }]
      },
      expectedErrors: []
    },
    {
      name: 'variable with array type',
      sourceCode: 'var some_var: array[10,20] of integer',
      expectedAST: {
        declarations: [{
          kind: DeclKind.VARIABLE,
          var: { kind: TokenKind.Var, position: { line: 1, col: 1 }, value: 'var' },
          name: { kind: TokenKind.Identifier, position: { line: 1, col: 5 }, value: 'some_var' },
          colon: { kind: TokenKind.Colon, position: { line: 1, col: 13 }, value: ':' },
          type: {
            kind: TypeKind.ARRAY,
            array: { kind: TokenKind.Array, position: { line: 1, col: 15 }, value: 'array' },
            openSquare: { kind: TokenKind.OpenSquare, position: { line: 1, col: 20 }, value: '[' },
            dimension: {
              values: [
                {
                  kind: ExprKind.INTEGER_LIT,
                  value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 21 }, value: '10' }
                },
                {
                  kind: ExprKind.INTEGER_LIT,
                  value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 24 }, value: '20' }
                }
              ],
              commas: [{ kind: TokenKind.Comma, position: { line: 1, col: 23 }, value: ',' }]
            },
            closeSquare: { kind: TokenKind.CloseSquare, position: { line: 1, col: 26 }, value: ']' },
            of: { kind: TokenKind.Of, position: { line: 1, col: 28 }, value: 'of' },
            type: {
              kind: TypeKind.PRIMITIVE,
              type: { kind: TokenKind.Integer, position: { line: 1, col: 31 }, value: 'integer' }
            }
          }
        }]
      },
      expectedErrors: []
    }
  ]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      const { value: tokens, errors: scanErrors } = tokenize(testcase.sourceCode)
      if (scanErrors.length > 0) {
        expect(scanErrors).toStrictEqual(testcase.expectedErrors)
        return
      }

      const { value: root, errors: parseErrors } = parse(tokens)
      expect(root).toStrictEqual(testcase.expectedAST)
      expect(parseErrors).toStrictEqual(testcase.expectedErrors)
    })
  }
})
