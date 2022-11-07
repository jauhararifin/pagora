import { DeclKind, ExprKind, RootNode, StatementNodeKind, TypeExprNodeKind } from './ast'
import { TokenKind } from './tokens'
import { parse } from './parser'
import { tokenize } from './lexer'

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
          variable: {
            var: { kind: TokenKind.Var, position: { line: 1, col: 1 }, value: 'var' },
            name: { kind: TokenKind.Identifier, position: { line: 1, col: 5 }, value: 'some_var' },
            colon: { kind: TokenKind.Colon, position: { line: 1, col: 13 }, value: ':' },
            type: {
              kind: TypeExprNodeKind.PRIMITIVE,
              type: { kind: TokenKind.Integer, position: { line: 1, col: 15 }, value: 'integer' }
            }
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
          variable: {
            var: { kind: TokenKind.Var, position: { line: 1, col: 1 }, value: 'var' },
            name: { kind: TokenKind.Identifier, position: { line: 1, col: 5 }, value: 'some_var' },
            colon: { kind: TokenKind.Colon, position: { line: 1, col: 13 }, value: ':' },
            type: {
              kind: TypeExprNodeKind.ARRAY,
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
                kind: TypeExprNodeKind.PRIMITIVE,
                type: { kind: TokenKind.Integer, position: { line: 1, col: 31 }, value: 'integer' }
              }
            }
          }
        }]
      },
      expectedErrors: []
    },
    {
      name: 'plus and multiply precedence',
      sourceCode: 'begin 1 + 2 * 3 + 4 / 5 end',
      expectedAST: {
        declarations: [{
          kind: DeclKind.MAIN,
          body: {
            kind: StatementNodeKind.BLOCK,
            begin: { kind: TokenKind.Begin, position: { line: 1, col: 1 }, value: 'begin' },
            end: { kind: TokenKind.End, position: { line: 1, col: 25 }, value: 'end' },
            statements: [
              {
                kind: StatementNodeKind.EXPR,
                expr: {
                  kind: ExprKind.BINARY,
                  a: {
                    kind: ExprKind.INTEGER_LIT,
                    value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 7 }, value: '1' }
                  },
                  op: { kind: TokenKind.Plus, position: { line: 1, col: 9 }, value: '+' },
                  b: {
                    kind: ExprKind.BINARY,
                    op: { kind: TokenKind.Multiply, position: { line: 1, col: 13 }, value: '*' },
                    a: {
                      kind: ExprKind.INTEGER_LIT,
                      value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 11 }, value: '2' }
                    },
                    b: {
                      kind: ExprKind.BINARY,
                      op: { kind: TokenKind.Plus, position: { line: 1, col: 17 }, value: '+' },
                      a: {
                        kind: ExprKind.INTEGER_LIT,
                        value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 15 }, value: '3' }
                      },
                      b: {
                        kind: ExprKind.BINARY,
                        op: { kind: TokenKind.Div, position: { line: 1, col: 21 }, value: '/' },
                        a: {
                          kind: ExprKind.INTEGER_LIT,
                          value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 19 }, value: '4' }
                        },
                        b: {
                          kind: ExprKind.INTEGER_LIT,
                          value: { kind: TokenKind.IntegerLiteral, position: { line: 1, col: 23 }, value: '5' }
                        }
                      }
                    }
                  }
                }
              }
            ]
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

      if (tokens == null) fail()

      const { value: root, errors: parseErrors } = parse(tokens)
      expect(root).toStrictEqual(testcase.expectedAST)
      expect(parseErrors).toStrictEqual(testcase.expectedErrors)
    })
  }
})
