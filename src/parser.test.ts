import {
  DeclKind,
  ExprNodeKind,
  RootNode,
  StatementNodeKind,
  TypeExprNodeKind
} from './ast'
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
            var: { kind: TokenKind.VAR, position: { line: 1, col: 1 }, value: 'var' },
            name: { kind: TokenKind.IDENTIFIER, position: { line: 1, col: 5 }, value: 'some_var' },
            colon: { kind: TokenKind.COLON, position: { line: 1, col: 13 }, value: ':' },
            type: {
              kind: TypeExprNodeKind.PRIMITIVE,
              type: { kind: TokenKind.INTEGER, position: { line: 1, col: 15 }, value: 'integer' }
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
            var: { kind: TokenKind.VAR, position: { line: 1, col: 1 }, value: 'var' },
            name: { kind: TokenKind.IDENTIFIER, position: { line: 1, col: 5 }, value: 'some_var' },
            colon: { kind: TokenKind.COLON, position: { line: 1, col: 13 }, value: ':' },
            type: {
              kind: TypeExprNodeKind.ARRAY,
              array: { kind: TokenKind.ARRAY, position: { line: 1, col: 15 }, value: 'array' },
              openSquare: { kind: TokenKind.OPEN_SQUARE, position: { line: 1, col: 20 }, value: '[' },
              dimension: {
                values: [
                  {
                    kind: ExprNodeKind.INTEGER_LIT,
                    value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 21 }, value: '10' }
                  },
                  {
                    kind: ExprNodeKind.INTEGER_LIT,
                    value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 24 }, value: '20' }
                  }
                ],
                commas: [{ kind: TokenKind.COMMA, position: { line: 1, col: 23 }, value: ',' }]
              },
              closeSquare: { kind: TokenKind.CLOSE_SQUARE, position: { line: 1, col: 26 }, value: ']' },
              of: { kind: TokenKind.OF, position: { line: 1, col: 28 }, value: 'of' },
              type: {
                kind: TypeExprNodeKind.PRIMITIVE,
                type: { kind: TokenKind.INTEGER, position: { line: 1, col: 31 }, value: 'integer' }
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
            begin: { kind: TokenKind.BEGIN, position: { line: 1, col: 1 }, value: 'begin' },
            end: { kind: TokenKind.END, position: { line: 1, col: 25 }, value: 'end' },
            statements: [
              {
                kind: StatementNodeKind.EXPR,
                expr: {
                  kind: ExprNodeKind.BINARY,
                  a: {
                    kind: ExprNodeKind.INTEGER_LIT,
                    value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 7 }, value: '1' }
                  },
                  op: { kind: TokenKind.PLUS, position: { line: 1, col: 9 }, value: '+' },
                  b: {
                    kind: ExprNodeKind.BINARY,
                    a: {
                      kind: ExprNodeKind.BINARY,
                      a: {
                        kind: ExprNodeKind.INTEGER_LIT,
                        value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 11 }, value: '2' }
                      },
                      op: { kind: TokenKind.MULTIPLY, position: { line: 1, col: 13 }, value: '*' },
                      b: {
                        kind: ExprNodeKind.INTEGER_LIT,
                        value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 15 }, value: '3' }
                      }
                    },
                    op: { kind: TokenKind.PLUS, position: { line: 1, col: 17 }, value: '+' },
                    b: {
                      kind: ExprNodeKind.BINARY,
                      a: {
                        kind: ExprNodeKind.INTEGER_LIT,
                        value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 19 }, value: '4' }
                      },
                      op: { kind: TokenKind.DIV, position: { line: 1, col: 21 }, value: '/' },
                      b: {
                        kind: ExprNodeKind.INTEGER_LIT,
                        value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 23 }, value: '5' }
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
    },
    {
      name: 'plus and multiply precedence 2',
      sourceCode: 'begin i*2+1 end',
      expectedAST: {
        declarations: [{
          kind: DeclKind.MAIN,
          body: {
            kind: StatementNodeKind.BLOCK,
            begin: { kind: TokenKind.BEGIN, position: { line: 1, col: 1 }, value: 'begin' },
            end: { kind: TokenKind.END, position: { line: 1, col: 13 }, value: 'end' },
            statements: [
              {
                kind: StatementNodeKind.EXPR,
                expr: {
                  kind: ExprNodeKind.BINARY,
                  a: {
                    kind: ExprNodeKind.BINARY,
                    a: {
                      kind: ExprNodeKind.IDENT,
                      name: { kind: TokenKind.IDENTIFIER, position: { line: 1, col: 7 }, value: 'i' }
                    },
                    op: { kind: TokenKind.MULTIPLY, position: { line: 1, col: 8 }, value: '*' },
                    b: {
                      kind: ExprNodeKind.INTEGER_LIT,
                      value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 9 }, value: '2' }
                    }
                  },
                  op: { kind: TokenKind.PLUS, position: { line: 1, col: 10 }, value: '+' },
                  b: {
                    kind: ExprNodeKind.INTEGER_LIT,
                    value: { kind: TokenKind.INTEGER_LITERAL, position: { line: 1, col: 11 }, value: '1' }
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
