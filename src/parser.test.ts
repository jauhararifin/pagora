import { DeclKind, RootNode, TypeKind } from './ast'
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
