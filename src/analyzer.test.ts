import { BinaryOp, ExprKind, Program, StatementKind, TypeKind } from './semantic'
import { analyze } from './analyzer'
import { parse } from './parser'
import { tokenize } from './lexer'

const program1Source = `
function aplusb(a: integer, b: integer) -> integer
begin
    return a + b;
end

begin
    var a: integer := 10;
    var b: integer := 20;
    aplusb(a, b);
end
`

const program1: Program = {
  functions: [
    {
      name: 'aplusb',
      type: {
        kind: TypeKind.FUNCTION,
        arguments: [{ kind: TypeKind.INTEGER }, { kind: TypeKind.INTEGER }],
        return: { kind: TypeKind.INTEGER }
      },
      arguments: [
        { name: 'a', type: { kind: TypeKind.INTEGER } },
        { name: 'b', type: { kind: TypeKind.INTEGER } }
      ],
      body: {
        kind: StatementKind.BLOCK,
        body: [
          {
            kind: StatementKind.RETURN,
            value: {
              kind: ExprKind.BINARY,
              constValue: undefined,
              isAssignable: false,
              isConstexpr: false,
              type: { kind: TypeKind.INTEGER },
              a: {
                ident: 'a',
                isAssignable: true,
                isConstexpr: false,
                constValue: undefined,
                kind: ExprKind.IDENT,
                type: { kind: TypeKind.INTEGER }
              },
              op: BinaryOp.PLUS,
              b: {
                ident: 'b',
                isAssignable: true,
                isConstexpr: false,
                constValue: undefined,
                kind: ExprKind.IDENT,
                type: { kind: TypeKind.INTEGER }
              }
            }
          }
        ]
      }
    }
  ],
  globals: [],
  main: {
    kind: StatementKind.BLOCK,
    body: [
      {
        kind: StatementKind.VAR,
        variable: {
          name: 'a',
          type: { kind: TypeKind.INTEGER },
          value: {
            constValue: BigInt(10),
            isAssignable: false,
            isConstexpr: true,
            kind: ExprKind.INTEGER_LIT,
            type: { kind: TypeKind.INTEGER },
            value: BigInt(10)
          }
        }
      },
      {
        kind: StatementKind.VAR,
        variable: {
          name: 'b',
          type: { kind: TypeKind.INTEGER },
          value: {
            constValue: BigInt(20),
            isAssignable: false,
            isConstexpr: true,
            kind: ExprKind.INTEGER_LIT,
            type: { kind: TypeKind.INTEGER },
            value: BigInt(20)
          }
        }
      },
      {
        kind: StatementKind.EXPR,
        value: {
          arguments: [
            {
              ident: 'a',
              isAssignable: true,
              isConstexpr: false,
              constValue: undefined,
              kind: ExprKind.IDENT,
              type: { kind: TypeKind.INTEGER }
            },
            {
              ident: 'b',
              isAssignable: true,
              isConstexpr: false,
              constValue: undefined,
              kind: ExprKind.IDENT,
              type: { kind: TypeKind.INTEGER }
            }
          ],
          function: {
            ident: 'aplusb',
            isAssignable: true,
            isConstexpr: false,
            constValue: undefined,
            kind: ExprKind.IDENT,
            type: {
              arguments: [{ kind: TypeKind.INTEGER }, { kind: TypeKind.INTEGER }],
              kind: TypeKind.FUNCTION,
              return: { kind: TypeKind.INTEGER }
            }
          },
          isAssignable: false,
          isConstexpr: false,
          constValue: undefined,
          kind: ExprKind.CALL,
          type: { kind: TypeKind.INTEGER }
        }
      }
    ]
  }
}

interface Testcase {
  name: string
  sourceCode: string
  expectedProgram?: Program
  expectedErrors: Error[]
}

describe('analyzer test', () => {
  const testcases: Testcase[] = [{
    name: 'program 1',
    sourceCode: program1Source,
    expectedProgram: program1,
    expectedErrors: []
  }]

  for (const testcase of testcases) {
    it(testcase.name, () => {
      const { value: tokens, errors: scanErrors } = tokenize(testcase.sourceCode)
      if (scanErrors.length > 0) {
        expect(scanErrors).toStrictEqual(testcase.expectedErrors)
        return
      }

      if (tokens == null) fail()

      const { value: ast, errors: parseErrors } = parse(tokens)
      if (parseErrors.length > 0) {
        expect(parseErrors).toStrictEqual(testcase.expectedErrors)
        return
      }

      if (ast == null) fail()

      const { value: program, errors } = analyze(ast)

      expect(program).toStrictEqual(testcase.expectedProgram)
      expect(errors).toStrictEqual(testcase.expectedErrors)
    })
  }
})
