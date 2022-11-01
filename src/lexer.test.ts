import { tokenize } from "./lexer"
import { Token, TokenKind } from "./tokens"

test("Dummy unit test", () => {
    const actual = 1 + 2
    expect(actual).toBe(3)
})

interface Testcase {
    name: string,
    sourceCode: string,
    expectedTokens: Token[],
    expectedErrors: string[],
}

describe("tokenize test", () => {
    const testcases: Testcase[] = [
        {
            name: 'if token',
            sourceCode: `if`,
            expectedTokens: [{ kind: TokenKind.If, value: "if", position: { line: 1, col: 1 } }],
            expectedErrors: [],
        },
    ]

    for (const testcase of testcases) {
        it(testcase.name, () => {
            const [tokens, errors] = tokenize(testcase.sourceCode)
            expect(tokens).toStrictEqual(testcase.expectedTokens)
            expect(errors).toStrictEqual(testcase.expectedErrors)
        })
    }
})