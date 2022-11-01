import { Token, TokenKind, Position } from "./tokens";

export function tokenize(sourceCode: string): [Token[], string[]] {
    const lexer = new Lexer(sourceCode)
    lexer.scan()
    return [lexer.getTokens(), lexer.getErrors()]
}

class CharPos {
    c: string
    pos: Position

    constructor(c: string, pos: Position) {
        this.c = c
        this.pos = pos
    }

    isEnd(): Boolean {
        return this.c == ""
    }
}

class Lexer {
    private sourceCode: CharPos[]
    private index: number
    private tokens: Token[]
    private errors: string[]

    constructor(sourceCode: string) {
        this.sourceCode = []
        this.index = 0
        this.tokens = []
        this.errors = []

        let line = 1
        let col = 0
        const codes = Array.from(sourceCode)
        for (const c of codes) {
            col++
            this.sourceCode.push(new CharPos(c, { line, col }))

            if (c == '\n') {
                line++
                col = 0
            }
        }
    }

    scan() {
        while (true) {
            this.advance()
            const c = this.peek()
            if (c.isEnd())
                break

            if (c.c == "\n") {
                this.emitToken(TokenKind.Endl, c.pos, "\n")
                this.next()
            } else if (c.c == '_' || (c.c >= 'a' && c.c <= 'z') || (c.c >= 'A' && c.c <= 'A')) {
                this.scanWord()
            } else if (c.c == '\"' || c.c == '`') {
                this.scanString()
            } else if (c.c >= '0' && c.c <= '9') {
                this.scanNumberLiteral()
            } else if ("'!%&|^~(){}[]*+-:<>,=".includes(c.c)) {
                this.scanSymbol()
            } else if (c.c == '/') {
                this.next()
                const next = this.peek()
                if (next.c == "/") {
                    this.scanComment(c.pos)
                } else {
                    this.emitToken(TokenKind.Div, c.pos, "/")
                }
            } else {
                this.emitError(`unrecognize token '${c.c}' at ${c.pos.line}:${c.pos.col}`)
                this.next()
            }
        }
    }

    private scanWord() {
        const position = this.peek().pos

        const word = this.consumeWhile((c) => c == '_' ||
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'A') ||
            (c >= '0' && c <= '9')
        )

        const map: { [key: string]: TokenKind } = {
            "var": TokenKind.Var,
            "type": TokenKind.Type,
            "struct": TokenKind.Struct,
            "function": TokenKind.Function,
            "begin": TokenKind.Begin,
            "end": TokenKind.End,
            "array": TokenKind.Array,
            "of": TokenKind.Of,
            "integer": TokenKind.Integer,
            "char": TokenKind.Char,
            "and": TokenKind.And,
            "not": TokenKind.Not,
            "or": TokenKind.Or,
            "if": TokenKind.If,
            "then": TokenKind.Then,
            "else": TokenKind.Else,
            "while": TokenKind.While,
            "for": TokenKind.For,
            "do": TokenKind.Do,
            "continue": TokenKind.Continue,
            "break": TokenKind.Break,
        }

        if (word in map) {
            this.emitToken(map[word], position, word)
        } else {
            this.emitToken(TokenKind.Identifier, position, word)
        }
    }

    private scanString() {
        const c = this.peek()
        const pos = c.pos
        const openingQuote = c.c

        const backslashes: { [key: string]: string } = {
            "n": "\n",
            "r": "\r",
            "t": "\t",
            "\\": "\\",
            "0": "\0",
            "\"": "\"",
            "'": "'",
            "`": "`",
        }

        let afterBackslash = false
        let value = ""

        this.next()
        while (true) {
            const c = this.peek()
            this.next()
            if (c.c == "\n") {
                this.emitError(`unexpected newline on string literal at ${c.pos.line}:${c.pos.col}`)
            }

            if (afterBackslash) {
                if (c.c in backslashes) {
                    value += backslashes[c.c]
                } else {
                    this.emitError(`unknown \${c.c} character at ${c.pos.line}:${c.pos.col}`)
                }
                afterBackslash = false
            } else if (c.c == "\\") {
                afterBackslash = true
            } else if (c.c == openingQuote) {
                break
            } else {
                value += c.c
            }
        }

        this.emitToken(TokenKind.StringLiteral, pos, value)
    }

    private scanNumberLiteral() {
        const c = this.peek()
        const value = this.consumeWhile((c) => (c >= '0' && c <= '9') || c == '_')
        this.emitToken(TokenKind.IntegerLiteral, c.pos, value)
    }

    private scanSymbol() {
        const first = this.peek()
        this.next()
        const second = this.peek()

        const symbolMap: [string[], TokenKind][] = [
            [["!", "="], TokenKind.NotEqual],
            [["!"], TokenKind.Not],
            [["="], TokenKind.Equal],
            [["*"], TokenKind.Multiply],
            [["+"], TokenKind.Plus],
            [["->"], TokenKind.Arrow],
            [["-"], TokenKind.Minus],
            [["/"], TokenKind.Div],
            [[":"], TokenKind.Colon],
            [[";"], TokenKind.Semicolon],
            [["<", "="], TokenKind.LessThanEqual],
            [["<"], TokenKind.LessThan],
            [[">", "="], TokenKind.GreaterThanEqual],
            [[">"], TokenKind.GreaterThan],
            [["["], TokenKind.OpenSquare],
            [["]"], TokenKind.CloseSquare],
            [["("], TokenKind.OpenBrac],
            [[")"], TokenKind.CloseBrac],
        ]

        const key = [first.c, second.c]
        for (const item of symbolMap) {
            let matched = true
            let value = ""
            for (let i = 0; i < item[0].length; i++) {
                value += key[i]
                if (item[0][i] != key[i]) {
                    matched = false
                    break
                }
            }

            if (matched) {
                this.emitToken(item[1], first.pos, value)
                if (symbolMap.length > 1)
                    this.next()
                return
            }
        }

        this.emitError(`unexpected symbol at ${first.pos.line}:${first.pos.col}`)
    }

    private scanComment(pos: Position) {
        const value = this.consumeWhile((c) => c != '\n')
        this.emitToken(TokenKind.Comment, pos, value)
    }

    getTokens(): Token[] { return this.tokens }

    getErrors(): string[] { return this.errors }

    private advance(): CharPos {
        while (true) {
            const c = this.peek()
            if (c.c == " " || c.c == "\t" || c.c == "\r")
                this.next()
            else
                return c
        }
    }

    private peek(): CharPos {
        if (this.index >= this.sourceCode.length) {
            return new CharPos("", { line: 0, col: 0 })
        }

        return this.sourceCode[this.index]
    }

    private next() {
        if (this.index >= this.sourceCode.length) {
            return new CharPos("", { line: 0, col: 0 })
        }

        this.index++
    }

    private consumeWhile(f: (c: string) => Boolean): string {
        let value = ""
        while (true) {
            const c = this.peek()
            if (c.isEnd() || !f(c.c)) {
                break
            }
            value += c.c
            this.next()
        }
        return value
    }

    private emitToken(kind: TokenKind, position: Position, value: string) {
        this.tokens.push({ kind, position, value })
    }

    private emitError(err: string) {
        this.errors.push(err)
    }
}