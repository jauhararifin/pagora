import { tokenize } from "./lexer";

function compile(sourceCode: string) {
    const tokens = tokenize(sourceCode)
    console.log(tokens)
}