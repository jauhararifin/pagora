import { createContext } from "./context";
import { tokenize } from "./lexer";

function compile(sourceCode: string) {
    let ctx = createContext(sourceCode)
    const tokens = tokenize(ctx)
}