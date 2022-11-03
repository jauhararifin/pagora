import { tokenize } from './lexer'

export function compile (sourceCode: string): void {
  const tokens = tokenize(sourceCode)
  console.log(tokens)
}
