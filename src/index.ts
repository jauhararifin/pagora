import { analyze } from './analyzer'
import { parse } from './parser'
import { tokenize } from './lexer'

export function compile (sourceCode: string): void {
  const { value: tokens, errors: lexerErrors } = tokenize(sourceCode)
  if (lexerErrors.length > 0) {
    console.log(lexerErrors)
    return
  }

  if (tokens == null) return

  const { value: ast, errors: parsingErrors } = parse(tokens)
  if (parsingErrors.length > 0) {
    console.log(parsingErrors)
    return
  }

  if (ast == null) return

  const { value: program, errors: analyzingErrors } = analyze(ast)
  if (analyzingErrors.length > 0) {
    console.log(JSON.stringify(analyzingErrors))
    return
  }

  console.log(program)
}
