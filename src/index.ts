import { Program } from './semantic'
import { Result } from './errors'
import { analyze } from './analyzer'
import { parse } from './parser'
import { tokenize } from './lexer'

export function compile (sourceCode: string): Result<Program> {
  const { value: tokens, errors: lexerErrors } = tokenize(sourceCode)
  if (lexerErrors.length > 0) {
    return { errors: lexerErrors }
  }

  if (tokens == null) throw new Error('tokens is not null')

  const { value: ast, errors: parsingErrors } = parse(tokens)
  if (parsingErrors.length > 0) {
    console.log(parsingErrors)
    return { errors: parsingErrors }
  }

  if (ast == null) throw new Error('ast is not null')

  const { value: program, errors: analyzingErrors } = analyze(ast)
  if (analyzingErrors.length > 0) {
    console.log(JSON.stringify(analyzingErrors))
    return { errors: analyzingErrors }
  }

  return { value: program, errors: [] }
}
