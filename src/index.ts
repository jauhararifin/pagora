import { Program } from './semantic'
import { analyze } from './analyzer'
import { parse } from './parser'
import { tokenize } from './lexer'

export function compile(sourceCode: string): Program {
  const tokens = tokenize(sourceCode)
  const ast = parse(tokens)
  const program = analyze(ast)
  return program
}
