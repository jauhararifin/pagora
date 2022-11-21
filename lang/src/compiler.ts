import { analyze } from './analyzer'
import { parse } from './parser'
import { Program } from './semantic'
import { scan } from './scanner'

export function compile(sourceCode: string): Program {
  const tokens = scan(sourceCode)
  const ast = parse(tokens)
  const program = analyze(ast)
  return program
}
