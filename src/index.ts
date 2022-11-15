import { analyze } from './analyzer'
import { parse } from './parser'
import { tokenize } from './lexer'
import { Program } from './semantic'

export function compile(sourceCode: string): Program {
  const tokens = tokenize(sourceCode)
  const ast = parse(tokens)
  const program = analyze(ast)
  return program
}

const canvas = document.querySelector('canvas')!
const canvasWrapper = document.getElementById('canvas-wrapper')!

canvas.width = canvasWrapper.clientWidth
canvas.height = canvasWrapper.clientHeight
addEventListener('resize', () => {
  canvas.width = canvasWrapper.clientWidth
  canvas.height = canvasWrapper.clientHeight
})

const context = canvas.getContext('2d')!
context.fillStyle = '#000'
for (let i = 0; i < 20; i++) context.fillRect(10 + i, 10, 1, 1)
