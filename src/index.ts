import { compile } from './compiler'
import tetrisSourceCode from './examples/tetris.pago'

window.onload = function () {
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

  const exampleSelection = document.getElementById(
    'example-selection'
  )! as HTMLSelectElement
  const editor = document.getElementById('editor')! as HTMLTextAreaElement
  const interpretBtn = document.getElementById(
    'interpret-btn'
  )! as HTMLButtonElement

  interpretBtn.addEventListener('click', function () {
    const sourceCode = editor.value ?? ''
    console.log('source code', sourceCode)

    const program = compile(sourceCode)
    console.log(program)
  })

  const examples: { [key: string]: string } = {
    Tetris: tetrisSourceCode,
  }

  for (const name in examples) {
    const opt = document.createElement('option')
    opt.value = name
    opt.innerHTML = name
    exampleSelection.appendChild(opt)
  }

  exampleSelection.addEventListener('change', function () {
    const name = exampleSelection.value
    if (name in examples) {
      editor.value = examples[name]
    }
  })
}
