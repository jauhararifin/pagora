import { compile } from './compiler'
import { CompileError } from './errors'
import tetrisSourceCode from './examples/tetris.pago'
import { interpret } from './interpreter'

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

  const exampleSelection = document.getElementById(
    'example-selection'
  )! as HTMLSelectElement
  const editor = document.getElementById('editor')! as HTMLTextAreaElement
  const interpretBtn = document.getElementById(
    'interpret-btn'
  )! as HTMLButtonElement
  const statusText = document.getElementById('status')! as HTMLTextAreaElement

  interpretBtn.addEventListener('click', function () {
    const sourceCode = editor.value ?? ''

    try {
      const program = compile(sourceCode)
      statusText.value = ''
      interpret(program, context, statusText)
    } catch (e) {
      if (e instanceof CompileError) {
        statusText.value = e.message
      } else {
        console.log('got exception', e)
      }
    }
  })

  const examples: { [key: string]: string } = {
    Hello: `begin\n    output("Hello, World!");\nend`,
    Tetris: tetrisSourceCode,
  }

  for (const name in examples) {
    const opt = document.createElement('option')
    opt.value = name
    opt.innerHTML = name
    exampleSelection.appendChild(opt)
  }

  function setEditor(name: string): void {
    if (name in examples) {
      editor.value = examples[name]
    }
  }
  setEditor('Hello')

  exampleSelection.addEventListener('change', function () {
    const name = exampleSelection.value
    setEditor(name)
  })
}
