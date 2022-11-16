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
