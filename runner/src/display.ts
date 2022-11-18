export interface Displayer {
  putPixel: (x: number, y: number, color: string) => void
  getWidth: () => number
  getHeigh: () => number
  onKeyDown: (f: (key: string) => void) => void
  start: () => void
  stop: () => void
  clearAndReset: () => void
}

export class NopDisplayer implements Displayer {
  putPixel(x: number, y: number, color: string): void {}

  getWidth(): number {
    return 0
  }

  getHeigh(): number {
    return 0
  }

  onKeyDown(f: (key: string) => void): void {}

  onUpdate(f: () => void): void {}

  start(): void {}

  stop(): void {}

  clearAndReset(): void {}
}

export class CanvasDisplayer implements Displayer {
  context: CanvasRenderingContext2D
  keydownHandler: (key: string) => void
  updateHandler: () => void

  constructor(canvas: HTMLCanvasElement) {
    const context = canvas.getContext('2d')
    if (context == null) {
      throw new Error('cannot create 2d context from canvas')
    }
    this.context = context

    this.keydownHandler = (_) => {}
    this.updateHandler = () => {}
  }

  putPixel(x: number, y: number, color: string): void {
    this.context.fillStyle = color
    this.context.fillRect(x, y, 1, 1)
  }

  getWidth(): number {
    return this.context.canvas.width
  }

  getHeigh(): number {
    return this.context.canvas.height
  }

  onKeyDown(f: (key: string) => void): void {
    this.keydownHandler = f
  }

  start(): void {
    this.context.canvas.onkeydown = (ev) => this.keydownHandler(ev.key)
  }

  stop(): void {
    this.context.canvas.onkeydown = null
  }

  clearAndReset(): void {
    this.context.clearRect(0, 0, this.getWidth(), this.getHeigh())
  }
}
