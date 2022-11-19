export interface Displayer {
  drawRect: (
    x: number,
    y: number,
    width: number,
    height: number,
    color: string
  ) => void
  getWidth: () => number
  getHeigh: () => number
  onKeyDown: (f: (key: string) => void) => void
  start: () => void
  stop: () => void
  clearAndReset: () => void
}

export class NopDisplayer implements Displayer {
  drawRect(
    x: number,
    y: number,
    width: number,
    height: number,
    color: string
  ): void {}

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
  canvasKeydownHandler = (ev: KeyboardEvent): void => {
    this.keydownHandler(ev.key)
  }

  isStarted: boolean = false

  constructor(canvas: HTMLCanvasElement) {
    const context = canvas.getContext('2d')
    if (context == null) {
      throw new Error('cannot create 2d context from canvas')
    }
    this.context = context

    this.keydownHandler = (_) => {}
  }

  drawRect(
    x: number,
    y: number,
    width: number,
    height: number,
    color: string
  ): void {
    this.context.fillStyle = color
    this.context.fillRect(x, y, width, height)
  }

  getWidth(): number {
    return this.context.canvas.width
  }

  getHeigh(): number {
    return this.context.canvas.height
  }

  onKeyDown(f: (key: string) => void): void {
    this.keydownHandler = f
    if (this.isStarted) {
      this.context.canvas.addEventListener('keydown', this.canvasKeydownHandler)
    }
  }

  start(): void {
    this.context.canvas.addEventListener('keydown', this.canvasKeydownHandler)
    this.isStarted = true
  }

  stop(): void {
    this.context.canvas.removeEventListener(
      'keydown',
      this.canvasKeydownHandler
    )
    this.isStarted = false
  }

  clearAndReset(): void {
    this.context.clearRect(0, 0, this.getWidth(), this.getHeigh())
  }
}
