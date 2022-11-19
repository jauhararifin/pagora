export interface Displayer {
  drawRect: (
    x: number,
    y: number,
    width: number,
    height: number,
    strokeColor: string,
    bgColor: string
  ) => void
  getWidth: () => number
  getHeigh: () => number
  onKeyDown: (f: (key: string) => void) => void
  onMouseMove: (f: (x: number, y: number) => void) => void
  onMouseClick: (f: (x: number, y: number) => void) => void
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
    strokeColor: string,
    bgColor: string
  ): void {}

  getWidth(): number {
    return 0
  }

  getHeigh(): number {
    return 0
  }

  onKeyDown(f: (key: string) => void): void {}
  onMouseMove(f: (x: number, y: number) => void): void {}
  onMouseClick(f: (x: number, y: number) => void): void {}

  start(): void {}

  stop(): void {}

  clearAndReset(): void {}
}

export class CanvasDisplayer implements Displayer {
  context: CanvasRenderingContext2D
  keydownHandler: (key: string) => void
  mouseMoveHandler: (x: number, y: number) => void
  mouseClickHandler: (x: number, y: number) => void

  canvasKeydownHandler = (ev: KeyboardEvent): void => {
    this.keydownHandler(ev.key)
  }

  canvasMouseMoveHandler = (ev: MouseEvent): void => {
    this.mouseMoveHandler(ev.offsetX, ev.offsetY)
  }

  canvasMouseClickHandler = (ev: MouseEvent): void => {
    this.mouseClickHandler(ev.offsetX, ev.offsetY)
  }

  isStarted: boolean = false

  constructor(canvas: HTMLCanvasElement) {
    const context = canvas.getContext('2d')
    if (context == null) {
      throw new Error('cannot create 2d context from canvas')
    }
    this.context = context

    this.keydownHandler = (_) => {}
    this.mouseMoveHandler = (_, __) => {}
    this.mouseClickHandler = (_, __) => {}
  }

  drawRect(
    x: number,
    y: number,
    width: number,
    height: number,
    strokeColor: string,
    bgColor: string
  ): void {
    this.context.fillStyle = bgColor
    this.context.fillRect(x, y, width, height)
    this.context.strokeStyle = strokeColor
    this.context.strokeRect(x, y, width, height)
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

  onMouseMove(f: (x: number, y: number) => void): void {
    this.mouseMoveHandler = f
    if (this.isStarted) {
      this.context.canvas.addEventListener(
        'mousemove',
        this.canvasMouseMoveHandler
      )
    }
  }

  onMouseClick(f: (x: number, y: number) => void): void {
    this.mouseClickHandler = f
    if (this.isStarted) {
      this.context.canvas.addEventListener(
        'click',
        this.canvasMouseClickHandler
      )
    }
  }

  start(): void {
    this.context.canvas.addEventListener('keydown', this.canvasKeydownHandler)
    this.context.canvas.addEventListener(
      'mousemove',
      this.canvasMouseMoveHandler
    )
    this.context.canvas.addEventListener('click', this.canvasMouseClickHandler)
    this.isStarted = true
  }

  stop(): void {
    this.context.canvas.removeEventListener(
      'keydown',
      this.canvasKeydownHandler
    )
    this.context.canvas.removeEventListener(
      'mousemove',
      this.canvasMouseMoveHandler
    )
    this.context.canvas.removeEventListener(
      'click',
      this.canvasMouseClickHandler
    )
    this.isStarted = false
  }

  clearAndReset(): void {
    this.context.clearRect(0, 0, this.getWidth(), this.getHeigh())
  }
}
