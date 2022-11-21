import {
  BinaryExpr,
  BinaryOp,
  BlockStatement,
  Expr,
  ExprKind,
  Function,
  IfStatement,
  Program,
  Statement,
  StatementKind,
  Type,
  TypeKind,
  UnaryExpr,
  UnaryOp,
  VarStatement,
  WhileStatement,
} from '@pagora/lang'
import { Displayer, NopDisplayer } from './display'
import { NopStatusWriter, StatusWriter } from './status'
import { BooleanValue, FuncValue, Value, ValueKind } from './value'

export class Machine {
  displayer: Displayer = new NopDisplayer()
  statusWriter: StatusWriter = new NopStatusWriter()

  symbols: Array<{ [name: string]: Value }> = []
  returnVal: Value | undefined
  startTime: number | undefined
  timer: NodeJS.Timer | undefined

  onKeyDownHandler: FuncValue | undefined
  onUpdateHandler: FuncValue | undefined
  onMouseMoveHandler: FuncValue | undefined
  onMouseClickHandler: FuncValue | undefined

  attachDisplayer(displayer: Displayer): void {
    this.displayer = displayer
    this.displayer.onKeyDown((key) => {
      this.keydownHandler(key)
    })
  }

  attachStatusWriter(statusWriter: StatusWriter): void {
    this.statusWriter = statusWriter
  }

  start(program: Program): void {
    this.startTime = Date.now()
    this.symbols = []
    this.returnVal = { kind: ValueKind.VOID, value: undefined }

    this.addScope()

    for (const global of program.globals) {
      const value =
        global.value != null
          ? this.evalExpr(global.value)
          : this.zeroValue(global.type)
      this.setSymbol(global.name, value)
    }

    for (const func of program.functions) {
      const value: Value = {
        kind: ValueKind.FUNC,
        value: (args: Value[]): Value => {
          return this.executeFunc(func, args)
        },
      }
      this.setSymbol(func.name, value)
    }

    this.onKeyDownHandler = undefined
    this.onMouseMoveHandler = undefined
    this.onMouseClickHandler = undefined
    this.onUpdateHandler = undefined

    this.statusWriter.clear()
    this.displayer.clearAndReset()
    this.displayer.onKeyDown((key) => {
      this.keydownHandler(key)
    })
    this.displayer.onMouseMove((x, y) => {
      this.mouseMoveHandler(x, y)
    })
    this.displayer.onMouseClick((x, y) => {
      this.mouseClickHandler(x, y)
    })

    this.executeStatement(program.main)

    const fps = 12
    if (this.timer === undefined) {
      this.timer = setInterval(() => {
        if (this.onUpdateHandler != null) {
          this.onUpdateHandler.value([])
        }
      }, 1000 / fps)
    }

    this.displayer.start()
  }

  stop(): void {
    this.displayer.stop()

    if (this.timer !== undefined) {
      clearInterval(this.timer)
      this.timer = undefined
    }
  }

  keydownHandler(key: string): void {
    if (this.onKeyDownHandler === undefined) return
    this.onKeyDownHandler.value([{ kind: ValueKind.STRING, value: key }])
  }

  mouseMoveHandler(x: number, y: number): void {
    if (this.onMouseMoveHandler === undefined) return
    this.onMouseMoveHandler.value([
      { kind: ValueKind.INTEGER, value: BigInt(x) },
      { kind: ValueKind.INTEGER, value: BigInt(y) },
    ])
  }

  mouseClickHandler(x: number, y: number): void {
    if (this.onMouseClickHandler === undefined) return
    this.onMouseClickHandler.value([
      { kind: ValueKind.INTEGER, value: BigInt(x) },
      { kind: ValueKind.INTEGER, value: BigInt(y) },
    ])
  }

  executeFunc(funcDecl: Function, args: Value[]): Value {
    if (funcDecl.body === undefined) {
      return this.executeNativeFunc(funcDecl.name, args)
    }

    this.returnVal = this.zeroValue(funcDecl.type.return)

    this.addScope()
    for (let i = 0; i < funcDecl.arguments.length; i++) {
      this.setSymbol(funcDecl.arguments[i].name, args[i])
    }
    this.executeBlockStmt(funcDecl.body)
    this.popScope()
    const val = this.returnVal
    this.returnVal = { kind: ValueKind.VOID, value: undefined }
    return val
  }

  executeNativeFunc(name: string, args: Value[]): Value {
    switch (name) {
      case 'output':
        if (args[0].kind !== ValueKind.STRING)
          throw new Error('invalid arguments')
        // if (args[0].value.startsWith('step')) {
        //   console.log(this.symbols)
        //   // eslint-disable-next-line no-debugger
        //   debugger
        // }
        this.statusWriter.append(args[0].value)
        return { kind: ValueKind.VOID, value: undefined }
      case 'draw_rect': {
        const [xVal, yVal, wVal, hVal, strokeVal, bgVal] = args
        const x = Number(xVal.value as bigint)
        const y = Number(yVal.value as bigint)
        const w = Number(wVal.value as bigint)
        const h = Number(hVal.value as bigint)
        const strokeColor = strokeVal.value as string
        const bgColor = bgVal.value as string
        this.displayer.drawRect(x, y, w, h, strokeColor, bgColor)
        return { kind: ValueKind.VOID, value: undefined }
      }
      case 'get_width':
        return {
          kind: ValueKind.INTEGER,
          value: BigInt(this.displayer.getWidth()),
        }
      case 'get_height':
        return {
          kind: ValueKind.INTEGER,
          value: BigInt(this.displayer.getHeigh()),
        }
      case 'register_on_update':
        if (args[0].kind !== ValueKind.FUNC)
          throw new Error('invalid state. registering non function')
        this.onUpdateHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'register_on_keydown':
        if (args[0].kind !== ValueKind.FUNC)
          throw new Error('invalid state. registering non function')
        this.onKeyDownHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'register_on_mouse_move':
        if (args[0].kind !== ValueKind.FUNC)
          throw new Error('invalid state. registering non function')
        this.onMouseMoveHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'register_on_mouse_click':
        if (args[0].kind !== ValueKind.FUNC)
          throw new Error('invalid state. registering non function')
        this.onMouseClickHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'unix_time_millis':
        return {
          kind: ValueKind.INTEGER,
          value: BigInt(Date.now()),
        }
      case 'system_time_millis':
        return {
          kind: ValueKind.INTEGER,
          value: BigInt(Date.now() - this.startTime!),
        }
      default:
        throw new Error(`native function '${name}' is not implemented yet`)
    }
  }

  executeStatement(stmt: Statement): ControlKind {
    switch (stmt.kind) {
      case StatementKind.BLOCK:
        return this.executeBlockStmt(stmt)
      case StatementKind.VAR:
        return this.executeVarStmt(stmt)
      case StatementKind.IF: {
        return this.executeIfStmt(stmt)
      }
      case StatementKind.WHILE:
        return this.executeWhileStmt(stmt)
      case StatementKind.ASSIGN: {
        const value = this.evalExpr(stmt.value)
        const target = this.evalExpr(stmt.target)
        target.value = value.value
        return ControlKind.NORMAL
      }
      case StatementKind.EXPR:
        this.evalExpr(stmt.value)
        return ControlKind.NORMAL
      case StatementKind.RETURN:
        if (stmt.value !== undefined) {
          const value = this.evalExpr(stmt.value)
          this.setReturnVal(value)
        } else {
          this.setReturnVal({ kind: ValueKind.VOID, value: undefined })
        }
        return ControlKind.RETURN
      case StatementKind.BREAK:
        return ControlKind.BREAK
      case StatementKind.CONTINUE:
        return ControlKind.CONTINUE
      default:
        throw new Error('unreachable')
    }
  }

  executeBlockStmt(stmt: BlockStatement): ControlKind {
    this.addScope()

    for (const s of stmt.body) {
      const control = this.executeStatement(s)
      switch (control) {
        case ControlKind.BREAK:
        case ControlKind.CONTINUE:
        case ControlKind.RETURN:
          this.popScope()
          return control
        case ControlKind.NORMAL:
          break
        default:
          throw new Error('unreachable')
      }
    }

    this.popScope()
    return ControlKind.NORMAL
  }

  executeVarStmt(stmt: VarStatement): ControlKind {
    const varValue = this.zeroValue(stmt.variable.type)
    if (stmt.variable.value !== undefined) {
      const value = this.evalExpr(stmt.variable.value)
      varValue.value = value.value
    }
    this.setSymbol(stmt.variable.name, varValue)
    return ControlKind.NORMAL
  }

  executeIfStmt(stmt: IfStatement): ControlKind {
    const cond = this.evalExpr(stmt.condition) as BooleanValue
    if (cond.value) {
      return this.executeStatement(stmt.body)
    } else if (stmt.else !== undefined) {
      return this.executeStatement(stmt.else)
    }
    return ControlKind.NORMAL
  }

  executeWhileStmt(stmt: WhileStatement): ControlKind {
    while (true) {
      const cond = this.evalExpr(stmt.condition) as BooleanValue
      if (!cond.value) break

      const control = this.executeStatement(stmt.body)
      switch (control) {
        case ControlKind.BREAK:
          return ControlKind.NORMAL
        case ControlKind.CONTINUE:
          continue
        case ControlKind.RETURN:
          return control
        case ControlKind.NORMAL:
          break
        default:
          throw new Error('unreachable')
      }
    }

    return ControlKind.NORMAL
  }

  evalExpr(expr: Expr): Value {
    switch (expr.kind) {
      case ExprKind.BINARY:
        return this.evalBinary(expr)
      case ExprKind.UNARY:
        return this.evalUnary(expr)
      case ExprKind.INDEX: {
        const arr = this.evalExpr(expr.array)
        const indices = expr.indices.map((v) => this.evalExpr(v))

        if (expr.array.type.kind !== TypeKind.ARRAY) {
          throw new Error('invalid state. indexing non array')
        }

        let val = arr
        for (const i of indices) {
          if (i.kind !== ValueKind.INTEGER) {
            throw new Error('invalid state. indexing witout integer')
          }
          // if (val === undefined || i === undefined) {
          //   console.log('indexerrror', {
          //     val,
          //     i,
          //     expr,
          //     arr,
          //     indices,
          //     sym: this.symbols,
          //   })
          // }
          val = val.value[Number(i.value)]
        }

        return val
      }
      case ExprKind.CAST:
        throw new Error('not implemented yet')
      case ExprKind.CALL: {
        const args = expr.arguments.map((v) => this.evalExpr(v))
        const func = this.evalExpr(expr.function)
        if (func.kind !== ValueKind.FUNC) {
          throw new Error('invalid state. calling non functtion')
        }

        return func.value(args)
      }
      case ExprKind.INTEGER_LIT:
        return { kind: ValueKind.INTEGER, value: expr.value }
      case ExprKind.BOOLEAN_LIT:
        return { kind: ValueKind.BOOLEAN, value: expr.value }
      case ExprKind.STRING_LIT:
        return { kind: ValueKind.STRING, value: expr.value }
      case ExprKind.ARRAY_LIT:
        return {
          kind: ValueKind.ARRAY,
          value: expr.values.map((v) => {
            const val = this.evalExpr(v)
            return { kind: val.kind, value: val.value }
          }),
        }
      case ExprKind.IDENT:
        return this.getSymbol(expr.ident)
      default:
        throw new Error(`eval ${expr.kind} is not implemented yet`)
    }
  }

  evalBinary(expr: BinaryExpr): Value {
    switch (expr.op) {
      case BinaryOp.PLUS: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.REAL, value: a.value + b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value + b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.MINUS: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.REAL, value: a.value - b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value - b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.DIV: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value / b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.MUL: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.REAL, value: a.value * b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value * b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.MOD: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value % b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.AND: {
        const a = this.evalExpr(expr.a)
        if (a.kind !== ValueKind.BOOLEAN)
          throw new Error(`invalid state. cannot perform binop ${expr.op}`)
        if (!a.value) return a

        const b = this.evalExpr(expr.b)
        if (b.kind !== ValueKind.BOOLEAN)
          throw new Error(`invalid state. cannot perform binop ${expr.op}`)

        return { kind: ValueKind.BOOLEAN, value: a.value && b.value }
      }
      case BinaryOp.OR: {
        const a = this.evalExpr(expr.a)
        if (a.kind !== ValueKind.BOOLEAN)
          throw new Error(`invalid state. cannot perform binop ${expr.op}`)
        if (a.value) return a

        const b = this.evalExpr(expr.b)
        if (b.kind !== ValueKind.BOOLEAN)
          throw new Error(`invalid state. cannot perform binop ${expr.op}`)

        return { kind: ValueKind.BOOLEAN, value: a.value || b.value }
      }
      case BinaryOp.BIT_AND: {
        throw new Error('not implemented yet')
      }
      case BinaryOp.BIT_OR: {
        throw new Error('not implemented yet')
      }
      case BinaryOp.BIT_XOR: {
        throw new Error('not implemented yet')
      }
      case BinaryOp.EQUAL: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else if (a.kind === ValueKind.STRING && b.kind === ValueKind.STRING)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else {
          throw new Error(`invalid state. cannot perform binop ${expr.op}`)
        }
      }
      case BinaryOp.NOT_EQUAL: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else if (a.kind === ValueKind.STRING && b.kind === ValueKind.STRING)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.GREATER_THAN: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value > b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value > b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.GREATER_THAN_EQUAL: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value >= b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value >= b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.LESS_THAN: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value < b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value < b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.LESS_THAN_EQUAL: {
        const a = this.evalExpr(expr.a)
        const b = this.evalExpr(expr.b)
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value <= b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value <= b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      }
      case BinaryOp.SHIFT_LEFT:
        throw new Error('not implemented yet')
      case BinaryOp.SHIFT_RIGHT:
        throw new Error('not implemented yet')
    }
  }

  evalUnary(expr: UnaryExpr): Value {
    const v = this.evalExpr(expr.value)
    switch (expr.op) {
      case UnaryOp.PLUS:
        return v
      case UnaryOp.MINUS: {
        switch (v.kind) {
          case ValueKind.INTEGER:
            return { kind: ValueKind.INTEGER, value: -v.value }
          case ValueKind.REAL:
            return { kind: ValueKind.REAL, value: -v.value }
          default:
            throw new Error('invalid state. cannot perform MINUS operator')
        }
      }
      case UnaryOp.NOT:
        if (v.kind !== ValueKind.BOOLEAN) {
          throw new Error('invalid state. cannot perform NOT operator')
        }
        return { kind: ValueKind.BOOLEAN, value: !v.value }
      case UnaryOp.BIT_NOT:
        throw new Error('not implemented yet')
    }
  }

  zeroValue(t: Type): Value {
    switch (t.kind) {
      case TypeKind.INTEGER:
        return { kind: ValueKind.INTEGER, value: BigInt(0) }
      case TypeKind.REAL:
        return { kind: ValueKind.REAL, value: 0 }
      case TypeKind.BOOLEAN:
        return { kind: ValueKind.BOOLEAN, value: false }
      case TypeKind.STRING:
        return { kind: ValueKind.STRING, value: '' }
      case TypeKind.ARRAY: {
        return this.zeroArray(t.dimension, t.type)
      }
      case TypeKind.VOID:
        return { kind: ValueKind.VOID, value: undefined }
      default:
        throw new Error('not implemented yet')
    }
  }

  zeroArray(dimension: bigint[], elementType: Type): Value {
    const value = []
    if (dimension.length === 1) {
      for (let i = 0; i < dimension[0]; i++)
        value.push(this.zeroValue(elementType))
    } else {
      for (let i = 0; i < dimension[0]; i++)
        value.push(this.zeroArray(dimension.slice(1), elementType))
    }
    return { kind: ValueKind.ARRAY, value }
  }

  setReturnVal(value: Value): void {
    this.returnVal = value
  }

  addScope(): void {
    this.symbols.push({})
  }

  setSymbol(name: string, value: Value): void {
    this.symbols[this.symbols.length - 1][name] = value
  }

  getSymbol(name: string): Value {
    for (let i = this.symbols.length - 1; i >= 0; i--) {
      if (name in this.symbols[i]) {
        return this.symbols[i][name]
      }
    }
    throw new Error(`invalid state. searching undefined symbol ${name}`)
  }

  popScope(): void {
    this.symbols.pop()
  }
}

enum ControlKind {
  NORMAL = 'NORMAL',
  CONTINUE = 'CONTINUE',
  BREAK = 'BREAK',
  RETURN = 'RETURN',
}
