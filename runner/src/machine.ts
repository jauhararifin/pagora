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
import { SymbolTable } from './symbols'
import { BooleanValue, FuncValue, Value, ValueKind } from './value'

export class Machine {
  displayer: Displayer = new NopDisplayer()
  statusWriter: StatusWriter = new NopStatusWriter()

  symbols: SymbolTable = new SymbolTable()
  returnVal: Value | undefined
  startTime: number | undefined
  timer: NodeJS.Timer | undefined

  onKeyDownHandler: FuncValue | undefined
  onUpdateHandler: FuncValue | undefined
  onMouseMoveHandler: FuncValue | undefined
  onMouseClickHandler: FuncValue | undefined

  start(program: Program): void {
    this.startTime = Date.now()
    this.symbols.reset()
    this.symbols.addScope()
    this.returnVal = { kind: ValueKind.VOID, value: undefined }

    this.onKeyDownHandler = undefined
    this.onMouseMoveHandler = undefined
    this.onMouseClickHandler = undefined
    this.onUpdateHandler = undefined

    for (const global of program.globals) {
      const value =
        global.value != null
          ? evalExpr(this.symbols, global.value)
          : zeroValue(global.type)
      this.symbols.setSymbol(global.name, value)
    }

    for (const func of program.functions) {
      const value: Value = {
        kind: ValueKind.FUNC,
        value: (args: Value[]): Value => {
          return this.executeFunc(func, args)
        },
      }
      this.symbols.setSymbol(func.name, value)
    }

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

  attachDisplayer(displayer: Displayer): void {
    this.displayer = displayer
    this.displayer.onKeyDown((key) => {
      this.keydownHandler(key)
    })
  }

  attachStatusWriter(statusWriter: StatusWriter): void {
    this.statusWriter = statusWriter
  }

  private keydownHandler(key: string): void {
    if (this.onKeyDownHandler === undefined) return
    this.onKeyDownHandler.value([{ kind: ValueKind.STRING, value: key }])
  }

  private mouseMoveHandler(x: number, y: number): void {
    if (this.onMouseMoveHandler === undefined) return
    this.onMouseMoveHandler.value([
      { kind: ValueKind.INTEGER, value: BigInt(x) },
      { kind: ValueKind.INTEGER, value: BigInt(y) },
    ])
  }

  private mouseClickHandler(x: number, y: number): void {
    if (this.onMouseClickHandler === undefined) return
    this.onMouseClickHandler.value([
      { kind: ValueKind.INTEGER, value: BigInt(x) },
      { kind: ValueKind.INTEGER, value: BigInt(y) },
    ])
  }

  private executeFunc(funcDecl: Function, args: Value[]): Value {
    if (funcDecl.body === undefined) {
      return this.executeNativeFunc(funcDecl.name, args)
    }

    this.returnVal = zeroValue(funcDecl.type.return)

    this.symbols.addScope()
    for (let i = 0; i < funcDecl.arguments.length; i++) {
      this.symbols.setSymbol(funcDecl.arguments[i], args[i])
    }
    this.executeBlockStmt(funcDecl.body)
    this.symbols.popScope()
    const val = this.returnVal
    this.returnVal = { kind: ValueKind.VOID, value: undefined }
    return val
  }

  private executeNativeFunc(name: string, args: Value[]): Value {
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

  private executeStatement(stmt: Statement): ControlKind {
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
        const value = evalExpr(this.symbols, stmt.value)
        const target = evalExpr(this.symbols, stmt.target)
        target.value = value.value
        return ControlKind.NORMAL
      }
      case StatementKind.EXPR:
        evalExpr(this.symbols, stmt.value)
        return ControlKind.NORMAL
      case StatementKind.RETURN:
        if (stmt.value !== undefined) {
          const value = evalExpr(this.symbols, stmt.value)
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

  private executeBlockStmt(stmt: BlockStatement): ControlKind {
    this.symbols.addScope()

    for (const s of stmt.body) {
      const control = this.executeStatement(s)
      switch (control) {
        case ControlKind.BREAK:
        case ControlKind.CONTINUE:
        case ControlKind.RETURN:
          this.symbols.popScope()
          return control
        case ControlKind.NORMAL:
          break
        default:
          throw new Error('unreachable')
      }
    }

    this.symbols.popScope()
    return ControlKind.NORMAL
  }

  private executeVarStmt(stmt: VarStatement): ControlKind {
    const varValue = zeroValue(stmt.variable.type)
    if (stmt.variable.value !== undefined) {
      const value = evalExpr(this.symbols, stmt.variable.value)
      varValue.value = value.value
    }
    this.symbols.setSymbol(stmt.variable.name, varValue)
    return ControlKind.NORMAL
  }

  private executeIfStmt(stmt: IfStatement): ControlKind {
    const cond = evalExpr(this.symbols, stmt.condition) as BooleanValue
    if (cond.value) {
      return this.executeStatement(stmt.body)
    } else if (stmt.else !== undefined) {
      return this.executeStatement(stmt.else)
    }
    return ControlKind.NORMAL
  }

  private executeWhileStmt(stmt: WhileStatement): ControlKind {
    while (true) {
      const cond = evalExpr(this.symbols, stmt.condition) as BooleanValue
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

  private setReturnVal(value: Value): void {
    this.returnVal = value
  }
}

enum ControlKind {
  NORMAL = 'NORMAL',
  CONTINUE = 'CONTINUE',
  BREAK = 'BREAK',
  RETURN = 'RETURN',
}

function evalExpr(symbols: SymbolTable, expr: Expr): Value {
  switch (expr.kind) {
    case ExprKind.BINARY:
      return evalBinary(symbols, expr)
    case ExprKind.UNARY:
      return evalUnary(symbols, expr)
    case ExprKind.INDEX: {
      const arr = evalExpr(symbols, expr.array)
      const indices = expr.indices.map((v) => evalExpr(symbols, v))

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
      throw new Error('Cast operation is not implemented yet')
    case ExprKind.CALL: {
      const args = expr.arguments.map((v) => evalExpr(symbols, v))
      const func = evalExpr(symbols, expr.function)
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
          const val = evalExpr(symbols, v)
          return { kind: val.kind, value: val.value }
        }),
      }
    case ExprKind.IDENT:
      return symbols.getSymbol(expr.ident)
    default:
      throw new Error(`eval ${expr.kind} is not implemented yet`)
  }
}

function evalBinary(symbols: SymbolTable, expr: BinaryExpr): Value {
  switch (expr.op) {
    case BinaryOp.PLUS: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.REAL, value: a.value + b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value + b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.MINUS: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.REAL, value: a.value - b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value - b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.DIV: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.REAL, value: a.value - b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value / b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.MUL: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.REAL, value: a.value * b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value * b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.MOD: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value % b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.AND: {
      const a = evalExpr(symbols, expr.a)
      if (a.kind !== ValueKind.BOOLEAN) {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
      if (!a.value) {
        return a
      }

      const b = evalExpr(symbols, expr.b)
      if (b.kind !== ValueKind.BOOLEAN) {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }

      return { kind: ValueKind.BOOLEAN, value: a.value && b.value }
    }
    case BinaryOp.OR: {
      const a = evalExpr(symbols, expr.a)
      if (a.kind !== ValueKind.BOOLEAN) {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
      if (a.value) {
        return a
      }

      const b = evalExpr(symbols, expr.b)
      if (b.kind !== ValueKind.BOOLEAN) {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }

      return { kind: ValueKind.BOOLEAN, value: a.value || b.value }
    }
    case BinaryOp.BIT_AND: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, BigInt.asIntN(64, a.value & b.value)),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.BIT_OR: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, BigInt.asIntN(64, a.value | b.value)),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.BIT_XOR: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, BigInt.asIntN(64, a.value ^ b.value)),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.EQUAL: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN) {
        return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
      } else if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
      } else if (a.kind === ValueKind.STRING && b.kind === ValueKind.STRING) {
        return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.NOT_EQUAL: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN) {
        return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
      } else if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
      } else if (a.kind === ValueKind.STRING && b.kind === ValueKind.STRING) {
        return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.GREATER_THAN: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.BOOLEAN, value: a.value > b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return { kind: ValueKind.BOOLEAN, value: a.value > b.value }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.GREATER_THAN_EQUAL: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.BOOLEAN, value: a.value >= b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return { kind: ValueKind.BOOLEAN, value: a.value >= b.value }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.LESS_THAN: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.BOOLEAN, value: a.value < b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return { kind: ValueKind.BOOLEAN, value: a.value < b.value }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.LESS_THAN_EQUAL: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL) {
        return { kind: ValueKind.BOOLEAN, value: a.value <= b.value }
      } else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return { kind: ValueKind.BOOLEAN, value: a.value <= b.value }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.SHIFT_LEFT: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value << b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    case BinaryOp.SHIFT_RIGHT: {
      const a = evalExpr(symbols, expr.a)
      const b = evalExpr(symbols, expr.b)
      if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER) {
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, a.value >> b.value),
        }
      } else {
        throw new Error(
          `Cannot perform binary operation ${expr.op} on ${a.kind}`
        )
      }
    }
    default:
      throw new Error(`Cannot perform binary operation. Unrecognized operator`)
  }
}

function evalUnary(symbols: SymbolTable, expr: UnaryExpr): Value {
  const v = evalExpr(symbols, expr.value)
  switch (expr.op) {
    case UnaryOp.PLUS:
      switch (v.kind) {
        case ValueKind.INTEGER:
          return { kind: ValueKind.INTEGER, value: v.value }
        case ValueKind.REAL:
          return { kind: ValueKind.REAL, value: v.value }
        default:
          throw new Error(
            `Cannot perform unary PLUS operation on ${v.kind} type`
          )
      }
    case UnaryOp.MINUS: {
      switch (v.kind) {
        case ValueKind.INTEGER:
          return { kind: ValueKind.INTEGER, value: BigInt.asIntN(64, -v.value) }
        case ValueKind.REAL:
          return { kind: ValueKind.REAL, value: -v.value }
        default:
          throw new Error(
            `Cannot perform unary MINUS operation on ${v.kind} type`
          )
      }
    }
    case UnaryOp.NOT:
      if (v.kind !== ValueKind.BOOLEAN) {
        throw new Error(`Cannot perform unary NOT operation on ${v.kind} type`)
      }
      return { kind: ValueKind.BOOLEAN, value: !v.value }
    case UnaryOp.BIT_NOT:
      if (v.kind !== ValueKind.INTEGER) {
        throw new Error(
          `Cannot perform unary BIT_NOT operation on ${v.kind} type`
        )
      }
      return { kind: ValueKind.INTEGER, value: BigInt.asIntN(64, ~v.value) }
  }
}

function zeroValue(t: Type): Value {
  switch (t.kind) {
    case TypeKind.INTEGER:
      return { kind: ValueKind.INTEGER, value: BigInt(0) }
    case TypeKind.REAL:
      return { kind: ValueKind.REAL, value: 0 }
    case TypeKind.BOOLEAN:
      return { kind: ValueKind.BOOLEAN, value: false }
    case TypeKind.STRING:
      return { kind: ValueKind.STRING, value: '' }
    case TypeKind.ARRAY:
      return zeroArray(t.dimension, t.elementType)
    case TypeKind.VOID:
      return { kind: ValueKind.VOID, value: undefined }
    default:
      throw new Error(
        `Failed creating zero value of ${t.kind}. ${t.kind} type is not implemented yet.`
      )
  }
}

function zeroArray(dimension: bigint[], elementType: Type): Value {
  const value = []
  if (dimension.length === 1) {
    for (let i = 0; i < dimension[0]; i++) value.push(zeroValue(elementType))
  } else {
    for (let i = 0; i < dimension[0]; i++)
      value.push(zeroArray(dimension.slice(1), elementType))
  }
  return { kind: ValueKind.ARRAY, value }
}
