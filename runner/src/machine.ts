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

    executeStatement(this.symbols, program.main)

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

    let returnVal = zeroValue(funcDecl.type.return)

    this.symbols.addScope()
    for (let i = 0; i < funcDecl.arguments.length; i++) {
      this.symbols.setSymbol(funcDecl.arguments[i], args[i])
    }
    const control = executeBlockStmt(this.symbols, funcDecl.body)
    this.symbols.popScope()

    if (control.kind === ControlKind.RETURN) {
      returnVal = control.value
    }

    return returnVal
  }

  private executeNativeFunc(name: string, args: Value[]): Value {
    switch (name) {
      case 'output':
        if (args.length !== 1) {
          throw new Error(
            `invalid arguments. Expected 1 argument, but found ${args.length} arguments`
          )
        }
        if (args[0].kind !== ValueKind.STRING) {
          throw new Error(
            `invalid arguments. First argument expected to be string but found ${args[0].kind}`
          )
        }
        this.statusWriter.append(args[0].value)
        return { kind: ValueKind.VOID, value: undefined }
      case 'draw_rect': {
        if (args.length !== 6) {
          throw new Error(
            `invalid arguments. Expected 6 argument, but found ${args.length} arguments`
          )
        }

        const [xVal, yVal, wVal, hVal, strokeVal, bgVal] = args
        if (xVal.kind !== ValueKind.INTEGER) {
          throw new Error(
            `invalid arguments. First argument expected to be integer but found ${args[0].kind}`
          )
        }
        if (yVal.kind !== ValueKind.INTEGER) {
          throw new Error(
            `invalid arguments. Second argument expected to be integer but found ${args[1].kind}`
          )
        }
        if (wVal.kind !== ValueKind.INTEGER) {
          throw new Error(
            `invalid arguments. Third argument expected to be integer but found ${args[2].kind}`
          )
        }
        if (hVal.kind !== ValueKind.INTEGER) {
          throw new Error(
            `invalid arguments. Forth argument expected to be integer but found ${args[3].kind}`
          )
        }
        if (strokeVal.kind !== ValueKind.STRING) {
          throw new Error(
            `invalid arguments. Fifth argument expected to be integer but found ${args[4].kind}`
          )
        }
        if (bgVal.kind !== ValueKind.STRING) {
          throw new Error(
            `invalid arguments. Sixth argument expected to be integer but found ${args[5].kind}`
          )
        }

        const x = Number(xVal.value)
        const y = Number(yVal.value)
        const w = Number(wVal.value)
        const h = Number(hVal.value)
        const strokeColor = strokeVal.value
        const bgColor = bgVal.value
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
        if (args.length !== 1) {
          throw new Error(
            `invalid arguments. Expected 1 argument, but found ${args.length} arguments`
          )
        }
        if (args[0].kind !== ValueKind.FUNC) {
          throw new Error(
            `invalid arguments. First argument expected to be a function but found ${args[0].kind}`
          )
        }
        this.onUpdateHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'register_on_keydown':
        if (args.length !== 1) {
          throw new Error(
            `invalid arguments. Expected 1 argument, but found ${args.length} arguments`
          )
        }
        if (args[0].kind !== ValueKind.FUNC) {
          throw new Error(
            `invalid arguments. First argument expected to be a function but found ${args[0].kind}`
          )
        }
        if (args[0].kind !== ValueKind.FUNC)
          throw new Error('invalid state. registering non function')
        this.onKeyDownHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'register_on_mouse_move':
        if (args.length !== 1) {
          throw new Error(
            `invalid arguments. Expected 1 argument, but found ${args.length} arguments`
          )
        }
        if (args[0].kind !== ValueKind.FUNC) {
          throw new Error(
            `invalid arguments. First argument expected to be a function but found ${args[0].kind}`
          )
        }
        if (args[0].kind !== ValueKind.FUNC)
          throw new Error('invalid state. registering non function')
        this.onMouseMoveHandler = args[0]
        return { kind: ValueKind.VOID, value: undefined }
      case 'register_on_mouse_click':
        if (args.length !== 1) {
          throw new Error(
            `invalid arguments. Expected 1 argument, but found ${args.length} arguments`
          )
        }
        if (args[0].kind !== ValueKind.FUNC) {
          throw new Error(
            `invalid arguments. First argument expected to be a function but found ${args[0].kind}`
          )
        }
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
}

type Control =
  | { kind: ControlKind.NORMAL | ControlKind.CONTINUE | ControlKind.BREAK }
  | { kind: ControlKind.RETURN; value: Value }

enum ControlKind {
  NORMAL = 'NORMAL',
  CONTINUE = 'CONTINUE',
  BREAK = 'BREAK',
  RETURN = 'RETURN',
}

function executeStatement(symbols: SymbolTable, stmt: Statement): Control {
  switch (stmt.kind) {
    case StatementKind.BLOCK:
      return executeBlockStmt(symbols, stmt)
    case StatementKind.VAR:
      return executeVarStmt(symbols, stmt)
    case StatementKind.IF: {
      return executeIfStmt(symbols, stmt)
    }
    case StatementKind.WHILE:
      return executeWhileStmt(symbols, stmt)
    case StatementKind.ASSIGN: {
      const value = evalExpr(symbols, stmt.value)
      const target = evalExpr(symbols, stmt.target)
      target.value = value.value
      return { kind: ControlKind.NORMAL }
    }
    case StatementKind.EXPR:
      evalExpr(symbols, stmt.value)
      return { kind: ControlKind.NORMAL }
    case StatementKind.RETURN: {
      let returnValue: Value
      if (stmt.value !== undefined) {
        const value = evalExpr(symbols, stmt.value)
        returnValue = value
      } else {
        returnValue = { kind: ValueKind.VOID, value: undefined }
      }
      return { kind: ControlKind.RETURN, value: returnValue }
    }
    case StatementKind.BREAK:
      return { kind: ControlKind.BREAK }
    case StatementKind.CONTINUE:
      return { kind: ControlKind.CONTINUE }
  }
}

function executeBlockStmt(symbols: SymbolTable, stmt: BlockStatement): Control {
  symbols.addScope()

  for (const s of stmt.body) {
    const control = executeStatement(symbols, s)
    switch (control.kind) {
      case ControlKind.BREAK:
      case ControlKind.CONTINUE:
      case ControlKind.RETURN:
        symbols.popScope()
        return control
      case ControlKind.NORMAL:
        break
      default:
        throw new Error('unreachable')
    }
  }

  symbols.popScope()
  return { kind: ControlKind.NORMAL }
}

function executeVarStmt(symbols: SymbolTable, stmt: VarStatement): Control {
  const varValue = zeroValue(stmt.variable.type)
  if (stmt.variable.value !== undefined) {
    const value = evalExpr(symbols, stmt.variable.value)
    varValue.value = value.value
  }
  symbols.setSymbol(stmt.variable.name, varValue)
  return { kind: ControlKind.NORMAL }
}

function executeIfStmt(symbols: SymbolTable, stmt: IfStatement): Control {
  const cond = evalExpr(symbols, stmt.condition) as BooleanValue
  if (cond.value) {
    return executeStatement(symbols, stmt.body)
  } else if (stmt.else !== undefined) {
    return executeStatement(symbols, stmt.else)
  }

  return { kind: ControlKind.NORMAL }
}

function executeWhileStmt(symbols: SymbolTable, stmt: WhileStatement): Control {
  while (true) {
    const cond = evalExpr(symbols, stmt.condition) as BooleanValue
    if (!cond.value) break

    const control = executeStatement(symbols, stmt.body)
    if (control.kind === ControlKind.BREAK) {
      return { kind: ControlKind.NORMAL }
    } else if (control.kind === ControlKind.CONTINUE) {
      continue
    } else if (control.kind === ControlKind.RETURN) {
      return control
    } else if (control.kind !== ControlKind.NORMAL) {
      throw new Error(`unreachable state`)
    }
  }

  return { kind: ControlKind.NORMAL }
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
        val = val.value[Number(i.value)]
      }

      return val
    }
    case ExprKind.CAST:
      if (
        expr.source.type.kind === TypeKind.REAL &&
        expr.type.kind === TypeKind.INTEGER
      ) {
        const source = evalExpr(symbols, expr.source)
        if (source.kind !== ValueKind.REAL) {
          throw new Error(
            `invalid state, source should be resolved to a real number`
          )
        }
        return {
          kind: ValueKind.INTEGER,
          value: BigInt.asIntN(64, BigInt(source.value)),
        }
      }
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
