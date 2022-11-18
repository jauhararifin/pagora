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
    this.onKeyDownHandler = undefined
    this.onUpdateHandler = undefined

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
    this.onUpdateHandler = undefined

    this.statusWriter.clear()
    this.displayer.clearAndReset()
    this.displayer.onKeyDown((key) => {
      this.keydownHandler(key)
    })

    this.executeStatement(program.main)

    const fps = 24
    if (this.timer === undefined) {
      this.timer = setInterval(() => {
        if (this.onUpdateHandler != null) {
          this.onUpdateHandler.value([])
        }
      }, 1000 / fps)
    }
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

  executeFunc(funcDecl: Function, args: Value[]): Value {
    if (funcDecl.body === undefined) {
      return this.executeNativeFunc(funcDecl.name, args)
    }

    this.addScope()
    for (let i = 0; i < funcDecl.arguments.length; i++) {
      this.setSymbol(funcDecl.arguments[i].name, args[i])
    }
    this.executeBlockStmt(funcDecl.body)
    const val = this.returnVal!
    this.returnVal = { kind: ValueKind.VOID, value: undefined }
    return val
  }

  executeNativeFunc(name: string, args: Value[]): Value {
    switch (name) {
      case 'output':
        if (args[0].kind !== ValueKind.STRING)
          throw new Error('invalid arguments')
        this.statusWriter.append(args[0].value)
        return { kind: ValueKind.VOID, value: undefined }
      case 'draw_pixel': {
        const [xVal, yVal, colorVal] = args
        const x = Number(xVal.value as bigint)
        const y = Number(yVal.value as bigint)
        const color = colorVal.value as string
        this.displayer.putPixel(x, y, color)
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
      case 'system_time_milis':
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
    this.setSymbol(stmt.variable.name, this.zeroValue(stmt.variable.type))
    if (stmt.variable.value !== undefined) {
      const value = this.evalExpr(stmt.variable.value)
      this.setSymbol(stmt.variable.name, value)
    }
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

        let index = 0
        let size = 1
        for (let i = expr.array.type.dimension.length - 1; i >= 0; i--) {
          if (indices[i].kind !== ValueKind.INTEGER) {
            throw new Error('invalid state. indexing witout integer')
          }
          index += Number(indices[i].value) * size
          size *= Number(expr.array.type.dimension[i])
        }

        return arr.value[index]
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
          value: expr.values.flatMap((v) => this.evalExpr(v)),
        }
      case ExprKind.IDENT:
        return this.getSymbol(expr.ident)
      default:
        throw new Error(`eval ${expr.kind} is not implemented yet`)
    }
  }

  evalBinary(expr: BinaryExpr): Value {
    const a = this.evalExpr(expr.a)
    const b = this.evalExpr(expr.b)
    switch (expr.op) {
      case BinaryOp.PLUS:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.REAL, value: a.value + b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value + b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.MINUS:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.REAL, value: a.value - b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value - b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.DIV:
        if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value / b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.MUL:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.REAL, value: a.value * b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value * b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.MOD:
        if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.INTEGER, value: a.value % b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.AND:
        if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN)
          return { kind: ValueKind.BOOLEAN, value: a.value && b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.OR:
        if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN)
          return { kind: ValueKind.BOOLEAN, value: a.value || b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.BIT_AND:
        throw new Error('not implemented yet')
      case BinaryOp.BIT_OR:
        throw new Error('not implemented yet')
      case BinaryOp.BIT_XOR:
        throw new Error('not implemented yet')
      case BinaryOp.EQUAL:
        if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else if (a.kind === ValueKind.STRING && b.kind === ValueKind.STRING)
          return { kind: ValueKind.BOOLEAN, value: a.value === b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.NOT_EQUAL:
        if (a.kind === ValueKind.BOOLEAN && b.kind === ValueKind.BOOLEAN)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else if (a.kind === ValueKind.STRING && b.kind === ValueKind.STRING)
          return { kind: ValueKind.BOOLEAN, value: a.value !== b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.GREATER_THAN:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value > b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value > b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.GREATER_THAN_EQUAL:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value >= b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value >= b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.LESS_THAN:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value < b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value < b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
      case BinaryOp.LESS_THAN_EQUAL:
        if (a.kind === ValueKind.REAL && b.kind === ValueKind.REAL)
          return { kind: ValueKind.BOOLEAN, value: a.value <= b.value }
        else if (a.kind === ValueKind.INTEGER && b.kind === ValueKind.INTEGER)
          return { kind: ValueKind.BOOLEAN, value: a.value <= b.value }
        else throw new Error(`invalid state. cannot perform binop ${expr.op}`)
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
        const value = []
        const size = t.dimension.reduce((a, b) => a * b)
        for (let i = 0; i < size; i++) {
          value.push(this.zeroValue(t.type))
        }
        return { kind: ValueKind.ARRAY, value }
      }
      case TypeKind.VOID:
        return { kind: ValueKind.VOID, value: undefined }
      default:
        throw new Error('not implemented yet')
    }
  }

  setReturnVal(value: Value): void {
    this.returnVal = value
  }

  addScope(): void {
    this.symbols.push({})
  }

  setSymbol(name: string, value: Value): void {
    console.log('set', name, value)
    this.symbols[this.symbols.length - 1][name] = value
  }

  getSymbol(name: string): Value {
    for (let i = this.symbols.length - 1; i >= 0; i--) {
      if (name in this.symbols[i]) {
        console.log('get', name, this.symbols[i][name])
        return this.symbols[i][name]
      }
    }
    throw new Error('invalid state. searching undefined symbol')
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