import { ArrayIndexExprNode, CallExprNode } from './ast'
import { BinaryOp, Expr, Type, TypeKind } from './semantic'
import { Position, Token, TokenKind } from './tokens'

export class CompileError extends Error {
  errors: CompileErrorItem[]
  constructor(errors: CompileErrorItem[]) {
    super(errors.map((err) => err.message).join('\n'))
    this.errors = errors
  }
}

export class CompileErrorItem extends Error {
  position?: Position
  msg: string

  constructor(message: string, position?: Position) {
    if (position != null) {
      super(`Error at ${position.line}:${position.col}: ${message}`)
    } else {
      super(message)
    }

    this.msg = message
    this.position = position
  }
}

export class UnexpectedCharacter extends CompileErrorItem {
  constructor(char: string, position: Position) {
    super(`Unexpected character '${char}'`, position)
  }
}

export class UnexpectedToken extends CompileErrorItem {
  constructor(expected: TokenKind[] | string, found: Token) {
    const expectedMessage =
      typeof expected === 'string'
        ? expected
        : expected.map((k) => k.toString()).join(', ')
    const message = `Expected ${expectedMessage}. But found a ${found.kind.toString()}`
    super(message, found.position)
  }
}

export class MultipleDeclaration extends CompileErrorItem {
  constructor(declaredAt: Position, redeclaredAt: Token) {
    super(
      `${redeclaredAt.value} is already declared at ${declaredAt.toString()}`,
      redeclaredAt.position
    )
  }
}

export class BuiltinRedeclared extends CompileErrorItem {
  constructor(declaredAt: Token) {
    super(
      `${
        declaredAt.value
      } is a builtin function, cannot be redeclared at ${declaredAt.position.toString()}`
    )
  }
}

export class TypeMismatch extends CompileErrorItem {
  constructor(source: Expr, expectedType: Type | TypeKind) {
    // TODO: use proper error message
    super(`Type mismatch`, source.position)
  }
}

export class MissingReturnValue extends CompileErrorItem {
  constructor(returnToken: Token) {
    // TODO: use proper error message
    super('Missing return value', returnToken.position)
  }
}

export class NotAConstant extends CompileErrorItem {
  constructor(source: Expr) {
    super('Not a compile-time constant expression', source.position)
  }
}

export class CannotAssign extends CompileErrorItem {
  constructor(source: Expr, target: Type) {
    // TODO: use proper error message
    super('Cannot assign source to target_type', source.position)
  }
}

export class NotAssignable extends CompileErrorItem {
  constructor(receiver: Expr) {
    // TODO: use proper error message
    super('receievr is not assignable', receiver.position)
  }
}

export class UndefinedSymbol extends CompileErrorItem {
  constructor(token: Token) {
    super(`Undefined symbol ${token.value}`, token.position)
  }
}

export class InvalidBinaryOperator extends CompileErrorItem {
  constructor(a: Expr, op: BinaryOp, b: Expr) {
    // TODO: use proper error message
    super(`Cannot perform ${op.toString()} operation with a and b`, a.position)
  }
}

export class InvalidUnaryOperator extends CompileErrorItem {
  constructor(value: Expr, op: Token) {
    // TODO: use proper error message
    super(
      `Cannot perform ${op.kind.toString()} operation with value_type`,
      value.position
    )
  }
}

export class NotInALoop extends CompileErrorItem {
  constructor(control: Token) {
    super('Not in a loop', control.position)
  }
}

export class MissingMain extends CompileErrorItem {
  constructor() {
    super('Missing main in the program')
  }
}

export class DuplicatedMain extends CompileErrorItem {
  constructor(declared: Token, redeclared: Token) {
    super(
      `Main program is already declared at ${declared.position.toString()}`,
      redeclared.position
    )
  }
}

export class WrongNumberOfArgument extends CompileErrorItem {
  constructor(expr: CallExprNode, expected: number) {
    super(
      `Wrong number of arguments. Expected ${expected}, got ${expr.arguments.values.length}`,
      expr.openBrac.position
    )
  }
}

export class WrongNumberOfIndex extends CompileErrorItem {
  constructor(expr: ArrayIndexExprNode, expected: number) {
    super(
      `Wrong number of index. Expected ${expected}, got ${expr.index.values.length}`,
      expr.openSquare.position
    )
  }
}
