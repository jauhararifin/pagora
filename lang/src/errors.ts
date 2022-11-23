import { ArrayIndexExprNode, CallExprNode } from './ast'
import { BinaryOp, Expr, Type, TypeKind } from './semantic'
import { Position, Token, TokenKind } from './tokens'

// TODO: I think CompileErrorItem is not necessary. We can just use `CompileError` only and have a special type to hold
// multiple errors, like `MultiCompileError`.

export class CompileError extends Error {}

export class MultiCompileError extends CompileError {
  errors: CompileError[]
  constructor(errors: CompileError[]) {
    errors = errors.flatMap((e) => {
      if (!(e instanceof MultiCompileError)) {
        return [e]
      }

      let errors: MultiCompileError[] = [e]
      const result: CompileError[] = [e]
      while (errors.length > 0) {
        const temp = errors.flatMap((e) => e.errors)

        errors = []
        for (const e of temp) {
          if (e instanceof MultiCompileError) {
            errors.push(e)
          } else {
            result.push(e)
          }
        }
      }

      return result
    })

    super(errors.map((v) => v.message).join('\n'))
    this.errors = errors
  }
}

export class ErrorWithPosition extends CompileError {
  constructor(message: string, position: Position) {
    super(`Error at ${position.toString()}: ${message}`)
  }
}

export class UnexpectedCharacter extends ErrorWithPosition {
  constructor(char: string, position: Position) {
    super(`Unexpected character ${JSON.stringify(char)}`, position)
  }
}

export class MissingClosingQuote extends ErrorWithPosition {
  constructor(position: Position) {
    super(`Missing closing quote in string literal`, position)
  }
}

export class TooManyErrors extends CompileError {
  constructor() {
    super(`Too many errors`)
  }
}

export class UnexpectedToken extends ErrorWithPosition {
  constructor(expected: TokenKind[] | string, found: Token) {
    const expectedMessage =
      typeof expected === 'string'
        ? expected
        : expected.map((k) => k.toString()).join(', ')
    const message = `Expected ${expectedMessage}. But found a ${found.kind.toString()}`
    super(message, found.position)
  }
}

export class MultipleDeclaration extends ErrorWithPosition {
  constructor(declaredAt: Position, redeclaredAt: Token) {
    super(
      `${redeclaredAt.value} is already declared at ${declaredAt.toString()}`,
      redeclaredAt.position
    )
  }
}

export class BuiltinRedeclared extends CompileError {
  constructor(declaredAt: Token) {
    super(
      `${
        declaredAt.value
      } is a builtin function, cannot be redeclared at ${declaredAt.position.toString()}`
    )
  }
}

export class TypeMismatch extends ErrorWithPosition {
  constructor(source: Expr, expectedType: Type | TypeKind) {
    // TODO: use proper error message
    super(`Type mismatch`, source.position)
  }
}

export class MissingReturnValue extends ErrorWithPosition {
  constructor(returnToken: Token) {
    // TODO: use proper error message
    super('Missing return value', returnToken.position)
  }
}

export class NotAConstant extends ErrorWithPosition {
  constructor(source: Expr) {
    super('Not a compile-time constant expression', source.position)
  }
}

export class CannotAssign extends ErrorWithPosition {
  constructor(source: Expr, target: Type) {
    // TODO: use proper error message
    super('Cannot assign source to target_type', source.position)
  }
}

export class NotAssignable extends ErrorWithPosition {
  constructor(receiver: Expr) {
    // TODO: use proper error message
    super('receievr is not assignable', receiver.position)
  }
}

export class UndefinedSymbol extends ErrorWithPosition {
  constructor(token: Token) {
    super(`Undefined symbol ${token.value}`, token.position)
  }
}

export class InvalidBinaryOperator extends ErrorWithPosition {
  constructor(a: Expr, op: BinaryOp, b: Expr) {
    // TODO: use proper error message
    super(`Cannot perform ${op.toString()} operation with a and b`, a.position)
  }
}

export class InvalidUnaryOperator extends ErrorWithPosition {
  constructor(value: Expr, op: Token) {
    // TODO: use proper error message
    super(
      `Cannot perform ${op.kind.toString()} operation with value_type`,
      value.position
    )
  }
}

export class NotInALoop extends ErrorWithPosition {
  constructor(control: Token) {
    super('Not in a loop', control.position)
  }
}

export class MissingMain extends CompileError {
  constructor() {
    super('Missing main in the program')
  }
}

export class DuplicatedMain extends ErrorWithPosition {
  constructor(declared: Token, redeclared: Token) {
    super(
      `Main program is already declared at ${declared.position.toString()}`,
      redeclared.position
    )
  }
}

export class WrongNumberOfArgument extends ErrorWithPosition {
  constructor(expr: CallExprNode, expected: number) {
    super(
      `Wrong number of arguments. Expected ${expected}, got ${expr.arguments.values.length}`,
      expr.openBrac.position
    )
  }
}

export class WrongNumberOfIndex extends ErrorWithPosition {
  constructor(expr: ArrayIndexExprNode, expected: number) {
    super(
      `Wrong number of index. Expected ${expected}, got ${expr.index.values.length}`,
      expr.openSquare.position
    )
  }
}
