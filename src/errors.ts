import { CallExpr, Expr, IndexExpr, Type, TypeKind } from './semantic'
import { Position, Token, TokenKind } from './tokens'

export class CompileError extends Error {
  errors: CompileErrorItem[]
  constructor (errors: CompileErrorItem[]) {
    super(errors.map(err => err.message).join('\n'))
    this.errors = errors
  }
}

export class CompileErrorItem extends Error {
  position: Position
  message: string

  constructor (message: string, position: Position) {
    super(`Error ar ${position.line}:${position.col}: ${message}`)
    this.message = message
    this.position = position
  }
}

export class UnexpectedCharacter extends CompileErrorItem {
  constructor (char: string, position: Position) {
    super(`Unexpected character '${char}'`, position)
  }
}

export class UnexpectedToken extends CompileErrorItem {
  constructor (expected: TokenKind[] | string, found: Token) {
    const expectedMessage = typeof (expected) === 'string'
      ? expected
      : expected.map(k => k.toString()).join(', ')
    const message = `Expected ${expectedMessage}. But found a ${found.kind.toString()}`
    super(message, found.position)
  }
}

export class MultipleDeclaration extends CompileErrorItem {
  constructor (declaredAt: Token, redeclaredAt: Token) {
    super(`${declaredAt.value} is already declared at ${declaredAt.position.toString()}`, redeclaredAt.position)
  }
}

export class TypeMismatch extends CompileErrorItem {
  constructor (source: Expr, expectedType: Type | TypeKind) {
    // TODO: use proper error message
    super('Invalid type', source.position)
  }
}

export class NotAConstant extends CompileErrorItem {
  constructor (source: Expr) {
    super('Not a compile-time constant expression', source.position)
  }
}

export class CannotAssign extends CompileErrorItem {
  constructor (source: Expr, target: Type) {
    // TODO: use proper error message
    super('Cannot assign source to target_type', source.position)
  }
}

export class UndefinedSymbol extends CompileErrorItem {
  constructor (token: Token) {
    super(`Undefined symbol ${token.value}`, token.position)
  }
}

export class InvalidBinaryOperator extends CompileErrorItem {
  constructor (a: Expr, op: Token, b: Expr) {
    // TODO: use proper error message
    super(`Cannot perform ${op.kind.toString()} operation with a and b`, a.position)
  }
}

export class InvalidUnaryOperator extends CompileErrorItem {
  constructor (value: Expr, op: Token) {
    // TODO: use proper error message
    super(`Cannot perform ${op.kind.toString()} operation with value_type`, value.position)
  }
}

export class MissingMain extends CompileErrorItem {
  constructor (position: Position) {
    super('Missing main program', position)
  }
}

export class DuplicatedMain extends CompileErrorItem {
  constructor (declared: Token, redeclared: Token) {
    super(`Main program is already declared at ${declared.position.toString()}`, redeclared.position)
  }
}

export class WrongNumberOfArgument extends CompileErrorItem {
  constructor (expr: CallExpr, expected: number) {
    super(`Wrong number of arguments. Expected ${expected}, got ${expr.arguments.length}`, expr.position)
  }
}

export class WrongNumberOfIndex extends CompileErrorItem {
  constructor (expr: IndexExpr, expected: number) {
    super(`Wrong number of index. Expected ${expected}, got ${expr.indices.length}`, expr.position)
  }
}
