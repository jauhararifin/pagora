export interface Token {
  value: string
  position: Position
  kind: TokenKind
}

export interface Position {
  line: number
  col: number
}

export enum TokenKind {
  EOF = 'EOF',
  PhantomSemicolon = 'PhantomSemicolon',
  Invalid = 'Invalid',
  Comment = 'Comment',

  Var = 'Var',
  Type = 'Type',
  Struct = 'Struct',
  As = 'As',
  Identifier = 'Identifier',
  Function = 'Function',
  Comma = 'Comma',
  Colon = 'Colon',
  Arrow = 'Arrow',
  Semicolon = 'Semicolon',

  Begin = 'Begin',
  End = 'End',

  Array = 'Array',
  Of = 'Of',

  OpenSquare = 'OpenSquare',
  CloseSquare = 'CloseSquare',
  OpenBrac = 'OpenBrac',
  CloseBrac = 'CloseBrac',

  IntegerLiteral = 'IntegerLiteral',
  RealLiteral = 'RealLiteral',
  StringLiteral = 'StringLiteral',
  True = 'True',
  False = 'False',

  Assign = 'Assign',

  Plus = 'Plus',
  Minus = 'Minus',
  Multiply = 'Multiply',
  Div = 'Div',
  Mod = 'Mod',
  BitOr = 'BitOr',
  BitAnd = 'BitAnd',
  BitXor = 'BitXor',
  BitNot = 'BitNot',
  ShiftLeft = 'ShiftLeft',
  ShiftRight = 'ShiftRight',

  And = 'And',
  Not = 'Not',
  Or = 'Or',

  Equal = 'Equal',
  NotEqual = 'NotEqual',
  GreaterThan = 'GreaterThan',
  GreaterThanEqual = 'GreaterThanEqual',
  LessThan = 'LessThan',
  LessThanEqual = 'LessThanEqual',

  If = 'If',
  Then = 'Then',
  Else = 'Else',
  While = 'While',
  For = 'For',
  Do = 'Do',

  Continue = 'Continue',
  Break = 'Break',
  Return = 'Return',

  Integer = 'Integer',
  Char = 'Char',
  Real = 'Real',
  Boolean = 'Boolean',
}

export const PrimitiveTypes = [
  TokenKind.Integer,
  TokenKind.Char,
  TokenKind.Real
]
