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
  Invalid = 'Invalid',
  Comment = 'Comment',

  Var = 'Var',
  Type = 'Type',
  Struct = 'Struct',
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

  Assign = 'Assign',

  Plus = 'Plus',
  Minus = 'Minus',
  Multiply = 'Multiply',
  Div = 'Div',
  Mod = 'Mod',

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

  Integer = 'Integer',
  Char = 'Char',
  Real = 'Real',
}
