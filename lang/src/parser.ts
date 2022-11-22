import {
  ArrayTypeNode,
  BlockStatementNode,
  CommaSeparatedExpr,
  ExprNode,
  ExprNodeKind,
  FunctionNode,
  IfStatementNode,
  KeywordStatementNode,
  MainNode,
  ParamNode,
  ParamsNode,
  ReturnStatementNode,
  RootNode,
  StatementNode,
  StatementNodeKind,
  TypeExprNode,
  TypeExprNodeKind,
  VarNode,
  VarStatementNode,
  WhileStatementNode,
} from './ast'
import {
  CompileError,
  DuplicatedMain,
  MultiCompileError,
  UnexpectedToken,
} from './errors'
import { Position, Token, TokenKind } from './tokens'

// TODO: implement non stack-safe recursive with depth limit. Do not use exception as it will be tedious to work with
// non recursive technique.

export function parse(tokens: Token[]): RootNode {
  const tokenIter = new TokenIterator(tokens)
  const [root, error] = parseRoot(tokenIter)
  if (error !== undefined) {
    throw error
  }
  return root
}

type Result<T> = [T, undefined] | [undefined, CompileError]

function ok<T>(val: T): Result<T> {
  return [val, undefined]
}

function err(err: CompileError): Result<never> {
  return [undefined, err]
}

export class TokenIterator {
  tokens: Token[]
  index: number

  constructor(tokens: Token[]) {
    this.tokens = tokens.filter((tok) => tok.kind !== TokenKind.COMMENT)
    this.index = 0
  }

  token(): Token {
    if (this.index < this.tokens.length) {
      return this.tokens[this.index]
    }
    if (this.tokens.length === 0) {
      return new Token(TokenKind.EOF, '', new Position(0, 0))
    }
    return new Token(
      TokenKind.EOF,
      '',
      this.tokens[this.tokens.length - 1].position
    )
  }

  kind(): TokenKind {
    return this.token().kind
  }

  take(): Token {
    const token = this.token()
    this.next()
    return token
  }

  next(): void {
    if (this.index < this.tokens.length) {
      this.index++
    }
  }
}

function parseRoot(tokens: TokenIterator): Result<RootNode> {
  const variables: VarNode[] = []
  const functions: FunctionNode[] = []
  const errors: CompileError[] = []
  let main: MainNode | undefined

  while (tokens.kind() !== TokenKind.EOF) {
    if (tokens.kind() === TokenKind.SEMICOLON) {
      tokens.next()
    } else if (tokens.kind() === TokenKind.VAR) {
      const [varNode, error] = parseVariable(tokens)
      if (error !== undefined) {
        errors.push(error)
        skipUntil(tokens, [TokenKind.SEMICOLON])
        continue
      }
      variables.push(varNode)
    } else if (tokens.kind() === TokenKind.FUNCTION) {
      const [functionNode, error] = parseFunction(tokens)
      if (error !== undefined) {
        errors.push(error)
        skipUntil(tokens, [TokenKind.SEMICOLON])
        continue
      }
      functions.push(functionNode)
    } else if (tokens.kind() === TokenKind.BEGIN) {
      if (main !== undefined) {
        errors.push(new DuplicatedMain(main.body.begin, tokens.token()))
        skipUntil(tokens, [TokenKind.SEMICOLON])
      }

      const [body, error] = parseBlockStatement(tokens)
      if (error !== undefined) {
        errors.push(error)
      } else {
        main = { body }
      }
    } else {
      errors.push(new UnexpectedToken('declaration', tokens.token()))
      skipUntil(tokens, [TokenKind.SEMICOLON])
    }
  }

  if (errors.length > 0) {
    return err(new MultiCompileError(errors))
  }

  return ok({ variables, functions, main })
}

function parseVariable(tokens: TokenIterator): Result<VarNode> {
  if (tokens.kind() !== TokenKind.VAR) {
    return err(new UnexpectedToken(TokenKind.VAR, tokens.token()))
  }
  const varToken = tokens.take()

  if (tokens.kind() !== TokenKind.IDENTIFIER) {
    return err(new UnexpectedToken(TokenKind.IDENTIFIER, tokens.token()))
  }
  const varName = tokens.take()

  if (tokens.kind() === TokenKind.COLON) {
    const colon = tokens.take()

    const [typeExpr, error] = parseTypeExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }

    let assignToken: Token | undefined
    let value: ExprNode | undefined

    if (tokens.kind() === TokenKind.ASSIGN) {
      assignToken = tokens.take()

      const [v, error] = parseExpr(tokens)
      if (error !== undefined) {
        return err(error)
      }
      value = v
    }

    if (tokens.kind() !== TokenKind.SEMICOLON) {
      return err(new UnexpectedToken([TokenKind.SEMICOLON], tokens.token()))
    }
    return ok({
      var: varToken,
      name: varName,
      colon,
      type: typeExpr,
      assign: assignToken,
      value,
    })
  } else if (tokens.kind() === TokenKind.ASSIGN) {
    const assignToken = tokens.take()

    const [value, error] = parseExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }

    if (tokens.kind() !== TokenKind.SEMICOLON) {
      return err(new UnexpectedToken([TokenKind.SEMICOLON], tokens.token()))
    }

    return ok({
      var: varToken,
      name: varName,
      assign: assignToken,
      value,
    })
  } else {
    return err(
      new UnexpectedToken([TokenKind.COLON, TokenKind.ASSIGN], tokens.token())
    )
  }
}

function parseFunction(tokens: TokenIterator): Result<FunctionNode> {
  if (tokens.kind() !== TokenKind.FUNCTION) {
    return err(new UnexpectedToken([TokenKind.FUNCTION], tokens.token()))
  }
  const functionTok = tokens.take()

  if (tokens.kind() !== TokenKind.IDENTIFIER) {
    return err(new UnexpectedToken([TokenKind.IDENTIFIER], tokens.token()))
  }
  const nameTok = tokens.take()

  if (tokens.kind() !== TokenKind.OPEN_BRAC) {
    return err(new UnexpectedToken([TokenKind.OPEN_BRAC], tokens.token()))
  }
  const openBracTok = tokens.take()

  const [params, error] = parseParams(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.CLOSE_BRAC) {
    return err(new UnexpectedToken([TokenKind.CLOSE_BRAC], tokens.token()))
  }
  const closeBracTok = tokens.take()

  let arrowTok: Token | undefined
  let returnType: TypeExprNode | undefined
  if (tokens.kind() === TokenKind.ARROW) {
    arrowTok = tokens.take()

    const [retType, error] = parseTypeExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }
    returnType = retType
  }

  if (tokens.kind() === TokenKind.SEMICOLON) {
    return ok({
      function: functionTok,
      name: nameTok,
      openBrac: openBracTok,
      params,
      closeBrac: closeBracTok,
      arrow: arrowTok,
      returnType,
    })
  } else {
    const [body, error] = parseBlockStatement(tokens)
    if (error !== undefined) {
      return err(error)
    }

    return ok({
      function: functionTok,
      name: nameTok,
      openBrac: openBracTok,
      params,
      closeBrac: closeBracTok,
      arrow: arrowTok,
      returnType,
      body,
    })
  }
}

function parseParams(tokens: TokenIterator): Result<ParamsNode> {
  const params: ParamNode[] = []
  const commas: Token[] = []

  if (tokens.kind() === TokenKind.CLOSE_BRAC) {
    return ok({ params, commas })
  }

  while (true) {
    if (tokens.kind() !== TokenKind.IDENTIFIER) {
      return err(new UnexpectedToken([TokenKind.IDENTIFIER], tokens.token()))
    }
    const name = tokens.take()

    if (tokens.kind() !== TokenKind.COLON) {
      return err(new UnexpectedToken([TokenKind.COLON], tokens.token()))
    }
    const colon = tokens.take()

    const [type, error] = parseTypeExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }

    params.push({ name, colon, type })

    if (tokens.kind() !== TokenKind.COMMA) {
      break
    }
    tokens.next()
  }

  return ok({ params, commas })
}

export function parseStatement(tokens: TokenIterator): Result<StatementNode> {
  while (tokens.kind() === TokenKind.SEMICOLON) {
    tokens.next()
  }

  let result: Result<StatementNode>

  const kind = tokens.kind()
  if (kind === TokenKind.BEGIN) {
    result = parseBlockStatement(tokens)
  } else if (kind === TokenKind.IF) {
    result = parseIfStatement(tokens)
  } else if (kind === TokenKind.WHILE) {
    result = parseWhileStatement(tokens)
  } else if (kind === TokenKind.VAR) {
    result = parseVarStatement(tokens)
  } else if (kind === TokenKind.RETURN) {
    result = parseReturnStatement(tokens)
  } else if (kind === TokenKind.CONTINUE || kind === TokenKind.BREAK) {
    result = parseKeywordStatement(tokens)
  } else {
    result = parseAssignStatement(tokens)
  }

  const [stmt, error] = result
  if (error !== undefined) {
    skipUntil(tokens, [TokenKind.SEMICOLON])
    return err(error)
  }

  if (tokens.kind() === TokenKind.SEMICOLON) {
    tokens.next()
  }

  return ok(stmt)
}

function parseBlockStatement(
  tokens: TokenIterator
): Result<BlockStatementNode> {
  if (tokens.kind() !== TokenKind.BEGIN) {
    return err(new UnexpectedToken([TokenKind.BEGIN], tokens.token()))
  }
  const begin = tokens.take()

  const statements: StatementNode[] = []
  const errors: CompileError[] = []
  while (tokens.kind() !== TokenKind.EOF && tokens.kind() !== TokenKind.END) {
    const [stmt, error] = parseStatement(tokens)
    if (error !== undefined) {
      errors.push(error)
    } else {
      statements.push(stmt)
    }
  }

  if (tokens.kind() !== TokenKind.END) {
    return err(new UnexpectedToken([TokenKind.END], tokens.token()))
  }
  const end = tokens.take()

  return ok({ kind: StatementNodeKind.BLOCK, begin, statements, end })
}

function parseIfStatement(tokens: TokenIterator): Result<IfStatementNode> {
  if (tokens.kind() !== TokenKind.IF) {
    return err(new UnexpectedToken([TokenKind.IF], tokens.token()))
  }
  const ifToken = tokens.take()

  const [condition, error] = parseExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.THEN) {
    return err(new UnexpectedToken([TokenKind.THEN], tokens.token()))
  }
  const thenToken = tokens.take()

  const stmt = parseStatement(tokens)
  if (stmt[1] !== undefined) {
    return err(stmt[1])
  }
  const body = stmt[0]

  if (tokens.kind() !== TokenKind.ELSE) {
    return ok({
      kind: StatementNodeKind.IF,
      if: ifToken,
      condition,
      then: thenToken,
      body,
    })
  }
  const elseToken = tokens.take()

  const elseStmt = parseStatement(tokens)
  if (elseStmt[1] !== undefined) {
    return err(elseStmt[1])
  }
  const elseBody = elseStmt[0]

  return ok({
    kind: StatementNodeKind.IF,
    if: ifToken,
    condition,
    then: thenToken,
    body,
    else: elseToken,
    elseBody,
  })
}

function parseWhileStatement(
  tokens: TokenIterator
): Result<WhileStatementNode> {
  if (tokens.kind() !== TokenKind.WHILE) {
    return err(new UnexpectedToken([TokenKind.WHILE], tokens.token()))
  }
  const whileToken = tokens.take()

  const [condition, error] = parseExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.DO) {
    return err(new UnexpectedToken([TokenKind.DO], tokens.token()))
  }
  const doToken = tokens.take()

  const stmt = parseStatement(tokens)
  if (stmt[1] !== undefined) {
    return err(stmt[1])
  }
  const body = stmt[0]

  return ok({
    kind: StatementNodeKind.WHILE,
    while: whileToken,
    condition,
    do: doToken,
    body,
  })
}

function parseVarStatement(tokens: TokenIterator): Result<VarStatementNode> {
  const [variable, error] = parseVariable(tokens)
  if (error !== undefined) {
    return err(error)
  }
  return ok({ kind: StatementNodeKind.VAR, variable })
}

function parseReturnStatement(
  tokens: TokenIterator
): Result<ReturnStatementNode> {
  if (tokens.kind() !== TokenKind.RETURN) {
    return err(new UnexpectedToken([TokenKind.RETURN], tokens.token()))
  }
  const ret = tokens.take()

  if (tokens.kind() === TokenKind.SEMICOLON) {
    return ok({
      kind: StatementNodeKind.RETURN,
      return: ret,
    })
  }

  const [value, error] = parseExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  return ok({
    kind: StatementNodeKind.RETURN,
    return: ret,
    value,
  })
}

function parseAssignStatement(tokens: TokenIterator): Result<StatementNode> {
  const [receiver, error] = parseExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.ASSIGN) {
    return ok({ kind: StatementNodeKind.EXPR, expr: receiver })
  }
  const assign = tokens.take()

  const valueResult = parseExpr(tokens)
  if (valueResult[1] !== undefined) {
    return err(valueResult[1])
  }
  const value = valueResult[0]

  if (tokens.kind() !== TokenKind.SEMICOLON) {
    return err(new UnexpectedToken([TokenKind.SEMICOLON], tokens.token()))
  }
  tokens.next()

  return ok({ kind: StatementNodeKind.ASSIGN, receiver, assign, value })
}

const keywordTokens = [TokenKind.CONTINUE, TokenKind.BREAK]

function parseKeywordStatement(
  tokens: TokenIterator
): Result<KeywordStatementNode> {
  if (!keywordTokens.includes(tokens.kind())) {
    return err(new UnexpectedToken(keywordTokens, tokens.token()))
  }
  const keyword = tokens.take()

  return ok({
    kind: StatementNodeKind.KEYWORD,
    keyword,
  })
}

const primitiveTypes = [
  TokenKind.INTEGER,
  TokenKind.BYTE,
  TokenKind.REAL,
  TokenKind.BOOLEAN,
  TokenKind.STRING,
]

function parseTypeExpr(tokens: TokenIterator): Result<TypeExprNode> {
  if (tokens.kind() === TokenKind.ARRAY) {
    return parseArrayTypeExpr(tokens)
  }

  if (!primitiveTypes.includes(tokens.kind())) {
    return err(new UnexpectedToken(primitiveTypes, tokens.token()))
  }
  const typeToken = tokens.take()
  return ok({ kind: TypeExprNodeKind.IDENT, type: typeToken })
}

function parseArrayTypeExpr(tokens: TokenIterator): Result<ArrayTypeNode> {
  if (tokens.kind() !== TokenKind.ARRAY) {
    return err(new UnexpectedToken([TokenKind.ARRAY], tokens.token()))
  }
  const arrayToken = tokens.take()

  if (tokens.kind() !== TokenKind.OPEN_SQUARE) {
    return err(new UnexpectedToken([TokenKind.OPEN_SQUARE], tokens.token()))
  }
  const openSquare = tokens.take()

  const [commaSeparatedSize, error] = parseCommaSeparatedExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.CLOSE_SQUARE) {
    return err(new UnexpectedToken([TokenKind.CLOSE_SQUARE], tokens.token()))
  }
  const closeSquare = tokens.take()

  if (tokens.kind() !== TokenKind.OF) {
    return err(new UnexpectedToken([TokenKind.OF], tokens.token()))
  }
  const ofToken = tokens.take()

  const elementTypeResult = parseTypeExpr(tokens)
  if (elementTypeResult[1] !== undefined) {
    return err(elementTypeResult[1])
  }
  const elementType = elementTypeResult[0]

  return ok({
    kind: TypeExprNodeKind.ARRAY,
    array: arrayToken,
    openSquare,
    dimension: commaSeparatedSize,
    closeSquare,
    of: ofToken,
    elementType,
  })
}

function parseExpr(tokens: TokenIterator): Result<ExprNode> {
  return parseBinaryExpr(tokens, operatorPrecedences[0])
}

const operatorPrecedences = [
  TokenKind.OR,
  TokenKind.AND,
  TokenKind.BIT_OR,
  TokenKind.BIT_XOR,
  TokenKind.BIT_AND,
  TokenKind.EQUAL,
  TokenKind.NOT_EQUAL,
  TokenKind.LESS_THAN,
  TokenKind.LESS_THAN_EQUAL,
  TokenKind.GREATER_THAN,
  TokenKind.GREATER_THAN_EQUAL,
  TokenKind.SHIFT_LEFT,
  TokenKind.SHIFT_RIGHT,
  TokenKind.PLUS,
  TokenKind.MINUS,
  TokenKind.MULTIPLY,
  TokenKind.DIV,
  TokenKind.MOD,
]

function parseBinaryExpr(
  tokens: TokenIterator,
  op: TokenKind
): Result<ExprNode> {
  const i = operatorPrecedences.indexOf(op)
  const [aExpr, error] =
    i === operatorPrecedences.length - 1
      ? parseArrayIndexExpr(tokens)
      : parseBinaryExpr(tokens, operatorPrecedences[i + 1])
  if (error !== undefined) {
    return err(error)
  }

  let result: ExprNode = aExpr
  while (true) {
    if (tokens.kind() !== op) {
      return ok(result)
    }
    const opToken = tokens.take()

    const [bExpr, error] =
      i === operatorPrecedences.length - 1
        ? parseArrayIndexExpr(tokens)
        : parseBinaryExpr(tokens, operatorPrecedences[i + 1])
    if (error !== undefined) {
      return err(error)
    }

    result = {
      kind: ExprNodeKind.BINARY,
      a: result,
      op: opToken,
      b: bExpr,
    }
  }
}

function parseArrayIndexExpr(tokens: TokenIterator): Result<ExprNode> {
  const [arraySource, error] = parseCastExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.OPEN_SQUARE) {
    return ok(arraySource)
  }
  const openSquare = tokens.take()

  const indexResult = parseCommaSeparatedExpr(tokens)
  if (indexResult[1] !== undefined) {
    return err(indexResult[1])
  }
  const index = indexResult[0]

  if (tokens.kind() !== TokenKind.CLOSE_SQUARE) {
    return err(new UnexpectedToken([TokenKind.CLOSE_SQUARE], tokens.token()))
  }
  const closeSquare = tokens.take()

  return ok({
    kind: ExprNodeKind.ARRAY_INDEX,
    array: arraySource,
    openSquare,
    index,
    closeSquare,
  })
}

function parseCastExpr(tokens: TokenIterator): Result<ExprNode> {
  const [source, error] = parseUnaryExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.AS) {
    return ok(source)
  }
  const asToken = tokens.take()

  const typeResult = parseTypeExpr(tokens)
  if (typeResult[1] !== undefined) {
    return err(typeResult[1])
  }
  const target = typeResult[0]

  return ok({ kind: ExprNodeKind.CAST, source, as: asToken, target })
}

const unaryOp = [
  TokenKind.BIT_NOT,
  TokenKind.MINUS,
  TokenKind.PLUS,
  TokenKind.NOT,
]

function parseUnaryExpr(tokens: TokenIterator): Result<ExprNode> {
  if (!unaryOp.includes(tokens.kind())) {
    return parseCallExpr(tokens)
  }
  const op = tokens.take()

  const [value, error] = parseCallExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  return ok({ kind: ExprNodeKind.UNARY, op, value })
}

function parseCallExpr(tokens: TokenIterator): Result<ExprNode> {
  const [callee, error] = parsePrimaryExpr(tokens)
  if (error !== undefined) {
    return err(error)
  }

  if (tokens.kind() !== TokenKind.OPEN_BRAC) {
    return ok(callee)
  }
  const openBrac = tokens.take()

  if (tokens.kind() === TokenKind.CLOSE_BRAC) {
    const closeBrac = tokens.take()

    return ok({
      kind: ExprNodeKind.CALL,
      callee,
      openBrac,
      arguments: { values: [], commas: [] },
      closeBrac,
    })
  }

  const argsResult = parseCommaSeparatedExpr(tokens)
  if (argsResult[1] !== undefined) {
    return err(argsResult[1])
  }
  const args = argsResult[0]

  if (tokens.kind() !== TokenKind.CLOSE_BRAC) {
    return err(new UnexpectedToken([TokenKind.CLOSE_BRAC], tokens.token()))
  }
  const closeBrac = tokens.take()

  return ok({
    kind: ExprNodeKind.CALL,
    callee,
    openBrac,
    arguments: args,
    closeBrac,
  })
}

function parsePrimaryExpr(tokens: TokenIterator): Result<ExprNode> {
  if (tokens.kind() === TokenKind.OPEN_BRAC) {
    const openBrac = tokens.take()

    const [value, error] = parseExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }

    if (tokens.kind() !== TokenKind.CLOSE_BRAC) {
      return err(new UnexpectedToken([TokenKind.CLOSE_BRAC], tokens.token()))
    }
    const closeBrac = tokens.take()

    return ok({ kind: ExprNodeKind.GROUPED, openBrac, value, closeBrac })
  } else if (tokens.kind() === TokenKind.OPEN_SQUARE) {
    const openSquare = tokens.take()

    const [value, error] = parseCommaSeparatedExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }

    if (tokens.kind() !== TokenKind.CLOSE_SQUARE) {
      return err(new UnexpectedToken([TokenKind.CLOSE_BRAC], tokens.token()))
    }
    const closeSquare = tokens.take()

    return ok({ kind: ExprNodeKind.ARRAY_LIT, openSquare, value, closeSquare })
  } else if (tokens.kind() === TokenKind.INTEGER_LITERAL) {
    const value = tokens.take()
    return ok({ kind: ExprNodeKind.INTEGER_LIT, value })
  } else if (
    tokens.kind() === TokenKind.TRUE ||
    tokens.kind() === TokenKind.FALSE
  ) {
    const value = tokens.take()
    return ok({ kind: ExprNodeKind.BOOLEAN_LIT, value })
  } else if (tokens.kind() === TokenKind.IDENTIFIER) {
    const name = tokens.take()
    return ok({ kind: ExprNodeKind.IDENT, name })
  } else if (tokens.kind() === TokenKind.STRING_LITERAL) {
    const value = tokens.take()
    return ok({ kind: ExprNodeKind.STRING_LIT, value })
  } else {
    throw new UnexpectedToken('Expression', tokens.token())
  }
}

function parseCommaSeparatedExpr(
  tokens: TokenIterator
): Result<CommaSeparatedExpr> {
  const exprs: ExprNode[] = []
  const commas: Token[] = []
  while (true) {
    const [expr, error] = parseExpr(tokens)
    if (error !== undefined) {
      return err(error)
    }
    exprs.push(expr)

    if (tokens.kind() !== TokenKind.COMMA) {
      break
    }
    const commaTok = tokens.take()
    commas.push(commaTok)
  }

  return ok({ values: exprs, commas })
}

function skipUntil(tokens: TokenIterator, kinds: TokenKind[]): void {
  while (tokens.kind() !== TokenKind.EOF && !kinds.includes(tokens.kind())) {
    tokens.next()
  }
  tokens.next()
}
