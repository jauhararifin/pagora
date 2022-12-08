use crate::{
    ast::{
        ArrayLitNode, ArrayTypeNode, AssignStmtNode, BinaryExprNode, BlockStmtNode, CallExprNode,
        CastExprNode, ElseIfStmtNode, ElseStmtNode, ExprNode, FuncHeadNode, FuncNode,
        GroupedExprNode, IfStmtNode, IndexExprNode, Item, ParameterNode, ReturnStmtNode, RootNode,
        StmtNode, TypeExprNode, UnaryExprNode, VarNode, WhileStmtNode,
    },
    errors::{CompileError, MultiErrors, UnexpectedToken},
    tokens::{Position, Token, TokenKind},
};
use std::{iter::Peekable, rc::Rc, vec::IntoIter};

pub fn parse(tokens: Vec<Token>) -> Result<RootNode, CompileError> {
    let mut token_stream = TokenStream::new(tokens);
    parse_root(&mut token_stream)
}

struct TokenStream {
    tokens: Peekable<IntoIter<Token>>,
}

impl TokenStream {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    fn kind(&mut self) -> TokenKind {
        self.tokens
            .peek()
            .map(|tok| tok.kind)
            .unwrap_or(TokenKind::Eof)
    }

    fn token(&mut self) -> Token {
        self.tokens
            .peek()
            .unwrap_or(&Token {
                kind: TokenKind::Eof,
                position: Position::default(),
                value: Rc::new(String::new()),
            })
            .clone()
    }

    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token {
            kind: TokenKind::Eof,
            position: Position::default(),
            value: Rc::new(String::new()),
        })
    }

    fn take(&mut self, kind: TokenKind) -> Result<Token, CompileError> {
        let token = self.next();
        if token.kind == kind {
            Ok(token)
        } else {
            Err(UnexpectedToken {
                expected: vec![kind],
                token,
            }
            .into())
        }
    }

    fn skip(&mut self) {
        self.tokens.next();
    }

    fn skip_until(&mut self, kind: &[TokenKind]) {
        while !kind.contains(&self.kind()) && self.kind() != TokenKind::Eof {
            self.skip();
        }
    }

    fn take_if(&mut self, kind: TokenKind) -> Option<Token> {
        self.tokens.next_if(|token| token.kind == kind)
    }
}

const ITEM_SYNC_TOKENS: &'static [TokenKind] = &[TokenKind::Var, TokenKind::Function];

fn parse_root(tokens: &mut TokenStream) -> Result<RootNode, CompileError> {
    let items = parse_multiple(
        tokens,
        ITEM_SYNC_TOKENS,
        &[TokenKind::Semicolon],
        &[TokenKind::Eof],
        parse_item,
    )?;
    Ok(RootNode { items })
}

fn parse_multiple<T, F>(
    tokens: &mut TokenStream,
    sync_tokens: &[TokenKind],
    ignores: &[TokenKind],
    stop_tokens: &[TokenKind],
    parse_fn: F,
) -> Result<Vec<T>, CompileError>
where
    F: Fn(&mut TokenStream) -> Result<T, CompileError>,
{
    let mut items = Vec::<T>::new();
    let mut errors = MultiErrors::new();

    while !stop_tokens.contains(&tokens.kind()) {
        if ignores.contains(&tokens.kind()) {
            tokens.skip();
            continue;
        }

        if sync_tokens.contains(&tokens.kind()) {
            match (parse_fn)(tokens) {
                Ok(item) => items.push(item),
                Err(err) => {
                    errors.push(err);
                    tokens.skip();
                    tokens.skip_until(&sync_tokens);
                }
            }
        }
    }

    if !errors.is_empty() {
        Err(errors.into())
    } else {
        Ok(items)
    }
}

fn parse_item(tokens: &mut TokenStream) -> Result<Item, CompileError> {
    Ok(match tokens.kind() {
        TokenKind::Var => Item::Var(parse_variable(tokens)?),
        TokenKind::Function => Item::Func(parse_func(tokens)?),
        _ => {
            return Err(UnexpectedToken {
                expected: vec![TokenKind::Var, TokenKind::Function],
                token: tokens.token(),
            }
            .into());
        }
    })
}

fn parse_variable(tokens: &mut TokenStream) -> Result<VarNode, CompileError> {
    let var = tokens.take(TokenKind::Var)?;
    let name = tokens.take(TokenKind::Ident)?;

    let colon = tokens.take_if(TokenKind::Colon);
    let typ = if colon.is_none() {
        None
    } else {
        Some(parse_type_expr(tokens)?)
    };

    let assign = tokens.take_if(TokenKind::Assign);
    let value = if assign.is_none() {
        None
    } else {
        Some(parse_expr(tokens)?)
    };

    tokens.take(TokenKind::Semicolon)?;

    Ok(VarNode {
        var,
        name,
        colon,
        typ,
        assign,
        value,
    })
}

fn parse_func(tokens: &mut TokenStream) -> Result<FuncNode, CompileError> {
    let head = parse_func_head(tokens)?;
    let body = if tokens.kind() == TokenKind::OpenBlock {
        Some(parse_block_stmt(tokens)?)
    } else {
        None
    };
    Ok(FuncNode { head, body })
}

fn parse_func_head(tokens: &mut TokenStream) -> Result<FuncHeadNode, CompileError> {
    let func = tokens.take(TokenKind::Function)?;
    let native = tokens.take_if(TokenKind::Native);
    let name = tokens.take(TokenKind::Ident)?;
    let (open_brac, parameters, close_brac) = parse_sequence(
        tokens,
        TokenKind::OpenBrac,
        TokenKind::Comma,
        TokenKind::CloseBrac,
        parse_parameter,
    )?;
    let arrow = tokens.take_if(TokenKind::Arrow);
    let return_type = if arrow.is_some() {
        Some(parse_type_expr(tokens)?)
    } else {
        None
    };

    Ok(FuncHeadNode {
        func,
        native,
        name,
        open_brac,
        parameters,
        close_brac,
        arrow,
        return_type,
    })
}

fn parse_sequence<T, F>(
    tokens: &mut TokenStream,
    begin_tok: TokenKind,
    delim_tok: TokenKind,
    end_tok: TokenKind,
    parse_fn: F,
) -> Result<(Token, Vec<T>, Token), CompileError>
where
    F: Fn(&mut TokenStream) -> Result<T, CompileError>,
{
    let opening = tokens.take(begin_tok)?;

    let mut items = Vec::<T>::new();
    let mut errors = MultiErrors::new();
    while tokens.kind() != end_tok {
        match parse_fn(tokens) {
            Ok(item) => items.push(item),
            Err(err) => errors.push(err),
        }
        tokens.take_if(delim_tok);
    }

    let closing = tokens.take(end_tok)?;

    if !errors.is_empty() {
        Err(errors.into())
    } else {
        Ok((opening, items, closing))
    }
}

fn parse_parameter(tokens: &mut TokenStream) -> Result<ParameterNode, CompileError> {
    let name = tokens.take(TokenKind::Ident)?;
    let colon = tokens.take(TokenKind::Colon)?;
    let typ = parse_type_expr(tokens)?;
    Ok(ParameterNode { name, colon, typ })
}

fn parse_type_expr(tokens: &mut TokenStream) -> Result<TypeExprNode, CompileError> {
    let ident = tokens.take(TokenKind::Ident)?;
    let typ = TypeExprNode::Ident(ident);

    if let Some(open_square) = tokens.take_if(TokenKind::OpenSquare) {
        let close_square = tokens.take(TokenKind::CloseSquare)?;
        return Ok(TypeExprNode::Array(ArrayTypeNode {
            element_type: Box::new(typ),
            open_square,
            close_square,
        }));
    }

    Ok(typ)
}

fn parse_stmt(tokens: &mut TokenStream) -> Result<StmtNode, CompileError> {
    Ok(match tokens.kind() {
        TokenKind::OpenBlock => StmtNode::Block(parse_block_stmt(tokens)?),
        TokenKind::Var => StmtNode::Var(parse_variable(tokens)?),
        TokenKind::Return => StmtNode::Return(parse_return_stmt(tokens)?),
        TokenKind::Continue | TokenKind::Break => StmtNode::Keyword(parse_keyword_stmt(tokens)?),
        TokenKind::If => StmtNode::If(parse_if_stmt(tokens)?),
        TokenKind::While => StmtNode::While(parse_while_stmt(tokens)?),
        _ => parse_assign_or_call_stmt(tokens)?,
    })
}

const STMT_SYNC_TOKEN: &'static [TokenKind] = &[
    TokenKind::OpenBlock,
    TokenKind::Var,
    TokenKind::Return,
    TokenKind::Continue,
    TokenKind::Break,
    TokenKind::If,
    TokenKind::While,
    TokenKind::Ident,
    TokenKind::Semicolon,
    TokenKind::OpenBrac,
];

fn parse_block_stmt(tokens: &mut TokenStream) -> Result<BlockStmtNode, CompileError> {
    let open_block = tokens.take(TokenKind::OpenBlock)?;
    let statements = parse_multiple(
        tokens,
        STMT_SYNC_TOKEN,
        &[TokenKind::Semicolon],
        &[TokenKind::CloseBlock, TokenKind::Eof],
        parse_stmt,
    )?;
    let close_block = tokens.take(TokenKind::CloseBlock)?;
    Ok(BlockStmtNode {
        open_block,
        statements,
        close_block,
    })
}

fn parse_return_stmt(tokens: &mut TokenStream) -> Result<ReturnStmtNode, CompileError> {
    let return_tok = tokens.take(TokenKind::Return)?;
    let value = if tokens.kind() != TokenKind::Semicolon {
        Some(parse_expr(tokens)?)
    } else {
        None
    };
    tokens.take(TokenKind::Semicolon)?;
    Ok(ReturnStmtNode { return_tok, value })
}

const KEYWORD_TOKENS: &'static [TokenKind] = &[TokenKind::Break, TokenKind::Continue];

fn parse_keyword_stmt(tokens: &mut TokenStream) -> Result<Token, CompileError> {
    if KEYWORD_TOKENS.contains(&tokens.kind()) {
        let keyword = tokens.next();
        tokens.take(TokenKind::Semicolon)?;
        Ok(keyword)
    } else {
        Err(CompileError::unexpected_token(
            Vec::from(KEYWORD_TOKENS),
            tokens.next(),
        ))
    }
}

fn parse_if_stmt(tokens: &mut TokenStream) -> Result<IfStmtNode, CompileError> {
    let if_tok = tokens.take(TokenKind::If)?;
    let condition = parse_expr(tokens)?;
    let body = parse_block_stmt(tokens)?;

    let mut else_ifs = Vec::<ElseIfStmtNode>::new();
    let mut else_stmt = None;

    while tokens.kind() == TokenKind::Else {
        let else_tok = tokens.take(TokenKind::Else)?;
        if let Some(if_tok) = tokens.take_if(TokenKind::If) {
            let condition = parse_expr(tokens)?;
            let body = parse_block_stmt(tokens)?;
            else_ifs.push(ElseIfStmtNode {
                else_tok,
                if_tok,
                condition,
                body,
            });
        } else {
            let body = parse_block_stmt(tokens)?;
            else_stmt = Some(ElseStmtNode { else_tok, body });
            break;
        }
    }

    Ok(IfStmtNode {
        if_tok,
        condition,
        body,
        else_ifs,
        else_stmt,
    })
}

fn parse_while_stmt(tokens: &mut TokenStream) -> Result<WhileStmtNode, CompileError> {
    let while_tok = tokens.take(TokenKind::While)?;
    let condition = parse_expr(tokens)?;
    let body = parse_block_stmt(tokens)?;
    Ok(WhileStmtNode {
        while_tok,
        condition,
        body,
    })
}

fn parse_assign_or_call_stmt(tokens: &mut TokenStream) -> Result<StmtNode, CompileError> {
    let expr = parse_expr(tokens)?;

    match tokens.kind() {
        TokenKind::Assign => {
            let assign = tokens.take(TokenKind::Assign)?;
            let value = parse_expr(tokens)?;
            tokens.take(TokenKind::Semicolon)?;
            Ok(StmtNode::Assign(AssignStmtNode {
                receiver: expr,
                assign,
                value,
            }))
        }
        TokenKind::Semicolon => {
            tokens.take(TokenKind::Semicolon)?;
            match expr {
                ExprNode::Call(call_expr) => Ok(StmtNode::Call(call_expr)),
                // TODO: fix this, should be: expected statement found expr.
                _ => Err(CompileError::unexpected_token(
                    vec![TokenKind::Assign, TokenKind::Semicolon],
                    tokens.next(),
                )),
            }
        }
        _ => Err(CompileError::unexpected_token(
            vec![TokenKind::Assign, TokenKind::Semicolon],
            tokens.next(),
        )),
    }
}

const BINOP_PRECEDENCE: &'static [TokenKind] = &[
    TokenKind::Or,
    TokenKind::And,
    TokenKind::BitOr,
    TokenKind::BitXor,
    TokenKind::BitAnd,
    TokenKind::Eq,
    TokenKind::NEq,
    TokenKind::Lt,
    TokenKind::LEq,
    TokenKind::Gt,
    TokenKind::GEq,
    TokenKind::ShiftLeft,
    TokenKind::ShiftRight,
    TokenKind::Add,
    TokenKind::Sub,
    TokenKind::Mul,
    TokenKind::Div,
    TokenKind::Mod,
];

fn parse_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    parse_binary_expr(tokens, TokenKind::Or)
}

fn parse_binary_expr(tokens: &mut TokenStream, op: TokenKind) -> Result<ExprNode, CompileError> {
    let next_op = BINOP_PRECEDENCE
        .iter()
        .skip_while(|p| *p != &op)
        .skip(1)
        .next();

    let a = if let Some(next_op) = next_op {
        parse_binary_expr(tokens, *next_op)?
    } else {
        parse_index_expr(tokens)?
    };

    let mut result = a;
    while let Some(op_token) = tokens.take_if(op) {
        let b = if let Some(next_op) = next_op {
            parse_binary_expr(tokens, *next_op)?
        } else {
            parse_index_expr(tokens)?
        };
        result = ExprNode::Binary(BinaryExprNode {
            a: Box::new(result),
            op: op_token,
            b: Box::new(b),
        });
    }

    Ok(result)
}

fn parse_index_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    let target = parse_cast_expr(tokens)?;
    if let Some(open_square) = tokens.take_if(TokenKind::OpenSquare) {
        let index = parse_expr(tokens)?;
        let close_square = tokens.take(TokenKind::CloseSquare)?;
        Ok(ExprNode::Index(IndexExprNode {
            target: Box::new(target),
            open_square,
            index: Box::new(index),
            close_square,
        }))
    } else {
        Ok(target)
    }
}

fn parse_cast_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    let value = parse_unary_expr(tokens)?;
    if let Some(as_tok) = tokens.take_if(TokenKind::As) {
        let target = parse_type_expr(tokens)?;
        Ok(ExprNode::Cast(CastExprNode {
            value: Box::new(value),
            as_tok,
            target: Box::new(target),
        }))
    } else {
        Ok(value)
    }
}

const UNARY_OP: &'static [TokenKind] = &[
    TokenKind::BitNot,
    TokenKind::Sub,
    TokenKind::Add,
    TokenKind::Not,
];

fn parse_unary_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    if !UNARY_OP.contains(&tokens.kind()) {
        return parse_call_expr(tokens);
    }

    let op = tokens.next();
    let value = parse_call_expr(tokens)?;
    Ok(ExprNode::Unary(UnaryExprNode {
        op,
        value: Box::new(value),
    }))
}

fn parse_call_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    let target = parse_primary_expr(tokens)?;

    if tokens.kind() != TokenKind::OpenBrac {
        return Ok(target);
    }

    let (open_brac, arguments, close_brac) = parse_sequence(
        tokens,
        TokenKind::OpenBrac,
        TokenKind::Comma,
        TokenKind::CloseBrac,
        parse_expr,
    )?;

    Ok(ExprNode::Call(CallExprNode {
        target: Box::new(target),
        open_brac,
        arguments,
        close_brac,
    }))
}

const EXPR_SYNC_TOKEN: &'static [TokenKind] = &[
    TokenKind::OpenBrac,
    TokenKind::OpenSquare,
    TokenKind::Ident,
    TokenKind::IntegerLit,
    TokenKind::RealLit,
    TokenKind::StringLit,
    TokenKind::True,
    TokenKind::False,
];

fn parse_primary_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    match tokens.kind() {
        TokenKind::OpenBrac => {
            let open_brac = tokens.take(TokenKind::OpenBrac)?;
            let value = parse_expr(tokens)?;
            let close_brac = tokens.take(TokenKind::CloseBrac)?;
            Ok(ExprNode::Grouped(GroupedExprNode {
                open_brac,
                value: Box::new(value),
                close_brac,
            }))
        }
        TokenKind::OpenSquare => {
            let (open_square, elements, close_square) = parse_sequence(
                tokens,
                TokenKind::OpenSquare,
                TokenKind::Comma,
                TokenKind::CloseSquare,
                parse_expr,
            )?;
            Ok(ExprNode::ArrayLit(ArrayLitNode {
                open_square,
                elements,
                close_square,
            }))
        }
        TokenKind::Ident => Ok(ExprNode::Ident(tokens.next())),
        TokenKind::IntegerLit => Ok(ExprNode::IntegerLit(tokens.next())),
        TokenKind::RealLit => Ok(ExprNode::RealLit(tokens.next())),
        TokenKind::StringLit => Ok(ExprNode::StringLit(tokens.next())),
        TokenKind::True | TokenKind::False => Ok(ExprNode::BooleanLit(tokens.next())),
        _ => Err(CompileError::unexpected_token(
            Vec::from(EXPR_SYNC_TOKEN),
            tokens.next(),
        )),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::scan;

    #[test]
    fn testcase_1() {
        let source_code = r##"
        var a: int = 10; 
        func add() -> int { 
            return a + b; 
        }
        "##;
        let tokens = scan(source_code).unwrap();
        let ast = parse(tokens).unwrap();
        let expected = RootNode {
            items: vec![
                Item::Var(VarNode {
                    var: Token {
                        kind: TokenKind::Var,
                        position: Position { line: 2, col: 9 },
                        value: Rc::new("var".into()),
                    },
                    name: Token {
                        kind: TokenKind::Ident,
                        position: Position { line: 2, col: 13 },
                        value: Rc::new("a".into()),
                    },
                    colon: Some(Token {
                        kind: TokenKind::Colon,
                        position: Position { line: 2, col: 14 },
                        value: Rc::new(":".into()),
                    }),
                    typ: Some(TypeExprNode::Ident(Token {
                        kind: TokenKind::Ident,
                        position: Position { line: 2, col: 16 },
                        value: Rc::new("int".into()),
                    })),
                    assign: Some(Token {
                        kind: TokenKind::Assign,
                        position: Position { line: 2, col: 20 },
                        value: Rc::new("=".into()),
                    }),
                    value: Some(ExprNode::IntegerLit(Token {
                        kind: TokenKind::IntegerLit,
                        position: Position { line: 2, col: 22 },
                        value: Rc::new("10".into()),
                    })),
                }),
                Item::Func(FuncNode {
                    head: FuncHeadNode {
                        func: Token {
                            kind: TokenKind::Function,
                            position: Position { line: 3, col: 9 },
                            value: Rc::new("func".into()),
                        },
                        native: None,
                        name: Token {
                            kind: TokenKind::Ident,
                            position: Position { line: 3, col: 14 },
                            value: Rc::new("add".into()),
                        },
                        open_brac: Token {
                            kind: TokenKind::OpenBrac,
                            position: Position { line: 3, col: 17 },
                            value: Rc::new("(".into()),
                        },
                        parameters: vec![],
                        close_brac: Token {
                            kind: TokenKind::CloseBrac,
                            position: Position { line: 3, col: 18 },
                            value: Rc::new(")".into()),
                        },
                        arrow: Some(Token {
                            kind: TokenKind::Arrow,
                            position: Position { line: 3, col: 20 },
                            value: Rc::new("->".into()),
                        }),
                        return_type: Some(TypeExprNode::Ident(Token {
                            kind: TokenKind::Ident,
                            position: Position { line: 3, col: 23 },
                            value: Rc::new("int".into()),
                        })),
                    },
                    body: Some(BlockStmtNode {
                        open_block: Token {
                            kind: TokenKind::OpenBlock,
                            position: Position { line: 3, col: 27 },
                            value: Rc::new("{".into()),
                        },
                        statements: vec![StmtNode::Return(ReturnStmtNode {
                            return_tok: Token {
                                kind: TokenKind::Return,
                                position: Position { line: 4, col: 13 },
                                value: Rc::new("return".into()),
                            },
                            value: Some(ExprNode::Binary(BinaryExprNode {
                                a: Box::new(ExprNode::Ident(Token {
                                    kind: TokenKind::Ident,
                                    position: Position { line: 4, col: 20 },
                                    value: Rc::new("a".into()),
                                })),
                                op: Token {
                                    kind: TokenKind::Add,
                                    position: Position { line: 4, col: 22 },
                                    value: Rc::new("+".into()),
                                },
                                b: Box::new(ExprNode::Ident(Token {
                                    kind: TokenKind::Ident,
                                    position: Position { line: 4, col: 24 },
                                    value: Rc::new("b".into()),
                                })),
                            })),
                        })],
                        close_block: Token {
                            kind: TokenKind::CloseBlock,
                            position: Position { line: 5, col: 9 },
                            value: Rc::new("}".into()),
                        },
                    }),
                }),
            ],
        };
        assert_eq!(expected, ast,);
    }
}
