use std::{iter::Peekable, vec::IntoIter};

use crate::{
    ast::{
        ArrayTypeNode, AssignStmtNode, BlockStmtNode, CallExprNode, ElseIfStmtNode, ElseStmtNode,
        ExprNode, FuncHeadNode, FuncNode, IfStmtNode, Item, ParameterNode, ReturnStmtNode,
        RootNode, StmtNode, TypeExprNode, VarNode, WhileStmtNode,
    },
    errors::{CompileError, MultiErrors, UnexpectedToken},
    tokens::{Position, Token, TokenKind},
};

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
                value: "".into(),
            })
            .clone()
    }

    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token {
            kind: TokenKind::Eof,
            position: Position::default(),
            value: "".into(),
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
                }
            }
        }

        tokens.skip_until(&sync_tokens);
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
            .into())
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
        let length = parse_expr(tokens)?;
        let close_square = tokens.take(TokenKind::CloseSquare)?;
        return Ok(TypeExprNode::Array(ArrayTypeNode {
            element_type: Box::new(typ),
            open_square,
            length,
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
        &[TokenKind::CloseBlock],
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
    Ok(ReturnStmtNode { return_tok, value })
}

const KEYWORD_TOKENS: &'static [TokenKind] = &[TokenKind::Break, TokenKind::Continue];

fn parse_keyword_stmt(tokens: &mut TokenStream) -> Result<Token, CompileError> {
    if KEYWORD_TOKENS.contains(&tokens.kind()) {
        Ok(tokens.next())
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
            Ok(StmtNode::Assign(AssignStmtNode {
                receiver: expr,
                assign,
                value,
            }))
        }
        TokenKind::OpenBrac => {
            let (open_brac, arguments, close_brac) = parse_sequence(
                tokens,
                TokenKind::OpenBrac,
                TokenKind::Comma,
                TokenKind::CloseBrac,
                parse_expr,
            )?;
            Ok(StmtNode::Call(CallExprNode {
                target: Box::new(expr),
                open_brac,
                arguments,
                close_brac,
            }))
        }
        _ => Err(CompileError::unexpected_token(
            vec![TokenKind::Assign, TokenKind::OpenBrac],
            tokens.next(),
        )),
    }
}

fn parse_call_stmt(tokens: &mut TokenStream) -> Result<CallExprNode, CompileError> {
    todo!();
}

fn parse_expr(tokens: &mut TokenStream) -> Result<ExprNode, CompileError> {
    todo!();
}
