use crate::{
    errors::{missing_closing_quote, unexpected_char, CompileError, Result},
    tokens::{Position, Token, TokenKind},
};
use std::{iter::Peekable, rc::Rc, vec::IntoIter};

pub fn scan(code: &str) -> Result<Vec<Token>> {
    let mut scanner = Scanner::new(code);
    let mut result = vec![];
    let mut errors = CompileError::new();

    loop {
        match scanner.next() {
            ScanResult::Token(tok) => result.push(tok),
            ScanResult::Err(err) => errors.push(err),
            ScanResult::None => break,
        }
    }

    if !errors.is_empty() {
        Err(errors.into())
    } else {
        Ok(result)
    }
}

struct CharPos {
    ch: char,
    pos: Position,
}

fn load_chars(s: &str) -> Vec<CharPos> {
    let mut line = 1;
    let mut col = 0;
    let mut result = vec![];
    for ch in s.chars() {
        col += 1;
        result.push(CharPos {
            ch,
            pos: Position { line, col },
        });

        if ch == '\n' {
            col = 0;
            line += 1;
        }
    }
    result
}

struct Scanner {
    source_code: Peekable<IntoIter<CharPos>>,
}

enum ScanResult {
    Token(Token),
    Err(CompileError),
    None,
}

impl ScanResult {
    fn or_else(self, f: impl FnOnce() -> ScanResult) -> ScanResult {
        match self {
            ScanResult::Token(token) => ScanResult::Token(token),
            ScanResult::Err(err) => ScanResult::Err(err),
            ScanResult::None => f(),
        }
    }
}

impl Scanner {
    fn new(code: &str) -> Scanner {
        let source_code = load_chars(code).into_iter().peekable();
        Self { source_code }
    }

    fn next(&mut self) -> ScanResult {
        self.skip_whitespace();
        self.scan_word()
            .or_else(|| self.scan_string_lit())
            .or_else(|| self.scan_number_lit())
            .or_else(|| self.scan_symbol())
            .or_else(|| self.scan_comment())
            .or_else(|| self.scan_unexpected_chars())
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.source_code.peek() {
            if ch.ch.is_whitespace() {
                self.source_code.next();
            } else {
                break;
            }
        }
    }

    fn scan_word(&mut self) -> ScanResult {
        let initial = |c: &CharPos| c.ch.is_alphabetic() || c.ch == '_';
        let Some(tok) = self.source_code.next_if(initial) else {
            return ScanResult::None;
        };

        let position = tok.pos;
        let mut value = String::from(tok.ch);

        let valid_char = |c: &CharPos| c.ch.is_alphabetic() || c.ch.is_digit(10) || c.ch == '_';
        while let Some(c) = self.source_code.next_if(valid_char) {
            value.push(c.ch);
        }

        let kind = match value.as_str() {
            "var" => TokenKind::Var,
            "as" => TokenKind::As,
            "func" => TokenKind::Function,
            "native" => TokenKind::Native,
            "type" => TokenKind::Type,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            "return" => TokenKind::Return,
            _ => TokenKind::Ident,
        };

        ScanResult::Token(Token {
            kind,
            position,
            value: Rc::new(value),
        })
    }

    fn scan_string_lit(&mut self) -> ScanResult {
        let Some(tok) = self.source_code.next_if(|c| c.ch == '"') else {
            return ScanResult::None;
        };

        let position = tok.pos;
        let opening_quote = tok.ch;
        let mut value = String::from(tok.ch);
        let mut errors = CompileError::new();

        let mut after_backslash = false;
        let mut is_closed = false;

        while let Some(c) = self.source_code.next() {
            if c.ch == '\n' {
                errors.push(unexpected_char(c.pos, c.ch));
                break;
            }

            if after_backslash {
                let next_char = match c.ch {
                    'n' => Ok('\n'),
                    'r' => Ok('\r'),
                    't' => Ok('\t'),
                    '\\' => Ok('\\'),
                    '0' => Ok('\0'),
                    '"' => Ok('"'),
                    '\'' => Ok('\''),
                    '`' => Ok('`'),
                    _ => Err(unexpected_char(c.pos, c.ch)),
                };

                if let Ok(c) = next_char {
                    value.push(c);
                } else if let Err(err) = next_char {
                    errors.push(err);
                }
                after_backslash = false;
            } else if c.ch == '\\' {
                after_backslash = true;
            } else if c.ch == opening_quote {
                value.push(c.ch);
                is_closed = true;
                break;
            } else {
                value.push(c.ch);
            }
        }

        if !is_closed {
            errors.push(missing_closing_quote(position));
        }

        if !errors.is_empty() {
            ScanResult::Err(errors.into())
        } else {
            ScanResult::Token(Token {
                kind: TokenKind::StringLit,
                position,
                value: Rc::new(value),
            })
        }
    }

    fn scan_number_lit(&mut self) -> ScanResult {
        let Some(tok) = self.source_code.next_if(|c| c.ch.is_digit(10)) else {
            return ScanResult::None;
        };

        let position = tok.pos;
        let mut value = String::from(tok.ch);

        while let Some(c) = self.source_code.next_if(|c| c.ch.is_digit(10)) {
            value.push(c.ch);
        }

        if let Some(dot) = self.source_code.next_if(|c| c.ch == '.') {
            value.push(dot.ch);
            while let Some(c) = self.source_code.next_if(|c| c.ch.is_digit(10)) {
                value.push(c.ch);
            }
            return ScanResult::Token(Token {
                kind: TokenKind::RealLit,
                position,
                value: Rc::new(value),
            });
        }

        ScanResult::Token(Token {
            kind: TokenKind::IntegerLit,
            position,
            value: Rc::new(value),
        })
    }

    fn scan_symbol(&mut self) -> ScanResult {
        let symbols = vec![
            (".", TokenKind::Dot),
            ("!=", TokenKind::NEq),
            ("!", TokenKind::Not),
            ("==", TokenKind::Eq),
            ("=", TokenKind::Assign),
            ("*", TokenKind::Mul),
            ("+", TokenKind::Add),
            ("->", TokenKind::Arrow),
            ("-", TokenKind::Sub),
            ("/", TokenKind::Div),
            (":", TokenKind::Colon),
            (";", TokenKind::Semicolon),
            ("<<", TokenKind::ShiftLeft),
            ("<=", TokenKind::LEq),
            ("<", TokenKind::Lt),
            (">>", TokenKind::ShiftRight),
            (">=", TokenKind::GEq),
            (">", TokenKind::Gt),
            ("{", TokenKind::OpenBlock),
            ("}", TokenKind::CloseBlock),
            ("[", TokenKind::OpenSquare),
            ("]", TokenKind::CloseSquare),
            ("(", TokenKind::OpenBrac),
            (")", TokenKind::CloseBrac),
            (",", TokenKind::Comma),
            ("%", TokenKind::Mod),
            ("&&", TokenKind::And),
            ("&", TokenKind::BitAnd),
            ("||", TokenKind::Or),
            ("|", TokenKind::BitOr),
            ("^", TokenKind::BitXor),
            ("~", TokenKind::BitNot),
        ];

        let Some(position) = self.source_code.peek().and_then(|c| Some(c.pos)) else {
            return ScanResult::None;
        };

        let mut value = String::new();
        let mut sym = String::new();
        let mut kind = TokenKind::Invalid;

        while let Some(c) = self.source_code.peek() {
            sym.push(c.ch);
            let mut found = false;
            for (op, k) in symbols.iter() {
                if op.starts_with(&sym) {
                    found = true;
                    kind = *k;
                }
            }
            if !found {
                break;
            }

            value.push(c.ch);
            self.source_code.next();
        }

        if let TokenKind::Invalid = kind {
            ScanResult::None
        } else {
            ScanResult::Token(Token {
                kind,
                position,
                value: Rc::new(value),
            })
        }
    }

    // scan_comment should be called after scan_symbol. scan_comment assumes that the
    // first two characters are not a valid operator symbol. scan_comment also assumes
    // that if the first character read is '/', the second character must be '/' as well.
    fn scan_comment(&mut self) -> ScanResult {
        let Some(c) = self.source_code.next_if(|c| c.ch == '/') else {
            return ScanResult::None;
        };

        let position = c.pos;
        let mut value = String::from(c.ch);

        let Some(c) = self.source_code.next_if(|c| c.ch == '/') else {
            unreachable!();
        };
        value.push(c.ch);

        while let Some(c) = self.source_code.next() {
            value.push(c.ch);
            if c.ch == '\n' {
                break;
            }
        }

        ScanResult::Token(Token {
            kind: TokenKind::Comment,
            position,
            value: Rc::new(value),
        })
    }

    fn scan_unexpected_chars(&mut self) -> ScanResult {
        if let Some(char_pos) = self.source_code.next() {
            ScanResult::Err(unexpected_char(char_pos.pos, char_pos.ch))
        } else {
            ScanResult::None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple_scan() {
        let source_code = "var a: integer = 10;";
        let tokens: Vec<TokenKind> = scan(&source_code)
            .unwrap()
            .into_iter()
            .map(|tok| tok.kind)
            .collect();
        assert_eq!(
            vec![
                TokenKind::Var,
                TokenKind::Ident,
                TokenKind::Colon,
                TokenKind::Ident,
                TokenKind::Assign,
                TokenKind::IntegerLit,
                TokenKind::Semicolon,
            ],
            tokens
        );
    }
}
