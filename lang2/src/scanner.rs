use crate::{
    errors::{missing_closing_quote, unexpected_char, CompileError, Result},
    tokens::{Position, Token, TokenKind},
};
use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    fs::read_to_string,
    iter::Peekable,
    path::{Path, PathBuf},
    rc::Rc,
    vec::IntoIter,
};

pub fn scan(file_path: &Path) -> Result<Vec<Token>> {
    let source_code = read_to_string(file_path)?;
    let mut source_code = load_chars(file_path, &source_code).into_iter().peekable();

    let mut result = vec![];
    let mut errors = CompileError::new();

    loop {
        match next(&mut source_code) {
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

fn load_chars(file_path: &Path, s: &str) -> Vec<CharPos> {
    let file_path = Rc::new(PathBuf::from(file_path));
    let mut line = 1;
    let mut col = 0;
    let mut result = vec![];
    for ch in s.chars() {
        col += 1;
        result.push(CharPos {
            ch,
            pos: Position {
                file_path: file_path.clone(),
                line,
                col,
            },
        });

        if ch == '\n' {
            col = 0;
            line += 1;
        }
    }
    result
}

type SourceCode = Peekable<IntoIter<CharPos>>;

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

fn next(source_code: &mut SourceCode) -> ScanResult {
    skip_whitespace(source_code);
    scan_word(source_code)
        .or_else(|| scan_string_lit(source_code))
        .or_else(|| scan_number_lit(source_code))
        .or_else(|| scan_comment(source_code))
        .or_else(|| scan_symbol(source_code))
        .or_else(|| scan_unexpected_chars(source_code))
}

fn skip_whitespace(source_code: &mut SourceCode) {
    while let Some(ch) = source_code.peek() {
        if ch.ch.is_whitespace() {
            source_code.next();
        } else {
            break;
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenKind> = HashMap::from([
        ("var", TokenKind::Var),
        ("as", TokenKind::As),
        ("func", TokenKind::Function),
        ("native", TokenKind::Native),
        ("true", TokenKind::True),
        ("false", TokenKind::False),
        ("if", TokenKind::If),
        ("else", TokenKind::Else),
        ("while", TokenKind::While),
        ("continue", TokenKind::Continue),
        ("break", TokenKind::Break),
        ("return", TokenKind::Return),
        ("pub", TokenKind::Pub),
        ("struct", TokenKind::Struct),
        ("tuple", TokenKind::Tuple),
        ("import", TokenKind::Import),
        ("builtin", TokenKind::Builtin),
    ]);
}

fn scan_word(source_code: &mut SourceCode) -> ScanResult {
    let initial = |c: &CharPos| c.ch.is_alphabetic() || c.ch == '_';
    let Some(tok) = source_code.next_if(initial) else {
            return ScanResult::None;
        };

    let position = tok.pos;
    let mut value = String::from(tok.ch);

    let valid_char = |c: &CharPos| c.ch.is_alphabetic() || c.ch.is_digit(10) || c.ch == '_';
    while let Some(c) = source_code.next_if(valid_char) {
        value.push(c.ch);
    }

    let kind = KEYWORDS
        .get(value.as_str())
        .map(|kind| kind.clone())
        .unwrap_or(TokenKind::Ident);

    ScanResult::Token(Token {
        kind,
        position,
        value: Rc::new(value),
    })
}

fn scan_string_lit(source_code: &mut SourceCode) -> ScanResult {
    let Some(tok) = source_code.next_if(|c| c.ch == '"') else {
        return ScanResult::None;
    };

    let position = tok.pos.clone();
    let opening_quote = tok.ch;
    let mut value = String::from(tok.ch);
    let mut errors = CompileError::new();

    let mut after_backslash = false;
    let mut is_closed = false;

    while let Some(c) = source_code.next() {
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
        errors.push(missing_closing_quote(position.clone()));
    }

    if !errors.is_empty() {
        ScanResult::Err(errors.into())
    } else {
        ScanResult::Token(Token {
            kind: TokenKind::StringLit,
            position: position.clone(),
            value: Rc::new(value),
        })
    }
}

fn scan_number_lit(source_code: &mut SourceCode) -> ScanResult {
    let Some(tok) = source_code.next_if(|c| c.ch.is_digit(10)) else {
            return ScanResult::None;
        };

    let position = tok.pos;
    let mut value = String::from(tok.ch);

    while let Some(c) = source_code.next_if(|c| c.ch.is_digit(10)) {
        value.push(c.ch);
    }

    if let Some(dot) = source_code.next_if(|c| c.ch == '.') {
        value.push(dot.ch);
        while let Some(c) = source_code.next_if(|c| c.ch.is_digit(10)) {
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

lazy_static! {
    static ref SYMBOLS: Vec<(&'static str, TokenKind)> = vec![
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
}

fn scan_symbol(source_code: &mut SourceCode) -> ScanResult {
    let Some(position) = source_code.peek().and_then(|c| Some(c.pos.clone())) else {
        return ScanResult::None;
    };

    let mut value = String::new();
    let mut sym = String::new();
    let mut kind = TokenKind::Invalid;

    while let Some(c) = source_code.peek() {
        sym.push(c.ch);
        let mut found = false;
        for (_, k) in SYMBOLS.iter().filter(|(op, _)| op.starts_with(&sym)) {
            found = true;
            kind = *k;
        }
        if !found {
            break;
        }

        value.push(c.ch);
        source_code.next();
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
fn scan_comment(source_code: &mut SourceCode) -> ScanResult {
    let Some(c) = source_code.next_if(|c| c.ch == '/') else {
            return ScanResult::None;
        };

    let position = c.pos;
    let mut value = String::from(c.ch);

    let Some(c) = source_code.next_if(|c| c.ch == '/') else {
            unreachable!();
        };
    value.push(c.ch);

    while let Some(c) = source_code.next() {
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

fn scan_unexpected_chars(source_code: &mut SourceCode) -> ScanResult {
    if let Some(char_pos) = source_code.next() {
        ScanResult::Err(unexpected_char(char_pos.pos, char_pos.ch))
    } else {
        ScanResult::None
    }
}
