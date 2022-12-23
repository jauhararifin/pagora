use std::{iter::FromIterator, rc::Rc, str::FromStr};

use crate::{tokens::Token, types::Type};

pub fn parse_string(token: &Token) -> String {
    String::from_str(&token.value[1..token.value.len() - 1]).expect("invalid string literal")
}
