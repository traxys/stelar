use super::{AddOperation, MulOperation, TokenKind, TokenValue};
use stelar::{Extract, ValuedToken};

fn is_quote(s: &str) -> bool {
    s == "'" || s == "\""
}
fn is_rawstr(s: &str) -> bool {
    !s.contains('\"')
}
fn is_separator(s: &str) -> bool {
    s == ","
}
fn is_name(s: &str) -> bool {
    s.chars().all(char::is_alphabetic)
}
fn is_assign(s: &str) -> bool {
    match s {
        "=" | ":=" => true,
        _ => false,
    }
}
fn is_integer(s: &str) -> bool {
    s.parse::<u64>().is_ok()
}
fn is_add_op(s: &str) -> bool {
    match s {
        "+" | "-" => true,
        _ => false,
    }
}
fn is_mul_op(s: &str) -> bool {
    match s {
        "*" | "/" => true,
        _ => false,
    }
}
fn is_skip(s: &str) -> bool {
    match s {
        " " => true,
        _ => false,
    }
}
fn is_lparen(s: &str) -> bool {
    match s {
        "(" => true,
        _ => false,
    }
}
fn is_rparen(s: &str) -> bool {
    match s {
        ")" => true,
        _ => false,
    }
}

pub struct TokenContext {
    accept_raw_str: bool,
}

impl TokenContext {
    pub fn new() -> TokenContext {
        TokenContext {
            accept_raw_str: false,
        }
    }
}

impl Extract<String> for TokenKind {
    type Value = TokenValue;
    type Context = TokenContext;
    fn extract(
        input: &mut String,
        ctx: &mut TokenContext,
    ) -> Option<ValuedToken<Self, TokenValue>> {
        let mut possible = None;
        let mut max_valid = 1;

        if input.is_empty() {
            return None;
        } else if input.len() == 1 {
            if is_name(input) {
                possible = Some(TokenKind::Name)
            }
            if is_skip(input) {
                possible = Some(TokenKind::Skip);
            }
            if is_add_op(input) {
                possible = Some(TokenKind::AddOp);
            }
            if is_mul_op(input) {
                possible = Some(TokenKind::MulOp);
            }
            if is_rparen(input) {
                possible = Some(TokenKind::RParen);
            }
            if is_lparen(input) {
                possible = Some(TokenKind::RParen);
            }
            if is_assign(input) {
                possible = Some(TokenKind::Assign);
            }
            if is_separator(input) {
                possible = Some(TokenKind::Separator);
            }
            if is_integer(input) {
                possible = Some(TokenKind::Int);
            }
            if is_quote(input) {
                possible = Some(TokenKind::Quote);
            }
        } else {
            for i in 1..=input.len() {
                let mut has_valid = false;
                let sub = &input[0..i];
                max_valid = i;
                if ctx.accept_raw_str {
                    if is_rawstr(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::RawStr);
                    }
                } else {
                    if is_name(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::Name);
                    }
                    if is_integer(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::Int);
                    }
                    if is_add_op(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::AddOp);
                    }
                    if is_mul_op(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::MulOp);
                    }
                    if is_rparen(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::RParen);
                    }
                    if is_lparen(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::LParen);
                    }
                    if is_skip(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::Skip);
                    }
                    if is_assign(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::Assign);
                    }
                    if is_separator(sub) {
                        has_valid = true;
                        possible = Some(TokenKind::Separator);
                    }
                }
                if is_quote(sub) {
                    possible = Some(TokenKind::Quote);
                    max_valid = i;
                    ctx.accept_raw_str = !ctx.accept_raw_str;
                    break;
                }
                if !has_valid {
                    max_valid = i - 1;
                    break;
                }
            }
        }
        let mut token = input.split_off(max_valid);
        std::mem::swap(&mut token, input);

        match possible {
            Some(TokenKind::Quote) => Some(ValuedToken {
                token: TokenKind::Quote,
                value: None,
            }),
            Some(TokenKind::RawStr) => Some(ValuedToken {
                token: TokenKind::RawStr,
                value: Some(TokenValue::Litteral(token)),
            }),
            Some(TokenKind::Int) => Some(ValuedToken {
                token: TokenKind::Int,
                value: Some(TokenValue::Integer(token.parse().unwrap())),
            }),
            Some(TokenKind::AddOp) => Some(ValuedToken {
                token: TokenKind::AddOp,
                value: Some(TokenValue::Plus(AddOperation::from_string(&token).unwrap())),
            }),
            Some(TokenKind::MulOp) => Some(ValuedToken {
                token: TokenKind::MulOp,
                value: Some(TokenValue::Mul(MulOperation::from_string(&token).unwrap())),
            }),
            Some(TokenKind::LParen) => Some(ValuedToken {
                token: TokenKind::LParen,
                value: None,
            }),
            Some(TokenKind::RParen) => Some(ValuedToken {
                token: TokenKind::RParen,
                value: None,
            }),
            Some(TokenKind::Skip) => Some(ValuedToken {
                token: TokenKind::Skip,
                value: None,
            }),
            Some(TokenKind::Name) => Some(ValuedToken {
                token: TokenKind::Name,
                value: Some(TokenValue::Litteral(token)),
            }),
            Some(TokenKind::Assign) => Some(ValuedToken {
                token: TokenKind::Assign,
                value: None,
            }),
            Some(TokenKind::Separator) => Some(ValuedToken {
                token: TokenKind::Separator,
                value: None,
            }),
            None => None,
        }
    }
}
