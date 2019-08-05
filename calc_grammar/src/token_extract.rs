use super::{AddOperation, MulOperation, TokenKind, Value};
use stelar::{Extract, ValuedToken};

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
impl Extract<String> for TokenKind {
    type Value = Value;
    fn extract(input: &mut String) -> Option<ValuedToken<Self, Value>> {
        let mut possible = None;
        let mut max_valid = 1;
        if input.len() == 0 {
            return None;
        } else if input.len() == 1 {
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
        } else {
            for i in 1..input.len() {
                let mut has_valid = false;
                let sub = &input[0..i];
                max_valid = i;
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
                if !has_valid {
                    max_valid = i - 1;
                    break;
                }
            }
        }
        let mut token = input.split_off(max_valid);
        std::mem::swap(&mut token, input);

        match possible {
            Some(TokenKind::Int) => Some(ValuedToken {
                token: TokenKind::Int,
                value: Some(Value::Integer(token.parse().unwrap())),
            }),
            Some(TokenKind::AddOp) => Some(ValuedToken {
                token: TokenKind::AddOp,
                value: Some(Value::Plus(AddOperation::from_string(&token).unwrap())),
            }),
            Some(TokenKind::MulOp) => Some(ValuedToken {
                token: TokenKind::MulOp,
                value: Some(Value::Mul(MulOperation::from_string(&token).unwrap())),
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
            None => None,
        }
    }
}
