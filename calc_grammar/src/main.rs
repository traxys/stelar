use stelar::grammar::{Rule, Symbol};
use stelar::{TokenStream, ValuedToken};
mod token_extract;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum AddOperation {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MulOperation {
    Times,
    Div,
}

impl AddOperation {
    fn from_string(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            _ => None,
        }
    }
}

impl MulOperation {
    fn from_string(s: &str) -> Option<Self> {
        match s {
            "*" => Some(Self::Times),
            "/" => Some(Self::Div),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum NonTerminal {
    Axiom,
    E,
    T,
    F,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Value {
    Integer(u64),
    Plus(AddOperation),
    Mul(MulOperation),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenKind {
    Int,
    AddOp,
    MulOp,
    Skip,
    LParen,
    RParen,
}

fn main() {
    let input = "( 9 + 12 * ( 42 + 3 ) - ( 5 * 6 + 3 ))".to_string();
    for token in TokenStream::new(input).filter(|t| {
        if let ValuedToken {
            token: TokenKind::Skip,
            value: _,
        } = t
        {
            false
        } else {
            true
        }
    }) {
        println!("Token: {:?}", token);
    }

    let grammar = vec![
        Rule {
            index: 0,
            lhs: NonTerminal::Axiom,
            rhs: vec![Symbol::NonTerminal(NonTerminal::E)],
        },
        Rule {
            index: 1,
            lhs: NonTerminal::E,
            rhs: vec![
                Symbol::NonTerminal(NonTerminal::E),
                Symbol::Terminal(TokenKind::AddOp),
                Symbol::NonTerminal(NonTerminal::T),
            ],
        },
        Rule {
            index: 2,
            lhs: NonTerminal::E,
            rhs: vec![Symbol::NonTerminal(NonTerminal::T)],
        },
        Rule {
            index: 3,
            lhs: NonTerminal::T,
            rhs: vec![
                Symbol::NonTerminal(NonTerminal::T),
                Symbol::Terminal(TokenKind::MulOp),
                Symbol::NonTerminal(NonTerminal::F),
            ],
        },
        Rule {
            index: 4,
            lhs: NonTerminal::T,
            rhs: vec![Symbol::NonTerminal(NonTerminal::F)],
        },
        Rule {
            index: 5,
            lhs: NonTerminal::F,
            rhs: vec![
                Symbol::Terminal(TokenKind::LParen),
                Symbol::NonTerminal(NonTerminal::E),
                Symbol::Terminal(TokenKind::RParen),
            ],
        },
        Rule {
            index: 6,
            lhs: NonTerminal::F,
            rhs: vec![Symbol::Terminal(TokenKind::Int)],
        },
    ];
}
