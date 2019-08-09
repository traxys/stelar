use rustyline::error::ReadlineError;
use stelar::grammar::{Rule, Symbol};
use stelar::parse_table::ParseTable;
use stelar::rule_rhs;
use stelar::{Parser, TokenStream, ValuedToken};

mod completer;
mod interpret;
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
pub enum NonTerminal {
    Axiom,
    E,
    T,
    F,
    L,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenValue {
    Identifier(String),
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
    Assign,
    Name,
    Separator,
}

#[allow(dead_code)]
fn print_grammar(grammar: &[Rule<TokenKind, NonTerminal>]) {
    for rule in grammar {
        println!("({}) {:?} -> {:?}", rule.index, rule.lhs, rule.rhs)
    }
}

fn main() {
    let grammar = stelar::grammar::create_rules(vec![
        (NonTerminal::Axiom, rule_rhs![(NonTerminal::E)]),
        (
            NonTerminal::E,
            rule_rhs![(NonTerminal::E), TokenKind::AddOp, (NonTerminal::T)],
        ),
        (NonTerminal::E, rule_rhs![(NonTerminal::T)]),
        (
            NonTerminal::T,
            rule_rhs![(NonTerminal::T), TokenKind::MulOp, (NonTerminal::F)],
        ),
        (NonTerminal::T, rule_rhs![(NonTerminal::F)]),
        (
            NonTerminal::F,
            rule_rhs![TokenKind::LParen, (NonTerminal::E), TokenKind::RParen],
        ),
        (NonTerminal::F, rule_rhs![TokenKind::Int]),
        (
            NonTerminal::L,
            rule_rhs![(NonTerminal::E), TokenKind::Separator, (NonTerminal::L)],
        ),
        (NonTerminal::L, rule_rhs![(NonTerminal::E)]),
        (
            NonTerminal::F,
            rule_rhs![
                TokenKind::Name,
                TokenKind::LParen,
                (NonTerminal::L),
                TokenKind::RParen
            ],
        ),
    ]);

    let start_rule = grammar[0].clone();
    //print_grammar(&grammar);
    let parse_table = ParseTable::new(grammar.clone(), start_rule).unwrap();
    //parse_table.print_tables();
    let mut parser = Parser::new(parse_table);

    let mut rl = completer::get_editor();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit" {
                    println!("Bye !");
                    break;
                }
                if line == "grammar" {
                    print_grammar(&grammar);
                    continue;
                }
                rl.add_history_entry(line.as_str());
                let input = TokenStream::new(line).filter(|t| {
                    if let ValuedToken {
                        token: TokenKind::Skip,
                        ..
                    } = t
                    {
                        false
                    } else {
                        true
                    }
                });
                let tree = match parser.parse(input) {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        continue;
                    }
                };
                match interpret::interpret_tree(tree) {
                    Ok(n) => println!("-> {}", n),
                    Err(interpret::EvalError::InvalidArguments {
                        function: f,
                        got: n,
                        expected: e,
                    }) => eprintln!(
                        "Invalid number of arguments for {}: expected {} got {}",
                        f, e, n
                    ),
                    Err(interpret::EvalError::UnknownLitteral(s)) => {
                        eprintln!("Unknown symbol: {}", s)
                    }
                    Err(e) => eprintln!("Internal Error: {:?}", e),
                }
            }
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => {
                println!("Bye !");
                break;
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }
}
