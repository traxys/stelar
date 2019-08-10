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
    Statement,
    Expr,
    Term,
    Factor,
    List,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenValue {
    Litteral(String),
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
    RawStr,
    Quote,
}

#[allow(dead_code)]
fn print_grammar(grammar: &[Rule<TokenKind, NonTerminal>]) {
    for rule in grammar {
        println!("({}) {:?} -> {:?}", rule.index, rule.lhs, rule.rhs)
    }
}

fn main() {
    let grammar = stelar::grammar::create_rules(vec![
        (NonTerminal::Axiom, rule_rhs![(NonTerminal::Statement)]),
        (
            NonTerminal::Expr,
            rule_rhs![(NonTerminal::Expr), TokenKind::AddOp, (NonTerminal::Term)],
        ),
        (NonTerminal::Expr, rule_rhs![(NonTerminal::Term)]),
        (
            NonTerminal::Term,
            rule_rhs![(NonTerminal::Term), TokenKind::MulOp, (NonTerminal::Factor)],
        ),
        (NonTerminal::Term, rule_rhs![(NonTerminal::Factor)]),
        (
            NonTerminal::Factor,
            rule_rhs![TokenKind::LParen, (NonTerminal::Expr), TokenKind::RParen],
        ),
        (NonTerminal::Factor, rule_rhs![TokenKind::Int]),
        (
            NonTerminal::List,
            rule_rhs![
                (NonTerminal::Expr),
                TokenKind::Separator,
                (NonTerminal::List)
            ],
        ),
        (NonTerminal::List, rule_rhs![(NonTerminal::Expr)]),
        (
            NonTerminal::Factor,
            rule_rhs![
                TokenKind::Name,
                TokenKind::LParen,
                (NonTerminal::List),
                TokenKind::RParen
            ],
        ),
        (NonTerminal::Statement, rule_rhs![(NonTerminal::Expr)]),
        (
            NonTerminal::Statement,
            rule_rhs![TokenKind::Name, TokenKind::Assign, (NonTerminal::Expr)],
        ),
        (NonTerminal::Term, rule_rhs![TokenKind::Name]),
        (
            NonTerminal::Expr,
            rule_rhs![TokenKind::Quote, TokenKind::RawStr, TokenKind::Quote],
        ),
    ]);

    let start_rule = grammar[0].clone();
    let parse_table = ParseTable::new(grammar.clone(), start_rule).unwrap();
    // parse_table.print_tables();
    let mut parser = Parser::new(parse_table);
    let mut print_stream = false;

    let mut rl = completer::get_editor();
    let mut scope = std::collections::HashMap::new();
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
                if line == "tokens" {
                    print_stream = !print_stream;
                    continue;
                }
                rl.add_history_entry(line.as_str());
                let input = TokenStream::new(line, token_extract::TokenContext::new())
                    .inspect(|t| {
                        if print_stream {
                            println!("{:?}", t)
                        }
                    })
                    .filter(|t| {
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
                let (res, new_scope) = interpret::interpret_tree(tree, scope);
                scope = new_scope;
                match res {
                    Ok(Some(v)) => println!("-> {}", v),
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
                    _ => (),
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
