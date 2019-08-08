use stelar::grammar::Symbol;
use stelar::parse_table::ParseTable;
use stelar::rule_rhs;
use stelar::{Parser, SyntaxTree, TokenStream, ValuedToken};
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

fn interpret_tree(mut tree: SyntaxTree<TokenKind, NonTerminal, Value>) -> Result<u64, ()> {
    match tree.rule_applied {
        Some(i) => {
            if i == 1 {
                let rhs = interpret_tree(tree.children.pop().unwrap())?;
                let operation = tree.children.pop().unwrap().symbol;
                let lhs = interpret_tree(tree.children.pop().unwrap())?;
                match operation {
                    Symbol::Terminal(ValuedToken {
                        token: _,
                        value: Some(Value::Plus(add_op)),
                    }) => match add_op {
                        AddOperation::Plus => Ok(lhs + rhs),
                        AddOperation::Minus => Ok(lhs - rhs),
                    },
                    _ => Err(()),
                }
            } else if i == 2 {
                interpret_tree(tree.children.pop().unwrap())
            } else if i == 3 {
                let rhs = interpret_tree(tree.children.pop().unwrap())?;
                let operation = tree.children.pop().unwrap().symbol;
                let lhs = interpret_tree(tree.children.pop().unwrap())?;
                match operation {
                    Symbol::Terminal(ValuedToken {
                        token: _,
                        value: Some(Value::Mul(mul_op)),
                    }) => match mul_op {
                        MulOperation::Times => Ok(lhs * rhs),
                        MulOperation::Div => Ok(lhs / rhs),
                    },
                    _ => Err(()),
                }
            } else if i == 4 {
                interpret_tree(tree.children.pop().unwrap())
            } else if i == 5 {
                tree.children.pop();
                interpret_tree(tree.children.pop().unwrap())
            } else if i == 6 {
                interpret_tree(tree.children.pop().unwrap())
            } else {
                Err(())
            }
        }
        None => match tree.symbol {
            Symbol::Terminal(t) => match t.token {
                TokenKind::Int => match t.value {
                    Some(Value::Integer(n)) => Ok(n),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            _ => Err(()),
        },
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
    ]);

    let input_litteral = "(9 * 4 + 12 * ( 42 + 3 ) * 42 - ( (5 - 2) * 6 + 3 ))";
    let input = TokenStream::new(input_litteral.to_string()).filter(|t| {
        if let ValuedToken {
            token: TokenKind::Skip,
            value: _,
        } = t
        {
            false
        } else {
            true
        }
    });

    let start_rule = grammar[0].clone();
    let parse_table = ParseTable::new(grammar, start_rule).unwrap();
    //parse_table.print_tables();
    let mut parser = Parser::new(parse_table);
    let tree = parser.parse(input).unwrap();
    println!("{} = {:?}", input_litteral, interpret_tree(tree));
}
