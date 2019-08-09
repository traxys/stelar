use stelar::grammar::Symbol;
use stelar::{SyntaxTree, ValuedToken};
type AST = SyntaxTree<crate::TokenKind, crate::NonTerminal, crate::TokenValue>;

#[derive(Debug)]
pub enum EvalError {
    MalformedToken {
        got: ValuedToken<crate::TokenKind, crate::TokenValue>,
        expected: crate::TokenKind,
        rule: usize,
    },
    InvalidNonTerminal {
        got: crate::NonTerminal,
        expected: crate::TokenKind,
        rule: usize,
    },
    NonTerminalNotDerived(crate::NonTerminal),
    InvalideRule {
        context: crate::NonTerminal,
        rule: usize,
    },
    EvaluateTerminal(crate::TokenKind),
    MissingValue(crate::TokenKind),
    UnknownLitteral(String),
    InvalidArguments {
        function: String,
        expected: usize,
        got: usize,
    },
}

fn compute_closure<F>(
    function: String,
    args: &[u64],
    arg_count: usize,
    f: F,
) -> Result<u64, EvalError>
where
    F: Fn(&[u64]) -> u64,
{
    let args_len = args.len();
    if args_len == arg_count {
        Ok(f(args))
    } else {
        Err(EvalError::InvalidArguments {
            function,
            expected: 2,
            got: args_len,
        })
    }
}

fn interpret_function(name: String, args: &[u64]) -> Result<u64, EvalError> {
    match name.as_ref() {
        "pow" => compute_closure(name, args, 2, |a| a[0].pow(a[1] as u32)),
        _ => Err(EvalError::UnknownLitteral(name)),
    }
}

fn interpret_list(mut tree: AST, mut previous: Vec<u64>) -> Result<Vec<u64>, EvalError> {
    match tree.rule_applied {
        Some(i) => {
            if i == 7 {
                let rest = tree.children.pop().unwrap();
                tree.children.pop();
                let value = interpret_expression(tree.children.pop().unwrap())?;
                previous.push(value);
                interpret_list(rest, previous)
            } else if i == 8 {
                let value = interpret_expression(tree.children.pop().unwrap())?;
                previous.push(value);
                Ok(previous)
            } else {
                Err(EvalError::InvalideRule {
                    context: crate::NonTerminal::L,
                    rule: i,
                })
            }
        }
        None => Err(EvalError::NonTerminalNotDerived(crate::NonTerminal::L)),
    }
}

fn interpret_expression(mut tree: AST) -> Result<u64, EvalError> {
    match tree.rule_applied {
        Some(i) => {
            if i == 1 {
                let rhs = interpret_expression(tree.children.pop().unwrap())?;
                let operation = tree.children.pop().unwrap().symbol;
                let lhs = interpret_expression(tree.children.pop().unwrap())?;
                match operation {
                    Symbol::Terminal(crate::ValuedToken {
                        value: Some(crate::TokenValue::Plus(add_op)),
                        ..
                    }) => match add_op {
                        crate::AddOperation::Plus => Ok(lhs + rhs),
                        crate::AddOperation::Minus => Ok(lhs - rhs),
                    },
                    Symbol::Terminal(malformed) => Err(EvalError::MalformedToken {
                        got: malformed,
                        expected: crate::TokenKind::AddOp,
                        rule: 1,
                    }),
                    Symbol::NonTerminal(invalid) => Err(EvalError::InvalidNonTerminal {
                        got: invalid,
                        expected: crate::TokenKind::AddOp,
                        rule: 1,
                    }),
                }
            } else if i == 2 {
                interpret_expression(tree.children.pop().unwrap())
            } else if i == 3 {
                let rhs = interpret_expression(tree.children.pop().unwrap())?;
                let operation = tree.children.pop().unwrap().symbol;
                let lhs = interpret_expression(tree.children.pop().unwrap())?;
                match operation {
                    Symbol::Terminal(crate::ValuedToken {
                        value: Some(crate::TokenValue::Mul(mul_op)),
                        ..
                    }) => match mul_op {
                        crate::MulOperation::Times => Ok(lhs * rhs),
                        crate::MulOperation::Div => Ok(lhs / rhs),
                    },
                    Symbol::Terminal(malformed) => Err(EvalError::MalformedToken {
                        got: malformed,
                        expected: crate::TokenKind::MulOp,
                        rule: 3,
                    }),
                    Symbol::NonTerminal(invalid) => Err(EvalError::InvalidNonTerminal {
                        got: invalid,
                        expected: crate::TokenKind::MulOp,
                        rule: 3,
                    }),
                }
            } else if i == 4 {
                interpret_expression(tree.children.pop().unwrap())
            } else if i == 5 {
                tree.children.pop();
                interpret_expression(tree.children.pop().unwrap())
            } else if i == 6 {
                interpret_expression(tree.children.pop().unwrap())
            } else if i == 9 {
                tree.children.pop().unwrap();
                let args = interpret_list(tree.children.pop().unwrap(), Vec::new())?;
                tree.children.pop().unwrap();
                let name = match tree.children.pop().unwrap().symbol {
                    Symbol::Terminal(crate::ValuedToken {
                        value: Some(crate::TokenValue::Identifier(name)),
                        ..
                    }) => name,
                    Symbol::Terminal(malformed) => {
                        return Err(EvalError::MalformedToken {
                            got: malformed,
                            expected: crate::TokenKind::Name,
                            rule: 9,
                        })
                    }
                    Symbol::NonTerminal(invalid) => {
                        return Err(EvalError::InvalidNonTerminal {
                            got: invalid,
                            expected: crate::TokenKind::Name,
                            rule: 9,
                        })
                    }
                };
                interpret_function(name, &args)
            } else {
                Err(EvalError::InvalideRule {
                    context: crate::NonTerminal::E,
                    rule: i,
                })
            }
        }
        None => match tree.symbol {
            Symbol::Terminal(t) => match t.token {
                crate::TokenKind::Int => match t.value {
                    Some(crate::TokenValue::Integer(n)) => Ok(n),
                    _ => Err(EvalError::MissingValue(t.token)),
                },
                _ => Err(EvalError::EvaluateTerminal(t.token)),
            },
            Symbol::NonTerminal(nt) => Err(EvalError::NonTerminalNotDerived(nt)),
        },
    }
}

pub fn interpret_tree(tree: AST) -> Result<u64, EvalError> {
    interpret_expression(tree)
}
