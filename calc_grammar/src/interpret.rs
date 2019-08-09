use stelar::grammar::Symbol;
use stelar::{SyntaxTree, ValuedToken};
type AST = SyntaxTree<crate::TokenKind, crate::NonTerminal, crate::TokenValue>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueKind {
    Integer,
    Str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Integer(u64),
    Str(String),
}

impl Value {
    fn kind(&self) -> ValueKind {
        match self {
            Value::Integer(_) => ValueKind::Integer,
            Value::Str(_) => ValueKind::Str,
        }
    }
    pub fn to_integer(self) -> u64 {
        match self {
            Value::Integer(n) => n,
            _ => panic!("Value is {:?}", self.kind()),
        }
    }
    pub fn to_string(self) -> String {
        match self {
            Value::Str(s) => s,
            _ => panic!("Value is {:?}", self.kind()),
        }
    }

    pub fn as_string(self) -> Value {
        match self {
            Value::Integer(u) => Value::Str(format!("{}", u)),
            s => s,
        }
    }

    pub fn assert_type(&self, kind: Option<ValueKind>) -> Result<(), TypeError> {
        match kind {
            Some(kind) => {
                if self.kind() == kind {
                    Ok(())
                } else {
                    Err(TypeError::InvalidType {
                        got: self.kind(),
                        expected: kind,
                    })
                }
            }
            None => Ok(()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Integer(u) => write!(f, "{}", u),
            Value::Str(s) => write!(f, "{}", s),
        }
    }
}

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
    Type(TypeError),
}

#[derive(Debug)]
pub enum TypeError {
    InvalidType { got: ValueKind, expected: ValueKind },
}

impl From<TypeError> for EvalError {
    fn from(err: TypeError) -> EvalError {
        EvalError::Type(err)
    }
}

struct BuiltIn<'a> {
    pub f: &'a dyn Fn(Vec<Value>) -> Value,
    name: String,
    expected: &'a [Option<ValueKind>],
}

impl<'a> BuiltIn<'a> {
    fn compute(&self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() != self.expected.len() {
            Err(EvalError::InvalidArguments {
                function: self.name.clone(),
                expected: self.expected.len(),
                got: args.len(),
            })
        } else {
            for (arg, exp) in args.iter().zip(self.expected) {
                arg.assert_type(*exp)?
            }
            Ok((*self.f)(args))
        }
    }
}

use std::collections::HashMap;
type BuiltInMap<'a> = HashMap<String, BuiltIn<'a>>;

fn get_builtins<'a>() -> BuiltInMap<'a> {
    let mut map = HashMap::new();
    map.insert(
        "pow".to_string(),
        BuiltIn {
            f: &|mut args| {
                let b = args.pop().unwrap();
                let a = args.pop().unwrap();
                Value::Integer(a.to_integer().pow(b.to_integer() as u32))
            },
            name: "pow".to_string(),
            expected: &[Some(ValueKind::Integer), Some(ValueKind::Integer)],
        },
    );
    map.insert(
        "add".to_string(),
        BuiltIn {
            f: &|mut args| {
                let b = args.pop().unwrap();
                let a = args.pop().unwrap();
                Value::Integer(a.to_integer() + b.to_integer())
            },
            name: "add".to_string(),
            expected: &[Some(ValueKind::Integer), Some(ValueKind::Integer)],
        },
    );
    map.insert(
        "sub".to_string(),
        BuiltIn {
            f: &|mut args| {
                let b = args.pop().unwrap();
                let a = args.pop().unwrap();
                Value::Integer(a.to_integer() - b.to_integer())
            },
            name: "sub".to_string(),
            expected: &[Some(ValueKind::Integer), Some(ValueKind::Integer)],
        },
    );
    map.insert(
        "mul".to_string(),
        BuiltIn {
            f: &|mut args| {
                let b = args.pop().unwrap();
                let a = args.pop().unwrap();
                Value::Integer(a.to_integer() * b.to_integer())
            },
            name: "mul".to_string(),
            expected: &[Some(ValueKind::Integer), Some(ValueKind::Integer)],
        },
    );
    map.insert(
        "div".to_string(),
        BuiltIn {
            f: &|mut args| {
                let b = args.pop().unwrap();
                let a = args.pop().unwrap();
                Value::Integer(a.to_integer() / b.to_integer())
            },
            name: "div".to_string(),
            expected: &[Some(ValueKind::Integer), Some(ValueKind::Integer)],
        },
    );
    map.insert(
        "str".to_string(),
        BuiltIn {
            f: &|mut args| args.pop().unwrap().as_string(),
            name: "str".to_string(),
            expected: &[None],
        },
    );
    map
}

type Scope = HashMap<String, Value>;
struct Context<'a> {
    builtins: BuiltInMap<'a>,
    scope: Scope,
}

impl<'a> Context<'a> {
    fn new(scope: Scope) -> Context<'a> {
        Context {
            builtins: get_builtins(),
            scope,
        }
    }
}

fn calculate_function(name: &str, args: Vec<Value>, ctx: &Context) -> Result<Value, EvalError> {
    match ctx.builtins.get(name) {
        Some(b) => b.compute(args),
        _ => Err(EvalError::UnknownLitteral(name.to_owned())),
    }
}

fn interpret_list(
    mut tree: AST,
    mut previous: Vec<Value>,
    ctx: &mut Context,
) -> Result<Vec<Value>, EvalError> {
    match tree.rule_applied {
        Some(i) => {
            if i == 7 {
                let rest = tree.children.pop().unwrap();
                tree.children.pop();
                let value = interpret_expression(tree.children.pop().unwrap(), ctx)?;
                previous.push(value);
                interpret_list(rest, previous, ctx)
            } else if i == 8 {
                let value = interpret_expression(tree.children.pop().unwrap(), ctx)?;
                previous.push(value);
                Ok(previous)
            } else {
                Err(EvalError::InvalideRule {
                    context: crate::NonTerminal::List,
                    rule: i,
                })
            }
        }
        None => Err(EvalError::NonTerminalNotDerived(crate::NonTerminal::List)),
    }
}

fn interpret_expression(mut tree: AST, ctx: &mut Context) -> Result<Value, EvalError> {
    match tree.rule_applied {
        Some(i) => {
            if i == 1 {
                let rhs = interpret_expression(tree.children.pop().unwrap(), ctx)?;
                let operation = tree.children.pop().unwrap().symbol;
                let lhs = interpret_expression(tree.children.pop().unwrap(), ctx)?;
                match operation {
                    Symbol::Terminal(crate::ValuedToken {
                        value: Some(crate::TokenValue::Plus(add_op)),
                        ..
                    }) => match add_op {
                        crate::AddOperation::Plus => calculate_function("add", vec![lhs, rhs], ctx),
                        crate::AddOperation::Minus => {
                            calculate_function("sub", vec![lhs, rhs], ctx)
                        }
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
                interpret_expression(tree.children.pop().unwrap(), ctx)
            } else if i == 3 {
                let rhs = interpret_expression(tree.children.pop().unwrap(), ctx)?;
                let operation = tree.children.pop().unwrap().symbol;
                let lhs = interpret_expression(tree.children.pop().unwrap(), ctx)?;
                match operation {
                    Symbol::Terminal(crate::ValuedToken {
                        value: Some(crate::TokenValue::Mul(mul_op)),
                        ..
                    }) => match mul_op {
                        crate::MulOperation::Times => {
                            calculate_function("mul", vec![lhs, rhs], ctx)
                        }
                        crate::MulOperation::Div => calculate_function("div", vec![lhs, rhs], ctx),
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
                interpret_expression(tree.children.pop().unwrap(), ctx)
            } else if i == 5 {
                tree.children.pop();
                interpret_expression(tree.children.pop().unwrap(), ctx)
            } else if i == 6 {
                interpret_expression(tree.children.pop().unwrap(), ctx)
            } else if i == 9 {
                tree.children.pop().unwrap();
                let args = interpret_list(tree.children.pop().unwrap(), Vec::new(), ctx)?;
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
                calculate_function(&name, args, ctx)
            } else if i == 12 {
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
                match ctx.scope.get(&name) {
                    Some(n) => Ok(n.clone()),
                    None => Err(EvalError::UnknownLitteral(name)),
                }
            } else {
                Err(EvalError::InvalideRule {
                    context: crate::NonTerminal::Expr,
                    rule: i,
                })
            }
        }
        None => match tree.symbol {
            Symbol::Terminal(t) => match t.token {
                crate::TokenKind::Int => match t.value {
                    Some(crate::TokenValue::Integer(n)) => Ok(Value::Integer(n)),
                    _ => Err(EvalError::MissingValue(t.token)),
                },
                _ => Err(EvalError::EvaluateTerminal(t.token)),
            },
            Symbol::NonTerminal(nt) => Err(EvalError::NonTerminalNotDerived(nt)),
        },
    }
}

fn interpret_statement(mut tree: AST, ctx: &mut Context) -> Result<Option<Value>, EvalError> {
    match tree.rule_applied {
        Some(i) => {
            if i == 10 {
                Ok(Some(interpret_expression(
                    tree.children.pop().unwrap(),
                    ctx,
                )?))
            } else if i == 11 {
                let exp = interpret_expression(tree.children.pop().unwrap(), ctx)?;
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
                ctx.scope.insert(name, exp);
                Ok(None)
            } else {
                Err(EvalError::InvalideRule {
                    context: crate::NonTerminal::List,
                    rule: i,
                })
            }
        }
        None => Err(EvalError::NonTerminalNotDerived(
            crate::NonTerminal::Statement,
        )),
    }
}

pub fn interpret_tree(tree: AST, scope: Scope) -> (Result<Option<Value>, EvalError>, Scope) {
    let mut context = Context::new(scope);
    (interpret_statement(tree, &mut context), context.scope)
}
