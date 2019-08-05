use crate::grammar::Symbol;
use crate::parse_table::{Action, ParseTable};
use crate::ValuedToken;
use std::collections::HashSet;
use std::hash::Hash;

///! A parser wrapping a ParseTable with the parsing logic
pub struct Parser<T, V, NT>
where
    T: PartialEq + Eq + Hash + Clone,
    NT: PartialEq + Eq + Hash + Clone,
{
    state: usize,
    parse_table: ParseTable<T, NT>,
    stack: Vec<SyntaxTree<T, NT, V>>,
}

pub struct SyntaxTree<T, NT, V> {
    pub symbol: Symbol<ValuedToken<T, V>, NT>,
    pub rule_applied: Option<usize>,
    pub children: Vec<SyntaxTree<T, NT, V>>,
}

pub enum ParseError<T>
where
    T: PartialEq + Eq + Hash,
{
    Unexpected { expected: HashSet<T>, got: T },
}

impl<T, V, NT> Parser<T, V, NT>
where
    NT: Clone + PartialEq + Eq + Hash,
    T: PartialEq + Eq + Hash + Clone,
{
    pub fn new(parse_table: ParseTable<T, NT>) -> Parser<T, V, NT> {
        Parser {
            parse_table,
            stack: Vec::new(),
            state: 0,
        }
    }

    pub fn parse<I>(&mut self, input: I) -> Result<SyntaxTree<T, NT, V>, ParseError<T>>
    where
        I: Iterator<Item = ValuedToken<T, V>>,
    {
        for token in input {
            self.handle_token(token)?
        }
        let res = Ok(self.stack.pop().unwrap());

        // Cleanup for parser to be used again
        self.state = 0;
        self.stack.clear();
        res
    }

    fn shift(&mut self, value: ValuedToken<T, V>) {
        self.stack.push(SyntaxTree {
            children: Vec::new(),
            rule_applied: None,
            symbol: Symbol::Terminal(value),
        })
    }

    fn handle_token(&mut self, token: ValuedToken<T, V>) -> Result<(), ParseError<T>> {
        let (token, value) = (token.token, token.value);
        let trans = (self.state, Some(token));
        let action = self.parse_table.action_table.get(&trans).cloned();
        let (state, token) = trans;
        let token = token.unwrap();
        match action {
            None => Err(ParseError::Unexpected {
                expected: self
                    .parse_table
                    .expected_in_state
                    .get(&state)
                    .cloned()
                    .unwrap(),
                got: token,
            }),
            Some(Action::Shift(i)) => {
                self.shift(ValuedToken { token, value });
                self.state = i;
                Ok(())
            }
            Some(Action::Reduce(i)) => {
                let nt = self.reduce(i).clone();
                self.state = *self.parse_table.goto_table.get(&(state, nt)).unwrap();
                self.handle_token(ValuedToken { token, value })
            }
            _ => Ok(()),
        }
    }

    fn reduce(&mut self, i: usize) -> &NT {
        let rule = &self.parse_table.rules[i];
        let children = self.stack.split_off(self.stack.len() - rule.rhs.len());
        self.stack.push(SyntaxTree {
            children,
            rule_applied: Some(i),
            symbol: Symbol::NonTerminal(rule.lhs.clone()),
        });
        &rule.lhs
    }
}
