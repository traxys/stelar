use crate::grammar::Symbol;
use crate::parse_table::{Action, ParseTable};
use crate::ValuedToken;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

///! A parser wrapping a ParseTable with the parsing logic
pub struct Parser<T, V, NT>
where
    T: PartialEq + Eq + Hash + Clone,
    NT: PartialEq + Eq + Hash + Clone,
{
    parse_table: ParseTable<T, NT>,
    tree_stack: Vec<SyntaxTree<T, NT, V>>,
    state_stack: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct SyntaxTree<T, NT, V> {
    pub symbol: Symbol<ValuedToken<T, V>, NT>,
    pub rule_applied: Option<usize>,
    pub children: Vec<SyntaxTree<T, NT, V>>,
}

#[derive(Debug)]
pub enum ParseError<T>
where
    T: PartialEq + Eq + Hash,
{
    Unexpected {
        expected: HashSet<T>,
        got: Option<T>,
    },
}

impl<T, V, NT> Parser<T, V, NT>
where
    V: Debug,
    NT: Clone + PartialEq + Eq + Hash + Debug,
    T: PartialEq + Eq + Hash + Clone + Debug,
{
    pub fn new(parse_table: ParseTable<T, NT>) -> Parser<T, V, NT> {
        Parser {
            parse_table,
            tree_stack: Vec::new(),
            state_stack: vec![0],
        }
    }

    pub fn parse<I>(&mut self, input: I) -> Result<SyntaxTree<T, NT, V>, ParseError<T>>
    where
        I: Iterator<Item = ValuedToken<T, V>>,
    {
        for token in input {
            self.handle_token(Some(token))?
        }
        self.handle_token(None)?;
        let res = self.tree_stack.pop().unwrap();

        // Cleanup for parser to be used again
        self.state_stack = vec![0];
        self.tree_stack.clear();
        Ok(res)
    }

    fn shift(&mut self, value: ValuedToken<T, V>, new_state: usize) {
        self.state_stack.push(new_state);
        self.tree_stack.push(SyntaxTree {
            children: Vec::new(),
            rule_applied: None,
            symbol: Symbol::Terminal(value),
        });
    }

    fn handle_token(&mut self, token: Option<ValuedToken<T, V>>) -> Result<(), ParseError<T>> {
        //let (token, value) = (token.token, token.value);

        let trans = (
            *self.state_stack.last().unwrap(),
            match &token {
                Some(ValuedToken { token: t, value: _ }) => Some(t.clone()),
                None => None,
            },
        );
        let action = self.parse_table.action_table.get(&trans).cloned();
        let (state, _) = trans;

        match action {
            None => Err(ParseError::Unexpected {
                expected: self
                    .parse_table
                    .expected_in_state
                    .get(&state)
                    .cloned()
                    .unwrap(),
                got: token.map(|t| t.token),
            }),
            Some(Action::Shift(new_state)) => {
                self.shift(token.unwrap(), new_state);
                Ok(())
            }
            Some(Action::Reduce(rule_count)) => {
                self.reduce(rule_count);
                self.handle_token(token)
            }
            _ => Ok(()),
        }
    }

    fn reduce(&mut self, i: usize) -> &NT {
        let rule = &self.parse_table.rules[i];
        let children = self
            .tree_stack
            .split_off(self.tree_stack.len() - rule.rhs.len());
        self.state_stack
            .split_off(self.state_stack.len() - rule.rhs.len());
        let prior_state = self.state_stack.last().unwrap();
        let new_state = self
            .parse_table
            .goto_table
            .get(&(*prior_state, rule.lhs.clone()))
            .unwrap();
        self.state_stack.push(*new_state);
        self.tree_stack.push(SyntaxTree {
            children: children,
            rule_applied: Some(i),
            symbol: Symbol::NonTerminal(rule.lhs.clone()),
        });
        &rule.lhs
    }
}
