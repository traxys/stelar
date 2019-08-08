#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

///! Symbols in a grammar
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Symbol<T, NT> {
    Terminal(T),
    NonTerminal(NT),
}

///! A numbered rule (or production) of a grammar, of the form `lhs -> rhs`
#[derive(Hash, Clone, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Rule<T, NT> {
    pub index: usize,
    pub lhs: NT,
    pub rhs: Vec<Symbol<T, NT>>,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct Token<T> {
    pub kind: T,
    pub info: TokenInfo,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct TokenInfo {
    pub position: usize,
}

pub fn create_rules<T, NT>(rules: Vec<(NT, Vec<Symbol<T, NT>>)>) -> Vec<Rule<T, NT>> {
    rules
        .into_iter()
        .enumerate()
        .map(|(index, (lhs, rhs))| Rule { index, lhs, rhs })
        .collect()
}
