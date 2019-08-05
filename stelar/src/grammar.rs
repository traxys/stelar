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
