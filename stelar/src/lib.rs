#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

///! Types used to define a grammar
pub mod grammar;
///! Define a parse table for a parser
pub mod parse_table;
mod parser;
pub use parser::Parser;
pub use parser::SyntaxTree;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ValuedToken<T, V> {
    pub token: T,
    pub value: Option<V>,
}

///! Trait to extract values out of a input storage
pub trait Extract<I>: Sized {
    type Value;

    fn extract(input: &mut I) -> Option<ValuedToken<Self, Self::Value>>;
}

///! Tokens over an input storage
pub struct TokenStream<I, T>
where
    T: Extract<I>,
{
    input: I,
    _marker: std::marker::PhantomData<T>,
}

impl<T: Extract<I>, I> TokenStream<I, T> {
    pub fn new(input: I) -> TokenStream<I, T> {
        TokenStream {
            input,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<I, T> Iterator for TokenStream<I, T>
where
    T: Extract<I>,
{
    type Item = ValuedToken<T, T::Value>;
    fn next(&mut self) -> Option<Self::Item> {
        T::extract(&mut self.input)
    }
}

#[macro_export]
macro_rules! rule_rhs {
    (@impl ($($stack:expr,)*), ($e:expr), $($rest:tt)*) => (rule_rhs!(@impl ($($stack,)* Symbol::NonTerminal($e),), $($rest)*));
    (@impl ($($stack:expr,)*), $e:expr, $($rest:tt)*) => (rule_rhs!(@impl ($($stack,)* Symbol::Terminal($e),), $($rest)*));
    (@impl ($($stack:expr,)*), $(,)?) => (vec![$($stack),*]);
    (@impl ($($stack:expr,)*), $($rest:tt)*) => (compile_error!("invalid input"));
    ($($rest:tt)*) => (rule_rhs!(@impl (), $($rest)*,)); // initialization
}
