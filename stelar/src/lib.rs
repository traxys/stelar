#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

///! Types used to define a grammar
pub mod grammar;
///! Define a parse table for a parser
pub mod parse_table;

///! Trait to extract values out of a input storage
pub trait Extract<I>: Sized {
    fn extract(input: &mut I) -> Option<Self>;
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
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        T::extract(&mut self.input)
    }
}
