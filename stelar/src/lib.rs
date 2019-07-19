///! Trait to extract values out of a input storage
pub trait Extract<I>: Sized {
    type Error;
    fn extract(input: &mut I) -> Result<Option<Self>, Self::Error>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
