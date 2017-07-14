pub trait FromToken<T>: Sized {
    fn from_token(tok: &T) -> Option<Self>;
}
