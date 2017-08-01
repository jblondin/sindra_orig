pub trait Lowest {
    fn lowest() -> Self;
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum StandardPrecedence {
    Lowest          = 0 ,
    Equals          = 1 ,
    LessGreater     = 2 ,
    Sum             = 3 ,
    Product         = 4 ,
    Prefix          = 5 ,
    Postfix         = 6 ,
    Power           = 7 ,
    Call            = 8 ,
    Index           = 9,
}
impl Lowest for StandardPrecedence {
    fn lowest() -> StandardPrecedence {
        StandardPrecedence::Lowest
    }
}
