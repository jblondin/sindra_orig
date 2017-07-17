

pub trait Lowest {
    fn lowest() -> Self;
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum StandardPrecedence {
    Lowest          = 1,
    Equals          = 2,
    LessGreater     = 3,
    Sum             = 4,
    Product         = 5,
    Power           = 6,
    Prefix          = 7,
    Call            = 8,
    Index           = 9,
}
impl Lowest for StandardPrecedence {
    fn lowest() -> StandardPrecedence {
        StandardPrecedence::Lowest
    }
}

pub trait Precedence {
    type OpPrecedence: Lowest;

    fn precedence(&self) -> Option<Self::OpPrecedence>;
}
