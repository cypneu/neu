#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'src> {
    None,
    Boolean(bool),
    String(&'src str),
    Number(f64),
}
