#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    None,
    Boolean(bool),
    String(String),
    Number(f64),
}
