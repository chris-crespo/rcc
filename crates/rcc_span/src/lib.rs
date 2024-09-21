#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }
}

