use miette::{LabeledSpan, SourceOffset, SourceSpan};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }

    pub fn length(self) -> u32 {
        self.end - self.start
    }

    pub fn label<S>(self, s: S) -> LabeledSpan
    where
        S: Into<String>,
    {
        LabeledSpan::new_with_span(Some(s.into()), self)
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(
            SourceOffset::from(value.start as usize),
            value.length() as usize,
        )
    }
}
