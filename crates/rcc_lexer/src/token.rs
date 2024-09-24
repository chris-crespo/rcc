use rcc_span::Span;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    #[default]
    Eof,

    Undetermined,

    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Semicolon,

    Plus,
    Minus,
    Minus2,
    Star,
    Slash,
    Percent,
    Tilde,

    Int,
    Return,
    Void,

    Identifier,
    Number,
}

impl TokenKind {
    pub fn as_str(self) -> &'static str {
        match self {
            TokenKind::Eof => "eof",
            TokenKind::Undetermined => "undetermined",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::Semicolon => ";",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Minus2 => "--",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Tilde => "~",
            TokenKind::Int => "int",
            TokenKind::Return => "return",
            TokenKind::Void => "void",
            TokenKind::Identifier => "identifier",
            TokenKind::Number => "number",
        }
    }
}
