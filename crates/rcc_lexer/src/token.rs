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
    Plus2,
    Minus,
    Minus2,
    Star,
    Slash,
    Percent,
    Tilde,

    Amp,
    Amp2,
    Bang,
    BangEq,
    Pipe,
    Pipe2,
    Caret,
    Eq,
    Eq2,
    Lt,
    Lt2,
    LtEq,
    Gt,
    Gt2,
    GtEq,

    Int,
    Return,
    Typedef,
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
            TokenKind::Plus2 => "++",
            TokenKind::Minus => "-",
            TokenKind::Minus2 => "--",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Tilde => "~",
            TokenKind::Amp => "&",
            TokenKind::Amp2 => "&&",
            TokenKind::Bang => "!",
            TokenKind::BangEq => "!=",
            TokenKind::Pipe => "|",
            TokenKind::Pipe2 => "||",
            TokenKind::Caret => "^",
            TokenKind::Eq => "=",
            TokenKind::Eq2 => "==",
            TokenKind::Lt => "<",
            TokenKind::Lt2 => "<<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::Gt2 => ">>",
            TokenKind::GtEq => ">=",
            TokenKind::Int => "int",
            TokenKind::Return => "return",
            TokenKind::Typedef => "typedef",
            TokenKind::Void => "void",
            TokenKind::Identifier => "identifier",
            TokenKind::Number => "number",
        }
    }

    pub fn is_assignment_op(self) -> bool {
        matches!(self, TokenKind::Eq)
    }

    pub fn is_binary_op(self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Amp
                | TokenKind::Amp2
                | TokenKind::BangEq
                | TokenKind::Pipe
                | TokenKind::Pipe2
                | TokenKind::Caret
                | TokenKind::Eq2
                | TokenKind::Lt
                | TokenKind::Lt2
                | TokenKind::LtEq
                | TokenKind::Gt
                | TokenKind::Gt2
                | TokenKind::GtEq
        )
    }
}
