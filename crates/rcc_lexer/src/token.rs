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
    PlusEq,
    Minus,
    Minus2,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Percent,
    PercentEq,
    Tilde,

    Amp,
    Amp2,
    AmpEq,
    Bang,
    BangEq,
    Pipe,
    Pipe2,
    PipeEq,
    Caret,
    CaretEq,
    Eq,
    Eq2,
    Lt,
    Lt2,
    LtEq,
    Lt2Eq,
    Gt,
    Gt2,
    GtEq,
    Gt2Eq,

    Int,
    Return,
    Typedef,
    Void,

    Identifier,
    Number,
}

#[macro_export]
macro_rules! assignment_tokens {
    () => {
        TokenKind::Eq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq
            | TokenKind::PercentEq
            | TokenKind::AmpEq
            | TokenKind::PipeEq
            | TokenKind::CaretEq
            | TokenKind::Lt2Eq
            | TokenKind::Gt2Eq
    };
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
            TokenKind::PlusEq => "+=",
            TokenKind::Minus => "-",
            TokenKind::Minus2 => "--",
            TokenKind::MinusEq => "-=",
            TokenKind::Star => "*",
            TokenKind::StarEq => "*=",
            TokenKind::Slash => "/",
            TokenKind::SlashEq => "/=",
            TokenKind::Percent => "%",
            TokenKind::PercentEq => "%=",
            TokenKind::Tilde => "~",
            TokenKind::Amp => "&",
            TokenKind::AmpEq => "&=",
            TokenKind::Amp2 => "&&",
            TokenKind::Bang => "!",
            TokenKind::BangEq => "!=",
            TokenKind::Pipe => "|",
            TokenKind::Pipe2 => "||",
            TokenKind::PipeEq => "|=",
            TokenKind::Caret => "^",
            TokenKind::CaretEq => "^=",
            TokenKind::Eq => "=",
            TokenKind::Eq2 => "==",
            TokenKind::Lt => "<",
            TokenKind::Lt2 => "<<",
            TokenKind::LtEq => "<=",
            TokenKind::Lt2Eq => "<<=",
            TokenKind::Gt => ">",
            TokenKind::Gt2 => ">>",
            TokenKind::GtEq => ">=",
            TokenKind::Gt2Eq => ">>=",
            TokenKind::Int => "int",
            TokenKind::Return => "return",
            TokenKind::Typedef => "typedef",
            TokenKind::Void => "void",
            TokenKind::Identifier => "identifier",
            TokenKind::Number => "number",
        }
    }

    pub fn is_assignment_op(self) -> bool {
        matches!(self, assignment_tokens!())
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
