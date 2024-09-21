use std::str::Chars;

use rcc_span::Span;

use crate::{Token, TokenKind};

pub struct Lexer<'src> {
    source: &'src str,
    chars: Chars<'src>,
    start: u32,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer {
        Lexer {
            source,
            chars: source.chars(),
            start: 0,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek_char2(&self) -> Option<char> {
        self.chars.clone().nth(1)
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn skip_while<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        while self.peek_char().is_some_and(&f) {
            self.next_char();
        }
    }

    fn skip_ambience(&mut self) {
        while let Some(c) = self.peek_char() {
            match c {
                '/' if self.peek_char2().is_some_and(|c| c == '/') => {
                    self.skip_while(|c| c != '\n')
                }
                c if c.is_whitespace() => {
                    self.next_char();
                }
                _ => break,
            }
        }
    }

    fn offset(&self) -> u32 {
        (self.source.len() - self.chars.as_str().len()) as _
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.offset())
    }

    fn source(&self) -> &'src str {
        &self.source[self.start as _..self.offset() as _]
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let span = self.span();
        Token { span, kind }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_ambience();
        self.start = self.offset();

        let Some(c) = self.next_char() else {
            return self.make_token(TokenKind::Eof);
        };

        match c {
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            ';' => self.make_token(TokenKind::Semicolon),
            c if is_start_of_number(c) => self.parse_number(),
            c if is_start_of_identifier(c) => self.parse_identifier(),
            _ => self.make_token(TokenKind::Undetermined),
        }
    }

    fn parse_number(&mut self) -> Token {
        self.skip_while(|c| c.is_ascii_digit());
        self.make_token(TokenKind::Number)
    }

    fn parse_identifier(&mut self) -> Token {
        self.parse_rest_of_identifier();
        self.match_keyword()
    }

    fn parse_rest_of_identifier(&mut self) {
        self.skip_while(|c| c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_');
    }

    fn match_keyword(&self) -> Token {
        match self.source() {
            "int" => self.make_token(TokenKind::Int),
            "return" => self.make_token(TokenKind::Return),
            "void" => self.make_token(TokenKind::Void),
            _ => self.make_token(TokenKind::Identifier),
        }
    }
}

fn is_start_of_number(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_start_of_identifier(c: char) -> bool {
    c.is_ascii_alphabetic()
}
