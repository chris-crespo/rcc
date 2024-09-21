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

    fn next_char(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn eat_char(&mut self, c: char) -> bool {
        if self.peek_char().is_some_and(|c2| c == c2) {
            self.next_char();
            return true;
        }

        false
    }

    fn skip_while<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        while self.peek_char().is_some_and(&f) {
            self.next_char();
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
        loop {
            self.start = self.offset();

            let Some(c) = self.next_char() else {
                return self.make_token(TokenKind::Eof);
            };

            return match c {
                ' ' | '\x09'..='\x0d' => continue,
                '{' => self.make_token(TokenKind::LeftBrace),
                '}' => self.make_token(TokenKind::RightBrace),
                '(' => self.make_token(TokenKind::LeftParen),
                ')' => self.make_token(TokenKind::RightParen),
                ';' => self.make_token(TokenKind::Semicolon),
                '/' => match self.peek_char() {
                    Some('/') => {
                        self.next_char();
                        self.skip_single_line_comment();
                        continue;
                    }
                    Some('*') => {
                        self.next_char();
                        self.skip_multiline_comment();
                        continue;
                    }
                    _ => self.make_token(TokenKind::Undetermined),
                },
                '0'..='9' => self.parse_number(),
                'a'..='z' | 'A'..='Z' => self.parse_identifier(),
                _ => self.make_token(TokenKind::Undetermined),
            };
        }
    }

    fn skip_single_line_comment(&mut self) {
        while self.next_char().is_some_and(|c| c != '\n') {}
    }

    fn skip_multiline_comment(&mut self) {
        loop {
            let Some(c) = self.next_char() else {
                break;
            };

            if c == '*' && self.eat_char('/') {
                break;
            }
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
