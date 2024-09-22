use rcc_arena::Arena;
use rcc_ast::{
    AstBuilder, Expression, FunctionDeclaration, Identifier, NumberLiteral, Program, ReturnStatement, Statement
};
use rcc_interner::Interner;
use rcc_lexer::{Lexer, Token, TokenKind};
use rcc_span::Span;

mod diagnostics;

type Result<T> = std::result::Result<T, miette::Report>;

pub struct Parser<'a, 'src> {
    source: &'src str,
    lexer: Lexer<'src>,

    ast: AstBuilder<'src>,
    interner: &'a mut Interner<'src>,

    curr_token: Token,
    prev_token_end: u32,
}

impl<'a, 'src> Parser<'a, 'src> {
    pub fn new(
        source: &'src str,
        arena: &'src Arena,
        interner: &'a mut Interner<'src>,
    ) -> Parser<'a, 'src> {
        let mut parser = Parser {
            source,
            lexer: Lexer::new(source),

            ast: AstBuilder::new(arena),
            interner,

            curr_token: Token::default(),
            prev_token_end: 0,
        };

        parser.bump();
        parser
    }

    fn curr_kind(&self) -> TokenKind {
        self.curr_token.kind
    }

    fn curr_str(&self) -> &'src str {
        let span = self.curr_token.span;

        // Safety: spans comes from the lexer, which are guaranteed
        // to satisfy the `get_unchecked` conditions.
        unsafe { self.source.get_unchecked(span.start as _..span.end as _) }
    }

    fn start_span(&self) -> Span {
        Span::new(self.curr_token.span.start, 0)
    }

    fn end_span(&self, span: Span) -> Span {
        Span::new(span.start, self.prev_token_end)
    }

    fn bump(&mut self) {
        self.prev_token_end = self.curr_token.span.end;
        self.curr_token = self.lexer.next_token();
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            return true;
        }

        false
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.curr_kind() == kind
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if !self.at(kind) {
            return Err(self.expected(kind));
        }

        self.bump();

        Ok(())
    }

    fn expected(&self, kind: TokenKind) -> miette::Report {
        diagnostics::expected(
            kind.as_str(),
            self.curr_kind().as_str(),
            self.curr_token.span,
        )
    }

    pub fn parse(mut self) -> Result<Program<'src>> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program<'src>> {
        let span = self.start_span();
        let func = self.parse_function_declaration()?;

        let span = self.end_span(span);
        let program = Program { span, func };

        Ok(program)
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration<'src>> {
        let span = self.start_span();

        self.expect(TokenKind::Int)?;
        let name = self.parse_id()?;

        self.expect(TokenKind::LeftParen)?;
        self.expect(TokenKind::Void)?;
        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::LeftBrace)?;
        let stmt = self.parse_stmt()?;
        self.expect(TokenKind::RightBrace)?;

        let span = self.end_span(span);
        let decl = self.ast.decl_func(span, name, stmt);

        Ok(decl)
    }

    fn parse_stmt(&mut self) -> Result<Statement<'src>> {
        let stmt = self.parse_stmt_return()?;
        Ok(stmt)
    }

    fn parse_stmt_return(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::Return)?;

        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_return(span, expr);

        Ok(stmt)
    }

    fn parse_expr(&mut self) -> Result<Expression<'src>> {
        self.parse_lit_number()
    }

    fn parse_lit_number(&mut self) -> Result<Expression<'src>> {
        if !self.at(TokenKind::Number) {
            return Err(self.expected(TokenKind::Number));
        }

        let span = self.start_span();
        let str = self.curr_str();
        let value = str
            .parse::<u64>()
            .expect("lexer should not allow invalid numbers");

        self.bump();

        let span = self.end_span(span);
        let expr = self.ast.expr_number_lit(span, value);

        Ok(expr)
    }

    fn parse_id(&mut self) -> Result<Identifier> {
        if !self.at(TokenKind::Identifier) {
            return Err(self.expected(TokenKind::Identifier));
        }

        let span = self.start_span();
        let str = self.curr_str();
        let symbol = self.interner.intern(str);

        self.bump();

        let span = self.end_span(span);
        let id = Identifier { span, symbol };

        Ok(id)
    }
}
