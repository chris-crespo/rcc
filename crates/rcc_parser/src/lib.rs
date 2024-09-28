use rcc_arena::Arena;
use rcc_ast::{
    AstBuilder, BinaryOperator, Expression, FunctionDeclaration, Identifier, Program, Statement,
    UnaryOperator,
};
use rcc_interner::Interner;
use rcc_lexer::{Lexer, Token, TokenKind};
use rcc_span::Span;

mod diagnostics;

fn map_unary_operator(kind: TokenKind) -> UnaryOperator {
    match kind {
        TokenKind::Minus => UnaryOperator::Negation,
        TokenKind::Tilde => UnaryOperator::BitwiseComplement,
        _ => unreachable!("Unary operator: {kind:?}"),
    }
}

fn map_binary_operator(kind: TokenKind) -> BinaryOperator {
    match kind {
        TokenKind::Plus => BinaryOperator::Add,
        TokenKind::Minus => BinaryOperator::Substract,
        TokenKind::Star => BinaryOperator::Multiply,
        TokenKind::Slash => BinaryOperator::Divide,
        TokenKind::Percent => BinaryOperator::Remainder,
        TokenKind::Amp => BinaryOperator::BitwiseAnd,
        TokenKind::Pipe => BinaryOperator::BitwiseOr,
        TokenKind::Caret => BinaryOperator::BitwiseXor,
        TokenKind::Lt2 => BinaryOperator::LeftShift,
        TokenKind::Gt2 => BinaryOperator::RightShift,
        _ => unreachable!("Binary operatoor: {kind:?}"),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Shift,
    Term,
    Factor,
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Lt2 | TokenKind::Gt2 => Precedence::Shift,
            TokenKind::Caret => Precedence::BitwiseXor,
            TokenKind::Pipe => Precedence::BitwiseOr,
            TokenKind::Amp => Precedence::BitwiseAnd,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            _ => Precedence::None,
        }
    }
}

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

    fn curr_prec(&self) -> Precedence {
        Precedence::from(self.curr_kind())
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
            self.curr_token.span,
            kind.as_str(),
            self.curr_kind().as_str(),
        )
    }

    fn unexpected(&self) -> miette::Report {
        diagnostics::unexpected(self.curr_token.span)
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

    #[inline(always)]
    fn parse_expr(&mut self) -> Result<Expression<'src>> {
        self.parse_expr_(Precedence::None)
    }

    fn parse_expr_(&mut self, prec: Precedence) -> Result<Expression<'src>> {
        let mut lhs = self.parse_expr_lhs()?;

        while prec < self.curr_prec() {
            lhs = self.parse_expr_infix(lhs)?;
        }

        Ok(lhs)
    }

    fn parse_expr_lhs(&mut self) -> Result<Expression<'src>> {
        match self.curr_kind() {
            TokenKind::Minus => self.parse_expr_unary(),
            TokenKind::Tilde => self.parse_expr_unary(),
            TokenKind::LeftParen => self.parse_expr_group(),
            TokenKind::Number => self.parse_expr_number_lit(),
            _ => Err(self.unexpected()),
        }
    }

    fn parse_expr_infix(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let span = self.start_span();
        let kind = self.curr_kind();

        self.bump(); // Skip operator.

        let op = map_binary_operator(kind);
        let rhs = self.parse_expr_(Precedence::from(kind))?;

        let span = self.end_span(span);
        let expr = self.ast.expr_binary(span, op, lhs, rhs);
        Ok(expr)
    }

    fn parse_expr_unary(&mut self) -> Result<Expression<'src>> {
        let span = self.start_span();
        let kind = self.curr_kind();

        self.bump(); // Skip operator.

        let op = map_unary_operator(kind);
        let expr = self.parse_expr()?;

        let span = self.end_span(span);
        let expr = self.ast.expr_unary(span, op, expr);
        Ok(expr)
    }

    fn parse_expr_group(&mut self) -> Result<Expression<'src>> {
        self.bump(); // Skip left paren

        let expr = self.parse_expr()?;
        self.expect(TokenKind::RightParen)?;

        Ok(expr)
    }

    fn parse_expr_number_lit(&mut self) -> Result<Expression<'src>> {
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
