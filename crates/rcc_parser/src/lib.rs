use std::collections::HashMap;

use rcc_arena::Arena;
use rcc_ast::{
    AstBuilder, BinaryOperator, Block, BlockItem, Declaration, Expression, FunctionDeclaration,
    Identifier, Lvalue, Program, Statement, Type, UnaryOperator,
};
use rcc_interner::{Interner, Symbol};
use rcc_lexer::{Lexer, LexerCheckpoint, Token, TokenKind};
use rcc_span::Span;

mod diagnostics;

fn map_unary_operator(kind: TokenKind) -> UnaryOperator {
    match kind {
        TokenKind::Bang => UnaryOperator::Not,
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
        TokenKind::Amp2 => BinaryOperator::And,
        TokenKind::BangEq => BinaryOperator::NotEqual,
        TokenKind::Eq => BinaryOperator::Assign,
        TokenKind::Eq2 => BinaryOperator::Equal,
        TokenKind::Pipe => BinaryOperator::BitwiseOr,
        TokenKind::Pipe2 => BinaryOperator::Or,
        TokenKind::Caret => BinaryOperator::BitwiseXor,
        TokenKind::Lt => BinaryOperator::LessThan,
        TokenKind::Lt2 => BinaryOperator::LeftShift,
        TokenKind::LtEq => BinaryOperator::LessThanEqual,
        TokenKind::Gt => BinaryOperator::GreaterThan,
        TokenKind::Gt2 => BinaryOperator::RightShift,
        TokenKind::GtEq => BinaryOperator::GreaterThanEqual,
        _ => unreachable!("Binary operator: {kind:?}"),
    }
}

fn map_lvalue(expr: &Expression) -> Option<Lvalue> {
    match expr {
        Expression::Identifier(&id) => Some(Lvalue::Identifier(id)),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equality,
    Comparison,
    Shift,
    Term,
    Factor,
}

impl Precedence {
    fn prev(self) -> Precedence {
        match self {
            Precedence::None | Precedence::Assignment => Precedence::None,
            Precedence::LogicalOr => Precedence::Assignment,
            Precedence::LogicalAnd => Precedence::LogicalOr,
            Precedence::BitwiseOr => Precedence::LogicalAnd,
            Precedence::BitwiseXor => Precedence::BitwiseOr,
            Precedence::BitwiseAnd => Precedence::BitwiseXor,
            Precedence::Equality => Precedence::BitwiseAnd,
            Precedence::Comparison => Precedence::Equality,
            Precedence::Shift => Precedence::Comparison,
            Precedence::Term => Precedence::Shift,
            Precedence::Factor => Precedence::Term,
        }
    }
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Eq => Precedence::Assignment,
            TokenKind::Pipe2 => Precedence::LogicalOr,
            TokenKind::Amp2 => Precedence::LogicalAnd,
            TokenKind::Pipe => Precedence::BitwiseOr,
            TokenKind::Caret => Precedence::BitwiseXor,
            TokenKind::Amp => Precedence::BitwiseAnd,
            TokenKind::Eq2 | TokenKind::BangEq => Precedence::Equality,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => {
                Precedence::Comparison
            }
            TokenKind::Lt2 | TokenKind::Gt2 => Precedence::Shift,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            _ => Precedence::None,
        }
    }
}

#[derive(Debug)]
struct ParserCheckpoint<'a> {
    lexer: LexerCheckpoint<'a>,
    curr_token: Token,
    prev_token_end: u32,
}

#[derive(Debug, Default)]
struct Scope {
    functions: HashMap<Symbol, Span>,
    typedefs: HashMap<Symbol, Span>,
    variables: HashMap<Symbol, Span>,
}

type Result<T> = std::result::Result<T, miette::Report>;

pub struct Parser<'a, 'src> {
    source: &'src str,
    lexer: Lexer<'src>,

    ast: AstBuilder<'src>,
    interner: &'a mut Interner<'src>,

    curr_token: Token,
    prev_token_end: u32,

    scopes: Vec<Scope>,
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

            scopes: Vec::new(),
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

    fn checkpoint(&self) -> ParserCheckpoint<'src> {
        ParserCheckpoint {
            lexer: self.lexer.checkpoint(),
            curr_token: self.curr_token,
            prev_token_end: self.prev_token_end,
        }
    }

    fn rewind(&mut self, checkpoint: ParserCheckpoint<'src>) {
        self.lexer.rewind(checkpoint.lexer);
        self.curr_token = checkpoint.curr_token;
        self.prev_token_end = checkpoint.prev_token_end;
    }

    fn lookahead<T>(&mut self, f: impl FnOnce(&mut Parser<'a, 'src>) -> T) -> T {
        let checkpoint = self.checkpoint();
        let result = f(self);
        self.rewind(checkpoint);
        result
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

    fn start_scope(&mut self) {
        let scope = Scope::default();
        self.scopes.push(scope)
    }

    fn end_scope(&mut self) {
        self.scopes.pop().expect("scopes should never be empty");
    }

    fn scoped<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Parser<'a, 'src>) -> Result<T>,
    {
        self.start_scope();

        let result = f(self);
        self.end_scope();

        result
    }

    fn curr_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("scopes should never be empty")
    }

    fn declare_function(&mut self, id: &Identifier) -> Result<()> {
        let curr_scope = self.curr_scope_mut();
        if let Some(&span) = curr_scope.functions.get(&id.symbol) {
            let source_id = self.interner.get(id.symbol);
            return Err(diagnostics::redefined(source_id, span, id.span));
        }

        curr_scope.functions.insert(id.symbol, id.span);

        Ok(())
    }

    fn declare_typedef(&mut self, id: &Identifier) {
        self.curr_scope_mut().typedefs.insert(id.symbol, id.span);
    }

    fn declare_variable(&mut self, id: &Identifier) -> Result<()> {
        let curr_scope = self.curr_scope_mut();
        if let Some(&span) = curr_scope.variables.get(&id.symbol) {
            let source_id = self.interner.get(id.symbol);
            return Err(diagnostics::redefined(source_id, span, id.span));
        }

        curr_scope.variables.insert(id.symbol, id.span);

        Ok(())
    }

    fn lookup_typedef(&self, id: &Identifier) -> Result<()> {
        let variable = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.typedefs.contains_key(&id.symbol));

        if variable.is_some() {
            return Ok(());
        }

        let source_id = self.interner.get(id.symbol);
        Err(diagnostics::undefined(source_id, id.span))
    }

    fn lookup_variable(&self, id: &Identifier) -> Result<()> {
        let variable = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.variables.contains_key(&id.symbol));

        if variable.is_some() {
            return Ok(());
        }

        let source_id = self.interner.get(id.symbol);
        Err(diagnostics::undefined(source_id, id.span))
    }

    pub fn parse(mut self) -> Result<Program<'src>> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program<'src>> {
        self.scoped(|p| {
            let span = p.start_span();
            let func = p.parse_function_declaration()?;

            let span = p.end_span(span);
            let program = Program { span, func };

            Ok(program)
        })
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::Int)?;

        let name = self.parse_id()?;
        self.declare_function(&name)?;

        self.scoped(|p| {
            p.expect(TokenKind::LeftParen)?;
            p.expect(TokenKind::Void)?;
            p.expect(TokenKind::RightParen)?;

            let body = p.parse_block_impl()?;

            let span = p.end_span(span);
            let decl = p.ast.decl_func(span, name, body);

            Ok(decl)
        })
    }

    fn parse_block(&mut self) -> Result<Block<'src>> {
        self.scoped(|p| p.parse_block_impl())
    }

    fn parse_block_impl(&mut self) -> Result<Block<'src>> {
        let mut items = self.ast.vec();
        let span = self.start_span();

        self.expect(TokenKind::LeftBrace)?;

        while !self.eat(TokenKind::RightBrace) {
            let item = self.parse_block_item()?;
            items.push(item);
        }

        let span = self.end_span(span);
        let block = self.ast.block(span, items);

        Ok(block)
    }

    fn parse_block_item(&mut self) -> Result<BlockItem<'src>> {
        if self.curr_kind() == TokenKind::Typedef {
            let decl = self.parse_decl_typedef()?;
            let block_item = self.ast.block_item_decl(decl);
            return Ok(block_item);
        }

        match self.lookahead(|p| p.parse_ty()) {
            Ok(_) => {
                let decl = self.parse_decl_var()?;
                let block_item = self.ast.block_item_decl(decl);
                Ok(block_item)
            }
            Err(_) => {
                let stmt = self.parse_stmt()?;
                let block_item = self.ast.block_item_stmt(stmt);
                Ok(block_item)
            }
        }
    }

    fn parse_decl_typedef(&mut self) -> Result<Declaration<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `typedef`

        let ty = self.parse_ty()?;
        let id = self.parse_id()?;
        self.declare_typedef(&id);

        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let decl = self.ast.decl_typedef(span, ty, id);

        Ok(decl)
    }

    fn parse_decl_var(&mut self) -> Result<Declaration<'src>> {
        let span = self.start_span();
        let ty = self.parse_ty()?;
        let id = self.parse_id()?;
        self.declare_variable(&id)?;

        let expr = if self.eat(TokenKind::Eq) {
            let expr = self.parse_expr()?;
            Some(expr)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let decl = self.ast.decl_var(span, ty, id, expr);

        Ok(decl)
    }

    fn parse_stmt(&mut self) -> Result<Statement<'src>> {
        let kind = self.curr_kind();
        match kind {
            TokenKind::Semicolon => self.parse_stmt_empty(),
            TokenKind::Return => self.parse_stmt_return(),
            _ => self.parse_stmt_expr(),
        }
    }

    fn parse_stmt_empty(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_empty(span);

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

    fn parse_stmt_expr(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        let expr = self.parse_expr()?;

        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_expr(span, expr);

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
            TokenKind::Bang | TokenKind::Minus | TokenKind::Tilde => self.parse_expr_unary(),
            TokenKind::LeftParen => self.parse_expr_group(),
            TokenKind::Number => self.parse_expr_number_lit(),
            TokenKind::Identifier => self.parse_expr_id(),
            _ => Err(self.unexpected()),
        }
    }

    fn parse_expr_infix(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        match self.curr_kind() {
            kind if kind.is_assignment_op() => self.parse_expr_assignment(lhs),
            kind if kind.is_binary_op() => self.parse_expr_binary(lhs),
            _ => Err(self.unexpected()),
        }
    }

    fn parse_expr_assignment(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `=`

        let lvalue = map_lvalue(&lhs).ok_or_else(|| diagnostics::invalid_lvalue(lhs.span()))?;
        let expr = self.parse_expr()?;

        Ok(self.ast.expr_assignment(span, lvalue, expr))
    }

    fn parse_expr_binary(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let span = self.start_span();
        let kind = self.curr_kind();

        self.bump(); // Skip operator.

        let op = map_binary_operator(kind);
        let precedence = Precedence::from(kind);
        let rhs = self.parse_expr_(precedence)?;

        let span = self.end_span(span);
        let expr = self.ast.expr_binary(span, op, lhs, rhs);
        Ok(expr)
    }

    fn parse_expr_unary(&mut self) -> Result<Expression<'src>> {
        let span = self.start_span();
        let kind = self.curr_kind();

        self.bump(); // Skip operator.

        let op = map_unary_operator(kind);
        let expr = self.parse_expr_lhs()?;

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

    fn parse_expr_id(&mut self) -> Result<Expression<'src>> {
        let id = self.parse_id()?;
        self.lookup_variable(&id)?;

        let expr = self.ast.expr_id(id);
        Ok(expr)
    }

    fn parse_ty(&mut self) -> Result<Type<'src>> {
        match self.curr_kind() {
            TokenKind::Int => self.parse_ty_int(),
            _ => self.parse_ty_alias(),
        }
    }

    fn parse_ty_int(&mut self) -> Result<Type<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `int`

        let span = self.end_span(span);
        let ty = self.ast.ty_int(span);

        Ok(ty)
    }

    fn parse_ty_alias(&mut self) -> Result<Type<'src>> {
        let span = self.start_span();
        let id = self.parse_id()?;
        self.lookup_typedef(&id)?;

        let span = self.end_span(span);
        let ty = self.ast.ty_alias(span, id);

        Ok(ty)
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
