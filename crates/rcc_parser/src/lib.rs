use rcc_ast::{
    AliasType, AssignmentOperator, AstBuilder, BinaryOperator, Block, BlockItem, Declaration,
    Expression, ForInit, FunctionDeclaration, Identifier, Label, Lvalue, Param, Program, Statement,
    TopLevelItem, Type, TypedefDeclaration, UnaryOperator, UpdateOperator, VariableDeclaration,
};
use rcc_context::GlobalContext;
use rcc_interner::Symbol;
use rcc_lexer::{assignment_tokens, Lexer, LexerCheckpoint, Token, TokenKind};
use rcc_semantics::{display_canonical_type, CanonicalType, TyBuilder};
use rcc_semantics::{ScopeId, ScopeTree, Semantics, SymbolFlags, SymbolId};
use rcc_span::Span;

mod diagnostics;
mod fold;
mod semantics;

fn map_assignment_operator(kind: TokenKind) -> AssignmentOperator {
    match kind {
        TokenKind::Eq => AssignmentOperator::Assign,
        TokenKind::PlusEq => AssignmentOperator::Add,
        TokenKind::MinusEq => AssignmentOperator::Substract,
        TokenKind::StarEq => AssignmentOperator::Multiply,
        TokenKind::SlashEq => AssignmentOperator::Divide,
        TokenKind::PercentEq => AssignmentOperator::Remainder,
        TokenKind::AmpEq => AssignmentOperator::BitwiseAnd,
        TokenKind::PipeEq => AssignmentOperator::BitwiseOr,
        TokenKind::CaretEq => AssignmentOperator::BitwiseXor,
        TokenKind::Lt2Eq => AssignmentOperator::LeftShift,
        TokenKind::Gt2Eq => AssignmentOperator::RightShift,
        _ => unreachable!("Assignment operator: {kind:?}"),
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

fn map_unary_operator(kind: TokenKind) -> UnaryOperator {
    match kind {
        TokenKind::Bang => UnaryOperator::Not,
        TokenKind::Minus => UnaryOperator::Negation,
        TokenKind::Tilde => UnaryOperator::BitwiseComplement,
        _ => unreachable!("Unary operator: {kind:?}"),
    }
}

fn map_update_operator(kind: TokenKind) -> UpdateOperator {
    match kind {
        TokenKind::Plus2 => UpdateOperator::Inc,
        TokenKind::Minus2 => UpdateOperator::Dec,
        _ => unreachable!("Update operator: {kind:?}"),
    }
}

fn map_lvalue(expr: &Expression) -> Option<Lvalue> {
    match expr {
        Expression::Var(lit) => Some(Lvalue::Identifier(lit.id)),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,
    Conditional,
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
    Prefix,
    Postfix,
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Question => Precedence::Conditional,
            assignment_tokens!() => Precedence::Assignment,
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
            TokenKind::Plus2 | TokenKind::Minus2 | TokenKind::LeftParen => Precedence::Postfix,
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

#[derive(Debug)]
struct DeclarationSpecifiers<'a> {
    span: Span,
    ty: Option<Type<'a>>,
    id: Identifier,
}

#[derive(Debug, PartialEq, Eq)]
enum DeclarationContext {
    TopLevelItem,
    BlockItem,
    Loop,
}

pub(crate) type Result<T> = std::result::Result<T, miette::Report>;

pub struct Parser<'a, 'src> {
    source: &'src str,
    lexer: Lexer<'src>,

    ast: AstBuilder<'src>,
    ty: TyBuilder<'src>,
    gcx: &'a mut GlobalContext<'src>,

    curr_token: Token,
    prev_token_end: u32,

    current_scope: ScopeId,
    semantics: Semantics<'src>,
}

impl<'a, 'src> Parser<'a, 'src> {
    pub fn new(gcx: &'a mut GlobalContext<'src>, source: &'src str) -> Parser<'a, 'src> {
        let mut parser = Parser {
            source,
            lexer: Lexer::new(source),

            ast: AstBuilder::new(gcx.arenas.ast),
            ty: TyBuilder::new(gcx.arenas.ty),
            gcx,

            curr_token: Token::default(),
            prev_token_end: 0,

            current_scope: ScopeTree::ROOT_SCOPE_ID,
            semantics: Semantics::default(),
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

    fn try_parse<T>(&mut self, f: impl FnOnce(&mut Parser<'a, 'src>) -> Result<T>) -> Option<T> {
        let checkpoint = self.checkpoint();
        let result = f(self);

        match result {
            Ok(it) => Some(it),
            Err(_) => {
                self.rewind(checkpoint);
                None
            }
        }
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

    #[inline]
    fn in_root_scope(&mut self) -> bool {
        self.current_scope == ScopeTree::ROOT_SCOPE_ID
    }

    fn scoped<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Parser<'a, 'src>) -> Result<T>,
    {
        let previous_scope = self.current_scope;
        self.current_scope = self.semantics.scopes.create_scope(Some(previous_scope));

        let result = f(self);
        self.current_scope = previous_scope;

        result
    }

    fn declare_local_symbol(
        &mut self,
        span: Span,
        symbol: Symbol,
        flags: SymbolFlags,
        ty: rcc_semantics::Type<'src>,
    ) -> Result<SymbolId> {
        self.declare_symbol_in_scope(span, symbol, self.current_scope, flags, ty)
    }

    fn declare_symbol_in_scope(
        &mut self,
        span: Span,
        symbol: Symbol,
        scope_id: ScopeId,
        flags: SymbolFlags,
        ty: rcc_semantics::Type<'src>,
    ) -> Result<SymbolId> {
        let Some(symbol_id) = self.semantics.scopes.get_binding(scope_id, symbol) else {
            let symbol_id = self
                .semantics
                .symbols
                .create_symbol(span, symbol, scope_id, flags, ty);
            self.semantics
                .scopes
                .add_binding(scope_id, symbol, symbol_id);

            return Ok(symbol_id);
        };

        let name = self.gcx.interner.get(symbol);
        let span2 = self.semantics.symbols.span(symbol_id);

        let intersection_flags = self.semantics.symbols.flags(symbol_id).intersection(flags);
        if intersection_flags
            .difference(SymbolFlags::Definition)
            .is_empty()
        {
            return Err(diagnostics::redefinition_as_different_kind_of_symbol(
                name, span, span2,
            ));
        }

        let expected_ty = self.semantics.symbols.ty(symbol_id);
        if ty.canonical != expected_ty.canonical {
            let ty1 = display_canonical_type(&ty.canonical);
            let ty2 = display_canonical_type(&expected_ty.canonical);

            return Err(diagnostics::redefinition_with_different_type(
                name, span, span2, &ty1, &ty2,
            ));
        }

        if intersection_flags.is_variable_declaration() && ScopeTree::ROOT_SCOPE_ID != scope_id {
            return Err(diagnostics::redefinition(name, span, span2));
        }

        if intersection_flags.is_parameter_declaration() {
            return Err(diagnostics::redefinition(name, span, span2));
        }

        if intersection_flags.is_definition() {
            self.semantics.symbols.set_span(symbol_id, span);
            return Err(diagnostics::redefinition(name, span, span2));
        }

        if flags.is_definition() {
            self.semantics.symbols.set_span(symbol_id, span);
            self.semantics.symbols.set_flags(symbol_id, flags);
        }

        Ok(symbol_id)
    }

    fn resolve_function_type(
        &mut self,
        ret: &Type<'src>,
        params: &[Param<'src>],
    ) -> Result<rcc_semantics::Type<'src>> {
        let ret = self.resolve_source_type(ret)?;
        let params = self.resolve_function_params(params)?;

        Ok(self.ty.func(ret.canonical, params))
    }

    fn resolve_function_params(
        &self,
        params: &[Param<'src>],
    ) -> Result<rcc_arena::Vec<'src, rcc_semantics::CanonicalType<'src>>> {
        let mut resolved_params = self.ty.vec();
        for param in params {
            let resolved_param = self.resolve_source_type(&param.ty)?;
            resolved_params.push(resolved_param.canonical)
        }

        Ok(resolved_params)
    }

    fn resolve_symbol_type(&self, span: Span, symbol: Symbol) -> Result<rcc_semantics::Type<'src>> {
        let Some(symbol_id) = self
            .semantics
            .scopes
            .find_binding(self.current_scope, symbol)
        else {
            let name = self.gcx.interner.get(symbol);
            return Err(diagnostics::undefined(name, span));
        };

        Ok(self.semantics.symbols.ty(symbol_id))
    }

    fn resolve_source_type(&self, ty: &Type<'src>) -> Result<rcc_semantics::Type<'src>> {
        match ty {
            Type::Void(_) => Ok(rcc_semantics::Type::void()),
            Type::Int(_) => Ok(rcc_semantics::Type::int()),
            Type::Alias(ty) => self.resolve_source_alias_type(ty),
        }
    }

    #[inline]
    fn resolve_source_alias_type(&self, ty: &AliasType) -> Result<rcc_semantics::Type<'src>> {
        self.resolve_type_alias(ty.id.span, ty.id.symbol)
    }

    fn resolve_type_alias(&self, span: Span, symbol: Symbol) -> Result<rcc_semantics::Type<'src>> {
        let Some(symbol_id) = self
            .semantics
            .scopes
            .find_binding(self.current_scope, symbol)
        else {
            let name = self.gcx.interner.get(symbol);
            return Err(diagnostics::unknown_type(name, span));
        };

        if !self.semantics.symbols.flags(symbol_id).is_typedef() {
            let name = self.gcx.interner.get(symbol);
            return Err(diagnostics::unknown_type(name, span));
        }

        Ok(self.semantics.symbols.ty(symbol_id))
    }

    pub fn parse(mut self) -> Result<Program<'src>> {
        self.semantics.scopes.create_scope(None);
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program<'src>> {
        let span = self.start_span();
        let body = self.parse_program_body()?;

        let span = self.end_span(span);
        let program = Program { span, body };

        Ok(program)
    }

    fn parse_program_body(&mut self) -> Result<rcc_arena::Vec<'src, TopLevelItem<'src>>> {
        let mut body = self.ast.vec();

        while !self.at(TokenKind::Eof) {
            let decl = self.parse_top_level_item()?;
            body.push(decl);
        }

        Ok(body)
    }

    fn parse_top_level_item(&mut self) -> Result<TopLevelItem<'src>> {
        if self.curr_kind() == TokenKind::Typedef {
            let item = self.parse_top_level_item_typedef()?;
            return Ok(item);
        }

        let decl_specs = self.parse_decl_specs()?;
        if self.at(TokenKind::LeftParen) {
            self.parse_top_level_item_func(decl_specs)
        } else {
            self.parse_top_level_item_var(decl_specs)
        }
    }

    fn parse_top_level_item_func(
        &mut self,
        specs: DeclarationSpecifiers<'src>,
    ) -> Result<TopLevelItem<'src>> {
        let func_decl = self.parse_func_decl(specs, DeclarationContext::TopLevelItem)?;
        let item = TopLevelItem::Function(func_decl);
        Ok(item)
    }

    fn parse_top_level_item_typedef(&mut self) -> Result<TopLevelItem<'src>> {
        let typedef_decl = self.parse_typedef_decl(DeclarationContext::TopLevelItem)?;
        let item = TopLevelItem::Typedef(typedef_decl);
        Ok(item)
    }

    fn parse_top_level_item_var(
        &mut self,
        specs: DeclarationSpecifiers<'src>,
    ) -> Result<TopLevelItem<'src>> {
        let var_decl = self.parse_var_decl(specs, DeclarationContext::TopLevelItem)?;
        let item = TopLevelItem::Variable(var_decl);
        Ok(item)
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
        if let Some(decl) = self.maybe_parse_decl(DeclarationContext::BlockItem) {
            let block_item = self.ast.block_item_decl(decl?);
            return Ok(block_item);
        }

        let stmt = self.parse_stmt()?;
        let block_item = self.ast.block_item_stmt(stmt);
        Ok(block_item)
    }

    fn maybe_parse_decl(&mut self, ctx: DeclarationContext) -> Option<Result<Declaration<'src>>> {
        if self.curr_kind() == TokenKind::Typedef {
            let decl = self.parse_decl_typedef(ctx);
            return Some(decl);
        }

        if self.lookahead(|p| p.parse_ty()).is_err() {
            return None;
        }

        let decl_specs = match self.parse_decl_specs() {
            Ok(decl_specs) => decl_specs,
            Err(err) => return Some(Err(err)),
        };

        let decl = if self.at(TokenKind::LeftParen) {
            self.parse_decl_func(decl_specs, ctx)
        } else {
            self.parse_decl_var(decl_specs, ctx)
        };

        Some(decl)
    }

    fn parse_decl_specs(&mut self) -> Result<DeclarationSpecifiers<'src>> {
        let span = self.start_span();
        let ty = self.try_parse(|p| p.parse_ty());
        let id = self.parse_id()?;

        let span = self.end_span(span);
        let specs = DeclarationSpecifiers { span, ty, id };

        Ok(specs)
    }

    fn parse_decl_func(
        &mut self,
        specs: DeclarationSpecifiers<'src>,
        ctx: DeclarationContext,
    ) -> Result<Declaration<'src>> {
        let func_decl = self.parse_func_decl(specs, ctx)?;
        let decl = Declaration::Function(func_decl);

        Ok(decl)
    }

    fn parse_func_decl(
        &mut self,
        specs: DeclarationSpecifiers<'src>,
        ctx: DeclarationContext,
    ) -> Result<&'src FunctionDeclaration<'src>> {
        let declaration_scope = self.current_scope;
        self.scoped(|p| {
            let params = p.parse_params()?;

            let Some(ty) = specs.ty else {
                return Err(diagnostics::missing_type(specs.id.span));
            };

            let resolved_ty = p.resolve_function_type(&ty, &params)?;
            let symbol_id = p.declare_symbol_in_scope(
                specs.id.span,
                specs.id.symbol,
                declaration_scope,
                SymbolFlags::Function,
                resolved_ty,
            )?;

            let body = if p.eat(TokenKind::Semicolon) {
                None
            } else if p.at(TokenKind::LeftBrace) {
                if params.first().is_some_and(|param| !param.ty.is_void()) {
                    for param in &params {
                        let Some(id) = param.id else {
                            let diagnostic = diagnostics::omitting_the_parameter_name_in_a_function_definition_is_not_allowed(param.span);
                            return Err(diagnostic)
                        };

                        let symbol_id = p
                            .semantics
                            .scopes
                            .get_binding(p.current_scope, id.symbol)
                            .expect("param should have been declared");
                        p.semantics.symbols.union_flags(symbol_id, SymbolFlags::Definition);
                    }
                }

                if declaration_scope != ScopeTree::ROOT_SCOPE_ID {
                    return Err(diagnostics::function_definition_not_allowed(
                        p.curr_token.span,
                    ));
                }

                p.semantics
                    .symbols
                    .union_flags(symbol_id, SymbolFlags::Definition);

                let block = p.parse_block()?;
                Some(block)
            } else {
                return Err(diagnostics::expected_at(
                    Span::empty(p.prev_token_end),
                    TokenKind::Semicolon.as_str(),
                    "end of declaration",
                ));
            };

            let span = p.end_span(specs.span);
            let decl = p.ast.alloc_func_decl(span, ty, specs.id, params, body);

            Ok(decl)
        })
    }

    fn parse_params(&mut self) -> Result<rcc_arena::Vec<'src, Param<'src>>> {
        self.expect(TokenKind::LeftParen)?;

        let mut params = self.ast.vec();
        if self.eat(TokenKind::RightParen) {
            return Ok(params);
        }

        loop {
            let param = self.parse_param()?;
            params.push(param);

            if !self.eat(TokenKind::Comma) {
                self.expect(TokenKind::RightParen)?;
                break;
            }
        }

        for param in params.iter().skip(1) {
            if param.ty.is_void() {
                let diagnostic = diagnostics::void_must_be_the_first_and_only_parameter(param.span);
                return Err(diagnostic);
            }
        }

        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Param<'src>> {
        let span = self.start_span();
        let ty = self.parse_ty()?;
        let id = if !self.at(TokenKind::Comma) && !self.at(TokenKind::RightParen) {
            let id = self.parse_id()?;
            Some(id)
        } else {
            None
        };

        if let Some(id) = id {
            if ty.is_void() {
                let diagnostic = diagnostics::parameter_may_not_have_void_type(id.span);
                return Err(diagnostic);
            }

            let resolved_ty = self.resolve_source_type(&ty)?;
            self.declare_local_symbol(
                id.span,
                id.symbol,
                SymbolFlags::ParameterDeclaration,
                resolved_ty,
            )?;
        }

        let span = self.end_span(span);
        let param = self.ast.param(span, ty, id);

        Ok(param)
    }

    fn parse_decl_typedef(&mut self, ctx: DeclarationContext) -> Result<Declaration<'src>> {
        let typedef_decl = self.parse_typedef_decl(ctx)?;
        let decl = Declaration::Typedef(typedef_decl);

        Ok(decl)
    }

    fn parse_typedef_decl(
        &mut self,
        ctx: DeclarationContext,
    ) -> Result<&'src TypedefDeclaration<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `typedef`

        let ty = self.parse_ty()?;
        let id = self.parse_id()?;

        let resolved_ty = self.resolve_source_type(&ty)?;
        self.declare_local_symbol(id.span, id.symbol, SymbolFlags::Typedef, resolved_ty)?;

        // Top or block level.
        if ctx != DeclarationContext::Loop {
            self.expect(TokenKind::Semicolon)?;
        }

        let span = self.end_span(span);
        let decl = self.ast.alloc_typedef_decl(span, ty, id);

        Ok(decl)
    }

    fn parse_decl_var(
        &mut self,
        specs: DeclarationSpecifiers<'src>,
        ctx: DeclarationContext,
    ) -> Result<Declaration<'src>> {
        let var_decl = self.parse_var_decl(specs, ctx)?;
        let decl = Declaration::Variable(var_decl);
        Ok(decl)
    }

    fn parse_var_decl(
        &mut self,
        specs: DeclarationSpecifiers<'src>,
        ctx: DeclarationContext,
    ) -> Result<&'src VariableDeclaration<'src>> {
        let Some(ty) = specs.ty else {
            return Err(diagnostics::missing_type(specs.id.span));
        };

        let resolved_ty = self.resolve_source_type(&ty)?;
        self.declare_local_symbol(
            specs.id.span,
            specs.id.symbol,
            if self.at(TokenKind::Eq) {
                SymbolFlags::VariableDefinition
            } else {
                SymbolFlags::VariableDeclaration
            },
            resolved_ty,
        )?;

        let expr = if self.eat(TokenKind::Eq) {
            let expr = self.parse_expr()?;
            Some(expr)
        } else {
            None
        };

        if ctx != DeclarationContext::Loop {
            self.expect(TokenKind::Semicolon)?;
        }

        let span = self.end_span(specs.span);
        let var_decl = self.ast.alloc_var_decl(span, ty, specs.id, expr);

        Ok(var_decl)
    }

    fn parse_stmt(&mut self) -> Result<Statement<'src>> {
        let kind = self.curr_kind();
        match kind {
            TokenKind::Break => self.parse_stmt_break(),
            TokenKind::Case => self.parse_stmt_labeled_case(),
            TokenKind::LeftBrace => self.parse_stmt_compound(),
            TokenKind::Continue => self.parse_stmt_continue(),
            TokenKind::Default => self.parse_stmt_labeled_default(),
            TokenKind::Do => self.parse_stmt_do(),
            TokenKind::For => self.parse_stmt_for(),
            TokenKind::Goto => self.parse_stmt_goto(),
            TokenKind::If => self.parse_stmt_if(),
            TokenKind::Semicolon => self.parse_stmt_empty(),
            TokenKind::Return => self.parse_stmt_return(),
            TokenKind::Switch => self.parse_stmt_switch(),
            TokenKind::While => self.parse_stmt_while(),
            _ => self
                .try_parse(|p| p.parse_stmt_labeled_id_start())
                .map(|label| self.parse_stmt_labeled_id_rest(label))
                .map_or_else(|| self.parse_stmt_expr(), std::convert::identity),
        }
    }

    fn parse_stmt_break(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `break`.
        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_break(span);

        Ok(stmt)
    }

    fn parse_stmt_compound(&mut self) -> Result<Statement<'src>> {
        let block = self.parse_block()?;
        let stmt = self.ast.stmt_compound(block);

        Ok(stmt)
    }

    fn parse_stmt_continue(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `continue`.
        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_continue(span);

        Ok(stmt)
    }

    fn parse_stmt_do(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `do`.

        let body = self.parse_stmt()?;
        self.expect(TokenKind::While)?;
        self.expect(TokenKind::LeftParen)?;

        let condition = self.parse_expr()?;
        self.expect(TokenKind::RightParen)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_do(span, body, condition);

        Ok(stmt)
    }

    fn parse_stmt_empty(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_empty(span);

        Ok(stmt)
    }

    fn parse_stmt_for(&mut self) -> Result<Statement<'src>> {
        self.scoped(|p| {
            let span = p.start_span();
            p.bump(); // Skip `for`.
            p.expect(TokenKind::LeftParen)?;

            let init = if p.eat(TokenKind::Semicolon) {
                None
            } else {
                let init = p.parse_for_init()?;
                p.expect(TokenKind::Semicolon)?;

                Some(init)
            };

            let condition = if p.eat(TokenKind::Semicolon) {
                None
            } else {
                let condition = p.parse_expr()?;
                p.expect(TokenKind::Semicolon)?;

                Some(condition)
            };

            let post = if p.eat(TokenKind::RightParen) {
                None
            } else {
                let post = p.parse_expr()?;
                p.expect(TokenKind::RightParen)?;

                Some(post)
            };

            let body = p.parse_stmt()?;

            let span = p.end_span(span);
            let stmt = p.ast.stmt_for(span, init, condition, post, body);

            Ok(stmt)
        })
    }

    fn parse_for_init(&mut self) -> Result<ForInit<'src>> {
        let Some(decl) = self.maybe_parse_decl(DeclarationContext::Loop) else {
            let expr = self.parse_expr()?;
            let init = self.ast.for_init_expr(expr);
            return Ok(init);
        };

        match decl? {
            Declaration::Variable(var_decl) => {
                let init = self.ast.for_init_decl(var_decl);
                Ok(init)
            }
            decl => Err(diagnostics::non_variable_declaration_in_for_loop(
                decl.span(),
            )),
        }
    }

    fn parse_stmt_goto(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::Goto)?;

        let label = self.parse_label()?;
        self.expect(TokenKind::Semicolon)?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_goto(span, label);

        Ok(stmt)
    }

    fn parse_stmt_if(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LeftParen)?;

        let condition = self.parse_expr()?;
        self.expect(TokenKind::RightParen)?;

        let consequent = self.parse_stmt()?;
        let alternate = if self.eat(TokenKind::Else) {
            let stmt = self.parse_stmt()?;
            Some(stmt)
        } else {
            None
        };

        let span = self.end_span(span);
        let stmt = self.ast.stmt_if(span, condition, consequent, alternate);

        Ok(stmt)
    }

    fn parse_stmt_labeled_case(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `case`

        let expr = self.parse_expr()?;
        let constant = expr
            .try_as_number_lit()
            .map(Ok)
            .unwrap_or_else(|| Err(diagnostics::unfoldable_case_label(expr.span())))?;
        self.expect(TokenKind::Colon)?;

        let stmt = self.parse_stmt()?;

        let span = self.end_span(span);
        let labeled_stmt = self.ast.labeled_stmt_case(span, constant, stmt);
        let stmt = self.ast.stmt_labeled(labeled_stmt);

        Ok(stmt)
    }

    fn parse_stmt_labeled_default(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `default`
        self.expect(TokenKind::Colon)?;

        let stmt = self.parse_stmt()?;

        let span = self.end_span(span);
        let labeled_stmt = self.ast.labeled_stmt_default(span, stmt);
        let stmt = self.ast.stmt_labeled(labeled_stmt);

        Ok(stmt)
    }

    fn parse_stmt_labeled_id_start(&mut self) -> Result<Label> {
        let label = self.parse_label()?;
        self.expect(TokenKind::Colon)?;

        Ok(label)
    }

    fn parse_stmt_labeled_id_rest(&mut self, label: Label) -> Result<Statement<'src>> {
        let stmt = self.parse_stmt()?;

        let span = self.end_span(label.span);
        let labeled_stmt = self.ast.labeled_stmt_id(span, label, stmt);
        let stmt = self.ast.stmt_labeled(labeled_stmt);

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

    fn parse_stmt_switch(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.expect(TokenKind::Switch)?;
        self.expect(TokenKind::LeftParen)?;

        let expr = self.parse_expr()?;
        self.expect(TokenKind::RightParen)?;

        let body = self.parse_stmt()?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_switch(span, expr, body);

        Ok(stmt)
    }

    fn parse_stmt_while(&mut self) -> Result<Statement<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `while`.
        self.expect(TokenKind::LeftParen)?;

        let condition = self.parse_expr()?;
        self.expect(TokenKind::RightParen)?;

        let body = self.parse_stmt()?;

        let span = self.end_span(span);
        let stmt = self.ast.stmt_while(span, condition, body);

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
            TokenKind::Plus2 | TokenKind::Minus2 => self.parse_expr_update_prefix(),
            TokenKind::LeftParen => self.parse_expr_group(),
            TokenKind::Number => self.parse_expr_number_lit(),
            TokenKind::Identifier => self.parse_expr_var(),
            _ => Err(self.unexpected()),
        }
    }

    fn parse_expr_infix(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        match self.curr_kind() {
            kind if kind.is_assignment_op() => self.parse_expr_assignment(lhs),
            kind if kind.is_binary_op() => self.parse_expr_binary(lhs),
            TokenKind::LeftParen => self.parse_expr_call(lhs),
            TokenKind::Question => self.parse_expr_conditional(lhs),
            TokenKind::Plus2 | TokenKind::Minus2 => self.parse_expr_update_postfix(lhs),
            _ => Err(self.unexpected()),
        }
    }

    fn parse_expr_assignment(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let kind = self.curr_kind();
        self.bump(); // Skip operator

        let op = map_assignment_operator(kind);
        let lvalue = map_lvalue(&lhs).ok_or_else(|| diagnostics::invalid_lvalue(lhs.span()))?;
        let expr = self.parse_expr()?;

        let span = self.end_span(lhs.span());
        let expr = self.ast.expr_assignment(span, op, lvalue, expr);

        Ok(expr)
    }

    fn parse_expr_binary(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let kind = self.curr_kind();
        self.bump(); // Skip operator.

        let op = map_binary_operator(kind);
        let precedence = Precedence::from(kind);
        let rhs = self.parse_expr_(precedence)?;

        let span = self.end_span(lhs.span());
        let expr = self.fold_binary_expr(span, op, lhs, rhs);

        Ok(expr)
    }

    fn parse_expr_call(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let id = match lhs {
            Expression::Var(lit) => lit.id,
            _ => return Err(diagnostics::non_function_call(lhs.span())),
        };

        let Some(symbol_id) = self
            .semantics
            .scopes
            .find_binding(self.current_scope, id.symbol)
        else {
            // So far, symbol resolution is done in `parse_expr_var`. Since we will always
            // need to resolve the symbol again to perform further semantic analysis
            // (e.g. check if the symbol is a function, typechecking, ...), we should
            // remove it from there and do it here.
            todo!()
        };

        let CanonicalType::Function(ty) = self.semantics.symbols.ty(symbol_id).canonical else {
            return Err(diagnostics::non_function_call(lhs.span()));
        };

        let args = self.parse_expr_call_args()?;
        if args.len() != ty.params.len() {
            return Err(diagnostics::invalid_number_of_arguments(
                id.span,
                ty.params.len(),
                args.len(),
            ));
        }

        let span = self.end_span(lhs.span());
        let expr = self.ast.expr_call(span, id, args);

        Ok(expr)
    }

    fn parse_expr_call_args(&mut self) -> Result<rcc_arena::Vec<'src, Expression<'src>>> {
        let mut args = self.ast.vec();
        self.bump(); // Skip `(`

        if self.eat(TokenKind::RightParen) {
            return Ok(args);
        }

        loop {
            let expr = self.parse_expr()?;
            args.push(expr);

            if !self.eat(TokenKind::Comma) {
                self.expect(TokenKind::RightParen)?;
                break;
            }
        }

        Ok(args)
    }

    fn parse_expr_conditional(&mut self, condition: Expression<'src>) -> Result<Expression<'src>> {
        self.bump(); // Skip `?`

        let consequent = self.parse_expr()?;
        self.expect(TokenKind::Colon)?;

        let alternate = self.parse_expr_(Precedence::Assignment)?;

        let span = self.end_span(condition.span());
        let expr = self.fold_condition_expr(span, condition, consequent, alternate);

        Ok(expr)
    }

    fn parse_expr_unary(&mut self) -> Result<Expression<'src>> {
        let span = self.start_span();
        let kind = self.curr_kind();

        self.bump(); // Skip operator.

        let op = map_unary_operator(kind);
        let expr = self.parse_expr_(Precedence::Prefix)?;

        let span = self.end_span(span);
        let expr = self.fold_unary_expr(span, op, expr);

        Ok(expr)
    }

    fn parse_expr_update_prefix(&mut self) -> Result<Expression<'src>> {
        let span = self.start_span();
        let kind = self.curr_kind();

        self.bump(); // Skip operator

        let op = map_update_operator(kind);
        let expr = self.parse_expr_(Precedence::Prefix)?;
        let lvalue = map_lvalue(&expr).ok_or_else(|| diagnostics::invalid_lvalue(expr.span()))?;

        let span = self.end_span(span);
        let expr = self.ast.expr_update(span, op, false, lvalue);

        Ok(expr)
    }

    fn parse_expr_update_postfix(&mut self, lhs: Expression<'src>) -> Result<Expression<'src>> {
        let kind = self.curr_kind();

        self.bump(); // Skip operator

        let op = map_update_operator(kind);
        let lvalue = map_lvalue(&lhs).ok_or_else(|| diagnostics::invalid_lvalue(lhs.span()))?;

        let span = self.end_span(lhs.span());
        let expr = self.ast.expr_update(span, op, true, lvalue);

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

    fn parse_expr_var(&mut self) -> Result<Expression<'src>> {
        let id = self.parse_id()?;
        let ty = self.resolve_symbol_type(id.span, id.symbol)?;

        let expr = self.ast.expr_var_lit(id, ty);
        Ok(expr)
    }

    fn parse_ty(&mut self) -> Result<Type<'src>> {
        match self.curr_kind() {
            TokenKind::Void => self.parse_ty_void(),
            TokenKind::Int => self.parse_ty_int(),
            _ => self.parse_ty_alias(),
        }
    }

    fn parse_ty_void(&mut self) -> Result<Type<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `void`

        let span = self.end_span(span);
        let ty = self.ast.ty_void(span);

        Ok(ty)
    }

    fn parse_ty_int(&mut self) -> Result<Type<'src>> {
        let span = self.start_span();
        self.bump(); // Skip `int`

        let span = self.end_span(span);
        let ty = self.ast.ty_int(span);

        Ok(ty)
    }

    fn parse_ty_alias(&mut self) -> Result<Type<'src>> {
        let id = self.parse_id()?;
        self.resolve_type_alias(id.span, id.symbol)?;

        let ty = self.ast.ty_alias(id);
        Ok(ty)
    }

    fn parse_id(&mut self) -> Result<Identifier> {
        if !self.at(TokenKind::Identifier) {
            return Err(self.expected(TokenKind::Identifier));
        }

        let span = self.start_span();
        let str = self.curr_str();
        let symbol = self.gcx.interner.intern(str);

        self.bump();

        let span = self.end_span(span);
        let id = Identifier { span, symbol };

        Ok(id)
    }

    fn parse_label(&mut self) -> Result<Label> {
        let id = self.parse_id()?;
        let label = Label {
            span: id.span,
            symbol: id.symbol,
        };

        Ok(label)
    }
}
