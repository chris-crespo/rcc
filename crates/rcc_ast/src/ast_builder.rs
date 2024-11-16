use rcc_arena::Arena;
use rcc_span::Span;

use crate::{
    AliasType, AssignmentExpression, AssignmentOperator, BinaryExpression, BinaryOperator, Block,
    BlockItem, BreakStatement, CaseLabeledStatement, ConditionalExpression, ContinueStatement,
    Declaration, DefaultLabeledStatement, DoStatement, EmptyStatement, Expression,
    ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, GotoStatement, Identifier,
    IdentifierLabeledStatement, IfStatement, IntType, Label, LabeledStatement, Lvalue,
    NumberLiteral, Program, ReturnStatement, Statement, SwitchStatement, Type, TypedefDeclaration,
    UnaryExpression, UnaryOperator, UpdateExpression, UpdateOperator, VariableDeclaration,
    WhileStatement,
};

pub struct AstBuilder<'a> {
    arena: &'a Arena,
}

impl<'a> AstBuilder<'a> {
    pub fn new(arena: &'a Arena) -> AstBuilder<'a> {
        AstBuilder { arena }
    }

    #[inline]
    fn alloc<T>(&self, value: T) -> &'a T {
        self.arena.alloc(value)
    }

    pub fn vec<T>(&self) -> rcc_arena::Vec<'a, T> {
        rcc_arena::Vec::new_in(self.arena)
    }

    pub fn program(&self, span: Span, func: FunctionDeclaration<'a>) -> Program<'a> {
        Program { span, func }
    }

    pub fn decl_func(
        &self,
        span: Span,
        name: Identifier,
        body: Block<'a>,
    ) -> FunctionDeclaration<'a> {
        FunctionDeclaration { span, name, body }
    }

    pub fn block(&self, span: Span, items: rcc_arena::Vec<'a, BlockItem<'a>>) -> Block<'a> {
        Block { span, items }
    }

    pub fn block_item_decl(&self, decl: Declaration<'a>) -> BlockItem<'a> {
        BlockItem::Declaration(self.alloc(decl))
    }

    pub fn block_item_stmt(&self, stmt: Statement<'a>) -> BlockItem<'a> {
        BlockItem::Statement(self.alloc(stmt))
    }

    pub fn decl_typedef(&self, span: Span, ty: Type<'a>, id: Identifier) -> Declaration<'a> {
        let typedef_decl = self.typedef_decl(span, ty, id);
        Declaration::Typedef(self.alloc(typedef_decl))
    }

    pub fn typedef_decl(&self, span: Span, ty: Type<'a>, id: Identifier) -> TypedefDeclaration<'a> {
        TypedefDeclaration { span, ty, id }
    }

    pub fn decl_var(
        &self,
        span: Span,
        ty: Type<'a>,
        id: Identifier,
        expr: Option<Expression<'a>>,
    ) -> Declaration<'a> {
        let var_decl = self.var_decl(span, ty, id, expr);
        Declaration::Variable(self.alloc(var_decl))
    }

    pub fn var_decl(
        &self,
        span: Span,
        ty: Type<'a>,
        id: Identifier,
        expr: Option<Expression<'a>>,
    ) -> VariableDeclaration<'a> {
        VariableDeclaration { span, ty, id, expr }
    }

    pub fn stmt_break(&self, span: Span) -> Statement<'a> {
        let break_stmt = self.break_stmt(span);
        Statement::Break(self.alloc(break_stmt))
    }

    pub fn break_stmt(&self, span: Span) -> BreakStatement {
        BreakStatement { span }
    }

    pub fn stmt_compound(&self, block: Block<'a>) -> Statement<'a> {
        Statement::Compound(self.alloc(block))
    }

    pub fn stmt_continue(&self, span: Span) -> Statement<'a> {
        let continue_stmt = self.continue_stmt(span);
        Statement::Continue(self.alloc(continue_stmt))
    }

    pub fn continue_stmt(&self, span: Span) -> ContinueStatement {
        ContinueStatement { span }
    }

    pub fn stmt_do(
        &self,
        span: Span,
        body: Statement<'a>,
        condition: Expression<'a>,
    ) -> Statement<'a> {
        let do_stmt = self.do_stmt(span, body, condition);
        Statement::Do(self.alloc(do_stmt))
    }

    pub fn do_stmt(
        &self,
        span: Span,
        body: Statement<'a>,
        condition: Expression<'a>,
    ) -> DoStatement<'a> {
        DoStatement {
            span,
            body,
            condition,
        }
    }

    pub fn stmt_empty(&self, span: Span) -> Statement<'a> {
        let empty_stmt = self.empty_stmt(span);
        Statement::Empty(self.alloc(empty_stmt))
    }

    pub fn stmt_for(
        &self,
        span: Span,
        init: Option<ForInit<'a>>,
        condition: Option<Expression<'a>>,
        post: Option<Expression<'a>>,
        body: Statement<'a>,
    ) -> Statement<'a> {
        let for_stmt = self.for_stmt(span, init, condition, post, body);
        Statement::For(self.alloc(for_stmt))
    }

    pub fn for_stmt(
        &self,
        span: Span,
        init: Option<ForInit<'a>>,
        condition: Option<Expression<'a>>,
        post: Option<Expression<'a>>,
        body: Statement<'a>,
    ) -> ForStatement<'a> {
        ForStatement {
            span,
            init,
            condition,
            post,
            body,
        }
    }

    pub fn for_init_decl(&self, decl: &'a VariableDeclaration) -> ForInit<'a> {
        ForInit::Declaration(decl)
    }

    pub fn for_init_expr(&self, stmt: Expression<'a>) -> ForInit<'a> {
        ForInit::Expression(self.alloc(stmt))
    }

    pub fn empty_stmt(&self, span: Span) -> EmptyStatement {
        EmptyStatement { span }
    }

    pub fn stmt_goto(&self, span: Span, label: Label) -> Statement<'a> {
        let goto_stmt = self.goto_stmt(span, label);
        Statement::Goto(self.alloc(goto_stmt))
    }

    pub fn goto_stmt(&self, span: Span, label: Label) -> GotoStatement {
        GotoStatement { span, label }
    }

    pub fn stmt_if(
        &self,
        span: Span,
        condition: Expression<'a>,
        consequent: Statement<'a>,
        alternate: Option<Statement<'a>>,
    ) -> Statement<'a> {
        let if_stmt = self.if_stmt(span, condition, consequent, alternate);
        Statement::If(self.alloc(if_stmt))
    }

    pub fn if_stmt(
        &self,
        span: Span,
        condition: Expression<'a>,
        consequent: Statement<'a>,
        alternate: Option<Statement<'a>>,
    ) -> IfStatement<'a> {
        IfStatement {
            span,
            condition,
            consequent,
            alternate,
        }
    }

    pub fn stmt_labeled(&self, stmt: LabeledStatement<'a>) -> Statement<'a> {
        Statement::Labeled(self.alloc(stmt))
    }

    pub fn labeled_stmt_case(
        &self,
        span: Span,
        constant: &'a NumberLiteral,
        stmt: Statement<'a>,
    ) -> LabeledStatement<'a> {
        let case_labeled_stmt = self.case_labeled_stmt(span, constant, stmt);
        LabeledStatement::Case(self.alloc(case_labeled_stmt))
    }

    pub fn case_labeled_stmt(
        &self,
        span: Span,
        constant: &'a NumberLiteral,
        stmt: Statement<'a>,
    ) -> CaseLabeledStatement<'a> {
        CaseLabeledStatement {
            span,
            constant,
            stmt,
        }
    }

    pub fn labeled_stmt_default(&self, span: Span, stmt: Statement<'a>) -> LabeledStatement<'a> {
        let default_labeled_stmt = self.default_labeled_stmt(span, stmt);
        LabeledStatement::Default(self.alloc(default_labeled_stmt))
    }

    pub fn default_labeled_stmt(
        &self,
        span: Span,
        stmt: Statement<'a>,
    ) -> DefaultLabeledStatement<'a> {
        DefaultLabeledStatement { span, stmt }
    }

    pub fn labeled_stmt_id(
        &self,
        span: Span,
        label: Label,
        stmt: Statement<'a>,
    ) -> LabeledStatement<'a> {
        let id_labeled_stmt = self.id_labeled_stmt(span, label, stmt);
        LabeledStatement::Identifier(self.alloc(id_labeled_stmt))
    }

    pub fn id_labeled_stmt(
        &self,
        span: Span,
        label: Label,
        stmt: Statement<'a>,
    ) -> IdentifierLabeledStatement<'a> {
        IdentifierLabeledStatement { span, label, stmt }
    }

    pub fn stmt_return(&self, span: Span, expr: Expression<'a>) -> Statement<'a> {
        let return_stmt = self.return_stmt(span, expr);
        Statement::Return(self.alloc(return_stmt))
    }

    pub fn return_stmt(&self, span: Span, expr: Expression<'a>) -> ReturnStatement<'a> {
        ReturnStatement { span, expr }
    }

    pub fn stmt_switch(
        &self,
        span: Span,
        expr: Expression<'a>,
        body: Statement<'a>,
    ) -> Statement<'a> {
        let switch_stmt = self.switch_stmt(span, expr, body);
        Statement::Switch(self.alloc(switch_stmt))
    }

    pub fn switch_stmt(
        &self,
        span: Span,
        expr: Expression<'a>,
        body: Statement<'a>,
    ) -> SwitchStatement<'a> {
        SwitchStatement { span, expr, body }
    }

    pub fn stmt_while(
        &self,
        span: Span,
        condition: Expression<'a>,
        body: Statement<'a>,
    ) -> Statement<'a> {
        let while_stmt = self.while_stmt(span, condition, body);
        Statement::While(self.alloc(while_stmt))
    }

    pub fn while_stmt(
        &self,
        span: Span,
        condition: Expression<'a>,
        body: Statement<'a>,
    ) -> WhileStatement<'a> {
        WhileStatement {
            span,
            condition,
            body,
        }
    }

    pub fn stmt_expr(&self, span: Span, expr: Expression<'a>) -> Statement<'a> {
        let expr_stmt = self.expr_stmt(span, expr);
        Statement::Expression(self.alloc(expr_stmt))
    }

    pub fn expr_stmt(&self, span: Span, expr: Expression<'a>) -> ExpressionStatement<'a> {
        ExpressionStatement { span, expr }
    }

    pub fn expr_assignment(
        &self,
        span: Span,
        op: AssignmentOperator,
        lvalue: Lvalue,
        expr: Expression<'a>,
    ) -> Expression<'a> {
        let assignment_expr = self.assignment_expr(span, op, lvalue, expr);
        Expression::Assignment(self.alloc(assignment_expr))
    }

    pub fn assignment_expr(
        &self,
        span: Span,
        op: AssignmentOperator,
        lvalue: Lvalue,
        expr: Expression<'a>,
    ) -> AssignmentExpression<'a> {
        AssignmentExpression {
            span,
            op,
            lvalue,
            expr,
        }
    }

    pub fn expr_binary(
        &self,
        span: Span,
        op: BinaryOperator,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
    ) -> Expression<'a> {
        let binary_expr = self.binary_expr(span, op, lhs, rhs);
        Expression::Binary(self.alloc(binary_expr))
    }

    pub fn binary_expr(
        &self,
        span: Span,
        op: BinaryOperator,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
    ) -> BinaryExpression<'a> {
        BinaryExpression { span, op, lhs, rhs }
    }

    pub fn expr_conditional(
        &self,
        span: Span,
        condition: Expression<'a>,
        consequent: Expression<'a>,
        alternate: Expression<'a>,
    ) -> Expression<'a> {
        let conditional_expr = self.conditional_expr(span, condition, consequent, alternate);
        Expression::Conditional(self.alloc(conditional_expr))
    }

    pub fn conditional_expr(
        &self,
        span: Span,
        condition: Expression<'a>,
        consequent: Expression<'a>,
        alternate: Expression<'a>,
    ) -> ConditionalExpression<'a> {
        ConditionalExpression {
            span,
            condition,
            consequent,
            alternate,
        }
    }

    pub fn expr_unary(
        &self,
        span: Span,
        op: UnaryOperator,
        expr: Expression<'a>,
    ) -> Expression<'a> {
        let unary_expr = self.unary_expr(span, op, expr);
        Expression::Unary(self.alloc(unary_expr))
    }

    fn unary_expr(
        &self,
        span: Span,
        op: UnaryOperator,
        expr: Expression<'a>,
    ) -> UnaryExpression<'a> {
        UnaryExpression { span, op, expr }
    }

    pub fn expr_update(
        &self,
        span: Span,
        op: UpdateOperator,
        postfix: bool,
        lvalue: Lvalue,
    ) -> Expression<'a> {
        let update_expr = self.update_expr(span, op, postfix, lvalue);
        Expression::Update(self.alloc(update_expr))
    }

    pub fn update_expr(
        &self,
        span: Span,
        op: UpdateOperator,
        postfix: bool,
        lvalue: Lvalue,
    ) -> UpdateExpression {
        UpdateExpression {
            span,
            op,
            postfix,
            lvalue,
        }
    }

    pub fn expr_number_lit(&self, span: Span, value: u64) -> Expression<'a> {
        let number_lit = self.number_lit(span, value);
        Expression::NumberLiteral(self.alloc(number_lit))
    }

    pub fn number_lit(&self, span: Span, value: u64) -> NumberLiteral {
        NumberLiteral { span, value }
    }

    pub fn expr_id(&self, id: Identifier) -> Expression<'a> {
        Expression::Identifier(self.alloc(id))
    }

    pub fn ty_int(&self, span: Span) -> Type<'a> {
        let int_ty = self.int_ty(span);
        Type::Int(self.alloc(int_ty))
    }

    pub fn int_ty(&self, span: Span) -> IntType {
        IntType { span }
    }

    pub fn ty_alias(&self, span: Span, id: Identifier) -> Type<'a> {
        let alias_ty = self.alias_ty(span, id);
        Type::Alias(self.alloc(alias_ty))
    }

    pub fn alias_ty(&self, span: Span, id: Identifier) -> AliasType {
        AliasType { span, id }
    }
}
