use rcc_arena::Arena;
use rcc_span::Span;

use crate::{
    AliasType, AssignmentExpression, AssignmentOperator, BinaryExpression, BinaryOperator, Block,
    BlockItem, ConditionalExpression, Declaration, EmptyStatement, Expression, ExpressionStatement,
    FunctionDeclaration, GotoStatement, Identifier, IfStatement, IntType, Label, LabeledStatement,
    Lvalue, NumberLiteral, Program, ReturnStatement, Statement, Type, TypedefDeclaration,
    UnaryExpression, UnaryOperator, UpdateExpression, UpdateOperator, VariableDeclaration,
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
        let decl_var = self.var_decl(span, ty, id, expr);
        Declaration::Variable(self.alloc(decl_var))
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

    pub fn stmt_empty(&self, span: Span) -> Statement<'a> {
        let empty_stmt = self.empty_stmt(span);
        Statement::Empty(self.alloc(empty_stmt))
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

    pub fn stmt_labeled(&self, span: Span, label: Label, stmt: Statement<'a>) -> Statement<'a> {
        let labeled_stmt = self.labeled_stmt(span, label, stmt);
        Statement::Labeled(self.alloc(labeled_stmt))
    }

    pub fn labeled_stmt(&self, span: Span, label: Label, stmt: Statement<'a>) -> LabeledStatement<'a> {
        LabeledStatement { span, label, stmt }
    }

    pub fn stmt_return(&self, span: Span, expr: Expression<'a>) -> Statement<'a> {
        let return_stmt = self.return_stmt(span, expr);
        Statement::Return(self.alloc(return_stmt))
    }

    pub fn return_stmt(&self, span: Span, expr: Expression<'a>) -> ReturnStatement<'a> {
        ReturnStatement { span, expr }
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
