use std::fmt::Binary;

use rcc_arena::Arena;
use rcc_span::Span;

use crate::{
    BinaryExpression, BinaryOperator, Block, Expression, FunctionDeclaration, Identifier, NumberLiteral, Program, ReturnStatement, Statement, UnaryExpression, UnaryOperator
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

    pub fn stmt_return(&self, span: Span, expr: Expression<'a>) -> Statement<'a> {
        let return_stmt = self.return_stmt(span, expr);
        Statement::Return(self.alloc(return_stmt))
    }

    pub fn return_stmt(&self, span: Span, expr: Expression<'a>) -> ReturnStatement<'a> {
        ReturnStatement { span, expr }
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

    pub fn expr_number_lit(&self, span: Span, value: u64) -> Expression<'a> {
        let number_lit = self.number_lit(span, value);
        Expression::NumberLiteral(self.alloc(number_lit))
    }

    pub fn number_lit(&self, span: Span, value: u64) -> NumberLiteral {
        NumberLiteral { span, value }
    }
}
