use rcc_span::Span;

use crate::{
    Declaration, Expression, NumberLiteral, Param, Type, VariableDeclaration
};

impl<'a> Declaration<'a> {
    pub fn span(&self) -> Span {
        match self {
            Declaration::Function(decl) => decl.span,
            Declaration::Typedef(decl) => decl.span,
            Declaration::Variable(decl) => decl.span,
        }
    }
}

impl<'a> VariableDeclaration<'a> {
    pub fn is_definition(&self) -> bool {
        self.expr.is_some()
    }
}

impl<'a> Expression<'a> {
    pub fn span(&self) -> Span {
        match self {
            Expression::NumberLiteral(lit) => lit.span,
            Expression::Var(lit) => lit.id.span,
            Expression::Assignment(expr) => expr.span,
            Expression::Binary(expr) => expr.span,
            Expression::Call(expr) => expr.span,
            Expression::Conditional(expr) => expr.span,
            Expression::Unary(expr) => expr.span,
            Expression::Update(expr) => expr.span,
        }
    }

    pub fn try_as_number_lit(&self) -> Option<&'a NumberLiteral> {
        match self {
            Expression::NumberLiteral(lit) => Some(lit),
            _ => None,
        }
    }
}

impl<'a> Type<'a> {
    pub fn span(&self) -> Span {
        match self {
            Type::Void(ty) => ty.span,
            Type::Int(ty) => ty.span,
            Type::Alias(ty) => ty.id.span,
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void(_))
    }
}
