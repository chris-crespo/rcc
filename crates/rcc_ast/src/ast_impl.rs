use rcc_span::Span;

use crate::{Declaration, Expression};

impl<'a> Declaration<'a> {
    pub fn span(&self) -> Span {
        match self {
            Declaration::Typedef(decl) => decl.span,
            Declaration::Variable(decl) => decl.span,
        }
    }
}

impl<'a> Expression<'a> {
    pub fn span(&self) -> Span {
        match self {
            Expression::NumberLiteral(lit) => lit.span,
            Expression::Identifier(id) => id.span,
            Expression::Assignment(expr) => expr.span,
            Expression::Binary(expr) => expr.span,
            Expression::Conditional(expr) => expr.span,
            Expression::Unary(expr) => expr.span,
            Expression::Update(expr) => expr.span
        }
    }
}
