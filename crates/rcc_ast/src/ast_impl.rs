use rcc_span::Span;

use crate::Expression;

impl<'a> Expression<'a> {
    pub fn span(&self) -> Span {
        match self {
            Expression::NumberLiteral(lit) => lit.span,
            Expression::Identifier(id) => id.span,
            Expression::Assignment(expr) => expr.span,
            Expression::Binary(expr) => expr.span,
            Expression::Unary(expr) => expr.span,
        }
    }
}
