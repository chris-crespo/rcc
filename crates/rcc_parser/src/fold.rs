use rcc_ast::{BinaryOperator, Expression, UnaryOperator};
use rcc_span::Span;

use crate::Parser;

fn shift_left(lhs: u64, rhs: u64) -> u64 {
    if rhs >= u32::BITS.into() {
        return 0;
    }

    lhs << rhs
}

fn shift_right(lhs: u64, rhs: u64) -> u64 {
    if rhs >= u32::BITS.into() {
        return if (rhs as i64) < 0 { -1i64 as u64 } else { 0 };
    }

    lhs >> rhs
}

impl<'a, 'src> Parser<'a, 'src> {
    pub(crate) fn fold_binary_expr(
        &self,
        span: Span,
        op: BinaryOperator,
        lhs: Expression<'src>,
        rhs: Expression<'src>,
    ) -> Expression<'src> {
        let (Some(lhs_lit), Some(rhs_lit)) = (lhs.try_as_number_lit(), rhs.try_as_number_lit())
        else {
            return self.ast.expr_binary(span, op, lhs, rhs);
        };

        #[rustfmt::skip]
        let value = match op {
            BinaryOperator::Add => lhs_lit.value.wrapping_add(rhs_lit.value),
            BinaryOperator::Substract => lhs_lit.value.wrapping_sub(rhs_lit.value),
            BinaryOperator::Multiply => lhs_lit.value.wrapping_mul(rhs_lit.value),
            BinaryOperator::Divide | BinaryOperator::Remainder if rhs_lit.value == 0 => {
                return self.ast.expr_binary(span, op, lhs, rhs)
            }
            BinaryOperator::Divide => ((lhs_lit.value as i64) / (rhs_lit.value as i64)) as u64,
            BinaryOperator::Remainder => ((lhs_lit.value as i64) % (rhs_lit.value as i64)) as u64,
            BinaryOperator::BitwiseAnd => lhs_lit.value & rhs_lit.value,
            BinaryOperator::BitwiseOr => lhs_lit.value | rhs_lit.value,
            BinaryOperator::BitwiseXor => lhs_lit.value ^ rhs_lit.value,
            BinaryOperator::LeftShift => shift_left(lhs_lit.value, rhs_lit.value),
            BinaryOperator::RightShift => shift_right(lhs_lit.value, rhs_lit.value),
            BinaryOperator::And => u64::from(lhs_lit.value != 0 && rhs_lit.value != 0),
            BinaryOperator::Or => u64::from(lhs_lit.value != 0 || rhs_lit.value != 0),
            BinaryOperator::Equal => u64::from(lhs_lit.value == rhs_lit.value),
            BinaryOperator::NotEqual => u64::from(lhs_lit.value != rhs_lit.value),
            BinaryOperator::LessThan => u64::from(lhs_lit.value < rhs_lit.value),
            BinaryOperator::LessThanEqual => u64::from(lhs_lit.value <= rhs_lit.value),
            BinaryOperator::GreaterThan => u64::from(lhs_lit.value > rhs_lit.value),
            BinaryOperator::GreaterThanEqual => u64::from(lhs_lit.value >= rhs_lit.value),
        };

        self.ast.expr_number_lit(span, value)
    }

    #[rustfmt::skip]
    pub(crate) fn fold_condition_expr(
        &self,
        span: Span,
        condition: Expression<'src>,
        consequent: Expression<'src>,
        alternate: Expression<'src>,
    ) -> Expression<'src> {
        match condition.try_as_number_lit() {
            Some(condition) => if condition.value == 0 { alternate } else { consequent },
            None => self.ast.expr_conditional(span, condition, consequent, alternate)
        }
    }

    pub(crate) fn fold_unary_expr(
        &self,
        span: Span,
        op: UnaryOperator,
        expr: Expression<'src>,
    ) -> Expression<'src> {
        let Some(expr) = expr.try_as_number_lit() else {
            return self.ast.expr_unary(span, op, expr);
        };

        let value = match op {
            UnaryOperator::Negation => 0u64.wrapping_sub(expr.value),
            UnaryOperator::Not => u64::from(expr.value == 0),
            UnaryOperator::BitwiseComplement => !expr.value,
        };

        self.ast.expr_number_lit(span, value)
    }
}
