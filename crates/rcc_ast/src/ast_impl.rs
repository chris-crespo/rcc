use crate::BinaryOperator;

impl BinaryOperator {
    pub fn right_assoc(self) -> bool {
        matches!(self, BinaryOperator::Assign)
    }
}
