use crate::Operand;

impl Operand {
    pub fn is_mem_addr(&self) -> bool {
        matches!(self, Operand::Stack(_))
    }
}
