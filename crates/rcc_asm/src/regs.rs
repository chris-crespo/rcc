use crate::{Operand, RegisterOperand};

#[inline(always)]
pub fn ax() -> Operand {
    Operand::Register(RegisterOperand::Ax)
}

#[inline(always)]
pub fn dx() -> Operand {
    Operand::Register(RegisterOperand::Dx)
}

#[inline(always)]
pub fn r10() -> Operand {
    Operand::Register(RegisterOperand::R10)
}

#[inline(always)]
pub fn r11() -> Operand {
    Operand::Register(RegisterOperand::R11)
}
