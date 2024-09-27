use rcc_interner::Symbol;

mod asm_impl;
pub mod regs;

#[derive(Debug)]
pub struct Program {
    pub func: FunctionDeclaration
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Label,
    pub stack_size: u32,
    pub instructions: Vec<Instruction>
}

#[derive(Debug)]
pub enum Instruction {
    Mov(MovInstruction),
    Neg(NegInstruction),
    Not(NotInstruction),
    Add(AddInstruction),
    Sub(SubInstruction),
    Mul(MulInstruction),
    Idiv(IdivInstruction),
    And(AndInstruction),
    Or(OrInstruction),
    Xor(XorInstruction),
    Shl(ShlInstruction),
    Shr(ShrInstruction),
    Cdq,
    Ret
}

#[derive(Debug)]
pub struct MovInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct NegInstruction {
    pub dest: Operand
}

#[derive(Debug)]
pub struct NotInstruction {
    pub dest: Operand
}

#[derive(Debug)]
pub struct AddInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct SubInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct MulInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct IdivInstruction {
    pub src: Operand
}

#[derive(Debug)]
pub struct AndInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct OrInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct XorInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct ShlInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub struct ShrInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Imm(ImmOperand),
    Register(RegisterOperand),
    Stack(StackOperand)
}

#[derive(Debug, Clone, Copy)]
pub struct ImmOperand {
    pub value: u64
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterOperand {
    Ax,
    Cl,
    Dx,
    R10,
    R11
}

#[derive(Debug, Clone, Copy)]
pub struct StackOperand {
    pub offset: u32
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
    pub symbol: Symbol
}
