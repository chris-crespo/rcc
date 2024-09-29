use rcc_interner::Symbol;

mod asm_impl;
pub mod regs;

#[derive(Debug)]
pub struct Program {
    pub func: FunctionDeclaration,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Label,
    pub stack_size: u32,
    pub instructions: Vec<Instruction>,
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
    Cmp(CmpInstruction),
    Jmp(JmpInstruction),
    JmpCC(JmpCCInstruction),
    SetCC(SetCCInstruction),
    Label(Label),
    And(AndInstruction),
    Or(OrInstruction),
    Xor(XorInstruction),
    Shl(ShlInstruction),
    Sar(SarInstruction),
    Cdq,
    Ret,
}

#[derive(Debug)]
pub struct MovInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct NegInstruction {
    pub dest: Operand,
}

#[derive(Debug)]
pub struct NotInstruction {
    pub dest: Operand,
}

#[derive(Debug)]
pub struct AddInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct SubInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct MulInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct IdivInstruction {
    pub src: Operand,
}

#[derive(Debug)]
pub struct CmpInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct JmpInstruction {
    pub target: Label,
}

#[derive(Debug)]
pub struct JmpCCInstruction {
    pub code: CondCode,
    pub target: Label,
}

#[derive(Debug)]
pub struct SetCCInstruction {
    pub code: CondCode,
    pub src: Operand,
}

#[derive(Debug, Clone, Copy)]
pub enum CondCode {
    E,
    Ne,
    G,
    Ge,
    L,
    Le,
}

#[derive(Debug)]
pub struct AndInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct OrInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct XorInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct ShlInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug)]
pub struct SarInstruction {
    pub src: Operand,
    pub dest: Operand,
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Imm(ImmOperand),
    Register(RegisterOperand),
    Stack(StackOperand),
}

#[derive(Debug, Clone, Copy)]
pub struct ImmOperand {
    pub value: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterOperand {
    Ax,
    Cx,
    Dx,
    R10,
    R11,
}

#[derive(Debug, Clone, Copy)]
pub struct StackOperand {
    pub offset: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum Label {
    Named(NamedLabel),
    Unnamed(UnnamedLabel)
}

#[derive(Debug, Clone, Copy)]
pub struct NamedLabel {
    pub symbol: Symbol
}

#[derive(Debug, Clone, Copy)]
pub struct UnnamedLabel {
    pub id: u32
}
