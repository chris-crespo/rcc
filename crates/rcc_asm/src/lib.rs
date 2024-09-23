use rcc_interner::Symbol;

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
    R10
}

#[derive(Debug, Clone, Copy)]
pub struct StackOperand {
    pub offset: u32
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
    pub symbol: Symbol
}
