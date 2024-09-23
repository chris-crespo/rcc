use rcc_interner::Symbol;

#[derive(Debug)]
pub struct Program {
    pub func: FunctionDeclaration
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Label,
    pub instructions: Vec<Instruction>
}

#[derive(Debug)]
pub enum Instruction {
    AllocStack(AllocStackInstruction),
    Mov(MovInstruction),
    Neg(NegInstruction),
    Not(NotInstruction),
    Ret
}

#[derive(Debug)]
pub struct AllocStackInstruction {
    pub size: u32
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
pub enum Operand {
    Imm(ImmOperand),
    Register(RegisterOperand),
    Stack(StackOperand)
}

#[derive(Debug)]
pub struct ImmOperand {
    pub value: u64
}

#[derive(Debug)]
pub enum RegisterOperand {
    Ax,
    R10
}

#[derive(Debug)]
pub struct StackOperand {
    pub offset: u32
}

#[derive(Debug)]
pub struct Label {
    pub symbol: Symbol
}
