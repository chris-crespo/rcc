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
    Mov(MovInstruction),
    Ret
}

#[derive(Debug)]
pub struct MovInstruction {
    pub src: Operand,
    pub dest: Operand
}

#[derive(Debug)]
pub enum Operand {
    Imm(ImmOperand),
    Register
}

#[derive(Debug)]
pub struct ImmOperand {
    pub value: u64
}

#[derive(Debug)]
pub struct Label {
    pub symbol: Symbol
}
