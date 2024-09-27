use rcc_interner::Symbol;

mod tac_impl;

#[derive(Debug)]
pub struct Program {
    pub func: FunctionDeclaration
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub body: Vec<Instruction>
}

#[derive(Debug)]
pub enum Instruction {
    Return(ReturnInstruction),
    Binary(BinaryInstruction),
    Unary(UnaryInstruction),
}

#[derive(Debug)]
pub struct ReturnInstruction {
    pub value: Value
}

#[derive(Debug)]
pub struct BinaryInstruction {
    pub op: BinaryOperator,
    pub lhs: Value,
    pub rhs: Value,
    pub dest: Variable
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Substract,
    Multiply,
    Divide,
    Remainder
}

#[derive(Debug)]
pub struct UnaryInstruction {
    pub op: UnaryOperator,
    pub src: Value,
    pub dest: Variable
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement
}

#[derive(Debug)]
pub enum Value {
    Constant(Constant),
    Variable(Variable)
}

#[derive(Debug)]
pub struct Constant {
    pub value: u64
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variable {
    Temp(TempVar)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TempVar(u32);

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    pub symbol: Symbol
}
