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
    Copy(CopyInstruction),
    Jump(JumpInstruction),
    JumpIfZero(JumpIfZeroInstruction),
    JumpIfNotZero(JumpIfNotZeroInstruction),
    Label(Label),
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
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual
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
    Not,
    BitwiseComplement
}

#[derive(Debug)]
pub struct CopyInstruction {
    pub src: Value,
    pub dest: Variable
}

#[derive(Debug)]
pub struct JumpInstruction {
    pub target: Label
}

#[derive(Debug)]
pub struct JumpIfZeroInstruction {
    pub value: Value,
    pub target: Label
}

#[derive(Debug)]
pub struct JumpIfNotZeroInstruction {
    pub value: Value,
    pub target: Label
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Label(u32);

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
