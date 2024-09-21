use rcc_interner::Symbol;
use rcc_span::Span;

#[derive(Debug)]
pub struct Program {
    pub span: Span,
    pub func: FunctionDeclaration
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub name: Identifier,
    pub stmt: Statement
}

#[derive(Debug)]
pub enum Statement {
    Return(ReturnStatement)
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub span: Span,
    pub expr: Expression
}

#[derive(Debug)]
pub enum Expression {
    NumberLiteral(NumberLiteral)
}

#[derive(Debug)]
pub struct NumberLiteral {
    pub span: Span,
    pub value: u64
}

#[derive(Debug)]
pub struct Identifier {
    pub span: Span,
    pub symbol: Symbol
}
