use rcc_interner::Symbol;
use rcc_span::Span;

#[derive(Debug)]
pub struct Program<'a> {
    pub span: Span,
    pub func: FunctionDeclaration<'a>
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub span: Span,
    pub name: Identifier,
    pub stmt: Statement<'a>
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(&'a ReturnStatement<'a>)
}

#[derive(Debug)]
pub struct ReturnStatement<'a> {
    pub span: Span,
    pub expr: Expression<'a>
}

#[derive(Debug)]
pub enum Expression<'a> {
    NumberLiteral(&'a NumberLiteral),
    Unary(&'a UnaryExpression<'a>)
}

#[derive(Debug)]
pub struct UnaryExpression<'a> {
    pub span: Span,
    pub op: UnaryOperator,
    pub expr: Expression<'a>
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement
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
