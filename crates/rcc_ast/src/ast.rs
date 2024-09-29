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
    pub body: Block<'a>
}

#[derive(Debug)]
pub struct Block<'a> {
    pub span: Span,
    pub items: Vec<BlockItem<'a>>
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Declaration(&'a Declaration<'a>),
    Statement(&'a Statement<'a>)
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Variable(&'a VariableDeclaration<'a>)
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub span: Span,
    pub id: Identifier,
    pub expr: Option<Expression<'a>>
}

#[derive(Debug)]
pub enum Statement<'a> {
    Expression(&'a ExpressionStatement<'a>),
    Return(&'a ReturnStatement<'a>),
    Empty
}

#[derive(Debug)]
pub struct ExpressionStatement<'a> {
    pub span: Span,
    pub expr: Expression<'a>
}

#[derive(Debug)]
pub struct ReturnStatement<'a> {
    pub span: Span,
    pub expr: Expression<'a>
}

#[derive(Debug)]
pub enum Expression<'a> {
    NumberLiteral(&'a NumberLiteral),
    Identifier(&'a Identifier),

    Assignment(&'a AssignmentExpression<'a>),
    Binary(&'a BinaryExpression<'a>),
    Unary(&'a UnaryExpression<'a>)
}

#[derive(Debug)]
pub struct AssignmentExpression<'a> {
    pub span: Span,
    pub lvalue: Lvalue,
    pub expr: Expression<'a>
}

#[derive(Debug)]
pub enum Lvalue {
    Identifier(Identifier),
}

#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub span: Span,
    pub op: BinaryOperator,
    pub lhs: Expression<'a>,
    pub rhs: Expression<'a>
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
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual
}

#[derive(Debug)]
pub struct UnaryExpression<'a> {
    pub span: Span,
    pub op: UnaryOperator,
    pub expr: Expression<'a>
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Negation,
    Not,
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
