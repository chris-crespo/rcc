use rcc_arena::Vec;
use rcc_interner::Symbol;
use rcc_span::Span;

#[derive(Debug)]
pub struct Program<'a> {
    pub span: Span,
    pub func: FunctionDeclaration<'a>,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub span: Span,
    pub name: Identifier,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub span: Span,
    pub items: Vec<'a, BlockItem<'a>>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Declaration(&'a Declaration<'a>),
    Statement(&'a Statement<'a>),
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Typedef(&'a TypedefDeclaration<'a>),
    Variable(&'a VariableDeclaration<'a>),
}

#[derive(Debug)]
pub struct TypedefDeclaration<'a> {
    pub span: Span,
    pub ty: Type<'a>,
    pub id: Identifier
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub span: Span,
    pub ty: Type<'a>,
    pub id: Identifier,
    pub expr: Option<Expression<'a>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Break(&'a BreakStatement),
    Compound(&'a Block<'a>),
    Continue(&'a ContinueStatement),
    Do(&'a DoStatement<'a>),
    Empty(&'a EmptyStatement),
    For(&'a ForStatement<'a>),
    Goto(&'a GotoStatement),
    If(&'a IfStatement<'a>),
    Labeled(&'a LabeledStatement<'a>),
    Return(&'a ReturnStatement<'a>),
    Switch(&'a SwitchStatement<'a>),
    While(&'a WhileStatement<'a>),
    Expression(&'a ExpressionStatement<'a>),
}

#[derive(Debug)]
pub struct BreakStatement {
    pub span: Span
}

#[derive(Debug)]
pub struct ContinueStatement {
    pub span: Span
}

#[derive(Debug)]
pub struct DoStatement<'a> {
    pub span: Span,
    pub body: Statement<'a>,
    pub condition: Expression<'a>
}

#[derive(Debug)]
pub struct EmptyStatement {
    pub span: Span
}

#[derive(Debug)]
pub struct ForStatement<'a> {
    pub span: Span,
    pub init: Option<ForInit<'a>>,
    pub condition: Option<Expression<'a>>,
    pub post: Option<Expression<'a>>,
    pub body: Statement<'a>
}

#[derive(Debug)]
pub enum ForInit<'a> {
    Declaration(&'a VariableDeclaration<'a>),
    Expression(&'a Expression<'a>),
}

#[derive(Debug)]
pub struct GotoStatement {
    pub span: Span,
    pub label: Label,
}

#[derive(Debug)]
pub struct IfStatement<'a> {
    pub span: Span,
    pub condition: Expression<'a>,
    pub consequent: Statement<'a>,
    pub alternate: Option<Statement<'a>>
}

#[derive(Debug)]
pub enum LabeledStatement<'a> {
    Case(&'a CaseLabeledStatement<'a>),
    Default(&'a DefaultLabeledStatement<'a>),
    Identifier(&'a IdentifierLabeledStatement<'a>),
}

#[derive(Debug)]
pub struct CaseLabeledStatement<'a> {
    pub span: Span,
    pub constant: &'a NumberLiteral,
    pub stmt: Statement<'a>
}

#[derive(Debug)]
pub struct DefaultLabeledStatement<'a> {
    pub span: Span,
    pub stmt: Statement<'a>
}

#[derive(Debug)]
pub struct IdentifierLabeledStatement<'a> {
    pub span: Span,
    pub label: Label,
    pub stmt: Statement<'a>
}

#[derive(Debug)]
pub struct ReturnStatement<'a> {
    pub span: Span,
    pub expr: Expression<'a>,
}

#[derive(Debug)]
pub struct SwitchStatement<'a> {
    pub span: Span,
    pub expr: Expression<'a>,
    pub body: Statement<'a>
}

#[derive(Debug)]
pub struct WhileStatement<'a> {
    pub span: Span,
    pub condition: Expression<'a>,
    pub body: Statement<'a>
}

#[derive(Debug)]
pub struct ExpressionStatement<'a> {
    pub span: Span,
    pub expr: Expression<'a>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    NumberLiteral(&'a NumberLiteral),
    Identifier(&'a Identifier),

    Assignment(&'a AssignmentExpression<'a>),
    Binary(&'a BinaryExpression<'a>),
    Conditional(&'a ConditionalExpression<'a>),
    Unary(&'a UnaryExpression<'a>),
    Update(&'a UpdateExpression),
}

#[derive(Debug)]
pub struct AssignmentExpression<'a> {
    pub span: Span,
    pub op: AssignmentOperator,
    pub lvalue: Lvalue,
    pub expr: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum AssignmentOperator {
    Assign,
    Add,
    Substract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift
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
    pub rhs: Expression<'a>,
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
    GreaterThanEqual,
}

#[derive(Debug)]
pub struct ConditionalExpression<'a> {
    pub span: Span,
    pub condition: Expression<'a>,
    pub consequent: Expression<'a>,
    pub alternate: Expression<'a>
}

#[derive(Debug)]
pub struct UnaryExpression<'a> {
    pub span: Span,
    pub op: UnaryOperator,
    pub expr: Expression<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Negation,
    Not,
    BitwiseComplement,
}

#[derive(Debug)]
pub struct UpdateExpression {
    pub span: Span,
    pub op: UpdateOperator,
    pub postfix: bool,
    pub lvalue: Lvalue
}

#[derive(Debug, Clone, Copy)]
pub enum UpdateOperator {
    Inc,
    Dec
}

#[derive(Debug)]
pub struct NumberLiteral {
    pub span: Span,
    pub value: u64,
}

#[derive(Debug)]
pub enum Type<'a> {
    Int(&'a IntType),
    Alias(&'a AliasType)
}

#[derive(Debug)]
pub struct IntType {
    pub span: Span
}

#[derive(Debug)]
pub struct AliasType {
    pub span: Span,
    pub id: Identifier
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    pub span: Span,
    pub symbol: Symbol,
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
    pub span: Span,
    pub symbol: Symbol
}
