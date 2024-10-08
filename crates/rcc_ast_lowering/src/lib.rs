use instrs::Instrs;
use rcc_ast as ast;
use rcc_tac as tac;

mod instrs;

struct LoweringContext {
    temps: u32,
    labels: u32,
    instrs: Instrs,
}

impl LoweringContext {
    fn new() -> LoweringContext {
        LoweringContext {
            temps: 0,
            labels: 0,
            instrs: Instrs::new(),
        }
    }

    fn temp_var(&mut self) -> tac::Variable {
        let temp = self.temps;
        self.temps += 1;

        let temp_var = tac::TempVar::new(temp);
        tac::Variable::Temp(temp_var)
    }

    fn label(&mut self) -> tac::Label {
        let label = self.labels;
        self.labels += 1;

        tac::Label { id: label }
    }
}

pub fn lower_to_tac(program: &ast::Program) -> tac::Program {
    let mut ctx = LoweringContext::new();
    lower_program(&mut ctx, program)
}

fn lower_program(ctx: &mut LoweringContext, program: &ast::Program) -> tac::Program {
    let func = lower_decl_func(ctx, &program.func);
    tac::Program { func }
}

fn lower_decl_func(
    ctx: &mut LoweringContext,
    func: &ast::FunctionDeclaration,
) -> tac::FunctionDeclaration {
    lower_stmt(ctx, &func.stmt);

    let name = map_ast_id(&func.name);
    let body = ctx.instrs.take();

    tac::FunctionDeclaration { name, body }
}

fn lower_stmt(ctx: &mut LoweringContext, stmt: &ast::Statement) {
    match stmt {
        ast::Statement::Return(stmt) => lower_return_stmt(ctx, stmt),
    }
}

fn lower_return_stmt(ctx: &mut LoweringContext, stmt: &ast::ReturnStatement) {
    let value = lower_expr(ctx, &stmt.expr);
    ctx.instrs.ret(value);
}

fn lower_expr(ctx: &mut LoweringContext, expr: &ast::Expression) -> tac::Value {
    match expr {
        ast::Expression::NumberLiteral(lit) => map_number_literal(lit),
        ast::Expression::Binary(expr) if expr.op == ast::BinaryOperator::And => {
            lower_and_expr(ctx, expr)
        }
        ast::Expression::Binary(expr) if expr.op == ast::BinaryOperator::Or => {
            lower_or_expr(ctx, expr)
        }
        ast::Expression::Binary(expr) => lower_binary_expr(ctx, expr),
        ast::Expression::Unary(expr) => lower_unary_expr(ctx, expr),
    }
}

fn lower_and_expr(ctx: &mut LoweringContext, expr: &ast::BinaryExpression) -> tac::Value {
    let false_label = ctx.label();
    let end_label = ctx.label();

    let lhs = lower_expr(ctx, &expr.lhs);
    ctx.instrs.jmpz(lhs, false_label);

    let rhs = lower_expr(ctx, &expr.rhs);
    ctx.instrs.jmpz(rhs, false_label);

    let result_var = ctx.temp_var();

    let constant = tac::Constant { value: 1 };
    let value_const = tac::Value::Constant(constant);
    ctx.instrs.copy(value_const, result_var);

    ctx.instrs.jmp(end_label);
    ctx.instrs.label(false_label);

    let constant = tac::Constant { value: 0 };
    let value_const = tac::Value::Constant(constant);
    ctx.instrs.copy(value_const, result_var);

    ctx.instrs.label(end_label);

    tac::Value::Variable(result_var)
}

fn lower_or_expr(ctx: &mut LoweringContext, expr: &ast::BinaryExpression) -> tac::Value {
    let true_label = ctx.label();
    let end_label = ctx.label();

    let lhs = lower_expr(ctx, &expr.lhs);
    ctx.instrs.jmpnz(lhs, true_label);

    let rhs = lower_expr(ctx, &expr.rhs);
    ctx.instrs.jmpnz(rhs, true_label);

    let result_var = ctx.temp_var();

    let constant = tac::Constant { value: 0 };
    let value_const = tac::Value::Constant(constant);
    ctx.instrs.copy(value_const, result_var);

    ctx.instrs.jmp(end_label);
    ctx.instrs.label(true_label);

    let constant = tac::Constant { value: 1 };
    let value_const = tac::Value::Constant(constant);
    ctx.instrs.copy(value_const, result_var);

    ctx.instrs.label(end_label);

    tac::Value::Variable(result_var)
}

fn lower_binary_expr(ctx: &mut LoweringContext, expr: &ast::BinaryExpression) -> tac::Value {
    let op = map_ast_binary_op(expr.op);
    let lhs = lower_expr(ctx, &expr.lhs);
    let rhs = lower_expr(ctx, &expr.rhs);
    let dest = ctx.temp_var();

    ctx.instrs.binary(op, lhs, rhs, dest);

    tac::Value::Variable(dest)
}

fn lower_unary_expr(ctx: &mut LoweringContext, expr: &ast::UnaryExpression) -> tac::Value {
    let op = map_ast_unary_op(expr.op);
    let src = lower_expr(ctx, &expr.expr);
    let dest = ctx.temp_var();

    ctx.instrs.unary(op, src,dest);

    tac::Value::Variable(dest)
}

fn map_ast_binary_op(op: ast::BinaryOperator) -> tac::BinaryOperator {
    match op {
        ast::BinaryOperator::Add => tac::BinaryOperator::Add,
        ast::BinaryOperator::Substract => tac::BinaryOperator::Substract,
        ast::BinaryOperator::Multiply => tac::BinaryOperator::Multiply,
        ast::BinaryOperator::Divide => tac::BinaryOperator::Divide,
        ast::BinaryOperator::Remainder => tac::BinaryOperator::Remainder,
        ast::BinaryOperator::BitwiseAnd => tac::BinaryOperator::BitwiseAnd,
        ast::BinaryOperator::BitwiseOr => tac::BinaryOperator::BitwiseOr,
        ast::BinaryOperator::BitwiseXor => tac::BinaryOperator::BitwiseXor,
        ast::BinaryOperator::LeftShift => tac::BinaryOperator::LeftShift,
        ast::BinaryOperator::RightShift => tac::BinaryOperator::RightShift,
        ast::BinaryOperator::Equal => tac::BinaryOperator::Equal,
        ast::BinaryOperator::NotEqual => tac::BinaryOperator::NotEqual,
        ast::BinaryOperator::LessThan => tac::BinaryOperator::LessThan,
        ast::BinaryOperator::LessThanEqual => tac::BinaryOperator::LessThanEqual,
        ast::BinaryOperator::GreaterThan => tac::BinaryOperator::GreaterThan,
        ast::BinaryOperator::GreaterThanEqual => tac::BinaryOperator::GreaterThanEqual,
        _ => unreachable!("Binary operator: {op:?}"),
    }
}

fn map_ast_unary_op(op: ast::UnaryOperator) -> tac::UnaryOperator {
    match op {
        ast::UnaryOperator::Negation => tac::UnaryOperator::Negation,
        ast::UnaryOperator::Not => tac::UnaryOperator::Not,
        ast::UnaryOperator::BitwiseComplement => tac::UnaryOperator::BitwiseComplement,
    }
}

fn map_number_literal(lit: &ast::NumberLiteral) -> tac::Value {
    let constant_value = tac::Constant { value: lit.value };
    tac::Value::Constant(constant_value)
}

fn map_ast_id(id: &ast::Identifier) -> tac::Identifier {
    tac::Identifier { symbol: id.symbol }
}
