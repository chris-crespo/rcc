use rcc_ast as ast;
use rcc_tac::{self as tac, Instruction};

struct LoweringContext {
    temps: u32,
    instrs: Vec<tac::Instruction>
}

impl LoweringContext {
    fn new() -> LoweringContext {
        LoweringContext {  temps: 0, instrs: Vec::new() }
    }

    fn temp_var(&mut self) -> tac::Variable {
        let temp = self.temps;
        self.temps += 1;

        let temp_var = tac::TempVar::new(temp);
        tac::Variable::Temp(temp_var)
    }

    #[inline(always)]
    fn take_instrs(&mut self) -> Vec<tac::Instruction> {
        std::mem::take(&mut self.instrs)
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
    let body = ctx.take_instrs();

    tac::FunctionDeclaration { name, body }
}

fn lower_stmt(
    ctx: &mut LoweringContext,
    stmt: &ast::Statement,
) {
    match stmt {
        ast::Statement::Return(stmt) => lower_return_stmt(ctx, stmt),
    }
}

fn lower_return_stmt(
    ctx: &mut LoweringContext,
    stmt: &ast::ReturnStatement,
) {
    let value = lower_expr(ctx, &stmt.expr);
    let return_instr = tac::ReturnInstruction { value };
    let instr = tac::Instruction::Return(return_instr);

    ctx.instrs.push(instr);
}

fn lower_expr(
    ctx: &mut LoweringContext,
    expr: &ast::Expression,
) -> tac::Value {
    match expr {
        ast::Expression::NumberLiteral(lit) => map_number_literal(lit),
        ast::Expression::Binary(expr) => lower_binary_expr(ctx, expr),
        ast::Expression::Unary(expr) => lower_unary_expr(ctx, expr),
    }
}

fn lower_binary_expr(ctx: &mut LoweringContext, expr: &ast::BinaryExpression) -> tac::Value {
    let op = map_ast_binary_op(expr.op);
    let lhs = lower_expr(ctx, &expr.lhs);
    let rhs = lower_expr(ctx, &expr.rhs);
    let dest = ctx.temp_var();

    let binary_instr = tac::BinaryInstruction{ op, lhs, rhs, dest };
    let instr = tac::Instruction::Binary(binary_instr);

    ctx.instrs.push(instr);

    tac::Value::Variable(dest)
}

fn lower_unary_expr(
    ctx: &mut LoweringContext,
    expr: &ast::UnaryExpression,
) -> tac::Value {
    let op = map_ast_unary_op(expr.op);
    let src = lower_expr(ctx, &expr.expr);
    let dest = ctx.temp_var();

    let unary_instr = tac::UnaryInstruction { op, src, dest };
    let instr = tac::Instruction::Unary(unary_instr);

    ctx.instrs.push(instr);

    tac::Value::Variable(dest)
}

fn map_ast_binary_op(op: ast::BinaryOperator) -> tac::BinaryOperator {
    match op {
        ast::BinaryOperator::Add => tac::BinaryOperator::Add,
        ast::BinaryOperator::Substract => tac::BinaryOperator::Substract,
        ast::BinaryOperator::Multiply => tac::BinaryOperator::Multiply,
        ast::BinaryOperator::Divide => tac::BinaryOperator::Divide,
        ast::BinaryOperator::Remainder => tac::BinaryOperator::Remainder,
    }
}

fn map_ast_unary_op(op: ast::UnaryOperator) -> tac::UnaryOperator {
    match op {
        ast::UnaryOperator::Negation => tac::UnaryOperator::Negation,
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
