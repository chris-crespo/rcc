use std::collections::HashMap;

use instrs::Instrs;
use rcc_ast as ast;
use rcc_interner::Symbol;
use rcc_tac as tac;
use switch::{SwitchLabels, SwitchLabelsCollector};

mod instrs;
mod switch;

#[derive(Debug, Default)]
struct BlockScope {
    /// Maps user defined variables to tac variables.
    var_map: HashMap<Symbol, tac::Variable>,
}

struct LoweringContext {
    vars: u32,
    labels: u32,

    /// Maps user defined labels to tac labels.
    label_map: HashMap<Symbol, tac::Label>,
    scopes: Vec<BlockScope>,

    break_label: tac::Label,
    continue_label: tac::Label,
    switch_labels: SwitchLabels,

    instrs: Instrs,
}

impl LoweringContext {
    fn new() -> LoweringContext {
        let dummy_label = tac::Label { id: 0 };

        LoweringContext {
            vars: 0,
            labels: 0,
            label_map: HashMap::new(),
            scopes: Vec::new(),
            break_label: dummy_label,
            continue_label: dummy_label,
            switch_labels: SwitchLabels::new(dummy_label),
            instrs: Instrs::new(),
        }
    }

    fn temp_var(&mut self) -> tac::Variable {
        let var = self.vars;
        self.vars += 1;

        tac::Variable { id: var }
    }

    fn define_var(&mut self, id: ast::Identifier) -> tac::Variable {
        let var = self.temp_var();
        let curr_scope = self.scopes.last_mut().expect("scopes should not be empty");

        curr_scope.var_map.insert(id.symbol, var);
        var
    }

    fn lookup_var(&mut self, id: ast::Identifier) -> tac::Variable {
        *self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.var_map.get(&id.symbol))
            .expect("variable should have been defined")
    }

    fn block_scoped(&mut self, f: impl FnOnce(&mut LoweringContext)) {
        let scope = BlockScope::default();
        self.scopes.push(scope);

        f(self);
        self.scopes.pop();
    }

    fn loop_scoped(
        &mut self,
        break_label: tac::Label,
        continue_label: tac::Label,
        f: impl FnOnce(&mut LoweringContext),
    ) {
        let prev_break_label = std::mem::replace(&mut self.break_label, break_label);
        let prev_continue_label = std::mem::replace(&mut self.continue_label, continue_label);

        f(self);

        self.break_label = prev_break_label;
        self.continue_label = prev_continue_label;
    }

    fn switch_scoped(&mut self, labels: SwitchLabels, f: impl FnOnce(&mut LoweringContext)) {
        let prev_break_label = std::mem::replace(&mut self.break_label, labels.end_label);
        let prev_switch_labels = std::mem::replace(&mut self.switch_labels, labels);

        f(self);

        self.break_label = prev_break_label;
        self.switch_labels = prev_switch_labels;
    }

    fn label(&mut self) -> tac::Label {
        let label = self.labels;
        self.labels += 1;

        tac::Label { id: label }
    }

    fn label_for(&mut self, label: ast::Label) -> tac::Label {
        if let Some(&label) = self.label_map.get(&label.symbol) {
            return label;
        }

        let tac_label = self.label();
        self.label_map.insert(label.symbol, tac_label);
        tac_label
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
    lower_block(ctx, &func.body);
    ctx.instrs.ret_zero();

    let name = map_ast_id(&func.name);
    let body = ctx.instrs.take();

    tac::FunctionDeclaration { name, body }
}

fn lower_block(ctx: &mut LoweringContext, block: &ast::Block) {
    ctx.block_scoped(|ctx| {
        for item in &block.items {
            lower_block_item(ctx, item)
        }
    })
}

fn lower_block_item(ctx: &mut LoweringContext, block_item: &ast::BlockItem) {
    match block_item {
        ast::BlockItem::Declaration(decl) => lower_decl(ctx, decl),
        ast::BlockItem::Statement(stmt) => lower_stmt(ctx, stmt),
    }
}

fn lower_decl(ctx: &mut LoweringContext, decl: &ast::Declaration) {
    match decl {
        ast::Declaration::Typedef(_) => {}
        ast::Declaration::Variable(decl) => lower_var_decl(ctx, decl),
    }
}

fn lower_var_decl(ctx: &mut LoweringContext, decl: &ast::VariableDeclaration) {
    let var = ctx.define_var(decl.id);
    let Some(expr) = &decl.expr else {
        return;
    };

    let value = lower_expr(ctx, expr);
    ctx.instrs.copy(value, var);
}

fn lower_stmt(ctx: &mut LoweringContext, stmt: &ast::Statement) {
    match stmt {
        ast::Statement::Break(stmt) => lower_break_stmt(ctx, stmt),
        ast::Statement::Compound(block) => lower_block(ctx, block),
        ast::Statement::Continue(stmt) => lower_continue_stmt(ctx, stmt),
        ast::Statement::Do(stmt) => lower_do_stmt(ctx, stmt),
        ast::Statement::Expression(expr) => lower_expr_stmt(ctx, expr),
        ast::Statement::For(stmt) => lower_for_stmt(ctx, stmt),
        ast::Statement::Goto(stmt) => lower_goto_stmt(ctx, stmt),
        ast::Statement::If(stmt) => lower_if_stmt(ctx, stmt),
        ast::Statement::Labeled(stmt) => lower_labeled_stmt(ctx, stmt),
        ast::Statement::Return(stmt) => lower_return_stmt(ctx, stmt),
        ast::Statement::Switch(stmt) => lower_switch_stmt(ctx, stmt),
        ast::Statement::While(stmt) => lower_while_stmt(ctx, stmt),
        ast::Statement::Empty(_) => {}
    }
}

fn lower_break_stmt(ctx: &mut LoweringContext, _: &ast::BreakStatement) {
    ctx.instrs.jmp(ctx.break_label);
}

fn lower_continue_stmt(ctx: &mut LoweringContext, _: &ast::ContinueStatement) {
    ctx.instrs.jmp(ctx.continue_label);
}

fn lower_do_stmt(ctx: &mut LoweringContext, stmt: &ast::DoStatement) {
    let start_label = ctx.label();
    let continue_label = ctx.label();
    let end_label = ctx.label();

    ctx.loop_scoped(end_label, continue_label, |ctx| {
        ctx.instrs.label(start_label);
        lower_stmt(ctx, &stmt.body);

        ctx.instrs.label(continue_label);
        let value = lower_expr(ctx, &stmt.condition);
        ctx.instrs.jmpnz(value, start_label);

        ctx.instrs.label(end_label);
    })
}

fn lower_expr_stmt(ctx: &mut LoweringContext, stmt: &ast::ExpressionStatement) {
    lower_expr(ctx, &stmt.expr);
}

fn lower_for_stmt(ctx: &mut LoweringContext, stmt: &ast::ForStatement) {
    ctx.block_scoped(|ctx| {
        if let Some(init) = &stmt.init {
            lower_for_init(ctx, init);
        }

        let start_label = ctx.label();
        let continue_label = ctx.label();
        let end_label = ctx.label();

        ctx.loop_scoped(end_label, continue_label, |ctx| {
            ctx.instrs.label(start_label);
            if let Some(condition) = &stmt.condition {
                let value = lower_expr(ctx, condition);
                ctx.instrs.jmpz(value, end_label);
            }

            lower_stmt(ctx, &stmt.body);
            ctx.instrs.label(continue_label);

            if let Some(post) = &stmt.post {
                lower_expr(ctx, post);
            }

            ctx.instrs.jmp(start_label);
            ctx.instrs.label(end_label);
        })
    })
}

fn lower_for_init(ctx: &mut LoweringContext, init: &ast::ForInit) {
    match init {
        ast::ForInit::Declaration(decl) => lower_var_decl(ctx, decl),
        ast::ForInit::Expression(expr) => {
            lower_expr(ctx, expr);
        }
    }
}

fn lower_goto_stmt(ctx: &mut LoweringContext, stmt: &ast::GotoStatement) {
    let label = ctx.label_for(stmt.label);
    ctx.instrs.jmp(label);
}

fn lower_if_stmt(ctx: &mut LoweringContext, stmt: &ast::IfStatement) {
    match &stmt.alternate {
        Some(alternate) => lower_if_then_else(ctx, &stmt.condition, &stmt.consequent, alternate),
        None => lower_if_then(ctx, &stmt.condition, &stmt.consequent),
    }
}

fn lower_if_then(
    ctx: &mut LoweringContext,
    condition: &ast::Expression,
    consequent: &ast::Statement,
) {
    let end_label = ctx.label();

    let value = lower_expr(ctx, condition);
    ctx.instrs.jmpz(value, end_label);

    lower_stmt(ctx, consequent);
    ctx.instrs.label(end_label);
}

fn lower_if_then_else(
    ctx: &mut LoweringContext,
    condition: &ast::Expression,
    consequent: &ast::Statement,
    alternate: &ast::Statement,
) {
    let else_label = ctx.label();
    let end_label = ctx.label();

    let value = lower_expr(ctx, condition);
    ctx.instrs.jmpz(value, else_label);

    lower_stmt(ctx, consequent);
    ctx.instrs.jmp(end_label);
    ctx.instrs.label(else_label);

    lower_stmt(ctx, alternate);
    ctx.instrs.label(end_label);
}

fn lower_labeled_stmt(ctx: &mut LoweringContext, stmt: &ast::LabeledStatement) {
    match stmt {
        ast::LabeledStatement::Case(stmt) => lower_case_labeled_stmt(ctx, stmt),
        ast::LabeledStatement::Default(stmt) => lower_default_labeled_stmt(ctx, stmt),
        ast::LabeledStatement::Identifier(stmt) => lower_identifier_labeled_stmt(ctx, stmt),
    }
}

fn lower_case_labeled_stmt(ctx: &mut LoweringContext, stmt: &ast::CaseLabeledStatement) {
    let label = ctx
        .switch_labels
        .label(stmt.constant.value)
        .expect("case label should exist");

    ctx.instrs.label(label);
    lower_stmt(ctx, &stmt.stmt);
}

fn lower_default_labeled_stmt(ctx: &mut LoweringContext, stmt: &ast::DefaultLabeledStatement) {
    let label = ctx
        .switch_labels
        .default_label
        .expect("default label should exist");

    ctx.instrs.label(label);
    lower_stmt(ctx, &stmt.stmt);
}

fn lower_identifier_labeled_stmt(
    ctx: &mut LoweringContext,
    stmt: &ast::IdentifierLabeledStatement,
) {
    let label = ctx.label_for(stmt.label);
    ctx.label_map.insert(stmt.label.symbol, label);

    ctx.instrs.label(label);
    lower_stmt(ctx, &stmt.stmt);
}

fn lower_return_stmt(ctx: &mut LoweringContext, stmt: &ast::ReturnStatement) {
    let value = lower_expr(ctx, &stmt.expr);
    ctx.instrs.ret(value);
}

fn lower_switch_stmt(ctx: &mut LoweringContext, stmt: &ast::SwitchStatement) {
    let value = lower_expr(ctx, &stmt.expr);

    let temp_var = ctx.temp_var();
    let labels = SwitchLabelsCollector::new(ctx).collect(stmt);
    let end_label = labels.end_label;

    for &(constant, label) in &labels.case_labels {
        let constant = tac::Constant { value: constant };
        let rhs = tac::Value::Constant(constant);
        ctx.instrs.binary(tac::BinaryOperator::Equal, value, rhs, temp_var);

        let value = tac::Value::Variable(temp_var);
        ctx.instrs.jmpnz(value, label);
    }

    if let Some(label) = labels.default_label {
        ctx.instrs.jmp(label);
    } else {
        ctx.instrs.jmp(labels.end_label);
    }

    ctx.switch_scoped(labels, |ctx| lower_stmt(ctx, &stmt.body));
    ctx.instrs.label(end_label);
}

fn lower_while_stmt(ctx: &mut LoweringContext, stmt: &ast::WhileStatement) {
    let start_label = ctx.label();
    let end_label = ctx.label();

    ctx.loop_scoped(end_label, start_label, |ctx| {
        ctx.instrs.label(start_label);

        let value = lower_expr(ctx, &stmt.condition);
        ctx.instrs.jmpz(value, end_label);

        lower_stmt(ctx, &stmt.body);
        ctx.instrs.jmp(start_label);
        ctx.instrs.label(end_label);
    })
}

fn lower_expr(ctx: &mut LoweringContext, expr: &ast::Expression) -> tac::Value {
    match expr {
        ast::Expression::NumberLiteral(lit) => map_number_literal(lit),
        ast::Expression::Identifier(id) => map_id_expr(ctx, id),
        ast::Expression::Assignment(expr) => lower_assignment_expr(ctx, expr),
        ast::Expression::Binary(expr) if expr.op == ast::BinaryOperator::And => {
            lower_and_expr(ctx, expr)
        }
        ast::Expression::Binary(expr) if expr.op == ast::BinaryOperator::Or => {
            lower_or_expr(ctx, expr)
        }
        ast::Expression::Binary(expr) => lower_binary_expr(ctx, expr),
        ast::Expression::Conditional(expr) => lower_conditional_expr(ctx, expr),
        ast::Expression::Unary(expr) => lower_unary_expr(ctx, expr),
        ast::Expression::Update(expr) if expr.postfix => lower_update_expr_postfix(ctx, expr),
        ast::Expression::Update(expr) => lower_update_expr_prefix(ctx, expr),
    }
}

fn lower_assignment_expr(
    ctx: &mut LoweringContext,
    expr: &ast::AssignmentExpression,
) -> tac::Value {
    match expr.op {
        ast::AssignmentOperator::Assign => lower_assignment_expr_simple(ctx, expr),
        _ => lower_assignment_expr_compound(ctx, expr),
    }
}

fn lower_assignment_expr_simple(
    ctx: &mut LoweringContext,
    expr: &ast::AssignmentExpression,
) -> tac::Value {
    let var = map_lvalue(ctx, &expr.lvalue);
    let value = lower_expr(ctx, &expr.expr);
    ctx.instrs.copy(value, var);

    tac::Value::Variable(var)
}

fn lower_assignment_expr_compound(
    ctx: &mut LoweringContext,
    expr: &ast::AssignmentExpression,
) -> tac::Value {
    let op = map_ast_compount_assignemnt_op(expr.op);
    let var = map_lvalue(ctx, &expr.lvalue);
    let lhs = tac::Value::Variable(var);
    let rhs = lower_expr(ctx, &expr.expr);

    ctx.instrs.binary(op, lhs, rhs, var);

    tac::Value::Variable(var)
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

fn lower_conditional_expr(
    ctx: &mut LoweringContext,
    expr: &ast::ConditionalExpression,
) -> tac::Value {
    let result_var = ctx.temp_var();
    let alternate_label = ctx.label();
    let end_label = ctx.label();

    let condition_value = lower_expr(ctx, &expr.condition);
    ctx.instrs.jmpz(condition_value, alternate_label);

    let consequent_value = lower_expr(ctx, &expr.consequent);
    ctx.instrs.copy(consequent_value, result_var);
    ctx.instrs.jmp(end_label);
    ctx.instrs.label(alternate_label);

    let alternate_value = lower_expr(ctx, &expr.alternate);
    ctx.instrs.copy(alternate_value, result_var);
    ctx.instrs.label(end_label);

    tac::Value::Variable(result_var)
}

fn lower_unary_expr(ctx: &mut LoweringContext, expr: &ast::UnaryExpression) -> tac::Value {
    let op = map_ast_unary_op(expr.op);
    let src = lower_expr(ctx, &expr.expr);
    let dest = ctx.temp_var();

    ctx.instrs.unary(op, src, dest);

    tac::Value::Variable(dest)
}

// a++              ++a
//
// temp1 = a        a = a + 1
// a = a + 1
// temp1

fn lower_update_expr_prefix(ctx: &mut LoweringContext, expr: &ast::UpdateExpression) -> tac::Value {
    let var = map_lvalue(ctx, &expr.lvalue);

    let op = map_ast_update_op(expr.op);
    let lhs = tac::Value::Variable(var);
    let rhs = tac::Value::Constant(tac::Constant { value: 1 });
    ctx.instrs.binary(op, lhs, rhs, var);

    tac::Value::Variable(var)
}

fn lower_update_expr_postfix(
    ctx: &mut LoweringContext,
    expr: &ast::UpdateExpression,
) -> tac::Value {
    let temp1 = ctx.temp_var();
    let var = map_lvalue(ctx, &expr.lvalue);

    let src = tac::Value::Variable(var);
    ctx.instrs.copy(src, temp1);

    let op = map_ast_update_op(expr.op);
    let lhs = tac::Value::Variable(var);
    let rhs = tac::Value::Constant(tac::Constant { value: 1 });
    ctx.instrs.binary(op, lhs, rhs, var);

    tac::Value::Variable(temp1)
}

fn map_ast_compount_assignemnt_op(op: ast::AssignmentOperator) -> tac::BinaryOperator {
    match op {
        ast::AssignmentOperator::Add => tac::BinaryOperator::Add,
        ast::AssignmentOperator::Substract => tac::BinaryOperator::Substract,
        ast::AssignmentOperator::Multiply => tac::BinaryOperator::Multiply,
        ast::AssignmentOperator::Divide => tac::BinaryOperator::Divide,
        ast::AssignmentOperator::Remainder => tac::BinaryOperator::Remainder,
        ast::AssignmentOperator::BitwiseAnd => tac::BinaryOperator::BitwiseAnd,
        ast::AssignmentOperator::BitwiseOr => tac::BinaryOperator::BitwiseOr,
        ast::AssignmentOperator::BitwiseXor => tac::BinaryOperator::BitwiseXor,
        ast::AssignmentOperator::LeftShift => tac::BinaryOperator::LeftShift,
        ast::AssignmentOperator::RightShift => tac::BinaryOperator::RightShift,
        _ => unreachable!("Compount assingment operator: {op:?}"),
    }
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

fn map_ast_update_op(op: ast::UpdateOperator) -> tac::BinaryOperator {
    match op {
        rcc_ast::UpdateOperator::Inc => tac::BinaryOperator::Add,
        rcc_ast::UpdateOperator::Dec => tac::BinaryOperator::Substract,
    }
}

fn map_number_literal(lit: &ast::NumberLiteral) -> tac::Value {
    let constant_value = tac::Constant { value: lit.value };
    tac::Value::Constant(constant_value)
}

fn map_id_expr(ctx: &mut LoweringContext, id: &ast::Identifier) -> tac::Value {
    let var = ctx.lookup_var(*id);
    tac::Value::Variable(var)
}

fn map_lvalue(ctx: &mut LoweringContext, lvalue: &ast::Lvalue) -> tac::Variable {
    match lvalue {
        ast::Lvalue::Identifier(id) => ctx.lookup_var(*id),
    }
}

fn map_ast_id(id: &ast::Identifier) -> tac::Identifier {
    tac::Identifier { symbol: id.symbol }
}
