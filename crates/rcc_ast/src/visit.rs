use crate::{
    AssignmentExpression, BinaryExpression, Block, BlockItem, BreakStatement, CaseLabeledStatement, ConditionalExpression, ContinueStatement, Declaration, DefaultLabeledStatement, DoStatement, EmptyStatement, Expression, ExpressionStatement, ForInit, ForStatement, FunctionDeclaration, GotoStatement, Identifier, IdentifierLabeledStatement, IfStatement, Label, LabeledStatement, Lvalue, NumberLiteral, Program, ReturnStatement, Statement, SwitchStatement, Type, TypedefDeclaration, UnaryExpression, UpdateExpression, VariableDeclaration, WhileStatement
};

pub trait Visit<'src>: Sized {
    #[inline]
    fn visit_program(&self, program: &Program<'src>) {
        walk_program(self, program);
    }

    #[inline]
    fn visit_func_decl(&self, decl: &FunctionDeclaration<'src>) {
        walk_func_decl(self, decl);
    }

    #[inline]
    fn visit_block(&self, block: &Block<'src>) {
        walk_block(self, block);
    }

    #[inline]
    fn visit_block_item(&self, block_item: &BlockItem<'src>) {
        walk_block_item(self, block_item);
    }

    #[inline]
    fn visit_decl(&self, decl: &Declaration<'src>) {
        walk_decl(self, decl);
    }

    #[inline]
    fn visit_typedef_decl(&self, decl: &TypedefDeclaration<'src>) {
        walk_typedef_decl(self, decl);
    }

    #[inline]
    fn visit_variable_decl(&self, decl: &VariableDeclaration<'src>) {
        walk_variable_decl(self, decl);
    }

    #[inline]
    fn visit_stmt(&self, stmt: &Statement<'src>) {
        walk_stmt(self, stmt);
    }

    #[inline]
    fn visit_break_stmt(&self, stmt: &BreakStatement) {
        walk_break_stmt(self, stmt);
    }

    #[inline]
    fn visit_continue_stmt(&self, stmt: &ContinueStatement) {
        walk_continue_stmt(self, stmt);
    }

    #[inline]
    fn visit_do_stmt(&self, stmt: &DoStatement<'src>) {
        walk_do_stmt(self, stmt);
    }

    #[inline]
    fn visit_empty_stmt(&self, stmt: &EmptyStatement) {
        walk_empty_stmt(self, stmt);
    }

    #[inline]
    fn visit_expr_stmt(&self, stmt: &ExpressionStatement<'src>) {
        walk_expr_stmt(self, stmt);
    }

    #[inline]
    fn visit_for_stmt(&self, stmt: &ForStatement<'src>) {
        walk_for_stmt(self, stmt);
    }

    #[inline]
    fn visit_for_init(&self, init: &ForInit<'src>) {
        walk_for_init(self, init)
    }

    #[inline]
    fn visit_goto_stmt(&self, stmt: &GotoStatement) {
        walk_goto_stmt(self, stmt);
    }

    #[inline]
    fn visit_if_stmt(&self, stmt: &IfStatement<'src>) {
        walk_if_stmt(self, stmt);
    }

    #[inline]
    fn visit_labeled_stmt(&self, stmt: &LabeledStatement<'src>) {
        walk_labeled_stmt(self, stmt);
    }

    #[inline]
    fn visit_case_labeled_stmt(&self, stmt: &CaseLabeledStatement<'src>) {
        walk_case_labeled_stmt(self, stmt);
    }

    #[inline]
    fn visit_default_labeled_stmt(&self, stmt: &DefaultLabeledStatement<'src>) {
        walk_default_labeled_stmt(self, stmt);
    }

    #[inline]
    fn visit_id_labeled_stmt(&self, stmt: &IdentifierLabeledStatement<'src>) {
        walk_id_labeled_stmt(self, stmt);
    }

    #[inline]
    fn visit_return_stmt(&self, stmt: &ReturnStatement<'src>) {
        walk_return_stmt(self, stmt);
    }

    #[inline]
    fn visit_switch_stmt(&self, stmt: &SwitchStatement<'src>) {
        walk_switch_stmt(self, stmt);
    }

    #[inline]
    fn visit_while_stmt(&self, stmt: &WhileStatement<'src>) {
        walk_while_stmt(self, stmt)
    }

    #[inline]
    fn visit_expr(&self, expr: &Expression<'src>) {
        walk_expr(self, expr);
    }

    #[inline]
    fn visit_assignment_expr(&self, expr: &AssignmentExpression<'src>) {
        walk_assignment_expr(self, expr);
    }

    #[inline]
    fn visit_binary_expr(&self, expr: &BinaryExpression<'src>) {
        walk_binary_expr(self, expr);
    }

    #[inline]
    fn visit_conditional_expr(&self, expr: &ConditionalExpression<'src>) {
        walk_conditional_expr(self, expr);
    }

    #[inline]
    fn visit_unary_expr(&self, expr: &UnaryExpression<'src>) {
        walk_unary_expr(self, expr);
    }

    #[inline]
    fn visit_update_expr(&self, expr: &UpdateExpression) {
        walk_update_expr(self, expr);
    }

    #[inline]
    fn visit_lvalue(&self, lvalue: &Lvalue) {
        walk_lvalue(self, lvalue);
    }

    #[inline]
    fn visit_number_lit(&self, lit: &NumberLiteral) {
        walk_number_lit(self, lit);
    }

    #[inline]
    fn visit_type(&self, ty: &Type<'src>) {
        walk_type(self, ty);
    }

    #[inline]
    fn visit_id(&self, id: &Identifier) {
        walk_id(self, id);
    }

    #[inline]
    fn visit_label(&self, label: &Label) {
        walk_label(self, label)
    }
}

pub fn walk_program<'src, V: Visit<'src>>(v: &V, program: &Program<'src>) {
    v.visit_func_decl(&program.func);
}

pub fn walk_func_decl<'src, V: Visit<'src>>(v: &V, decl: &FunctionDeclaration<'src>) {
    v.visit_block(&decl.body)
}

pub fn walk_block<'src, V: Visit<'src>>(v: &V, block: &Block<'src>) {
    for item in &block.items {
        v.visit_block_item(item);
    }
}

pub fn walk_block_item<'src, V: Visit<'src>>(v: &V, block_item: &BlockItem<'src>) {
    match block_item {
        BlockItem::Declaration(decl) => v.visit_decl(decl),
        BlockItem::Statement(stmt) => v.visit_stmt(stmt),
    }
}

pub fn walk_decl<'src, V: Visit<'src>>(v: &V, decl: &Declaration<'src>) {
    match decl {
        Declaration::Typedef(decl) => v.visit_typedef_decl(decl),
        Declaration::Variable(decl) => v.visit_variable_decl(decl),
    }
}

pub fn walk_typedef_decl<'src, V: Visit<'src>>(v: &V, decl: &TypedefDeclaration<'src>) {
    v.visit_type(&decl.ty);
    v.visit_id(&decl.id);
}

pub fn walk_variable_decl<'src, V: Visit<'src>>(v: &V, decl: &VariableDeclaration<'src>) {
    v.visit_type(&decl.ty);
    v.visit_id(&decl.id);

    if let Some(expr) = &decl.expr {
        v.visit_expr(expr);
    }
}

pub fn walk_stmt<'src, V: Visit<'src>>(v: &V, stmt: &Statement<'src>) {
    match stmt {
        Statement::Break(stmt) => v.visit_break_stmt(stmt),
        Statement::Compound(block) => v.visit_block(block),
        Statement::Continue(stmt) => v.visit_continue_stmt(stmt),
        Statement::Do(stmt) => v.visit_do_stmt(stmt),
        Statement::Empty(stmt) => v.visit_empty_stmt(stmt),
        Statement::For(stmt) => v.visit_for_stmt(stmt),
        Statement::Goto(stmt) => v.visit_goto_stmt(stmt),
        Statement::If(stmt) => v.visit_if_stmt(stmt),
        Statement::Labeled(stmt) => v.visit_labeled_stmt(stmt),
        Statement::Return(stmt) => v.visit_return_stmt(stmt),
        Statement::Switch(stmt) => v.visit_switch_stmt(stmt),
        Statement::While(stmt) => v.visit_while_stmt(stmt),
        Statement::Expression(stmt) => v.visit_expr_stmt(stmt),
    }
}

pub fn walk_break_stmt<'src, V: Visit<'src>>(_v: &V, _stmt: &BreakStatement) {}

pub fn walk_continue_stmt<'src, V: Visit<'src>>(_v: &V, _stmt: &ContinueStatement) {}

pub fn walk_do_stmt<'src, V: Visit<'src>>(v: &V, stmt: &DoStatement<'src>) {
    v.visit_stmt(&stmt.body);
    v.visit_expr(&stmt.condition);
}

pub fn walk_empty_stmt<'src, V: Visit<'src>>(_v: &V, _stmt: &EmptyStatement) {}

pub fn walk_expr_stmt<'src, V: Visit<'src>>(v: &V, stmt: &ExpressionStatement<'src>) {
    v.visit_expr(&stmt.expr);
}

pub fn walk_for_stmt<'src, V: Visit<'src>>(v: &V, stmt: &ForStatement<'src>) {
    if let Some(init) = &stmt.init {
        v.visit_for_init(init);
    }

    if let Some(condition) = &stmt.condition {
        v.visit_expr(condition);
    }

    if let Some(post) = &stmt.post {
        v.visit_expr(post);
    }
}

pub fn walk_for_init<'src, V: Visit<'src>>(v: &V, init: &ForInit<'src>) {
    match init {
        ForInit::Declaration(decl) => v.visit_variable_decl(decl),
        ForInit::Expression(expr) => v.visit_expr(expr),
    }
}

pub fn walk_goto_stmt<'src, V: Visit<'src>>(v: &V, stmt: &GotoStatement) {
    v.visit_label(&stmt.label);
}

pub fn walk_if_stmt<'src, V: Visit<'src>>(v: &V, stmt: &IfStatement<'src>) {
    v.visit_expr(&stmt.condition);
    v.visit_stmt(&stmt.consequent);

    if let Some(alternate) = &stmt.alternate {
        v.visit_stmt(alternate);
    }
}

pub fn walk_labeled_stmt<'src, V: Visit<'src>>(v: &V, stmt: &LabeledStatement<'src>) {
    match stmt {
        LabeledStatement::Case(stmt) => v.visit_case_labeled_stmt(stmt),
        LabeledStatement::Default(stmt) => v.visit_default_labeled_stmt(stmt),
        LabeledStatement::Identifier(stmt) => v.visit_id_labeled_stmt(stmt),
    }
}

pub fn walk_case_labeled_stmt<'src, V: Visit<'src>>(v: &V, stmt: &CaseLabeledStatement<'src>) {
    v.visit_number_lit(&stmt.constant);
    v.visit_stmt(&stmt.stmt);
}

pub fn walk_default_labeled_stmt<'src, V: Visit<'src>>(v: &V, stmt: &DefaultLabeledStatement<'src>) {
    v.visit_stmt(&stmt.stmt);
}

pub fn walk_id_labeled_stmt<'src, V: Visit<'src>>(v: &V, stmt: &IdentifierLabeledStatement<'src>) {
    v.visit_label(&stmt.label);
    v.visit_stmt(&stmt.stmt)
}

pub fn walk_return_stmt<'src, V: Visit<'src>>(v: &V, stmt: &ReturnStatement<'src>) {
    v.visit_expr(&stmt.expr);
}

pub fn walk_switch_stmt<'src, V: Visit<'src>>(v: &V, stmt: &SwitchStatement<'src>) {
    v.visit_expr(&stmt.expr);
    v.visit_stmt(&stmt.body);
}

pub fn walk_while_stmt<'src, V: Visit<'src>>(v: &V, stmt: &WhileStatement<'src>) {
    v.visit_expr(&stmt.condition);
    v.visit_stmt(&stmt.body);
}

pub fn walk_expr<'src, V: Visit<'src>>(v: &V, expr: &Expression<'src>) {
    match expr {
        Expression::NumberLiteral(lit) => v.visit_number_lit(lit),
        Expression::Identifier(id) => v.visit_id(id),
        Expression::Assignment(expr) => v.visit_assignment_expr(expr),
        Expression::Binary(expr) => v.visit_binary_expr(expr),
        Expression::Conditional(expr) => v.visit_conditional_expr(expr),
        Expression::Unary(expr) => v.visit_unary_expr(expr),
        Expression::Update(expr) => v.visit_update_expr(expr),
    }
}

pub fn walk_assignment_expr<'src, V: Visit<'src>>(v: &V, expr: &AssignmentExpression<'src>) {
    v.visit_lvalue(&expr.lvalue);
    v.visit_expr(&expr.expr);
}

pub fn walk_binary_expr<'src, V: Visit<'src>>(v: &V, expr: &BinaryExpression<'src>) {
    v.visit_expr(&expr.lhs);
    v.visit_expr(&expr.rhs);
}

pub fn walk_conditional_expr<'src, V: Visit<'src>>(v: &V, expr: &ConditionalExpression<'src>) {
    v.visit_expr(&expr.condition);
    v.visit_expr(&expr.consequent);
    v.visit_expr(&expr.alternate);
}

pub fn walk_unary_expr<'src, V: Visit<'src>>(v: &V, expr: &UnaryExpression<'src>) {
    v.visit_expr(&expr.expr);
}

pub fn walk_update_expr<'src, V: Visit<'src>>(v: &V, expr: &UpdateExpression) {
    v.visit_lvalue(&expr.lvalue)
}

pub fn walk_lvalue<'src, V: Visit<'src>>(v: &V, lvalue: &Lvalue) {
    match lvalue {
        Lvalue::Identifier(id) => v.visit_id(id),
    }
}

pub fn walk_number_lit<'src, V: Visit<'src>>(_v: &V, _lit: &NumberLiteral) {}

pub fn walk_type<'src, V: Visit<'src>>(_v: &V, _ty: &Type<'src>) {}

pub fn walk_id<'src, V: Visit<'src>>(_v: &V, _id: &Identifier) {}

pub fn walk_label<'src, V: Visit<'src>>(_v: &V, _label: &Label) {}
