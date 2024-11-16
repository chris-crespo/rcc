use rcc_ast::{visit_mut::VisitMut, Program};

use crate::{diagnostics, ResolutionContext};

pub fn resolve<'src>(res: &mut ResolutionContext<'_, 'src>, program: &Program<'src>) {
    let mut loop_res = LoopResolution::new(res);
    loop_res.visit_program(program);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    None,
    Loop,
    Switch
}

struct LoopResolution<'a, 'res, 'src> {
    res: &'res mut ResolutionContext<'a, 'src>,
    scope: Scope,
}

impl<'a, 'res, 'src> LoopResolution<'a, 'res, 'src> {
    fn new(res: &'res mut ResolutionContext<'a, 'src>) -> LoopResolution<'a, 'res, 'src> {
        LoopResolution {
            res,
            scope: Scope::None,
        }
    }

    fn scoped(&mut self, scope: Scope, f: impl FnOnce(&mut LoopResolution<'a, 'res, 'src>)) {
        let old_scope = self.scope;
        self.scope = scope;

        f(self);

        self.scope = old_scope;
    }
}

impl<'a, 'res, 'src> VisitMut<'src> for LoopResolution<'a, 'res, 'src> {
    fn visit_break_stmt(&mut self, stmt: &rcc_ast::BreakStatement) {
        if self.scope != Scope::Loop && self.scope != Scope::Switch {
            let diagnostic = diagnostics::break_stmt_not_within_loop_or_switch(stmt.span);
            self.res.error(diagnostic);
        }
    }

    fn visit_continue_stmt(&mut self, stmt: &rcc_ast::ContinueStatement) {
        if self.scope != Scope::Loop {
            let diagnostics = diagnostics::continue_stmt_not_within_loop(stmt.span);
            self.res.error(diagnostics);
        }
    }

    fn visit_do_stmt(&mut self, stmt: &rcc_ast::DoStatement<'src>) {
        self.scoped(Scope::Loop, |v| v.visit_stmt(&stmt.body));
    }

    fn visit_for_stmt(&mut self, stmt: &rcc_ast::ForStatement<'src>) {
        self.scoped(Scope::Loop, |v| v.visit_stmt(&stmt.body));
    }

    fn visit_switch_stmt(&mut self, stmt: &rcc_ast::SwitchStatement<'src>) {
        self.scoped(Scope::Switch, |v| v.visit_stmt(&stmt.body));
    }

    fn visit_while_stmt(&mut self, stmt: &rcc_ast::WhileStatement<'src>) {
        self.scoped(Scope::Loop, |v| v.visit_stmt(&stmt.body));
    }
}
