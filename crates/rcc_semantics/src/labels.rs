use std::collections::{hash_map::Entry, HashMap};

use rcc_ast::{
    visit_mut::{self, VisitMut},
    CaseLabeledStatement, DefaultLabeledStatement, FunctionDeclaration, GotoStatement, Label,
    Program, SwitchStatement,
};
use rcc_interner::Symbol;
use rcc_span::Span;

use crate::{diagnostics, ResolutionContext};

pub fn resolve<'a, 'src>(res: &'a mut ResolutionContext<'_, 'src>, program: &'a Program<'src>) {
    resolve_program(res, program);
}

fn resolve_program<'a, 'src>(res: &'a mut ResolutionContext<'_, 'src>, program: &'a Program<'src>) {
    resolve_func_decl(res, &program.func)
}

fn resolve_func_decl<'a, 'src>(
    res: &'a mut ResolutionContext<'_, 'src>,
    decl: &'a FunctionDeclaration<'src>,
) {
    let labels = LabelCollector::new(res).collect_labels(decl);
    LabelResolver::new(res, &labels).resolve_labels(decl);
    SwitchLabelResolver::new(res).resolve_labels(decl);
}

type Labels = HashMap<Symbol, Label>;

struct LabelCollector<'a, 'res, 'src> {
    res: &'res mut ResolutionContext<'a, 'src>,
    labels: Labels,
}

impl<'a, 'res, 'src> LabelCollector<'a, 'res, 'src> {
    fn new(res: &'res mut ResolutionContext<'a, 'src>) -> LabelCollector<'a, 'res, 'src> {
        LabelCollector {
            res,
            labels: HashMap::new(),
        }
    }

    fn define_label(&mut self, label: Label) {
        match self.labels.entry(label.symbol) {
            Entry::Vacant(entry) => {
                entry.insert(label);
            }
            Entry::Occupied(entry) => {
                let id = self.res.interner.get(label.symbol);
                let span = entry.get().span;
                self.res
                    .error(diagnostics::redefined_label(id, span, label.span))
            }
        };
    }

    fn collect_labels(mut self, decl: &FunctionDeclaration<'src>) -> Labels {
        self.visit_func_decl(decl);
        self.labels
    }
}

impl<'a, 'res, 'src> VisitMut<'src> for LabelCollector<'a, 'res, 'src> {
    fn visit_id_labeled_stmt(&mut self, stmt: &rcc_ast::IdentifierLabeledStatement<'src>) {
        self.define_label(stmt.label);
        visit_mut::walk_stmt(self, &stmt.stmt);
    }
}

struct LabelResolver<'a, 'res, 'src> {
    res: &'a mut ResolutionContext<'res, 'src>,
    labels: &'a HashMap<Symbol, Label>,
}

impl<'a, 'res, 'src> LabelResolver<'a, 'res, 'src> {
    fn new(
        res: &'a mut ResolutionContext<'res, 'src>,
        labels: &'a Labels,
    ) -> LabelResolver<'a, 'res, 'src> {
        LabelResolver { res, labels }
    }

    fn lookup_label(&mut self, label: Label) {
        if !self.labels.contains_key(&label.symbol) {
            let id = self.res.interner.get(label.symbol);
            self.res.error(diagnostics::undefined_label(id, label.span))
        }
    }

    fn resolve_labels(mut self, decl: &FunctionDeclaration<'src>) {
        self.visit_func_decl(decl);
    }
}

impl<'a, 'res, 'src> VisitMut<'src> for LabelResolver<'a, 'res, 'src> {
    fn visit_goto_stmt(&mut self, stmt: &GotoStatement) {
        self.lookup_label(stmt.label);
    }
}

#[derive(Debug)]
struct CaseLabel {
    span: Span,
    id: u64,
}

#[derive(Debug)]
struct DefaultLabel {
    span: Span,
}

#[derive(Debug, Default)]
struct SwitchScope {
    case_labels: Vec<CaseLabel>,
    default_label: Option<DefaultLabel>,
}

struct SwitchLabelResolver<'a, 'res, 'src> {
    res: &'a mut ResolutionContext<'res, 'src>,
    scope: Option<SwitchScope>,
}

impl<'a, 'res, 'src> SwitchLabelResolver<'a, 'res, 'src> {
    fn new(res: &'a mut ResolutionContext<'res, 'src>) -> SwitchLabelResolver<'a, 'res, 'src> {
        SwitchLabelResolver { res, scope: None }
    }

    fn scoped(&mut self, f: impl FnOnce(&mut SwitchLabelResolver<'a, 'res, 'src>)) {
        let scope = SwitchScope::default();
        let prev_scope = std::mem::replace(&mut self.scope, Some(scope));

        f(self);

        self.scope = prev_scope;
    }

    fn record_case_labeled_stmt(&mut self, stmt: &CaseLabeledStatement<'src>) {
        const CASE_LEN: u32 = "case".len() as u32;

        let Some(scope) = &mut self.scope else {
            return self
                .res
                .error(diagnostics::case_label_not_within_switch(stmt.span));
        };

        let label = CaseLabel {
            span: Span::sized(stmt.span.start, CASE_LEN),
            id: stmt.constant.value,
        };

        if let Some(previous_label) = scope
            .case_labels
            .iter()
            .find(|stmt2| stmt.constant.value == stmt2.id)
        {
            let diagnostic = diagnostics::duplicate_case_label(previous_label.span, label.span);
            return self.res.error(diagnostic);
        }

        scope.case_labels.push(label)
    }

    fn record_default_labeled_stmt(&mut self, stmt: &DefaultLabeledStatement<'src>) {
        const DEFAULT_LEN: u32 = "default".len() as u32;

        let Some(scope) = &mut self.scope else {
            return self
                .res
                .error(diagnostics::case_label_not_within_switch(stmt.span));
        };

        let label = DefaultLabel {
            span: Span::sized(stmt.span.start, DEFAULT_LEN),
        };
        if let Some(previous_label) = &scope.default_label {
            let span1 = previous_label.span;
            let span2 = label.span;
            let diagnostic = diagnostics::multiple_default_labels(span1, span2);
            return self.res.error(diagnostic);
        }

        scope.default_label = Some(label);
    }

    fn resolve_labels(mut self, decl: &FunctionDeclaration<'src>) {
        self.visit_func_decl(decl);
    }
}

impl<'a, 'res, 'src> VisitMut<'src> for SwitchLabelResolver<'a, 'res, 'src> {
    fn visit_switch_stmt(&mut self, stmt: &SwitchStatement<'src>) {
        self.scoped(|v| v.visit_stmt(&stmt.body))
    }

    fn visit_case_labeled_stmt(&mut self, stmt: &CaseLabeledStatement<'src>) {
        self.record_case_labeled_stmt(stmt);
        self.visit_stmt(&stmt.stmt);
    }

    fn visit_default_labeled_stmt(&mut self, stmt: &DefaultLabeledStatement<'src>) {
        self.record_default_labeled_stmt(stmt);
        self.visit_stmt(&stmt.stmt);
    }
}
