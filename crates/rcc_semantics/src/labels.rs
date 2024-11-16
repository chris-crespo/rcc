use std::collections::{hash_map::Entry, HashMap};

use rcc_ast::{
    visit_mut::{self, VisitMut},
    FunctionDeclaration, GotoStatement, Label, Program,
};
use rcc_interner::Symbol;

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
