use std::collections::{hash_map::Entry, HashMap};

use rcc_ast::{
    Block, BlockItem, FunctionDeclaration, GotoStatement, IfStatement, Label, LabeledStatement,
    Program, Statement,
};
use rcc_interner::Symbol;

use crate::{diagnostics, ResolutionContext};

#[derive(Debug, Default)]
struct Scope {
    labels: HashMap<Symbol, Label>,
}

struct LabelResolutionContext<'a, 'res, 'src> {
    res: &'res mut ResolutionContext<'a, 'src>,
    scope: Scope,
}

impl<'a, 'res, 'src> LabelResolutionContext<'a, 'res, 'src> {
    fn new(res: &'res mut ResolutionContext<'a, 'src>) -> LabelResolutionContext<'a, 'res, 'src> {
        LabelResolutionContext {
            res,
            scope: Scope::default(),
        }
    }

    fn define_label(&mut self, label: Label) {
        match self.scope.labels.entry(label.symbol) {
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

    fn lookup_label(&mut self, label: Label) {
        if !self.scope.labels.contains_key(&label.symbol) {
            let id = self.res.interner.get(label.symbol);
            self.res.error(diagnostics::undefined_label(id, label.span))
        }
    }

    fn clear_scope(&mut self) {
        self.scope = Scope::default();
    }

    fn scoped<T>(&mut self, f: impl FnOnce(&mut LabelResolutionContext) -> T) -> T {
        let result = f(self);
        self.clear_scope();

        result
    }
}

pub fn resolve(res: &mut ResolutionContext, program: &Program) {
    let mut cx = LabelResolutionContext::new(res);
    resolve_program(&mut cx, program)
}

fn resolve_program(cx: &mut LabelResolutionContext, program: &Program) {
    resolve_func_decl(cx, &program.func)
}

fn resolve_func_decl(cx: &mut LabelResolutionContext, decl: &FunctionDeclaration) {
    cx.scoped(|cx| resolve_block(cx, &decl.body))
}

fn resolve_block(cx: &mut LabelResolutionContext, block: &Block) {
    for item in &block.items {
        resolve_block_item(cx, item)
    }
}

fn resolve_block_item(cx: &mut LabelResolutionContext, block_item: &BlockItem) {
    match block_item {
        BlockItem::Declaration(_) => {}
        BlockItem::Statement(stmt) => resolve_stmt(cx, stmt),
    }
}

fn resolve_stmt(cx: &mut LabelResolutionContext, stmt: &Statement) {
    match stmt {
        Statement::Goto(stmt) => resolve_goto_stmt(cx, stmt),
        Statement::If(stmt) => resolve_if_stmt(cx, stmt),
        Statement::Labeled(stmt) => resolve_labeled_stmt(cx, stmt),
        _ => {}
    }
}

fn resolve_goto_stmt(cx: &mut LabelResolutionContext, stmt: &GotoStatement) {
    cx.lookup_label(stmt.label);
}

fn resolve_if_stmt(cx: &mut LabelResolutionContext, stmt: &IfStatement) {
    resolve_stmt(cx, &stmt.consequent);

    if let Some(alternate) = &stmt.alternate {
        resolve_stmt(cx, alternate);
    }
}

fn resolve_labeled_stmt(cx: &mut LabelResolutionContext, stmt: &LabeledStatement) {
    cx.define_label(stmt.label)
}
