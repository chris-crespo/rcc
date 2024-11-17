use std::collections::HashMap;

use rcc_ast::{self as ast, visit_mut::VisitMut};
use rcc_tac as tac;

use crate::LoweringContext;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CaseLabelId(u32);

#[derive(Debug)]
pub(crate) struct SwitchLabels {
    pub(crate) case_labels: Vec<(u64, tac::Label)>,
    case_label_map: HashMap<u64, CaseLabelId>,
    pub(crate) default_label: Option<tac::Label>,
    pub(crate) end_label: tac::Label,
}

impl SwitchLabels {
    pub(crate) fn new(end_label: tac::Label) -> SwitchLabels {
        SwitchLabels {
            case_labels: Vec::new(),
            case_label_map: HashMap::new(),
            default_label: None,
            end_label,
        }
    }

    pub(crate) fn label(&self, id: u64) -> Option<tac::Label> {
        let id = self.case_label_map.get(&id)?;
        Some(self.case_labels[id.0 as usize].1)
    }

}

pub(crate) struct SwitchLabelsCollector<'a> {
    ctx: &'a mut LoweringContext,
    labels: SwitchLabels,
}

impl<'a> SwitchLabelsCollector<'a> {
    pub(crate) fn new(ctx: &'a mut LoweringContext) -> SwitchLabelsCollector<'a> {
        let end_label = ctx.label();
        SwitchLabelsCollector {
            ctx,
            labels: SwitchLabels::new(end_label),
        }
    }

    fn record_label(&mut self, id: u64) {
        let label = self.ctx.label();
        let label_id = CaseLabelId(self.labels.case_labels.len() as _);

        self.labels.case_labels.push((id, label));
        let prev = self.labels.case_label_map.insert(id, label_id);
        assert!(prev.is_none());
    }

    pub(crate) fn collect(mut self, stmt: &ast::SwitchStatement) -> SwitchLabels {
        self.visit_stmt(&stmt.body);
        self.labels
    }
}

impl<'a, 'src> VisitMut<'src> for SwitchLabelsCollector<'a> {
    fn visit_switch_stmt(&mut self, _: &ast::SwitchStatement<'src>) {
        // Noop.
    }

    fn visit_case_labeled_stmt(&mut self, stmt: &ast::CaseLabeledStatement<'src>) {
        self.record_label(stmt.constant.value);
        self.visit_stmt(&stmt.stmt);
    }

    fn visit_default_labeled_stmt(&mut self, stmt: &ast::DefaultLabeledStatement<'src>) {
        let label = self.ctx.label();
        self.labels.default_label = Some(label);

        self.visit_stmt(&stmt.stmt);
    }
}
