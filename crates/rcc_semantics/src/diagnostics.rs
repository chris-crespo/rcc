use miette::MietteDiagnostic;
use rcc_span::Span;

pub fn undefined_label(id: &str, span: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Undefined label `{}`", id))
        .with_label(span)
        .into()
}

pub fn redefined_label(id: &str, span1: Span, span2: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Redefined label `{}`", id))
        .with_label(span1.label(format!("previous definition of `{}`", id)))
        .and_label(span2)
        .into()
}

pub fn break_stmt_not_within_loop_or_switch(span: Span) -> miette::Report {
    MietteDiagnostic::new("Break statement not within loop or switch")
        .with_label(span)
        .into()
}

pub fn continue_stmt_not_within_loop(span: Span) -> miette::Report {
    MietteDiagnostic::new("Continue statement not within a loop")
        .with_label(span)
        .into()
}
