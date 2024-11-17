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

pub fn case_label_not_within_switch(span: Span) -> miette::Report {
    MietteDiagnostic::new("Case label not within a switch statement")
        .with_label(span)
        .into()
}

pub fn default_label_not_within_switch(span: Span) -> miette::Report {
    MietteDiagnostic::new("Default label not within a switch statement")
        .with_label(span)
        .into()
}

pub fn duplicate_case_label(span1: Span, span2: Span) -> miette::Report {
    MietteDiagnostic::new("Duplicate case label")
        .with_label(span1.label("previously used here"))
        .and_label(span2)
        .into()
}

pub fn multiple_default_labels(span1: Span, span2: Span) -> miette::Report {
    MietteDiagnostic::new("Multiple default labels in one switch")
        .with_label(span1.label("previous default label"))
        .and_label(span2)
        .into()
}
