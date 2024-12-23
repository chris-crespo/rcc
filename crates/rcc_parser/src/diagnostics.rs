use miette::MietteDiagnostic;
use rcc_span::Span;

pub fn expected(span: Span, expected: &str, found: &str) -> miette::Report {
    MietteDiagnostic::new(format!("Expected `{}` but found `{}`", expected, found))
        .with_label(span.label(format!("`{}` expected", expected)))
        .into()
}

pub fn unexpected(span: Span) -> miette::Report {
    MietteDiagnostic::new("Unexpected token")
        .with_label(span)
        .into()
}

pub fn redefined(id: &str, span1: Span, span2: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Redefined `{}`", id))
        .with_label(span1.label(format!("previous definition of `{}`", id)))
        .with_label(span2)
        .into()
}

pub fn undefined(id: &str, span: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Undefined `{}`", id))
        .with_label(span)
        .into()
}

pub fn invalid_lvalue(span: Span) -> miette::Report {
    MietteDiagnostic::new("Invalid lvalue")
        .with_label(span)
        .into()
}

pub fn non_variable_declaration_in_for_loop(span: Span) -> miette::Report {
    MietteDiagnostic::new("Invalid declaration in for loop initial declaration")
        .with_label(span.label("expected variable declaration"))
        .into()
}

pub fn unfoldable_case_label(span: Span) -> miette::Report {
    MietteDiagnostic::new("Case label cannot be folded into an integer constant")
        .with_label(span)
        .into()
}
