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
