use miette::MietteDiagnostic;
use rcc_span::Span;

pub fn expected(expected: &str, found: &str, span: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Expected `{}` but found `{}`", expected, found))
        .with_label(span.label(format!("`{}` expected", expected)))
        .into()
}
