use miette::MietteDiagnostic;
use rcc_span::Span;

pub fn expected(span: Span, expected: &str, found: &str) -> miette::Report {
    MietteDiagnostic::new(format!("Expected `{}` but found `{}`", expected, found))
        .with_label(span.label(format!("`{}` expected", expected)))
        .into()
}

pub fn expected_at(span: Span, expected: &str, at: &str) -> miette::Report {
    MietteDiagnostic::new(format!("Expected `{expected}` at {at}"))
        .with_label(span.label(expected))
        .into()
}

pub fn unexpected(span: Span) -> miette::Report {
    MietteDiagnostic::new("Unexpected token")
        .with_label(span)
        .into()
}

pub fn parameter_may_not_have_void_type(span: Span) -> miette::Report {
    MietteDiagnostic::new("Parameter may not have `void` type")
        .with_label(span)
        .into()
}

pub fn invalid_number_of_arguments(span: Span, expected: usize, actual: usize) -> miette::Report {
    MietteDiagnostic::new(format!(
        "Invalid number of arguments: expected {}, got {}",
        expected, actual
    ))
    .with_label(span)
    .into()
}

pub fn redefinition(name: &str, span1: Span, span2: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Redifinition of `{name}`"))
        .with_label(span1)
        .and_label(span2.label("previous definition"))
        .into()
}

pub fn redefinition_as_different_kind_of_symbol(
    name: &str,
    span1: Span,
    span2: Span,
) -> miette::Report {
    MietteDiagnostic::new(format!(
        "Redefinition of `{name}` as different kind of symbol"
    ))
    .with_label(span1)
    .and_label(span2.label("previous definition"))
    .into()
}

pub fn redefinition_with_different_type(
    name: &str,
    span1: Span,
    span2: Span,
    type1: &str,
    type2: &str,
) -> miette::Report {
    MietteDiagnostic::new(format!(
        "Redefinition of `{name}` with different type: `{type1}` vs `{type2}`"
    ))
    .with_label(span1)
    .and_label(span2.label("previous definition"))
    .into()
}

pub fn function_definition_not_allowed(span: Span) -> miette::Report {
    MietteDiagnostic::new("Function definition is not allowed here")
        .with_label(span)
        .into()
}

pub fn undefined(id: &str, span: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Undefined `{}`", id))
        .with_label(span)
        .into()
}

pub fn unknown_type(name: &str, span: Span) -> miette::Report {
    MietteDiagnostic::new(format!("Unknown type `{name}"))
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

pub fn non_function_call(span: Span) -> miette::Report {
    MietteDiagnostic::new("Called object is not a function")
        .with_label(span)
        .into()
}

pub fn missing_type(span: Span) -> miette::Report {
    MietteDiagnostic::new("Missing type in declaration")
        .with_label(span)
        .into()
}

pub fn omitting_the_parameter_name_in_a_function_definition_is_not_allowed(
    span: Span,
) -> miette::Report {
    MietteDiagnostic::new("Omitting the parameter name in a function definition is not allowed.")
        .with_label(span)
        .into()
}

pub fn void_must_be_the_first_and_only_parameter(span: Span) -> miette::Report {
    MietteDiagnostic::new("`void` must be the first and only parameter")
        .with_label(span)
        .into()
}
