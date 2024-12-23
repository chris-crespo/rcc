use std::fmt::Write;
use std::{path::PathBuf, process::Command};

use serde::Deserialize;

fn get_settings() -> insta::Settings {
    let mut settings = insta::Settings::clone_current();
    settings.remove_snapshot_suffix();
    settings.set_omit_expression(true);
    settings.set_prepend_module_to_snapshot(false);
    settings.set_snapshot_path("");

    settings
}

#[derive(Debug, Deserialize)]
struct Metadata {
    workspace_root: PathBuf,
    target_directory: PathBuf,
}

fn get_cargo_metadata() -> Metadata {
    let metadata = Command::new(
        std::env::var("CARGO")
            .ok()
            .unwrap_or_else(|| "cargo".into()),
    )
    .arg("metadata")
    .output()
    .unwrap();

    serde_json::from_slice(&metadata.stdout).unwrap()
}

fn get_cargo_bin(metadata: &Metadata, bin: &str) -> PathBuf {
    let target_dir = if cfg!(debug_assertions) {
        metadata.target_directory.join("debug")
    } else {
        metadata.target_directory.join("release")
    };

    target_dir.join(bin)
}

macro_rules! snapshot_tests {
    ($name: ident $(,)?) => {
        #[test]
        fn $name() {
            let settings = get_settings();

            let metadata = get_cargo_metadata();
            let cargo_bin = get_cargo_bin(&metadata, "rcc");

            let source_path = metadata
                .workspace_root
                .join("crates/rcc_tests/tests")
                .join(stringify!($name))
                .with_extension("c");

            let output = Command::new(&cargo_bin).arg(&source_path).output().unwrap();
            let stdout = std::str::from_utf8(&output.stdout).unwrap();

            let mut snapshot = String::new();
            writeln!(&mut snapshot, "source: {}.c", stringify!($name)).unwrap();

            if stdout.is_empty() {
                let status = Command::new(source_path.with_extension(""))
                    .output()
                    .unwrap()
                    .status;

                writeln!(&mut snapshot, "exit_status: {}", status.code().unwrap()).unwrap();

                std::fs::remove_file(source_path.with_extension("s")).unwrap();
                std::fs::remove_file(source_path.with_extension("")).unwrap();
            } else {
                writeln!(&mut snapshot, "compiler_output:").unwrap();
                writeln!(&mut snapshot, "{}", stdout).unwrap();
            }

            settings.bind(|| insta::assert_snapshot!(stringify!($name), snapshot))
        }
    };

    ($name: ident, $($names: ident),+ $(,)?) => {
        snapshot_tests!($name);
        snapshot_tests!($($names),+);
    };
}

snapshot_tests![
    invalid_at_sign, 
    invalid_bitwise_double_pipe,
    invalid_break_not_in_loop,
    invalid_case_outside_of_switch,
    invalid_continue_in_switch,
    invalid_compound_initializer,
    invalid_continue_not_in_loop,
    invalid_declaration_as_statement,
    invalid_declare_keyword_as_var,
    invalid_default_outside_switch,
    invalid_double_operation,
    invalid_duplicate_labels,
    invalid_duplicate_case_label,
    invalid_empty_if_body,
    invalid_extra_paren,
    invalid_function_name,
    invalid_goto_missing_label,
    invalid_goto_without_label,
    invalid_incomplete_ternary,
    invalid_increment_declaration,
    invalid_keyword_wrong_case,
    invalid_label_without_statement,
    invalid_labeled_declaration,
    invalid_lvalue,
    invalid_malformed_ternary,
    invalid_missing_const,
    invalid_missing_first_operand,
    invalid_missing_second_operand,
    invalid_missing_semicolon,
    invalid_missing_type,
    invalid_missplaced_semicolon,
    invalid_multiple_default_labels,
    invalid_nested_missing_const,
    invalid_non_constant_case,
    invalid_out_of_scope,
    invalid_parenthesize_operand,
    invalid_postfix_decr_non_lvalue,
    invalid_prefix_incr_non_lvalue,
    invalid_redefine,
    invalid_switch_missing_case_value,
    invalid_switch_missing_condition,
    invalid_switch_redefinitions,
    invalid_ternary_assign,
    invalid_type,
    invalid_unclosed_paren,
    invalid_undeclared_var,
    invalid_unfoldable_case_label,
    invalid_use_after_loop,
    invalid_var_case_label,
];

snapshot_tests![
    valid_add_variables,
    valid_and_false,
    valid_and_shortcircuit,
    valid_and_true,
    valid_assign,
    valid_assign_to_self,
    valid_assign_to_self_2,
    valid_assign_val_in_initializer,
    valid_assignment_lowest_precedence,
    valid_associativity,
    valid_associativity_2,
    valid_associativity_3,
    valid_associativity_and_precedence,
    valid_bitwise,
    valid_bitwise_and,
    valid_bitwise_and_precedence,
    valid_bitwise_or,
    valid_bitwise_or_precedence,
    valid_bitwise_precedence,
    valid_bitwise_shift_associativity,
    valid_bitwise_shift_precedence,
    valid_bitwise_shiftl,
    valid_bitwise_shiftr,
    valid_bitwise_shiftr_negative,
    valid_bitwise_xor,
    valid_bitwise_xor_precedence,
    valid_case_block,
    valid_compare_arithmetic_results,
    valid_compound_assignment_chain,
    valid_compound_bitwise_chain,
    valid_div,
    valid_div_negative,
    valid_duffs_device,
    valid_empty_blocks,
    valid_fibonacci_goto,
    valid_for_shadow,
    valid_do_while,
    valid_do_while_break_immediate,
    valid_goto_label_main,
    valid_goto_label_main_2,
    valid_hidden_then_visible,
    valid_if_else,
    valid_if_nested,
    valid_if_not_taken,
    valid_if_taken,
    valid_incr_expression_statement,
    valid_incr_in_binary_expression,
    valid_incr_parenthesized,
    valid_inner_uninitialized,
    valid_local_var_missing_return,
    valid_mod,
    valid_mul,
    valid_neg,
    valid_nested_ops,
    valid_nested_ops_2,
    valid_nested_ternary,
    valid_newlines,
    valid_no_newlines,
    valid_non_shortcircuit_or,
    valid_not,
    valid_not_zero,
    valid_null_statement,
    valid_or_false,
    valid_parens,
    valid_parens_2,
    valid_parens_3,
    valid_postfix_precedence,
    valid_redundant_parens,
    valid_relational_associativity,
    valid_return_0, 
    valid_return_2,
    valid_shortcircuit_or,
    valid_spaces,
    valid_sub,
    valid_sub_negative,
    valid_switch_break,
    valid_switch_default,
    valid_switch_default_fallthrough,
    valid_switch_goto_midcase,
    valid_switch_in_loop,
    valid_switch_nested_cases,
    valid_switch_nested_not_taken,
    valid_switch_nested_switch,
    valid_switch_no_case,
    valid_tabs,
    valid_ternary_shortcircuit,
    valid_unop_add,
    valid_unop_parens,
    valid_use_in_inner_scope,
    valid_while
];
