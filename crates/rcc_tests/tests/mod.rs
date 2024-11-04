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

snapshot_tests![invalid_at_sign, valid_return_0, valid_return_2];
