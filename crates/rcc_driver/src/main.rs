use std::process::{Child, Command, Stdio};

use clap::Parser;

#[derive(Debug, Parser)]
#[command(name = "rcc")]
#[command(about = "Rusty C Compiler")]
struct Cli {
    filename: String,

    #[clap(long, group = "option")]
    lex: bool,

    #[clap(long, group = "option")]
    parse: bool,

    #[clap(short = 'S', group = "option")]
    compile: bool,

    #[clap(long, group = "option")]
    codegen: bool,
}

fn main() {
    let args = Cli::parse();

    let preprocessor_output = spawn_preprocessor(&args.filename).expect("Failed to spawn preprocessor.");
    let compiler_output = spawn_compiler(preprocessor_output).expect("Failed to spawn compiler.");
    assemble(compiler_output).expect("Failed to spawn assembler");
}

fn spawn_preprocessor(filename: &str) -> std::io::Result<Child> {
    Command::new("gcc")
        .args(["-E", "-P", filename])
        .stdout(Stdio::piped())
        .spawn()
}

fn spawn_compiler(preprocessor_output: Child) -> std::io::Result<Child> {
    Command::new("gcc")
        .args([
            "-S",
            "-O",
            "-fno-asynchronous-unwind-tables",
            "-fcf-protection=none",
            "-x",
            "c",
            "-",
            "-o",
            "-",
        ])
        .stdin(preprocessor_output.stdout.unwrap())
        .stdout(Stdio::piped())
        .spawn()
}

fn assemble(compiler_output: Child) -> std::io::Result<()> {
    Command::new("gcc")
        .args(["-x", "assembler", "-", "-o", "output"])
        .stdin(compiler_output.stdout.unwrap())
        .status()?;

    Ok(())
}
