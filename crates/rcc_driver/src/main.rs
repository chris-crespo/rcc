use std::{fs::File, process::Command};

use clap::Parser as _;
use rcc_ast_lowering::lower_to_tac;
use rcc_context::{AstArena, GlobalContext, TyArena};
use rcc_parser::Parser;

#[derive(Debug, clap::Parser)]
#[command(name = "rcc")]
#[command(about = "Rusty C Compiler")]
struct Cli {
    filename: String,

    #[clap(long, group = "option")]
    lex: bool,

    #[clap(long, group = "option")]
    parse: bool,

    #[clap(long, group = "option")]
    validate: bool,

    #[clap(long, group = "option")]
    tacky: bool,

    #[clap(long, group = "option")]
    codegen: bool,

    #[clap(short = 'S', group = "option")]
    compile: bool,
}

fn main() {
    let cli = Cli::parse();

    let ast_arena = AstArena::new();
    let ty_arena = TyArena::new();
    let mut gcx = GlobalContext::new(&ast_arena, &ty_arena);
    let source = spawn_preprocessor(&cli.filename).expect("Failed to preprocess file.");

    if cli.lex {
        return;
    }

    let parser = Parser::new(&mut gcx, &source);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };

    if cli.parse {
        println!("{:#?}", program);
        return;
    }

    let resolution_result = rcc_resolve::resolve(&gcx, &program);
    if !resolution_result.errors.is_empty() {
        for error in resolution_result.errors {
            println!("{:?}", error.with_source_code(source.clone()))
        }
    }

    if cli.validate {
        return;
    }

    let tac = lower_to_tac(&program);
    if cli.tacky {
        println!("{:#?}", tac);
        return;
    }

    let asm = rcc_codegen::codegen(&tac);
    if cli.codegen {
        println!("{:#?}", asm);
        return;
    }

    let file =
        File::create(cli.filename.replace(".c", ".s")).expect("Failed to create output file");
    rcc_codeemit::emit(&mut gcx, &asm, &file).expect("Failed to emit assembly.");

    assemble(
        &cli.filename.replace(".c", ".s"),
        cli.filename.trim_end_matches(".c"),
    )
    .expect("Failed to assemble program.");
}

fn spawn_preprocessor(filename: &str) -> std::io::Result<String> {
    let output = Command::new("gcc")
        .args(["-E", "-P", filename])
        .output()?
        .stdout;

    let s = String::from_utf8(output).expect("Output from `gcc` is not UTF8");
    Ok(s)
}

fn assemble(input: &str, output: &str) -> std::io::Result<()> {
    Command::new("gcc").args([input, "-o", output]).status()?;

    Ok(())
}
