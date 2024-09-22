use std::{
    fs::File,
    process::{Child, Command, Stdio},
};

use clap::Parser as _;
use rcc_codeemit::CodeEmit;
use rcc_codegen::Codegen;
use rcc_interner::Interner;
use rcc_lexer::{Lexer, TokenKind};
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

    #[clap(short = 'S', group = "option")]
    compile: bool,

    #[clap(long, group = "option")]
    codegen: bool,
}

fn main() {
    let args = Cli::parse();

    if args.lex {
        lex(&args.filename);
    } else if args.parse {
        parse(&args.filename);
    } else if args.codegen {
        codegen(&args.filename)
    } else if args.compile {
        compile(&args.filename)
    } else {
        compile(&args.filename);
        assemble(
            &args.filename.replace(".c", ".s"),
            args.filename.trim_end_matches(".c"),
        ).expect("Failed to assemble program.");
    }
}

fn lex(filename: &str) {
    let source = std::fs::read_to_string(filename).expect("Failed to read file.");
    let mut lexer = Lexer::new(&source);
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::Eof {
            break;
        }

        // TODO: add better error reporting
        if token.kind == TokenKind::Undetermined {
            std::process::exit(1);
        }
    }
}

fn parse(filename: &str) {
    let source = std::fs::read_to_string(filename).expect("Failed to read file.");

    let mut interner = Interner::new();
    let parser = Parser::new(&source, &mut interner);
    match parser.parse() {
        Ok(program) => println!("{:#?}", program),
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    }
}

fn codegen(filename: &str) {
    let source = std::fs::read_to_string(filename).expect("Failed to read file.");

    let mut interner = Interner::new();
    let parser = Parser::new(&source, &mut interner);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };

    let codegen = Codegen::new();
    let asm = codegen.codegen(&program);
    println!("{:#?}", asm);
}

fn compile(filename: &str) {
    let source = std::fs::read_to_string(filename).expect("Failed to read file.");

    let mut interner = Interner::new();
    let parser = Parser::new(&source, &mut interner);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };

    let codegen = Codegen::new();
    let asm = codegen.codegen(&program);

    let file = File::create(filename.replace(".c", ".s")).expect("Failed to create output file");
    let codeemit = CodeEmit::new(&file, &interner);
    codeemit.emit(&asm).expect("Failed to emit assembly.");
}

fn spawn_preprocessor(filename: &str) -> std::io::Result<Child> {
    Command::new("gcc")
        .args(["-E", "-P", filename])
        .stdout(Stdio::piped())
        .spawn()
}

fn assemble(input: &str, output: &str) -> std::io::Result<()> {
    Command::new("gcc").args([input, "-o", output]).status()?;

    Ok(())
}
