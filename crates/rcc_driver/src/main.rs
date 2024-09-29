use std::{
    fs::File,
    process::Command,
};

use clap::Parser as _;
use rcc_arena::Arena;
use rcc_ast_lowering::lower_to_tac;
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

    #[clap(long, group = "option")]
    tacky: bool,
}

fn main() {
    let args = Cli::parse();

    if args.lex {
        lex(&args.filename);
    } else if args.parse {
        parse(&args.filename);
    } else if args.tacky {
        tacky(&args.filename)
    } else if args.codegen {
        codegen(&args.filename)
    } else if args.compile {
        compile(&args.filename)
    } else {
        compile(&args.filename);
        assemble(
            &args.filename.replace(".c", ".s"),
            args.filename.trim_end_matches(".c"),
        )
        .expect("Failed to assemble program.");
    }
}

fn lex(filename: &str) {
    let source = spawn_preprocessor(filename).expect("Failed to preprocess file.");
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
    let source = spawn_preprocessor(filename).expect("Failed to preprocess file.");

    let arena = Arena::new();
    let mut interner = Interner::new();

    let parser = Parser::new(&source, &arena, &mut interner);
    match parser.parse() {
        Ok(program) => println!("{:#?}", program),
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };
}

fn tacky(filename: &str) {
    let source = spawn_preprocessor(filename).expect("Failed to preprocess file.");

    let arena = Arena::new();
    let mut interner = Interner::new();

    let parser = Parser::new(&source, &arena, &mut interner);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };

    let tac = lower_to_tac(&program);
    println!("{:#?}", tac);
}

fn codegen(filename: &str) {
    let source = spawn_preprocessor(filename).expect("Failed to preprocess file.");

    let arena = Arena::new();
    let mut interner = Interner::new();

    let parser = Parser::new(&source, &arena, &mut interner);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };

    let tac = lower_to_tac(&program);
    let asm = rcc_codegen::codegen(&tac);
    println!("{:#?}", asm);
}

fn compile(filename: &str) {
    let source = spawn_preprocessor(filename).expect("Failed to preprocess file.");

    let arena = Arena::new();
    let mut interner = Interner::new();

    let parser = Parser::new(&source, &arena, &mut interner);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            println!("{:?}", err.with_source_code(source.clone()));
            std::process::exit(1)
        }
    };

    let tac = lower_to_tac(&program);
    let asm = rcc_codegen::codegen(&tac);

    let file = File::create(filename.replace(".c", ".s")).expect("Failed to create output file");
    rcc_codeemit::emit(&asm, &file, &mut interner).expect("Failed to emit assembly.");
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
