use std::{
    borrow::Cow,
    fs::File,
    io::{self, BufWriter, Write},
};

use rcc_asm::{FunctionDeclaration, ImmOperand, Instruction, MovInstruction, Operand, Program};
use rcc_interner::Interner;

pub struct CodeEmit<'a> {
    interner: &'a Interner<'a>,
    writer: BufWriter<&'a File>,
}

impl<'a> CodeEmit<'a> {
    pub fn new(file: &'a File, interner: &'a Interner<'a>) -> CodeEmit<'a> {
        CodeEmit {
            interner,
            writer: BufWriter::new(file),
        }
    }

    pub fn emit(mut self, program: &Program) -> io::Result<()> {
        self.emit_program(program)
    }

    fn emit_program(&mut self, program: &Program) -> io::Result<()> {
        self.emit_decl_func(&program.func)?;

        #[cfg(target_os = "linux")]
        self.emit_gnu_stack_hardening()?;

        Ok(())
    }

    fn emit_decl_func(&mut self, decl: &FunctionDeclaration) -> io::Result<()> {
        let label = self.interner.get(decl.name.symbol);
        #[cfg(target_os = "macos")]
        let label = if id == "main" { "_main" } else { id };

        writeln!(self.writer, "    .global {}", label)?;
        self.emit_label(label)?;
        self.emit_instrs(&decl.instructions)?;

        Ok(())
    }

    fn emit_instrs(&mut self, instrs: &[Instruction]) -> io::Result<()> {
        for instr in instrs {
            self.emit_instr(instr)?;
        }

        Ok(())
    }

    fn emit_instr(&mut self, instr: &Instruction) -> io::Result<()> {
        match instr {
            Instruction::Mov(instr) => self.emit_instr_mov(instr),
            Instruction::Ret => self.emit_instr_ret(),
        }
    }

    fn emit_instr_mov(&mut self, instr: &MovInstruction) -> io::Result<()> {
        let src = format_operand(&instr.src);
        let dest = format_operand(&instr.dest);

        writeln!(self.writer, "    movl {}, {}", src, dest)?;

        Ok(())
    }

    fn emit_instr_ret(&mut self) -> io::Result<()> {
        writeln!(self.writer, "    ret")
    }

    fn emit_label(&mut self, label: &str) -> io::Result<()> {
        writeln!(self.writer, "{}:", label)
    }

    fn emit_gnu_stack_hardening(&mut self) -> io::Result<()> {
        writeln!(
            self.writer,
            "    .section .note.GNU-stack,\"\", @progbits\n"
        )
    }
}

fn format_operand(operand: &Operand) -> Cow<'_, str> {
    match operand {
        Operand::Imm(imm) => format_imm(imm),
        Operand::Register => format_reg(),
    }
}

fn format_imm(imm: &ImmOperand) -> Cow<'_, str> {
    let imm = format!("${}", imm.value);
    Cow::Owned(imm)
}

fn format_reg<'a>() -> Cow<'a, str> {
    Cow::Borrowed("%eax")
}
