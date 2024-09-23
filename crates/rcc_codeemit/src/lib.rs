use std::{
    borrow::Cow,
    fs::File,
    io::{self, BufWriter, Write},
};

use rcc_asm::{FunctionDeclaration, ImmOperand, Instruction, MovInstruction, Operand, Program, RegisterOperand, StackOperand};
use rcc_interner::Interner;

struct EmitContext<'a> {
    interner: &'a mut Interner<'a>,
    output: BufWriter<&'a File>,
}

impl<'a> EmitContext<'a> {
    pub fn new(output: &'a File, interner: &'a mut Interner<'a>) -> EmitContext<'a> {
        EmitContext {
            output: BufWriter::new(output),
            interner,
        }
    }
}

pub fn emit<'a>(
    program: &Program,
    output: &'a File,
    interner: &'a mut Interner<'a>,
) -> io::Result<()> {
    let mut ctx = EmitContext::new(output, interner);
    emit_program(&mut ctx, program)
}

fn emit_program(ctx: &mut EmitContext, program: &Program) -> io::Result<()> {
    emit_decl_func(ctx, &program.func)?;

    #[cfg(target_os = "linux")]
    emit_gnu_stack_hardening(ctx)?;

    Ok(())
}

fn emit_decl_func(ctx: &mut EmitContext, decl: &FunctionDeclaration) -> io::Result<()> {
    let label = ctx.interner.get(decl.name.symbol);
    #[cfg(target_os = "macos")]
    let label = if id == "main" { "_main" } else { id };

    writeln!(ctx.output, "    .global {}", label)?;
    emit_label(ctx, label)?;
    emit_instrs(ctx, &decl.instructions)?;

    Ok(())
}

fn emit_instrs(ctx: &mut EmitContext, instrs: &[Instruction]) -> io::Result<()> {
    for instr in instrs {
        emit_instr(ctx, instr)?;
    }

    Ok(())
}

fn emit_instr(ctx: &mut EmitContext, instr: &Instruction) -> io::Result<()> {
    match instr {
        Instruction::Mov(instr) => emit_instr_mov(ctx, instr),
        Instruction::Ret => emit_instr_ret(ctx),
        instr => todo!("Emit {:?}", instr),
    }
}

fn emit_instr_mov(ctx: &mut EmitContext, instr: &MovInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);

    writeln!(ctx.output, "    movl {}, {}", src, dest)?;

    Ok(())
}

fn emit_instr_ret(ctx: &mut EmitContext) -> io::Result<()> {
    writeln!(ctx.output, "    ret")
}

fn emit_label(ctx: &mut EmitContext, label: &str) -> io::Result<()> {
    writeln!(ctx.output, "{}:", label)
}

fn emit_gnu_stack_hardening(ctx: &mut EmitContext) -> io::Result<()> {
    writeln!(ctx.output, "    .section .note.GNU-stack,\"\", @progbits\n")
}

fn format_operand(operand: &Operand) -> Cow<'_, str> {
    match operand {
        Operand::Imm(imm) => format_imm(imm),
        Operand::Register(reg) => format_reg(reg),
        Operand::Stack(op) => format_stack_operand(op)
    }
}

fn format_imm(imm: &ImmOperand) -> Cow<'_, str> {
    let imm = format!("${}", imm.value);
    Cow::Owned(imm)
}

fn format_reg<'a>(register: &RegisterOperand) -> Cow<'a, str> {
    let s = match register {
        RegisterOperand::Ax => "%eax",
        RegisterOperand::R10 => "%r10d",
    };

    Cow::Borrowed(s)
}

fn format_stack_operand(op: &StackOperand) -> Cow<'_, str> {
    let s = format!("-{}(%rbp)", op.offset);
    Cow::Owned(s)
}
