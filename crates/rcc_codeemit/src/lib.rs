use std::{
    borrow::Cow,
    fs::File,
    io::{self, BufWriter, Write},
};

use rcc_asm::{AddInstruction, AndInstruction, FunctionDeclaration, IdivInstruction, ImmOperand, Instruction, MovInstruction, MulInstruction, NegInstruction, NotInstruction, Operand, OrInstruction, Program, RegisterOperand, ShlInstruction, ShrInstruction, StackOperand, SubInstruction, XorInstruction};
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
    emit_prelude(ctx, decl)?;
    emit_instrs(ctx, &decl.instructions)?;

    Ok(())
}

fn emit_prelude(ctx: &mut EmitContext, decl: &FunctionDeclaration) -> io::Result<()> {
    writeln!(ctx.output, "    pushq %rbp")?;
    writeln!(ctx.output, "    movq %rsp, %rbp")?;
    writeln!(ctx.output, "    subq ${}, %rsp", decl.stack_size)?;

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
        Instruction::Neg(instr) => emit_instr_neg(ctx, instr),
        Instruction::Not(instr) => emit_instr_not(ctx, instr),
        Instruction::Add(instr) => emit_instr_add(ctx, instr),
        Instruction::Sub(instr) => emit_instr_sub(ctx, instr),
        Instruction::Mul(instr) => emit_instr_mul(ctx, instr),
        Instruction::Idiv(instr) => emit_instr_idiv(ctx, instr),
        Instruction::And(instr) => emit_instr_and(ctx, instr),
        Instruction::Or(instr) => emit_instr_or(ctx, instr),
        Instruction::Xor(instr) => emit_instr_xor(ctx, instr),
        Instruction::Shl(instr) => emit_instr_shl(ctx, instr),
        Instruction::Shr(instr) => emit_instr_shr(ctx, instr),
        Instruction::Cdq => emit_instr_cdq(ctx),
        Instruction::Ret => emit_instr_ret(ctx),
    }
}

fn emit_instr_mov(ctx: &mut EmitContext, instr: &MovInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);

    writeln!(ctx.output, "    movl {}, {}", src, dest)?;

    Ok(())
}

fn emit_instr_neg(ctx: &mut EmitContext, instr: &NegInstruction) -> io::Result<()> {
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    negl {}", dest)
}

fn emit_instr_not(ctx: &mut EmitContext, instr: &NotInstruction) -> io::Result<()> {
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    notl {}", dest)
}

fn emit_instr_add(ctx: &mut EmitContext, instr: &AddInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    addl {}, {}", src, dest)
}

fn emit_instr_sub(ctx: &mut EmitContext, instr: &SubInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    subl {}, {}", src, dest)
}

fn emit_instr_mul(ctx: &mut EmitContext, instr: &MulInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    imull {}, {}", src, dest)
}

fn emit_instr_idiv(ctx: &mut EmitContext, instr: &IdivInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    writeln!(ctx.output, "    idivl {}", src)
}

fn emit_instr_and(ctx: &mut EmitContext, instr: &AndInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    andl {}, {}", src, dest)
}

fn emit_instr_or(ctx: &mut EmitContext, instr: &OrInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    orl {}, {}", src, dest)
}

fn emit_instr_xor(ctx: &mut EmitContext, instr: &XorInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    xorl {}, {}", src, dest)
}

fn emit_instr_shl(ctx: &mut EmitContext, instr: &ShlInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    shll {}, {}", src, dest)
}

fn emit_instr_shr(ctx: &mut EmitContext, instr: &ShrInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    shrl {}, {}", src, dest)
}

fn emit_instr_cdq(ctx: &mut EmitContext) -> io::Result<()> {
    writeln!(ctx.output, "    cdq")
}

fn emit_instr_ret(ctx: &mut EmitContext) -> io::Result<()> {
    writeln!(ctx.output, "    movq %rbp, %rsp")?;
    writeln!(ctx.output, "    popq %rbp")?;
    writeln!(ctx.output, "    ret")?;

    Ok(())
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
        RegisterOperand::Cl => "%cl",
        RegisterOperand::Dx => "%edx",
        RegisterOperand::R10 => "%r10d",
        RegisterOperand::R11 => "%r11d",
    };

    Cow::Borrowed(s)
}

fn format_stack_operand(op: &StackOperand) -> Cow<'_, str> {
    let s = format!("-{}(%rbp)", op.offset);
    Cow::Owned(s)
}
