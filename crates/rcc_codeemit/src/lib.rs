use std::{
    borrow::Cow,
    fs::File,
    io::{self, BufWriter, Write},
};

use rcc_asm::{
    AddInstruction, AndInstruction, CmpInstruction, CondCode, FunctionDeclaration, IdivInstruction,
    ImmOperand, Instruction, JmpCCInstruction, JmpInstruction, Label, MovInstruction,
    MulInstruction, NegInstruction, NotInstruction, Operand, OrInstruction, Program,
    RegisterOperand, SetCCInstruction, ShlInstruction, SarInstruction, StackOperand,
    SubInstruction, XorInstruction,
};
use rcc_interner::Interner;

struct EmitContext<'a, 'src> {
    interner: &'a Interner<'src>,
    output: BufWriter<&'a File>,
}

impl<'a, 'src> EmitContext<'a, 'src> {
    pub fn new(output: &'a File, interner: &'a Interner<'src>) -> EmitContext<'a, 'src> {
        EmitContext {
            output: BufWriter::new(output),
            interner,
        }
    }
}

pub fn emit<'a>(program: &Program, output: &'a File, interner: &'a mut Interner) -> io::Result<()> {
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
    let label = format_label(ctx, decl.name);
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
        Instruction::Cmp(instr) => emit_instr_cmp(ctx, instr),
        Instruction::Jmp(instr) => emit_instr_jmp(ctx, instr),
        Instruction::JmpCC(instr) => emit_instr_jmpcc(ctx, instr),
        Instruction::SetCC(instr) => emit_instr_setcc(ctx, instr),
        Instruction::Label(label) => emit_instr_label(ctx, *label),
        Instruction::And(instr) => emit_instr_and(ctx, instr),
        Instruction::Or(instr) => emit_instr_or(ctx, instr),
        Instruction::Xor(instr) => emit_instr_xor(ctx, instr),
        Instruction::Shl(instr) => emit_instr_shl(ctx, instr),
        Instruction::Sar(instr) => emit_instr_sar(ctx, instr),
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

fn emit_instr_cmp(ctx: &mut EmitContext, instr: &CmpInstruction) -> io::Result<()> {
    let src = format_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    cmpl {}, {}", src, dest)
}

fn emit_instr_jmp(ctx: &mut EmitContext, instr: &JmpInstruction) -> io::Result<()> {
    let target = format_label(ctx, instr.target);
    writeln!(ctx.output, "    jmp {}", target)
}

fn emit_instr_jmpcc(ctx: &mut EmitContext, instr: &JmpCCInstruction) -> io::Result<()> {
    let cc = format_cc(instr.code);
    let target = format_label(ctx, instr.target);
    writeln!(ctx.output, "    j{} {}", cc, target)
}

fn emit_instr_setcc(ctx: &mut EmitContext, instr: &SetCCInstruction) -> io::Result<()> {
    let cc = format_cc(instr.code);
    let operand = format_byte_operand(&instr.src);
    writeln!(ctx.output, "    set{} {}", cc, operand)
}

fn emit_instr_label(ctx: &mut EmitContext, label: Label) -> io::Result<()> {
    let label = format_label(ctx, label);
    emit_label(ctx, label)
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

fn emit_instr_sar(ctx: &mut EmitContext, instr: &SarInstruction) -> io::Result<()> {
    let src = format_byte_operand(&instr.src);
    let dest = format_operand(&instr.dest);
    writeln!(ctx.output, "    sarl {}, {}", src, dest)
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

fn emit_label(ctx: &mut EmitContext, label: Cow<'_, str>) -> io::Result<()> {
    writeln!(ctx.output, "{}:", label)
}

fn emit_gnu_stack_hardening(ctx: &mut EmitContext) -> io::Result<()> {
    writeln!(ctx.output, "    .section .note.GNU-stack,\"\", @progbits\n")
}

fn format_cc(cc: CondCode) -> &'static str {
    match cc {
        CondCode::E => "e",
        CondCode::Ne => "ne",
        CondCode::G => "g",
        CondCode::Ge => "ge",
        CondCode::L => "l",
        CondCode::Le => "le",
    }
}

fn format_label<'src>(ctx: &mut EmitContext<'_, 'src>, label: Label) -> Cow<'src, str> {
    match label {
        Label::Named(label) => Cow::Borrowed(ctx.interner.get(label.symbol)),
        Label::Unnamed(label) => Cow::Owned(format!(".L{}", label.id)),
    }
}

fn format_byte_operand(operand: &Operand) -> Cow<'_, str> {
    match operand {
        Operand::Imm(imm) => format_imm(imm),
        Operand::Register(reg) => format_byte_reg(reg),
        Operand::Stack(stack) => format_stack_operand(stack),
    }
}

fn format_byte_reg(register: &RegisterOperand) -> Cow<'static, str> {
    let s = match register {
        RegisterOperand::Ax => "%al",
        RegisterOperand::Cx => "%cl",
        RegisterOperand::Dx => "%dl",
        RegisterOperand::R10 => "%r10b",
        RegisterOperand::R11 => "%r11b",
    };

    Cow::Borrowed(s)
}

fn format_operand(operand: &Operand) -> Cow<'_, str> {
    match operand {
        Operand::Imm(imm) => format_imm(imm),
        Operand::Register(reg) => format_reg(reg),
        Operand::Stack(op) => format_stack_operand(op),
    }
}

fn format_imm(imm: &ImmOperand) -> Cow<'_, str> {
    let imm = format!("${}", imm.value);
    Cow::Owned(imm)
}

fn format_reg(register: &RegisterOperand) -> Cow<'static, str> {
    let s = match register {
        RegisterOperand::Ax => "%eax",
        RegisterOperand::Cx => "%ecx",
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
