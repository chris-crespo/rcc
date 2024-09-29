use std::collections::HashMap;

use instrs::Instrs;
use rcc_asm as asm;
use rcc_tac as tac;

mod instrs;

struct CodegenContext {
    instrs: Instrs,
    stack_offsets: HashMap<tac::Variable, u32>,
}

impl CodegenContext {
    fn new() -> CodegenContext {
        CodegenContext {
            instrs: Instrs::new(),
            stack_offsets: HashMap::new(),
        }
    }

    fn stack_offset(&mut self, var: tac::Variable) -> asm::StackOperand {
        if let Some(&offset) = self.stack_offsets.get(&var) {
            return asm::StackOperand { offset };
        }

        let offset = self.stack_size();
        self.stack_offsets.insert(var, offset);

        asm::StackOperand { offset }
    }

    fn stack_size(&self) -> u32 {
        (self.stack_offsets.len() as u32 + 1) * 4
    }
}

pub fn codegen(program: &tac::Program) -> asm::Program {
    let mut ctx = CodegenContext::new();
    codegen_program(&mut ctx, program)
}

fn codegen_program(ctx: &mut CodegenContext, program: &tac::Program) -> asm::Program {
    let func = codegen_decl_func(ctx, &program.func);
    asm::Program { func }
}

fn codegen_decl_func(
    ctx: &mut CodegenContext,
    decl: &tac::FunctionDeclaration,
) -> asm::FunctionDeclaration {
    codegen_instrs(ctx, &decl.body);

    let name = codegen_id(decl.name);
    let stack_size = ctx.stack_size();
    let instrs = ctx.instrs.take();

    asm::FunctionDeclaration {
        name,
        stack_size,
        instructions: instrs,
    }
}

fn codegen_instrs(ctx: &mut CodegenContext, instrs: &[tac::Instruction]) {
    for instr in instrs {
        codegen_instr(ctx, instr);
    }
}

fn codegen_instr(ctx: &mut CodegenContext, instr: &tac::Instruction) {
    match instr {
        tac::Instruction::Return(instr) => codegen_return_instr(ctx, instr),
        tac::Instruction::Binary(instr) => codegen_binary_instr(ctx, instr),
        tac::Instruction::Unary(instr) => codegen_unary_instr(ctx, instr),
        tac::Instruction::Copy(instr) => codegen_copy_instr(ctx, instr),
        tac::Instruction::Jump(instr) => codegen_jmp_instr(ctx, instr),
        tac::Instruction::JumpIfZero(instr) => codegen_jmpz_instr(ctx, instr),
        tac::Instruction::JumpIfNotZero(instr) => codegen_jmpnz_instr(ctx, instr),
        tac::Instruction::Label(label) => codegen_label_instr(ctx, *label),
    }
}

fn codegen_return_instr(ctx: &mut CodegenContext, instr: &tac::ReturnInstruction) {
    let src = codegen_value(ctx, &instr.value);
    ctx.instrs.mov(src, asm::regs::ax());
    ctx.instrs.ret();
}

fn codegen_binary_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    match instr.op {
        tac::BinaryOperator::Add => codegen_add_instr(ctx, instr),
        tac::BinaryOperator::Substract => codegen_sub_instr(ctx, instr),
        tac::BinaryOperator::Multiply => codegen_mul_instr(ctx, instr),
        tac::BinaryOperator::Divide => codegen_div_instr(ctx, instr),
        tac::BinaryOperator::Remainder => codegen_rem_instr(ctx, instr),
        tac::BinaryOperator::BitwiseAnd => codegen_and_instr(ctx, instr),
        tac::BinaryOperator::BitwiseOr => codegen_or_instr(ctx, instr),
        tac::BinaryOperator::BitwiseXor => codegen_xor_instr(ctx, instr),
        tac::BinaryOperator::LeftShift => codegen_shl_instr(ctx, instr),
        tac::BinaryOperator::RightShift => codegen_sar_instr(ctx, instr),
        tac::BinaryOperator::Equal => codegen_eq_instr(ctx, instr),
        tac::BinaryOperator::NotEqual => codegen_neq_instr(ctx, instr),
        tac::BinaryOperator::LessThan => codegen_lt_instr(ctx, instr),
        tac::BinaryOperator::LessThanEqual => codegen_lte_instr(ctx, instr),
        tac::BinaryOperator::GreaterThan => codegen_gt_instr(ctx, instr),
        tac::BinaryOperator::GreaterThanEqual => codegen_gte_instr(ctx, instr),
    }
}

fn codegen_add_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.add_fixup(rhs, dest);
}

fn codegen_sub_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.sub_fixup(rhs, dest);
}

fn codegen_mul_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.mul_fixup(rhs, dest);
}

fn codegen_div_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    ctx.instrs.mov(lhs, asm::regs::ax());
    ctx.instrs.cdq();

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.idiv_fixup(rhs);

    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov(asm::regs::ax(), dest);
}

fn codegen_rem_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    ctx.instrs.mov(lhs, asm::regs::ax());
    ctx.instrs.cdq();

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.idiv_fixup(rhs);

    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov(asm::regs::dx(), dest);
}

fn codegen_and_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.and_fixup(rhs, dest);
}

fn codegen_or_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.or_fixup(rhs, dest);
}

fn codegen_xor_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.xor_fixup(rhs, dest);
}

fn codegen_shl_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.shl_fixup(rhs, dest);
}

fn codegen_sar_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(lhs, dest);

    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.sar_fixup(rhs, dest);
}

fn codegen_relational_instr(
    ctx: &mut CodegenContext,
    cc: asm::CondCode,
    instr: &tac::BinaryInstruction,
) {
    let lhs = codegen_value(ctx, &instr.lhs);
    let rhs = codegen_value(ctx, &instr.rhs);
    ctx.instrs.cmp_fixup(rhs, lhs);

    let zero = asm::Operand::Imm(asm::ImmOperand { value: 0 });
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov(zero, dest);

    ctx.instrs.setcc(cc, dest)
}

#[inline(always)]
fn codegen_eq_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    codegen_relational_instr(ctx, asm::CondCode::E, instr)
}

#[inline(always)]
fn codegen_neq_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    codegen_relational_instr(ctx, asm::CondCode::Ne, instr)
}

#[inline(always)]
fn codegen_lt_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    codegen_relational_instr(ctx, asm::CondCode::L, instr)
}

#[inline(always)]
fn codegen_lte_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    codegen_relational_instr(ctx, asm::CondCode::Le, instr)
}

#[inline(always)]
fn codegen_gt_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    codegen_relational_instr(ctx, asm::CondCode::G, instr)
}

#[inline(always)]
fn codegen_gte_instr(ctx: &mut CodegenContext, instr: &tac::BinaryInstruction) {
    codegen_relational_instr(ctx, asm::CondCode::Ge, instr)
}

fn codegen_unary_instr(ctx: &mut CodegenContext, instr: &tac::UnaryInstruction) {
    match instr.op {
        tac::UnaryOperator::BitwiseComplement => codegen_bitwise_compl_instr(ctx, instr),
        tac::UnaryOperator::Negation => codegen_neg_instr(ctx, instr),
        tac::UnaryOperator::Not => codegen_not_instr(ctx, instr),
    }
}

fn codegen_bitwise_compl_instr(ctx: &mut CodegenContext, instr: &tac::UnaryInstruction) {
    let src = codegen_value(ctx, &instr.src);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(src, dest);
    ctx.instrs.not(dest);
}

fn codegen_neg_instr(ctx: &mut CodegenContext, instr: &tac::UnaryInstruction) {
    let src = codegen_value(ctx, &instr.src);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov_fixup(src, dest);
    ctx.instrs.neg(dest);
}

fn codegen_not_instr(ctx: &mut CodegenContext, instr: &tac::UnaryInstruction) {
    let zero = asm::Operand::Imm(asm::ImmOperand { value: 0 });

    let src = codegen_value(ctx, &instr.src);
    ctx.instrs.cmp_fixup(zero, src);

    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov(zero, dest);

    ctx.instrs.setcc(asm::CondCode::E, dest);
}

fn codegen_copy_instr(ctx: &mut CodegenContext, instr: &tac::CopyInstruction) {
    let src = codegen_value(ctx, &instr.src);
    let dest = codegen_variable(ctx, &instr.dest);
    ctx.instrs.mov(src, dest);
}

fn codegen_jmp_instr(ctx: &mut CodegenContext, instr: &tac::JumpInstruction) {
    let target = codegen_label(instr.target);
    ctx.instrs.jmp(target)
}

fn codegen_jmpz_instr(ctx: &mut CodegenContext, instr: &tac::JumpIfZeroInstruction) {
    let zero = asm::Operand::Imm(asm::ImmOperand { value: 0 });
    let value = codegen_value(ctx, &instr.value);
    ctx.instrs.cmp_fixup(zero, value);

    let target = codegen_label(instr.target);
    ctx.instrs.jmpe(target);
}

fn codegen_jmpnz_instr(ctx: &mut CodegenContext, instr: &tac::JumpIfNotZeroInstruction) {
    let zero = asm::Operand::Imm(asm::ImmOperand { value: 0 });
    let value = codegen_value(ctx, &instr.value);
    ctx.instrs.cmp_fixup(zero, value);

    let target = codegen_label(instr.target);
    ctx.instrs.jmpne(target);
}

fn codegen_label_instr(ctx: &mut CodegenContext, label: tac::Label) {
    let label = codegen_label(label);
    ctx.instrs.label(label);
}

fn codegen_value(ctx: &mut CodegenContext, value: &tac::Value) -> asm::Operand {
    match value {
        tac::Value::Constant(value) => codegen_constant(value),
        tac::Value::Variable(value) => codegen_variable(ctx, value),
    }
}

fn codegen_constant(constant: &tac::Constant) -> asm::Operand {
    let imm_op = asm::ImmOperand {
        value: constant.value,
    };
    asm::Operand::Imm(imm_op)
}

fn codegen_variable(ctx: &mut CodegenContext, var: &tac::Variable) -> asm::Operand {
    let stack_op = ctx.stack_offset(*var);
    asm::Operand::Stack(stack_op)
}

fn codegen_id(id: tac::Identifier) -> asm::Label {
    asm::Label::Named(asm::NamedLabel { symbol: id.symbol })
}

fn codegen_label(label: tac::Label) -> asm::Label {
    asm::Label::Unnamed(asm::UnnamedLabel { id: label.id })
}
