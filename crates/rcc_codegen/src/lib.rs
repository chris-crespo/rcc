use std::collections::HashMap;

use rcc_asm as asm;
use rcc_tac::{self as tac, UnaryInstruction};

struct CodegenContext {
    instrs: Vec<asm::Instruction>,
    stack_offsets: HashMap<tac::Variable, u32>,
}

impl CodegenContext {
    fn new() -> CodegenContext {
        CodegenContext {
            instrs: Vec::new(),
            stack_offsets: HashMap::new(),
        }
    }

    fn take_instrs(&mut self) -> Vec<asm::Instruction> {
        std::mem::take(&mut self.instrs)
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

    let name = codegen_id(&decl.name);
    let stack_size = ctx.stack_size();
    let instrs = ctx.take_instrs();

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
        tac::Instruction::Unary(instr) => codegen_unary_instr(ctx, instr),
    }
}

fn codegen_return_instr(ctx: &mut CodegenContext, instr: &tac::ReturnInstruction) {
    let src = codegen_value(ctx, &instr.value);
    let mov = asm::Instruction::Mov(asm::MovInstruction {
        src,
        dest: asm::Operand::Register(asm::RegisterOperand::Ax),
    });
    let ret = asm::Instruction::Ret;

    ctx.instrs.push(mov);
    ctx.instrs.push(ret);
}

fn codegen_unary_instr(ctx: &mut CodegenContext, instr: &tac::UnaryInstruction) {
    let src = codegen_value(ctx, &instr.src);
    let dest = if matches!(src, asm::Operand::Imm(_)) {
        let dest = codegen_variable(ctx, &instr.dest);
        let mov = asm::Instruction::Mov(asm::MovInstruction { src, dest });

        ctx.instrs.push(mov);

        dest
    } else {
        let mov = asm::Instruction::Mov(asm::MovInstruction {
            src,
            dest: asm::Operand::Register(asm::RegisterOperand::R10),
        });

        ctx.instrs.push(mov);

        let dest = codegen_variable(ctx, &instr.dest);
        let mov2 = asm::Instruction::Mov(asm::MovInstruction {
            src: asm::Operand::Register(asm::RegisterOperand::R10),
            dest,
        });

        ctx.instrs.push(mov2);

        dest
    };

    let unary = match instr.op {
        tac::UnaryOperator::Negation => asm::Instruction::Neg(asm::NegInstruction { dest }),
        tac::UnaryOperator::BitwiseComplement => {
            asm::Instruction::Not(asm::NotInstruction { dest })
        }
    };

    ctx.instrs.push(unary);
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

fn codegen_id(id: &tac::Identifier) -> asm::Label {
    asm::Label { symbol: id.symbol }
}
