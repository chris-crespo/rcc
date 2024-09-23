use rcc_asm as asm;
use rcc_ast as ast;

pub fn codegen(program: &ast::Program) -> asm::Program {
    codegen_program(program)
}

fn codegen_program(program: &ast::Program) -> asm::Program {
    let func = codegen_decl_func(&program.func);
    asm::Program { func }
}

fn codegen_decl_func(decl: &ast::FunctionDeclaration) -> asm::FunctionDeclaration {
    let name = codegen_id(&decl.name);
    let instructions = codegen_stmt(&decl.stmt);

    asm::FunctionDeclaration { name, instructions }
}

fn codegen_stmt(stmt: &ast::Statement) -> Vec<asm::Instruction> {
    match stmt {
        ast::Statement::Return(stmt) => codegen_stmt_return(stmt),
    }
}

fn codegen_stmt_return(stmt: &ast::ReturnStatement) -> Vec<asm::Instruction> {
    let mut instrs = Vec::new();

    let mov = asm::MovInstruction {
        src: codegen_expr(&stmt.expr),
        dest: asm::Operand::Register,
    };

    instrs.push(asm::Instruction::Mov(mov));
    instrs.push(asm::Instruction::Ret);

    instrs
}

fn codegen_expr(expr: &ast::Expression) -> asm::Operand {
    match expr {
        ast::Expression::NumberLiteral(lit) => codegen_lit_number(lit),
        ast::Expression::Unary(expr) => todo!(),
    }
}

fn codegen_lit_number(lit: &ast::NumberLiteral) -> asm::Operand {
    let imm = asm::ImmOperand { value: lit.value };
    asm::Operand::Imm(imm)
}

fn codegen_id(id: &ast::Identifier) -> asm::Label {
    asm::Label { symbol: id.symbol }
}
