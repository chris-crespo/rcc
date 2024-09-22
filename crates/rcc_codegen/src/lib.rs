use rcc_asm as asm;
use rcc_ast as ast;

pub struct Codegen {}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {}
    }

    pub fn codegen(self, program: &ast::Program) -> asm::Program {
        self.codegen_program(program)
    }

    fn codegen_program(&self, program: &ast::Program) -> asm::Program {
        let func = self.codegen_decl_func(&program.func);
        asm::Program { func }
    }

    fn codegen_decl_func(&self, decl: &ast::FunctionDeclaration) -> asm::FunctionDeclaration {
        let name = self.codegen_id(&decl.name);
        let instructions = self.codegen_stmt(&decl.stmt);

        asm::FunctionDeclaration { name, instructions }
    }

    fn codegen_stmt(&self, stmt: &ast::Statement) -> Vec<asm::Instruction> {
        match stmt {
            ast::Statement::Return(stmt) => self.codegen_stmt_return(stmt),
        }
    }

    fn codegen_stmt_return(&self, stmt: &ast::ReturnStatement) -> Vec<asm::Instruction> {
        let mut instrs = Vec::new();

        let mov = asm::MovInstruction {
            src: self.codegen_expr(&stmt.expr),
            dest: asm::Operand::Register,
        };

        instrs.push(asm::Instruction::Mov(mov));
        instrs.push(asm::Instruction::Ret);

        instrs
    }

    fn codegen_expr(&self, expr: &ast::Expression) -> asm::Operand {
        match expr {
            ast::Expression::NumberLiteral(lit) => self.codegen_lit_number(lit),
            ast::Expression::Unary(expr) => todo!()
        }
    }

    fn codegen_lit_number(&self, lit: &ast::NumberLiteral) -> asm::Operand {
        let imm = asm::ImmOperand { value: lit.value };
        asm::Operand::Imm(imm)
    }

    fn codegen_id(&self, id: &ast::Identifier) -> asm::Label {
        asm::Label { symbol: id.symbol }
    }
}
