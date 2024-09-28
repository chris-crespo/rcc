use rcc_tac as tac;

pub struct Instrs(Vec<tac::Instruction>);

impl Instrs {
    pub fn new() -> Instrs {
        Instrs(Vec::new())
    }

    #[inline(always)]
    pub fn take(&mut self) -> Vec<tac::Instruction> {
        std::mem::take(&mut self.0)
    }

    pub fn binary(
        &mut self,
        op: tac::BinaryOperator,
        lhs: tac::Value,
        rhs: tac::Value,
        dest: tac::Variable,
    ) {
        let binary_instr = tac::BinaryInstruction { op, lhs, rhs, dest };
        let instr = tac::Instruction::Binary(binary_instr);
        self.0.push(instr)
    }

    pub fn unary(&mut self, op: tac::UnaryOperator, src: tac::Value, dest: tac::Variable) {
        let unary_instr = tac::UnaryInstruction { op, src, dest };
        let instr = tac::Instruction::Unary(unary_instr);
        self.0.push(instr)
    }

    pub fn ret(&mut self, value: tac::Value) {
        let return_instr = tac::ReturnInstruction { value };
        let instr = tac::Instruction::Return(return_instr);
        self.0.push(instr)
    }

    pub fn jmp(&mut self, target: tac::Label) {
        let jmp_instr = tac::JumpInstruction { target };
        let instr_jmp = tac::Instruction::Jump(jmp_instr);
        self.0.push(instr_jmp);
    }

    pub fn jmpz(&mut self, value: tac::Value, target: tac::Label) {
        let jmpz_instr = tac::JumpIfZeroInstruction { value, target };
        let instr_jmpz = tac::Instruction::JumpIfZero(jmpz_instr);
        self.0.push(instr_jmpz);
    }

    pub fn jmpnz(&mut self, value: tac::Value, target: tac::Label) {
        let jmpnz_instr = tac::JumpIfNotZeroInstruction { value, target };
        let instr_jmpnz = tac::Instruction::JumpIfNotZero(jmpnz_instr);
        self.0.push(instr_jmpnz)
    }

    pub fn copy(&mut self, src: tac::Value, dest: tac::Variable) {
        let copy_instr = tac::CopyInstruction { src, dest };
        let instr_copy = tac::Instruction::Copy(copy_instr);
        self.0.push(instr_copy);
    }

    pub fn label(&mut self, label: tac::Label) {
        let label_instr = tac::Instruction::Label(label);
        self.0.push(label_instr);
    }
}
