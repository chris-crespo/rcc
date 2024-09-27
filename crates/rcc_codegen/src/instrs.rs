use rcc_asm as asm;

pub struct Instrs(Vec<asm::Instruction>);

impl Instrs {
    pub fn new() -> Instrs {
        Instrs(Vec::new())
    }

    pub fn take(&mut self) -> Vec<asm::Instruction> {
        std::mem::take(&mut self.0)
    }

    pub fn mov(&mut self, src: asm::Operand, dest: asm::Operand) {
        let mov = asm::Instruction::Mov(asm::MovInstruction { src, dest });
        self.0.push(mov)
    }

    pub fn mov_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.mov(asm::regs::r10(), dest);
        } else {
            self.mov(src, dest)
        }
    }

    pub fn neg(&mut self, dest: asm::Operand) {
        let neg = asm::Instruction::Neg(asm::NegInstruction { dest });
        self.0.push(neg);
    }

    pub fn not(&mut self, dest: asm::Operand) {
        let not = asm::Instruction::Not(asm::NotInstruction { dest });
        self.0.push(not);
    }

    pub fn add(&mut self, src: asm::Operand, dest: asm::Operand) {
        let add = asm::Instruction::Add(asm::AddInstruction { src, dest });
        self.0.push(add)
    }

    pub fn add_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.add(asm::regs::r10(), dest)
        } else {
            self.add(src, dest)
        }
    }

    pub fn sub(&mut self, src: asm::Operand, dest: asm::Operand) {
        let sub = asm::Instruction::Sub(asm::SubInstruction { src, dest });
        self.0.push(sub)
    }

    pub fn sub_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.sub(asm::regs::r10(), dest)
        } else {
            self.sub(src, dest)
        }
    }

    pub fn mul(&mut self, src: asm::Operand, dest: asm::Operand) {
        let mul = asm::Instruction::Mul(asm::MulInstruction { src, dest });
        self.0.push(mul)
    }

    pub fn mul_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if dest.is_mem_addr() {
            self.mov(dest, asm::regs::r11());
            self.mul(src, asm::regs::r11());
            self.mov(asm::regs::r11(), dest);
        } else {
            self.mul(src, dest)
        }
    }

    pub fn idiv(&mut self, src: asm::Operand) {
        let idiv = asm::Instruction::Idiv(asm::IdivInstruction { src });
        self.0.push(idiv)
    }

    pub fn idiv_fixup(&mut self, src: asm::Operand) {
        if src.is_imm() {
            self.mov(src, asm::regs::r10());
            self.idiv(asm::regs::r10())
        } else {
            self.idiv(src);
        }
    }

    pub fn cdq(&mut self) {
        let cdq = asm::Instruction::Cdq;
        self.0.push(cdq)
    }

    pub fn ret(&mut self) {
        let ret = asm::Instruction::Ret;
        self.0.push(ret)
    }
}
