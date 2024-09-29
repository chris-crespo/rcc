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

    pub fn jmp(&mut self, target: asm::Label) {
        let jmp = asm::Instruction::Jmp(asm::JmpInstruction { target });
        self.0.push(jmp)
    }

    pub fn cmp(&mut self, src: asm::Operand, dest: asm::Operand) {
        let cmp = asm::Instruction::Cmp(asm::CmpInstruction { src, dest });
        self.0.push(cmp)
    }

    pub fn cmp_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.cmp(asm::regs::r10(), dest);
        } else if dest.is_imm() {
            self.mov(dest, asm::regs::r11());
            self.cmp(src, asm::regs::r11());
        } else {
            self.cmp(src, dest);
        }
    }

    pub fn jmpcc(&mut self, code: asm::CondCode, target: asm::Label) {
        let jmpcc = asm::Instruction::JmpCC(asm::JmpCCInstruction { code, target });
        self.0.push(jmpcc)
    }

    #[inline(always)]
    pub fn jmpe(&mut self, target: asm::Label) {
        self.jmpcc(asm::CondCode::E, target)
    }

    #[inline(always)]
    pub fn jmpne(&mut self, target: asm::Label) {
        self.jmpcc(asm::CondCode::Ne, target)
    }

    pub fn label(&mut self, label: asm::Label) {
        let label = asm::Instruction::Label(label);
        self.0.push(label)
    }

    pub fn setcc(&mut self, code: asm::CondCode, src: asm::Operand) {
        let setcc = asm::Instruction::SetCC(asm::SetCCInstruction { code, src });
        self.0.push(setcc)
    }

    pub fn and(&mut self, src: asm::Operand, dest: asm::Operand) {
        let and = asm::Instruction::And(asm::AndInstruction { src, dest });
        self.0.push(and)
    }

    pub fn and_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.and(asm::regs::r10(), dest)
        } else {
            self.and(src, dest)
        }
    }

    pub fn or(&mut self, src: asm::Operand, dest: asm::Operand) {
        let or = asm::Instruction::Or(asm::OrInstruction { src, dest });
        self.0.push(or)
    }

    pub fn or_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.or(asm::regs::r10(), dest)
        } else {
            self.or(src, dest)
        }
    }

    pub fn xor(&mut self, src: asm::Operand, dest: asm::Operand) {
        let xor = asm::Instruction::Xor(asm::XorInstruction { src, dest });
        self.0.push(xor)
    }

    pub fn xor_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_mem_addr() && dest.is_mem_addr() {
            self.mov(src, asm::regs::r10());
            self.xor(asm::regs::r10(), dest)
        } else {
            self.xor(src, dest)
        }
    }

    pub fn shl(&mut self, src: asm::Operand, dest: asm::Operand) {
        let shl = asm::Instruction::Shl(asm::ShlInstruction { src, dest });
        self.0.push(shl)
    }

    pub fn shl_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_imm() {
            self.shl(src, dest)
        } else {
            self.mov(src, asm::regs::cl());
            self.shl(asm::regs::cl(), dest)
        }
    }

    pub fn shr(&mut self, src: asm::Operand, dest: asm::Operand) {
        let shr = asm::Instruction::Shr(asm::ShrInstruction { src, dest });
        self.0.push(shr)
    }

    pub fn shr_fixup(&mut self, src: asm::Operand, dest: asm::Operand) {
        if src.is_imm() {
            self.shr(src, dest)
        } else {
            self.mov(src, asm::regs::cl());
            self.shr(asm::regs::cl(), dest)
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
