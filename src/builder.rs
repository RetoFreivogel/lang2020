use crate::module::Ident;
use std::fmt::Write;

use crate::module::Symbol;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub then_block: usize,
    pub else_block: usize,
    pub name: Option<Ident>,
    pub start_instr: usize,
    pub n_instr: usize,
}

impl Block {
    fn new(start_instr: usize, name: Option<Ident>) -> Block {
        Block {
            then_block: 0,
            else_block: 0,
            name,
            start_instr,
            n_instr: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Load(Symbol),
    Store(Symbol),
    Const(usize),
    Call(Symbol),
    //    ICall(Type),
    //    Check,
    //    And,
    //    Or,
    //    Not,
    Enter(usize, usize),
    Ret,
    Jmp,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Load(sym) => write!(f, "{} ", sym),
            Store(sym) => write!(f, "!{} ", sym),
            Const(n) => write!(f, "{} ", n),
            Call(sym) => write!(f, "#{} ", sym),
            //            ICall(typ)  => write!(f, "#{} ", typ),
            //            Check       => write!(f, "? "),
            //            And         => write!(f, "& "),
            //            Or          => write!(f, "| "),
            //            Not         => write!(f, "! "),
            Enter(_, _) => write!(f, "v "),
            Ret => write!(f, "^ "),
            Jmp => write!(f, "-> "),
            Eq => write!(f, "== "),
            Neq => write!(f, "<> "),
            Lt => write!(f, "< "),
            Gt => write!(f, "> "),
            Le => write!(f, "<= "),
            Ge => write!(f, ">= "),
            Add => write!(f, "+ "),
            Sub => write!(f, "- "),
            Mul => write!(f, "* "),
            Div => write!(f, "/ "),
            Mod => write!(f, "% "),
            Neg => write!(f, "0- "),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Builder {
    blocks: Vec<Block>,
    instructions: Vec<Instruction>,
    current_function: usize,
    current_block: usize,
}
impl fmt::Display for Builder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for b in &self.blocks {
            if b.name.is_some() {
                write!(f, "{:>16}  ", b.name.as_ref().unwrap())?;
            } else {
                write!(f, "{:>16}  ", "")?;
            }
            write!(f, "{:02}/{:02}  [ ", b.then_block, b.else_block)?;
            for instr in &self.instructions[b.start_instr..b.start_instr + b.n_instr] {
                write!(f, "{}", instr)?;
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

/* asm level
push R;
pop R;
mov R, val;
mov Mem, R;
call Id *;
label global Id;
ret;
j cond, Id;
add R, val;
sub R, val;
cmp R, val;
xor R;

mul R *;
div R *;
//multi instruction--------------
enter
leave

*/

impl Builder {
    //General-----------------------------------------
    pub fn new() -> Builder {
        Builder {
            blocks: Vec::new(),
            instructions: Vec::new(),
            current_function: 0,
            current_block: 0,
        }
    }

    pub fn out_asm(&self) -> Result<String, fmt::Error> {
        let mut out = String::new();
        writeln!(out, "section .text")?;
        let mut reg_n = 0;
        const REGS: [&str; 9] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9", "rax", "r10", "r11"];
        for (i, block) in self.blocks.iter().enumerate() {
            if block.name.is_some() {
                reg_n = 0;
                writeln!(out, "global {0}\n {0}:", block.name.as_ref().unwrap())?;
            }
            writeln!(out, ".{}:", i)?;
            for instr in &self.instructions[block.start_instr..block.start_instr + block.n_instr] {
                println!("{:?}, {}", instr, reg_n);
                match instr {
                    Instruction::Load(Symbol::Var { pos, .. })
                    | Instruction::Load(Symbol::Arg { pos, .. }) => {
                        writeln!(out, "\tmov {}, [rbp-{}] ", REGS[reg_n], pos * 8)?;
                        reg_n += 1;
                    }

                    Instruction::Load(_) => {
                        panic!("Wrong Symbol kind");
                    }
                    Instruction::Store(Symbol::Var { pos, .. })
                    | Instruction::Store(Symbol::Arg { pos, .. }) => {
                        reg_n -= 1;
                        writeln!(out, "\tmov [rbp-{}], {} ", pos * 8, REGS[reg_n])?;
                    }
                    Instruction::Store(_) => {
                        panic!("Wrong Symbol kind");
                    }
                    Instruction::Const(n) => {
                        writeln!(out, "\tmov {}, {}", REGS[reg_n], n)?;
                        reg_n += 1;
                    }
                    Instruction::Call(Symbol::Func { id, typ, .. }) => {
                        let arg_n = typ.args.len();
                        let save_n = reg_n - arg_n;
                        for i in 0..save_n {
                            writeln!(out, "\tpush {}", REGS[i])?; // save unused registers
                        }
                        if save_n > 0 {
                            for i in 0..arg_n {
                                writeln!(out, "\tmov {}, {}", REGS[i], REGS[i + save_n])?;
                            }
                        }
                        writeln!(out, "\tcall {}", id)?;
                        writeln!(out, "\tmov {}, rax", REGS[save_n])?;
                        for i in (0..save_n).rev() {
                            writeln!(out, "\tpop {}", REGS[i])?; // restore unused registers
                        }
                        reg_n = save_n + 1;
                    }
                    Instruction::Call(_) => {
                        panic!("Wrong Symbol kind");
                    }
                    Instruction::Enter(n_args, n_vars) => {
                        writeln!(out, "\tpush rbp")?;
                        writeln!(out, "\tmov rbp, rsp")?;
                        for i in 0..*n_args {
                            writeln!(out, "\tpush {}", REGS[i])?;
                        }
                        writeln!(out, "\tsub rsp, {}", n_vars * 8)?;
                    }
                    Instruction::Ret => {
                        reg_n -= 1;
                        writeln!(out, "\tmov rax, {}", REGS[reg_n])?;
                        writeln!(out, "\tmov rsp, rbp")?;
                        writeln!(out, "\tpop rbp")?;
                        writeln!(out, "\tret\n")?;
                    }
                    Instruction::Jmp => {
                        writeln!(out, "\tjmp .{}", block.then_block)?;
                    }
                    Instruction::Eq => {
                        writeln!(out, "\tcmp {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        writeln!(out, "\tje .{}", block.then_block)?;
                        writeln!(out, "\tjmp .{}", block.else_block)?;
                        reg_n -= 2;
                    }
                    Instruction::Neq => {
                        writeln!(out, "\tcmp {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        writeln!(out, "\tjne .{}", block.then_block)?;
                        writeln!(out, "\tjmp .{}", block.else_block)?;
                        reg_n -= 2;
                    }
                    Instruction::Lt => {
                        writeln!(out, "\tcmp {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        writeln!(out, "\tjb .{}", block.then_block)?;
                        writeln!(out, "\tjmp .{}", block.else_block)?;
                        reg_n -= 2;
                    }
                    Instruction::Gt => {
                        writeln!(out, "\tcmp {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        writeln!(out, "\tja .{}", block.then_block)?;
                        writeln!(out, "\tjmp .{}", block.else_block)?;
                        reg_n -= 2;
                    }
                    Instruction::Le => {
                        writeln!(out, "\tcmp {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        writeln!(out, "\tjbe .{}", block.then_block)?;
                        writeln!(out, "\tjmp .{}", block.else_block)?;
                        reg_n -= 2;
                    }
                    Instruction::Ge => {
                        writeln!(out, "\tcmp {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        writeln!(out, "\tjae .{}", block.then_block)?;
                        writeln!(out, "\tjmp .{}", block.else_block)?;
                        reg_n -= 2;
                    }
                    Instruction::Add => {
                        writeln!(out, "\tadd {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        reg_n -= 1;
                    }
                    Instruction::Sub => {
                        writeln!(out, "\tsub {}, {}", REGS[reg_n - 2], REGS[reg_n - 1])?;
                        reg_n -= 1;
                    }
                    Instruction::Neg => {
                        writeln!(out, "\tneg {}", REGS[reg_n - 1])?;
                    }
                    Instruction::Mul => {
                        writeln!(out, "\tpush rax")?;
                        writeln!(out, "\tpush rdx")?;
                        writeln!(out, "\tmov rax, {}", REGS[reg_n - 2])?;
                        writeln!(out, "\tmul {}", REGS[reg_n - 1])?;
                        writeln!(out, "\tmov {}, rax", REGS[reg_n - 2])?;
                        writeln!(out, "\tpop rdx")?;
                        writeln!(out, "\tpop rax")?;
                        reg_n -= 1;
                    }
                    Instruction::Div => {
                        writeln!(out, "\tpush rax")?;
                        writeln!(out, "\tpush rdx")?;
                        writeln!(out, "\txor rdx, rdx")?;
                        writeln!(out, "\tmov rax, {}", REGS[reg_n - 2])?;
                        writeln!(out, "\tdiv {}", REGS[reg_n - 1])?;
                        writeln!(out, "\tmov {}, rax", REGS[reg_n - 2])?;
                        writeln!(out, "\tpop rdx")?;
                        writeln!(out, "\tpop rax")?;
                        reg_n -= 1;
                    }
                    Instruction::Mod => {
                        writeln!(out, "\tpush rax")?;
                        writeln!(out, "\tpush rdx")?;
                        writeln!(out, "\txor rdx, rdx")?;
                        writeln!(out, "\tmov rax, {}", REGS[reg_n - 2])?;
                        writeln!(out, "\tdiv {}", REGS[reg_n - 1])?;
                        writeln!(out, "\tmov {}, rdx", REGS[reg_n - 2])?;
                        writeln!(out, "\tpop rdx")?;
                        writeln!(out, "\tpop rax")?;
                        reg_n -= 1;
                    } /*
                                          Check       => write!(f, "? "),
                                          And         => write!(f, "& "),
                                          Or          => write!(f, "| "),
                                          Not         => write!(f, "! "),
                      */
                }
            }
        }

        return Ok(out);
    }

    //Common----------------------------------------

    pub fn new_function(&mut self, id: Ident) -> usize {
        let b = Block::new(self.instructions.len(), Some(id));
        let id = self.blocks.len();
        self.blocks.push(b);
        return id;
    }
    pub fn new_block(&mut self) -> usize {
        let b = Block::new(self.instructions.len(), None);
        let id = self.blocks.len();
        self.blocks.push(b);
        return id;
    }
    pub fn select(&mut self, id: usize) {
        self.current_block = id;
        let b = &mut self.blocks[self.current_block];
        if b.n_instr == 0 {
            b.start_instr = self.instructions.len();
        } else {
            panic!("Cannot write to already written block");
        }
    }

    fn append_instruction(&mut self, i: Instruction) {
        self.instructions.push(i);
        self.blocks[self.current_block].n_instr += 1;
    }

    //Control Flow----------------------------------------

    pub fn enter(&mut self, n_args: usize, n_vars: usize) {
        self.append_instruction(Instruction::Enter(n_args, n_vars));
    }

    pub fn ret(&mut self) {
        self.append_instruction(Instruction::Ret);
    }

    pub fn jump(&mut self, to: usize) {
        self.append_instruction(Instruction::Jmp);
        self.blocks[self.current_block].then_block = to;
        self.blocks[self.current_block].else_block = to;
    }

    pub fn jump_if(&mut self, to: usize, else_to: usize) {
        self.blocks[self.current_block].then_block = to;
        self.blocks[self.current_block].else_block = else_to;
    }

    //Literals and Identifiers-----------------------------------
    pub fn load(&mut self, sym: Symbol) {
        self.append_instruction(Instruction::Load(sym));
    }
    pub fn constant(&mut self, val: usize) {
        self.append_instruction(Instruction::Const(val));
    }
    pub fn store(&mut self, sym: Symbol) {
        self.append_instruction(Instruction::Store(sym));
    }

    pub fn call(&mut self, sym: Symbol) {
        self.append_instruction(Instruction::Call(sym));
    }

    /*
        pub fn icall(&mut self, typ: Type){
            self.append_instruction(Instruction::ICall(typ));
        }
    */

    //Binop----------------------------------------
    /*
        pub fn and(&mut self){
            self.append_instruction(Instruction::And);
        }
        pub fn or(&mut self){
            self.append_instruction(Instruction::Or);
        }
        pub fn not(&mut self){
            self.append_instruction(Instruction::Not);
        }
    */
    pub fn comp_eq(&mut self) {
        self.append_instruction(Instruction::Eq);
    }
    pub fn comp_neq(&mut self) {
        self.append_instruction(Instruction::Neq);
    }
    pub fn comp_lt(&mut self) {
        self.append_instruction(Instruction::Lt);
    }
    pub fn comp_gt(&mut self) {
        self.append_instruction(Instruction::Gt);
    }
    pub fn comp_le(&mut self) {
        self.append_instruction(Instruction::Le);
    }
    pub fn comp_ge(&mut self) {
        self.append_instruction(Instruction::Ge);
    }
    pub fn add(&mut self) {
        self.append_instruction(Instruction::Add);
    }
    pub fn sub(&mut self) {
        self.append_instruction(Instruction::Sub);
    }
    pub fn mul(&mut self) {
        self.append_instruction(Instruction::Mul);
    }
    pub fn div(&mut self) {
        self.append_instruction(Instruction::Div);
    }
    pub fn modulo(&mut self) {
        self.append_instruction(Instruction::Mod);
    }
    pub fn negate(&mut self) {
        self.append_instruction(Instruction::Neg);
    }
}
