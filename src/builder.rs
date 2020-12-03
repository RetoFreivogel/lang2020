use crate::module::Symbol;
use std::fmt;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block{
    pub then_block: usize,
    pub else_block: usize,
    pub name: String,
    pub start_instr: usize,
    pub n_instr: usize,
}

impl Block{
    fn new(start_instr: usize, name: String) -> Block{
        Block{
            then_block: 0,
            else_block: 0,
            name,
            start_instr,
            n_instr: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction{
    Load(Symbol),
    Store(Symbol),
    Const(Symbol),
    Call(Symbol),
    Check,
    And,
    Or,
    Not,
    Eq,
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
impl Instruction{
    fn print(&self, f: &mut String) -> fmt::Result{
        use Instruction::*;
        match self{
            Load(sym)   => write!(f, "{} ", sym),
            Store(sym)  => write!(f, "!{} ", sym),
            Const(sym)  => write!(f, "{} ", sym),
            Call(sym)   => write!(f, "({}) ", sym),
            Check       => write!(f, "? "),
            And         => write!(f, "& "),
            Or          => write!(f, "| "),
            Not         => write!(f, "! "),
            Eq          => write!(f, "== "), 
            Lt          => write!(f, "< "),
            Gt          => write!(f, "> "), 
            Le          => write!(f, "<= "),
            Ge          => write!(f, ">= "),
            Add         => write!(f, "+ "),
            Sub         => write!(f, "- "),
            Mul         => write!(f, "* "),
            Div         => write!(f, "/ "),
            Mod         => write!(f, "% "),
            Neg         => write!(f, "0- "),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Builder{
    blocks: Vec<Block>,
    instructions: Vec<Instruction>,
    current_function: usize,
    current_block: usize,
}
impl Builder{
    pub fn print(&self, out: &mut String) -> fmt::Result{
        writeln!(out, "BLOCKS")?;
        for b in &self.blocks{
            write!(out, "{:>16}  {:02}/{:02}  [ ", b.name, b.then_block, b.else_block)?;
            for instr in &self.instructions[b.start_instr..b.start_instr+b.n_instr]{
                instr.print(out)?;
            }
            writeln!(out, "]")?;
        }
        Ok(())      
    }
}
    
impl Builder{
//General-----------------------------------------
    pub fn new() -> Builder{
        Builder{
            blocks: Vec::new(),
            instructions: Vec::new(),
            current_function: 0,
            current_block: 0,
        }
    }

//Common----------------------------------------

    pub fn new_function(&mut self, name: String) -> usize{
        let b = Block::new(self.instructions.len(), name);
        let id = self.blocks.len();
        self.blocks.push(b);
        return id;
    }
    pub fn new_block(&mut self) -> usize{
        let b = Block::new(self.instructions.len(), String::new());
        let id = self.blocks.len();
        self.blocks.push(b);
        return id;
    }
    pub fn select(&mut self, id: usize){
        self.current_block = id;
        let b = &mut self.blocks[self.current_block];
        if b.n_instr == 0{
            b.start_instr = self.instructions.len();
        }else{
            panic!("Cannot write to already written block");
        }    
    }

    fn append_instruction(&mut self, i: Instruction){
        self.instructions.push(i);
        self.blocks[self.current_block].n_instr += 1;
    }

//Control Flow----------------------------------------

    pub fn jump(&mut self, to: usize){
        self.blocks[self.current_block].then_block = to;
        self.blocks[self.current_block].else_block = to;
    }

    pub fn jump_if(&mut self, to: usize, else_to: usize){
        self.append_instruction(Instruction::Check);
        self.blocks[self.current_block].then_block = to;
        self.blocks[self.current_block].else_block = else_to;
    }


//Literals and Identifiers-----------------------------------
    pub fn load(&mut self, sym: Symbol){
        self.append_instruction(Instruction::Load(sym));
    }
    pub fn constant(&mut self, sym: Symbol){
        self.append_instruction(Instruction::Const(sym));
    }
    pub fn store(&mut self, sym: Symbol){
        self.append_instruction(Instruction::Store(sym));
    }

    pub fn call(&mut self, sym: Symbol){
        self.append_instruction(Instruction::Call(sym));
    }


//Binop----------------------------------------
    pub fn and(&mut self){
        self.append_instruction(Instruction::And);
    }
    pub fn or(&mut self){
        self.append_instruction(Instruction::Or);
    }
    pub fn not(&mut self){
        self.append_instruction(Instruction::Not);
    }
    pub fn comp_eq(&mut self){
        self.append_instruction(Instruction::Eq);
    }
    pub fn comp_lt(&mut self){
        self.append_instruction(Instruction::Lt);
    }
    pub fn comp_gt(&mut self){
        self.append_instruction(Instruction::Gt);
    }
    pub fn comp_le(&mut self){
        self.append_instruction(Instruction::Le);
    }
    pub fn comp_ge(&mut self){
        self.append_instruction(Instruction::Ge);
    }
    pub fn add(&mut self){
        self.append_instruction(Instruction::Add);
    }
    pub fn sub(&mut self){
        self.append_instruction(Instruction::Sub);
    }
    pub fn mul(&mut self){
        self.append_instruction(Instruction::Mul);
    }
    pub fn div(&mut self){
        self.append_instruction(Instruction::Div);
    }
    pub fn modulo(&mut self){
        self.append_instruction(Instruction::Mod);
    }
    pub fn negate(&mut self){
        self.append_instruction(Instruction::Neg);
    }
}
