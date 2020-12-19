use std::fmt;
use crate::strtab::Ident;

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Function>,
}
impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in &self.items {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Function {
    pub id: Ident,
    pub typ: Type,
    blocks: Vec<Block>,
}
impl Function{
    fn new(id: Ident, typ: Type) -> Function{
        Function{
            id,
            typ,
            blocks: Vec::new(),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.id)?;
        for block in &self.blocks {
            block.fmt(f)?;
        }
        Ok(())
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockId(usize);
impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
struct Block {
    id: BlockId,
    instructions: Vec<Instruction>,
}
impl Block {
    fn new(id: BlockId) -> Block {
        Block {
            id,
            instructions: Vec::new(),
        }
    }
}
impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} [", self.id)?;
        for instr in &self.instructions {
            instr.fmt(f)?;
        }
        writeln!(f, "]")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
    Branch(BlockId, BlockId),
    Jump(BlockId),
    Return,
    Call(Ident),
    Cmp(Relation),
    Neg,
    Add,
    Sub,
    Mul,
    Mod,
    Div,
    Const(usize),
    Load(usize),
    Store(usize),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Branch(l, r) => write!(f, "br {}/{}", l, r),
            Jump(t) => write!(f, "jmp {}", t),
            Return => write!(f, "ret"),
            Call(id) => write!(f, "{} ", id),
            Cmp(rel) => write!(f, "{} ", rel),
            Neg => write!(f, "0- "),
            Add => write!(f, "+ "),
            Sub => write!(f, "- "),
            Mul => write!(f, "* "),
            Mod => write!(f, "% "),
            Div => write!(f, "/ "),
            Const(c) => write!(f, "{} ", c),
            Load(c) => {
                for _ in 0..*c { write!(f, "-")?; }
                write!(f, "-> ")
            }
            Store(c) => {
                write!(f, "<-")?;
                for _ in 0..*c { write!(f, "-")?; }
                write!(f, " ")
            }
        }
    }
}

#[derive(Debug)]
pub struct FunctionBuilder {
    fun: Function,
    syms: Vec<Ident>,
    selected: usize,
}
impl FunctionBuilder {
    pub fn new(id: Ident, args: Vec<Ident>, typ: Type) -> FunctionBuilder {
        FunctionBuilder{
            fun: Function::new(id, typ),
            syms: args,
            selected: 0,
        }
    }

    pub fn done(self) -> Function {
        self.fun
    }

    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.fun.blocks.len());
        self.fun.blocks.push(Block::new(id.clone()));
        id
    }

    pub fn select_block(&mut self, id: BlockId) {
        self.selected = id.0;
    }

    fn get_current_block(&mut self) -> &mut Block {
        &mut self.fun.blocks[self.selected]
    }

    fn append_instruction(&mut self, i: Instruction) {
        let block = self.get_current_block();
        block.instructions.push(i);
    }

    pub fn branch(&mut self, then: BlockId, els: BlockId) {
        self.append_instruction(Instruction::Branch(then, els));
    }

    pub fn jump(&mut self, target: BlockId) {
        self.append_instruction(Instruction::Jump(target));
    }

    pub fn ret(&mut self) {
        self.append_instruction(Instruction::Return);
    }

    pub fn call(&mut self, id: Ident){
        self.append_instruction(Instruction::Call(id));
    }

    pub fn neg(&mut self){
        self.append_instruction(Instruction::Neg);
    }

    pub fn cmp(&mut self, rel: Relation){
        self.append_instruction(Instruction::Cmp(rel));
    }

    pub fn add(&mut self) {
        self.append_instruction(Instruction::Add);
    }

    pub fn sub(&mut self) {
        self.append_instruction(Instruction::Sub);
    }

    pub fn mul(&mut self){
        self.append_instruction(Instruction::Mul);
    }

    pub fn modulo(&mut self) {
        self.append_instruction(Instruction::Mod);
    }

    pub fn div(&mut self) {
        self.append_instruction(Instruction::Div);
    }

    pub fn constant(&mut self, c: usize) {
        self.append_instruction(Instruction::Const(c));
    }

    pub fn alloc(&mut self, ident: &Ident) {
        self.syms.push(ident.clone());
    }

    pub fn load(&mut self, ident: &Ident) {
        let pos = self.syms.iter().position(|id| id == ident).unwrap();
        self.append_instruction(Instruction::Load(pos));
    }

    pub fn store(&mut self, ident: &Ident) {
        let pos = self.syms.iter().position(|id| id == ident).unwrap();
        self.append_instruction(Instruction::Store(pos));
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Id(Ident),
    Func { args: Vec<Ident>, ret: Ident },
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Id(id) => id.fmt(f),
            Type::Func { args, ret } => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt(f)?;
                    if i < args.len() - 1 {
                        write!(f, ",")?;
                    }
                }
                write!(f, "){}", ret)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Relation {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}
impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Relation::*;
        match self {
            Eq => write!(f, "=="),
            Neq => write!(f, "<>"),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            Le => write!(f, "<="),
            Ge => write!(f, ">="),
        }
    }
}

impl Relation {
    pub fn negate(self) -> Relation {
        match self {
            Relation::Eq => Relation::Neq,
            Relation::Neq => Relation::Eq,
            Relation::Lt => Relation::Ge,
            Relation::Gt => Relation::Le,
            Relation::Le => Relation::Gt,
            Relation::Ge => Relation::Lt,
        }
    }
}
