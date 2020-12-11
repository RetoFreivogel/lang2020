use std::fmt;

//use crate::module::Symbol;
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
    pub vars: Vec<Ident>,
    blocks: Vec<Block>,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.id)?;
        for block in &self.blocks {
            block.fmt(f)?;
        }
        Ok(())
    }
}
impl Function {
    fn map_block_ids<F: Fn(usize) -> usize>(&mut self, f: F) {
        for block in self.blocks.iter_mut() {
            match block.terminator {
                Some(Terminator::Jump(ref mut t)) => t.0 = f(t.0),
                Some(Terminator::Branch(ref mut r, ref mut l)) => {
                    r.0 = f(r.0);
                    l.0 = f(l.0);
                }
                _ => {}
            }
        }
    }

    pub fn renumber_blocks(&mut self) {
        let mut map = vec![0; self.blocks.len()];
        for i in 0..self.blocks.len() {
            map[self.blocks[i].id.0] = i;
            self.blocks[i].id.0 = i;
        }
        self.map_block_ids(|i| map[i]);
    }

    pub fn remove_blocks(&mut self) {
        //remove jumps to empty blocks
        println!("{}", self);
        for i in 0..self.blocks.len() {
            if self.blocks[i].instructions.is_empty() {
                if let Some(Terminator::Jump(t)) = self.blocks[i].terminator {
                    let id = self.blocks[i].id.0;
                    self.map_block_ids(|i| if i == id { t.0 } else { i });
                    println!("{} -> {}", id, t.0);
                }
            }
        }
        println!("{}", self);

        //find unreachable blocks
        let mut reachable = vec![false; self.blocks.len()];
        let mut queue = vec![0];
        while let Some(i) = queue.pop() {
            if reachable[i] {
                continue;
            }
            reachable[i] = true;
            match self.blocks[i].terminator {
                Some(Terminator::Jump(t)) => queue.push(t.0),
                Some(Terminator::Branch(r, l)) => {
                    queue.push(r.0);
                    queue.push(l.0);
                }
                _ => {}
            }
        }

        //move unreachable blocks to the end
        let mut i = 0;
        let mut removed = 0;
        while i + removed < self.blocks.len() {
            if reachable[self.blocks[i].id.0] {
                i += 1;
            } else {
                removed += 1;
                let second = self.blocks.len() - removed;
                self.blocks.swap(i, second);
            }
        }
        self.renumber_blocks();
        for _ in 0..removed {
            self.blocks.pop();
        }
    }
}

/*
            if self.blocks[i].instructions.is_empty(){
                if let Some(Terminator::Jump(t)) = self.blocks[i].terminator{
                    reachable[i] = false;
                }
            }
*/

#[derive(Debug)]
pub struct FunctionBuilder {
    fun: Function,
    block_n: usize,
    value_n: usize,
}
impl FunctionBuilder {
    pub fn new(id: Ident) -> FunctionBuilder {
        FunctionBuilder {
            fun: Function {
                id,
                vars: Vec::new(),
                blocks: vec![Block::new(BlockId(0))],
            },
            block_n: 0,
            value_n: 0,
        }
    }

    pub fn done(self) -> Function {
        self.fun
    }

    pub fn new_block(&mut self) -> BlockId {
        self.block_n += 1;
        BlockId(self.block_n)
    }

    pub fn begin_block(&mut self, id: BlockId) {
        self.fun.blocks.push(Block::new(id));
    }

    fn new_temp(&mut self) -> Value {
        self.value_n += 1;
        Value::Temp(self.value_n)
    }

    pub fn add_var(&mut self, arg: Ident) {
        self.fun.vars.push(arg);
    }

    fn get_current_block(&mut self) -> &mut Block {
        let index = self.fun.blocks.len() - 1;
        &mut self.fun.blocks[index]
    }

    fn append_instruction(&mut self, i: Instruction) {
        let block = self.get_current_block();
        block.instructions.push(i);
    }

    fn set_terminator(&mut self, term: Terminator) {
        let block = self.get_current_block();
        if block.terminator.is_none() {
            block.terminator = Some(term);
        }
    }

    pub fn term_branch(&mut self, then: BlockId, els: BlockId) {
        self.set_terminator(Terminator::Branch(then, els));
    }

    pub fn term_jump(&mut self, target: BlockId) {
        self.set_terminator(Terminator::Jump(target));
    }

    pub fn term_return(&mut self, ret: Value) {
        self.set_terminator(Terminator::Return(ret));
    }

    pub fn add_call(&mut self, id: Ident, args: Vec<Value>) -> Value {
        let ret = self.new_temp();
        self.append_instruction(Instruction::Call(ret, id, args));
        ret
    }

    pub fn add_neg(&mut self, a: Value) -> Value {
        let ret = self.new_temp();
        let zero = Value::Const(0);
        self.append_instruction(Instruction::Sub(ret, zero, a));
        ret
    }

    pub fn add_cmp(&mut self, rel: Relation, a: Value, b: Value) {
        self.append_instruction(Instruction::Cmp(rel, a, b));
    }

    pub fn add_add(&mut self, a: Value, b: Value) -> Value {
        let ret = self.new_temp();
        self.append_instruction(Instruction::Add(ret, a, b));
        ret
    }

    pub fn add_sub(&mut self, a: Value, b: Value) -> Value {
        let ret = self.new_temp();
        self.append_instruction(Instruction::Sub(ret, a, b));
        ret
    }

    pub fn add_mul(&mut self, a: Value, b: Value) -> Value {
        let ret = self.new_temp();
        self.append_instruction(Instruction::Mul(ret, a, b));
        ret
    }

    pub fn add_mod(&mut self, a: Value, b: Value) -> Value {
        let ret = self.new_temp();
        self.append_instruction(Instruction::Mod(ret, a, b));
        ret
    }

    pub fn add_div(&mut self, a: Value, b: Value) -> Value {
        let ret = self.new_temp();
        self.append_instruction(Instruction::Div(ret, a, b));
        ret
    }

    pub fn add_const(&mut self, c: usize) -> Value {
        Value::Const(c)
    }

    pub fn add_load(&mut self, id: Ident) -> Value {
        Value::Sym(id)
    }

    pub fn add_store(&mut self, id: Ident, b: Value) {
        let ret = Value::Sym(id);
        self.append_instruction(Instruction::Mov(ret, b));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockId(usize);
impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".{}", self.0)
    }
}

#[derive(Debug)]
struct Block {
    id: BlockId,
    terminator: Option<Terminator>,
    instructions: Vec<Instruction>,
    var_use: u64,
    var_set: u64,
}
impl Block {
    fn new(id: BlockId) -> Block {
        Block {
            id,
            terminator: None,
            instructions: Vec::new(),
            var_use: 0,
            var_set: 0,
        }
    }
}
impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.id)?;
        for instr in &self.instructions {
            instr.fmt(f)?;
        }
        match &self.terminator {
            Some(Terminator::Branch(l, r)) => writeln!(f, "\tbr {} {}", l, r),
            Some(Terminator::Jump(t)) => writeln!(f, "\tjmp {}", t),
            Some(Terminator::Return(v)) => writeln!(f, "\tret {}", v),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Terminator {
    Branch(BlockId, BlockId),
    Jump(BlockId),
    Return(Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
    Call(Value, Ident, Vec<Value>),
    Mov(Value, Value),
    Cmp(Relation, Value, Value),
    Add(Value, Value, Value),
    Sub(Value, Value, Value),
    Mul(Value, Value, Value),
    Mod(Value, Value, Value),
    Div(Value, Value, Value),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Call(r, id, args) => {
                write!(f, "\t{} = {}", r, id)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                writeln!(f, "")
            }
            Cmp(r, a, b) => writeln!(f, "\tcmp {} {} {}", a, r, b),
            Mov(a, b) => writeln!(f, "\t{} = {}", a, b),
            Add(r, a, b) => writeln!(f, "\t{} = {} + {}", r, a, b),
            Sub(r, a, b) => writeln!(f, "\t{} = {} - {}", r, a, b),
            Mul(r, a, b) => writeln!(f, "\t{} = {} * {}", r, a, b),
            Mod(r, a, b) => writeln!(f, "\t{} = {} % {}", r, a, b),
            Div(r, a, b) => writeln!(f, "\t{} = {} / {}", r, a, b),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Temp(usize),
    Const(usize),
    Sym(Ident),
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Temp(n) => write!(f, "%{}", n),
            Value::Const(n) => write!(f, "{}", n),
            Value::Sym(id) => write!(f, "%{}", id),
        }
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

/*
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
*/
