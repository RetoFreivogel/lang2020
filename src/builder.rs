use std::fmt;
use crate::strtab::{Ident, Type};

#[derive(Debug)]
pub struct Module {
    items: Vec<Function>,
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
struct Function {
    id: Ident,
    typ: Type,
    blocks: Vec<Block>,
    instructions: Vec<Instruction>,
}

impl Function{
    fn new(id: Ident, typ: Type) -> Function{
        Function{
            id,
            typ,
            blocks: Vec::new(),
            instructions: Vec::new(),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operation::*;
        writeln!(f, "{}", self.id)?;
        let mut blk_n = BlockId(0);
        for i in &self.instructions{
            if blk_n != i.blk{
                blk_n = i.blk;
                writeln!(f, "{}", blk_n)?;
            }
            match i.op{
                Phi    => writeln!(f, "\t{} = phi {}", i.lhs, CommaList(&i.vals[..])),
                Call   => writeln!(f, "\t{} = {}(%{})", i.lhs, i.num, CommaList(&i.vals[..])),
                Lt     => writeln!(f, "\t{} = {} < {}", i.lhs, i.vals[0], i.vals[1]),
                Gt     => writeln!(f, "\t{} = {} > {}", i.lhs, i.vals[0], i.vals[1]),
                Le     => writeln!(f, "\t{} = {} <= {}", i.lhs, i.vals[0], i.vals[1]),
                Ge     => writeln!(f, "\t{} = {} >= {}", i.lhs, i.vals[0], i.vals[1]),
                Equ    => writeln!(f, "\t{} = {} == {}", i.lhs, i.vals[0], i.vals[1]),
                Neq    => writeln!(f, "\t{} = {} <> {}", i.lhs, i.vals[0], i.vals[1]),
                Add    => writeln!(f, "\t{} = {} + {}", i.lhs, i.vals[0], i.vals[1]),
                Sub    => writeln!(f, "\t{} = {} - {}", i.lhs, i.vals[0], i.vals[1]),
                Mul    => writeln!(f, "\t{} = {} * {}", i.lhs, i.vals[0], i.vals[1]),
                Mod    => writeln!(f, "\t{} = {} % {}", i.lhs, i.vals[0], i.vals[1]),
                Div    => writeln!(f, "\t{} = {} / {}", i.lhs, i.vals[0], i.vals[1]),
                Ret    => writeln!(f, "\tret {}", i.vals[0]),
                Branch => writeln!(
                    f, 
                    "\tbranch {} {} {}",
                    i.vals[0], 
                    self.blocks[i.blk.0].target,
                    self.blocks[i.blk.0].branch
                ),
                Jump   => writeln!(
                    f, 
                    "\tjump {}", 
                    self.blocks[i.blk.0].target
                ),
                Mov    => writeln!(f, "\t{} = {}", i.lhs, i.vals[0]),
            }?;
        }
        writeln!(f, "")
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
struct BlockId(usize);
impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".B{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Value{
    Const(usize),
    Norm(usize),
}

const UNDEF: Value = Value::Norm(usize::MAX);

impl Value{
    fn num(&mut self) -> usize{
        match self{
            Value::Norm(n) => *n,
            _ => panic!("Called num on a constant Value"),
        }
    }
    fn is_norm(&self) -> bool{
        match self{
            Value::Norm(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self{
            &UNDEF => write!(f, "UNDEF"),
            Value::Const(n) => write!(f, "{}", n),
            Value::Norm(n) => write!(f, "%{}", n),
        }
    }
}

#[derive(Debug)]
struct Block {
    id: BlockId,
    predecessors: Vec<BlockId>,
    idom: BlockId,
    target: BlockId,
    branch: BlockId,
    incomplete_phis: Vec<Value>,
    current_defs: Vec<Value>,
    loops: usize,
    filled: bool,
    sealed: bool,
}

impl Block {
    fn new(id: BlockId, n_args: usize) -> Block {
        Block {
            id,
            predecessors: Vec::new(),
            idom: id,
            target: BlockId(0),
            branch: BlockId(0),
            incomplete_phis: Vec::new(),
            current_defs: vec![UNDEF; n_args],
            loops: 0,
            filled: false,
            sealed: false,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Operation{
    Phi,
    Mov,
    Call,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Gt,
    Lt,
    Ge,
    Le,
    Equ,
    Neq,
    Ret,
    Branch,
    Jump,
}

impl Default for Operation{
    fn default() -> Self{
        Operation::Mov
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Instruction{
    op: Operation,
    blk: BlockId,
    lhs: Value,
    num: usize,
    vals: Vec<Value>,
}

struct CommaList<T>(T);
impl<T: std::fmt::Display> fmt::Display for CommaList<&[T]> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.0.is_empty(){
            write!(f, "{}", self.0[0])?;
            for arg in &self.0[1..]{
                write!(f, ", {}", arg)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct FunctionBuilder {
    fun: Function,
    idents: Vec<Ident>,
}

impl FunctionBuilder {
    fn new(id: Ident, args: Vec<Ident>, typ: Type) -> (FunctionBuilder, BlockId){
        let mut fb = FunctionBuilder{
            fun: Function::new(id, typ),
            idents: Vec::new(),
        };
        fb.new_block(); // Block Zero for static values
        let blk = fb.new_block(); // start block with function args
        fb.mark_sealed(blk);

        for i in (0..args.len()).rev(){
            let var_n = fb.alloc(args[i]);
            let val = fb.new_phi(blk, var_n); // TODO overthink arguments
            fb.write(blk, var_n, val);           
        }

        return (fb, blk);
    }

    fn new_block(&mut self) -> BlockId {
        let blk = BlockId(self.fun.blocks.len());
        let block = Block::new(blk, self.idents.len());
        self.fun.blocks.push(block);
        return blk;
    }

    fn new_temp(&mut self) -> Value {
        let val = Value::Norm(self.fun.instructions.len());
        return val;    
    }

    fn mark_sealed(&mut self, blk: BlockId){
        self.fun.blocks[blk.0].sealed = true;
        while let Some(phi) = self.fun.blocks[blk.0].incomplete_phis.pop(){
            self.build_phi(blk, phi);                
        }
    }

    fn branch(&mut self, cond: Value, blk: BlockId, yes: BlockId, no: BlockId) {
        let op = Operation::Branch;
        let vals = vec![cond];
        self.append_instruction(op, blk, vals, 0);
        self.fun.blocks[blk.0].filled = true;
        self.fun.blocks[blk.0].target = yes;
        self.fun.blocks[blk.0].branch = no;
        self.fun.blocks[yes.0].predecessors.push(blk);
        self.fun.blocks[no.0].predecessors.push(blk);
    }

    fn jump(&mut self, blk: BlockId, target: BlockId) {
        let op = Operation::Jump;
        let vals= vec![];
        self.append_instruction(op, blk, vals, 0);
        self.fun.blocks[blk.0].filled = true;
        self.fun.blocks[blk.0].target = target;
        self.fun.blocks[target.0].predecessors.push(blk);
    }

    fn alloc(&mut self, ident: Ident) -> usize{
        self.idents.push(ident);
        for blk in &mut self.fun.blocks{
            blk.current_defs.push(UNDEF);
        }
        return self.idents.len() - 1;
    }

    fn lookup(&self, ident: Ident) -> Option<usize>{
        for i in 0..self.idents.len(){
            if self.idents[i] == ident{
                return Some(i);
            }
        }
        return None;
    }

    fn write(&mut self, blk: BlockId, var_n: usize, val: Value){
        let block = &mut self.fun.blocks[blk.0];
        block.current_defs[var_n] = val;     
    }

    fn read(&mut self, blk: BlockId, var_n: usize) -> Value{
        let mut val = self.fun.blocks[blk.0].current_defs[var_n];
        if val == UNDEF {
            val = self.new_phi(blk, var_n);
            if self.fun.blocks[blk.0].sealed{
                self.write(blk, var_n, val); // To avoid loops
                val = self.build_phi(blk, val);                
            }
            self.write(blk, var_n, val);
        }
        return val;
    }    

    fn new_phi(&mut self, blk: BlockId, num: usize) -> Value{
        let phi = self.new_temp();
        self.fun.blocks[blk.0].incomplete_phis.push(phi);
        let op = Operation::Phi;
        let lhs = self.new_temp();
        let vals = vec![];
        let instr = Instruction{op, blk, lhs, vals, num};
        self.fun.instructions.push(instr);
        return lhs;
    }

    fn build_phi(&mut self, blk: BlockId, mut phi: Value) -> Value{
        let var_n = self.fun.instructions[phi.num()].num;

        let mut vals = Vec::new();
        for i in 0..self.fun.blocks[blk.0].predecessors.len(){
            let pred = self.fun.blocks[blk.0].predecessors[i];
            let val = self.read(pred, var_n);
            if val != phi{
                vals.push(val);
            }
        }
        vals.sort();
        vals.dedup();

        let instr = &mut self.fun.instructions[phi.num()];
        if vals.len() == 0{
            panic!("Variable is Undefined");
        }else if vals.len() == 1{
            instr.op = Operation::Mov;
            instr.vals = vals;
            instr.vals[0]
        }else{
            instr.vals = vals;
            instr.lhs
        } 
    }

    fn append_instruction(&mut self, op: Operation, blk: BlockId, vals: Vec<Value>, num: usize) -> Value{
        assert!(!self.fun.blocks[blk.0].filled);
        let lhs = self.new_temp();
        let instr = Instruction{op, blk, lhs, vals, num};
        self.fun.instructions.push(instr);
        return lhs; 
    }

    //Block related methods-----------------------------------------

    fn ret(&mut self, blk: BlockId, a: Value) -> Value{
        let op = Operation::Ret;
        let vals = vec![a];
        let b = UNDEF;
        let num = 0;
        self.append_instruction(op, blk, vals, num)
    }

    fn call(&mut self, blk: BlockId, id: Ident, vals: Vec<Value>) -> Value{
        let op = Operation::Call;
        let num = id.0;
        self.append_instruction(op, blk, vals, num)
    }

    fn neg(&mut self, blk: BlockId, val: Value) -> Value{
        let zero = self.constant(0);
        return self.sub(blk, zero, val);
    }

    fn lt(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Lt;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn gt(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Gt;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn le(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Le;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn ge(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Ge;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn equ(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Equ;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn neq(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Neq;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn add(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Add;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn sub(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Sub;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn mul(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Mul;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn modulo(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Mod;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn div(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let op = Operation::Div;
        let num = 0;
        let vals = vec![a,b];
        self.append_instruction(op, blk, vals, num)
    }

    fn constant(&mut self, num: usize) -> Value{
        Value::Const(num)
    }

    fn done(mut self) -> Function {
        self.schedule();
        self.fun
    }

    fn schedule(&mut self){
        fn op_order(op: Operation) -> usize{
            match op{
                Operation::Phi => 0,
                Operation::Branch | Operation::Jump | Operation::Ret => 2,
                _ => 1,
            }
        }
        let instructions = &mut self.fun.instructions;
        instructions.sort_by_key(|instr|
            (instr.blk, op_order(instr.op), instr.lhs)
        );
        let mut table = vec![UNDEF; instructions.len()];
        let mut i = 0;
        while i < instructions.len(){
            let instr = &mut instructions[i];
            if instr.op == Operation::Mov{
                if instr.vals[0].is_norm(){
                    table[instr.lhs.num()] = table[instr.vals[0].num()];
                }else{
                    table[instr.lhs.num()] = instr.vals[0];
                }
                instructions.remove(i);
            }else{
                table[instr.lhs.num()] = Value::Norm(i);
                instr.lhs = Value::Norm(i);
                i += 1;
            }
        }
        for instr in instructions.iter_mut(){
            for val in &mut instr.vals{
                if val.is_norm() && val != &UNDEF{
                    *val = table[val.num()];
                }
            }
        }
    }    
}

use crate::parser;

pub fn build_mod(module: &parser::AstMod) -> crate::builder::Module{
    let mut items = Vec::new();
    for parser::AstItem::Fun(fun) in &module.items{
        let (mut fb, mut blk) = FunctionBuilder::new(fun.id, fun.args.clone(), fun.typ);
        for stmt in &fun.stmts{
            blk = build_stmt(&mut fb, blk, stmt);
        }
        let val = build_expr(&mut fb, blk, &fun.ret);
        fb.ret(blk, val);
        items.push(fb.done());
    }
    crate::builder::Module{items}
}

fn build_stmt(fb: &mut FunctionBuilder, blk: BlockId, stmt: &parser::AstStmt) -> BlockId{
    match stmt{
        parser::AstStmt::If(cond, then, elifs, els) => {
            let mut cond_blk = blk;
            let mut then_blk = fb.new_block();
            let mut else_blk = fb.new_block();
            let end_blk  = fb.new_block();
            let v = build_rel(fb, cond_blk, cond);
            fb.branch(v, cond_blk, then_blk, else_blk);
            fb.mark_sealed(then_blk);
            fb.mark_sealed(else_blk);
            for stmt in then{
                then_blk = build_stmt(fb, then_blk, stmt);
            }
            fb.jump(then_blk, end_blk);
            for (cond, then) in elifs{
                cond_blk = else_blk;
                then_blk = fb.new_block();
                else_blk = fb.new_block();
                let v = build_rel(fb, cond_blk, cond);
                fb.branch(v, cond_blk, then_blk, else_blk);
                fb.mark_sealed(then_blk);
                fb.mark_sealed(else_blk);
                for stmt in then{
                    then_blk = build_stmt(fb, then_blk, stmt);
                }
                fb.jump(then_blk, end_blk);
            }
            for stmt in els{
                else_blk = build_stmt(fb, else_blk, stmt);
            }
            fb.jump(else_blk, end_blk);
            fb.mark_sealed(end_blk);
            end_blk
        }
        parser::AstStmt::While(cond, lop) => {
            let cond_blk = fb.new_block();
            let mut lop_blk = fb.new_block();
            let end_blk = fb.new_block();
            fb.jump(blk, cond_blk);
            let v = build_rel(fb, cond_blk, cond);
            fb.branch(v, cond_blk, lop_blk, end_blk);
            fb.mark_sealed(lop_blk);
            for stmt in lop{
                lop_blk = build_stmt(fb, lop_blk, stmt);
            }
            fb.jump(lop_blk, cond_blk);
            fb.mark_sealed(cond_blk);
            fb.mark_sealed(end_blk);
            end_blk             
        }
        parser::AstStmt::Declaration(id, expr) => {
            let val = build_expr(fb, blk, expr);
            let var = fb.alloc(*id);
            fb.write(blk, var, val);
            blk
        }
        parser::AstStmt::Assign(id, pos, expr) => {
            let val = build_expr(fb, blk, expr);
            if let Some(var) = fb.lookup(*id){
                fb.write(blk, var, val);
            }else{
                panic!("{}: Undefined {}", pos, id);
            }
            blk
        }
    }
}

fn build_rel(fb: &mut FunctionBuilder, blk: BlockId, rel: &parser::AstRel) -> Value{
    use crate::parser::RelOp;
    let a = build_expr(fb, blk, &rel.lhs);
    let b = build_expr(fb, blk, &rel.rhs);
    match rel.op{
        RelOp::Lt  => fb.lt(blk, a, b),
        RelOp::Gt  => fb.gt(blk, a, b),
        RelOp::Le  => fb.le(blk, a, b),
        RelOp::Ge  => fb.ge(blk, a, b),
        RelOp::Eq => fb.equ(blk, a, b),
        RelOp::Neq => fb.neq(blk, a, b),
    }
}

fn build_expr(fb: &mut FunctionBuilder, blk: BlockId, expr: &parser::AstExpr) -> Value{
    match expr{
        parser::AstExpr::Integer(n) => fb.constant(*n),
        parser::AstExpr::Id(id) => {
            let var = fb.lookup(*id).unwrap();
            fb.read(blk, var)
        }
        parser::AstExpr::Call(id, args) => {
            let mut vals = Vec::new();
            for arg in args{
                vals.push(build_expr(fb, blk, arg));
            }
            fb.call(blk, *id, vals)
        }
        parser::AstExpr::Uminus(rhs) => {
            let a = build_expr(fb, blk, rhs);
            fb.neg(blk, a)
        }
        parser::AstExpr::Plus(rhs, lhs) => {
            let a = build_expr(fb, blk, rhs);
            let b = build_expr(fb, blk, lhs);
            fb.add(blk, a, b)
        }
        parser::AstExpr::Minus(rhs, lhs) =>  {
            let a = build_expr(fb, blk, rhs);
            let b = build_expr(fb, blk, lhs);
            fb.sub(blk, a, b)
        }
        parser::AstExpr::Times(rhs, lhs) =>  {
            let a = build_expr(fb, blk, rhs);
            let b = build_expr(fb, blk, lhs);
            fb.mul(blk, a, b)
        }
        parser::AstExpr::Divide(rhs, lhs) =>  {
            let a = build_expr(fb, blk, rhs);
            let b = build_expr(fb, blk, lhs);
            fb.div(blk, a, b)
        }
        parser::AstExpr::Modulo(rhs, lhs) =>  {
            let a = build_expr(fb, blk, rhs);
            let b = build_expr(fb, blk, lhs);
            fb.modulo(blk, a, b)
        }       
    }
}
