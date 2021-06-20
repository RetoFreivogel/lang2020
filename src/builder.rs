use crate::parser::RELOP_CHARS;
use crate::parser::RelOp;
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
                Phi    => writeln!(f, "\t{} = phi", i.lhs),
                Call   => writeln!(f, "\t{} = {}(%{})", i.lhs, i.const_val, i.a),
                Const  => writeln!(f, "\t{} = {}", i.lhs, i.const_val),
                Cmp    => writeln!(f, "\t{} = {} {} {}", i.lhs, i.a, RELOP_CHARS[i.const_val], i.b),
                Add    => writeln!(f, "\t{} = {} + {}", i.lhs, i.a, i.b),
                Sub    => writeln!(f, "\t{} = {} - {}", i.lhs, i.a, i.b),
                Mul    => writeln!(f, "\t{} = {} * {}", i.lhs, i.a, i.b),
                Mod    => writeln!(f, "\t{} = {} % {}", i.lhs, i.a, i.b),
                Div    => writeln!(f, "\t{} = {} / {}", i.lhs, i.a, i.b),
                Ret    => writeln!(f, "\tret {}", i.a),
                Branch => writeln!(
                    f, 
                    "\tbranch {} {} {}",
                    i.a, 
                    self.blocks[i.blk.0].target,
                    self.blocks[i.blk.0].branch
                ),
                Jump   => writeln!(
                    f, 
                    "\tjump {}", 
                    self.blocks[i.blk.0].target
                ),
                Mov    => writeln!(f, "\t{} = {}", i.lhs, i.a),
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
    Bottom,
    Norm(usize),
    Top,
}

impl Value{
    fn num(&mut self) -> &mut usize{
        match self{
            Value::Norm(n) => n,
            Value::Top => panic!("Called num on a 'top' Value"),
            Value::Bottom => panic!("Called num on a 'bottom' Value"),
        }
    }
    fn is_norm(&self) -> bool{
        match self{
            Value::Norm(_) => true,
            _ => false,
        }
    }
    fn join(self, other: Value) -> Value{
        match (self, other){
            (Value::Top, any) | (any, Value::Top) => any,
            (a, b) if a == b => a,
            _ => Value::Bottom
        }
    }
}

impl Default for Value{
    fn default() -> Value{Value::Bottom}
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self{
            Value::Bottom => write!(f, "%X"),
            Value::Norm(n) => write!(f, "%{}", n),
            Value::Top => write!(f, "%%"),
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
            current_defs: vec![Value::default(); n_args],
            loops: 0,
            filled: false,
            sealed: false,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Operation{
    Phi,
    Const,
    Mov,
    Call,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Cmp,
    Ret,
    Branch,
    Jump,
    //Noop,
}

impl Operation{
    fn kind(&self) -> usize{
        const OP_PRIO: [usize; Operation::Jump as usize + 1] = [
            0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3
        ];
        OP_PRIO[*self as usize]
    }
}

impl Default for Operation{
    fn default() -> Self{
        Operation::Mov
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct Instruction{
    op: Operation,
    blk: BlockId,
    const_val: usize,
    lhs: Value,
    a: Value,
    b: Value,
}

    //Block related methods-----------------------------------------
impl Instruction{
    fn ret(blk: BlockId, a: Value) -> Instruction{
        Instruction{ op: Operation::Ret, blk, a, lhs: Value::Top, ..Default::default()}
    }

    fn call(blk: BlockId, id: Ident, lhs: Value, a: Value) -> Instruction{
        Instruction{op: Operation::Call, blk, const_val: id.0, lhs, a, ..Default::default()}
    }

    fn phi(blk: BlockId, lhs: Value, var_n: usize) -> Instruction{
        Instruction{op: Operation::Phi, blk, lhs, const_val: var_n, ..Default::default()}
    }

    fn mov(blk: BlockId, lhs: Value, a: Value) -> Instruction{
        Instruction{op: Operation::Mov, blk, lhs, a, ..Default::default()}
    }

    fn cmp(blk: BlockId, lhs: Value, rel: usize, a: Value, b: Value) -> Instruction{
        Instruction{op: Operation::Cmp, blk, lhs, const_val: rel, a, b, ..Default::default()}
    }

    fn add(blk: BlockId, lhs: Value, a: Value, b: Value) -> Instruction{
        Instruction{op: Operation::Add, blk, lhs, a, b, ..Default::default()}
    }

    fn sub(blk: BlockId, lhs: Value, a: Value, b: Value) -> Instruction{
        Instruction{op: Operation::Sub, blk, lhs, a, b, ..Default::default()}
    }

    fn mul(blk: BlockId, lhs: Value, a: Value, b: Value) -> Instruction{
        Instruction{op: Operation::Mul, blk, lhs, a, b, ..Default::default()}
    }

    fn modulo(blk: BlockId, lhs: Value, a: Value, b: Value) -> Instruction{
        Instruction{op: Operation::Mod, blk, lhs, a, b, ..Default::default()}
    }

    fn div(blk: BlockId, lhs: Value, a: Value, b: Value) -> Instruction{
        Instruction{op: Operation::Div, blk, lhs, a, b, ..Default::default()}
    }

    fn constant(blk: BlockId, lhs: Value, const_val: usize) -> Instruction{
        Instruction{op: Operation::Const, blk, const_val, lhs, ..Default::default()}
    }

    fn branch(blk: BlockId, a: Value) -> Instruction{
        Instruction{ op: Operation::Branch, blk, lhs: Value::Top, a, ..Default::default()}
    }

    fn jump(blk: BlockId) -> Instruction{
        Instruction{ op: Operation::Jump, blk, lhs: Value::Top, ..Default::default()}
    }
}

struct CommaList<T>(T);
impl<T: std::fmt::Display> fmt::Display for CommaList<&Vec<T>> {
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
        fb.new_block(); // Empty Block Zero

        for i in (0..args.len()).rev(){
             fb.alloc(args[i]);           
        }
        let blk = fb.new_block(); // start block with function args
        fb.mark_sealed(blk);
        return (fb, blk);
    }

    fn fill_idoms(&mut self){
        for blk in &mut self.fun.blocks{
            if blk.predecessors.is_empty(){
                blk.idom = BlockId(0);
            }else{
                blk.idom = blk.predecessors[0];
                assert!(blk.idom < blk.id);
            }
        }
        // using indices i and j to appease the borrow checker 
        for i in 0..self.fun.blocks.len(){
            let mut this = self.fun.blocks[i].idom;
            for j in 0..self.fun.blocks[i].predecessors.len(){
                this = self.common_idom(this, self.fun.blocks[i].predecessors[j])
            }
            self.fun.blocks[i].idom = this; 
        }
    }

    fn find_loops(&mut self){
        let mut visit_count = 0;
        let mut last_visit = vec![0; self.fun.blocks.len()];
        let mut queue = Vec::new();
        for i in 0..self.fun.blocks.len(){
            let blk = &self.fun.blocks[i];
            if blk.target != BlockId(0) && blk.target < blk.id{
                let loop_head = blk.target;
                visit_count += 1;
                queue.clear();
                queue.push(blk.id);
                last_visit[blk.id.0] = visit_count;
                while let Some(next) = queue.pop(){
                    self.fun.blocks[next.0].loops += 1;
                    if next != loop_head{
                        for pred in &self.fun.blocks[next.0].predecessors{
                            if last_visit[pred.0] < visit_count{
                                last_visit[pred.0] = visit_count;
                                queue.push(*pred);
                            }
                        }
                    }
                }
            }
        }
    }

    fn common_idom(&self, mut a: BlockId, mut b: BlockId) -> BlockId{
        while a != b{
            if a < b{
                b = self.fun.blocks[b.0].idom;
            }else{
                a = self.fun.blocks[a.0].idom;
            }
        }
        return a;        
    }

    fn renumber(&mut self){
        self.fun.instructions.sort_by_key(|instr| (instr.blk, instr.op.kind(), instr.lhs));

        let mut dict = vec![Value::default(); self.fun.instructions.len()];
        let mut i = 0;

        while i < self.fun.instructions.len(){
            let instr = &mut self.fun.instructions[i];
            if instr.op == Operation::Mov{
                dict[*instr.lhs.num()] = dict[*instr.a.num()];
                self.fun.instructions.remove(i);
                continue;
            }
            if instr.lhs.is_norm(){
                let index = *instr.lhs.num();
                *instr.lhs.num() = i;
                dict[index] = instr.lhs;
            }
            i += 1;
        //}
        //for instr in &mut self.fun.instructions{
            if instr.a.is_norm(){
                instr.a = dict[*instr.a.num()];
                assert!(instr.a != Value::default());
            } 
            if instr.b.is_norm(){
                instr.b = dict[*instr.b.num()];
                assert!(instr.b != Value::default());
            }
        }        
    }

    fn common_subexpression_elimination(&mut self){
        self.fun.instructions.sort_by_key(
            |instr| (instr.op, instr.a, instr.b, instr.const_val, instr.lhs)
        );
        let mut prev_index = 0;
        let mut prev = self.fun.instructions[prev_index];
        //once again using indices to appease the borrow checker
        for i in 1..self.fun.instructions.len(){
            let mut instr = self.fun.instructions[i];
            if instr.op == Operation::Add || instr.op == Operation::Mul{
                if instr.a > instr.b{
                    let temp = instr.b;
                    instr.b = instr.a;
                    instr.a = temp;
                }
            }
            if 
                instr.op != Operation::Phi &&
                instr.op != Operation::Mov &&
                instr.op != Operation::Jump &&
                instr.op != Operation::Branch &&
                instr.op != Operation::Ret &&
                instr.op == prev.op &&
                instr.a == prev.a && 
                instr.b == prev.b && 
                instr.const_val == prev.const_val
            {
                prev.blk = self.common_idom(instr.blk, prev.blk);
                instr = Instruction::mov(instr.blk, instr.lhs, prev.lhs);
            }else{
                // The handling of 'prev' needs to be so complex to handle consecutive substitutions 
                self.fun.instructions[prev_index] = prev;
                prev = instr;
                prev_index = i;
            }
            self.fun.instructions[i] = instr;
        }
    }

    fn loop_invariant_move(&mut self){
        use std::cmp::max;
        self.fun.instructions.sort_by_key(|instr| (instr.blk, instr.op.kind(), instr.lhs));
        for i in 0..self.fun.instructions.len(){
            let mut instr = self.fun.instructions[i];
            match instr.op{
                Operation::Phi | Operation::Jump | Operation::Branch |
                Operation::Ret | Operation::Mov | Operation::Call => continue,
                _ => {}
            }
            let blk_a = if instr.a.is_norm(){
                self.fun.instructions[*instr.a.num()].blk
            }else{
                BlockId(0)
            };
            let blk_b = if instr.b.is_norm(){
                self.fun.instructions[*instr.b.num()].blk
            }else{
                BlockId(0)
            };
            let old = self.fun.instructions[i].blk;
            let new = max(blk_a, blk_b);
            if self.fun.blocks[new.0].loops < self.fun.blocks[old.0].loops{
                self.fun.instructions[i].blk = new;
            }
        }
    }

    fn done(mut self) -> Function {
        self.fill_idoms();
        self.find_loops();
        self.common_subexpression_elimination();
        self.renumber();
        self.loop_invariant_move();
        self.renumber();

        //self.fun.instructions.sort_by_key(|instr| (instr.blk, instr.op.kind(), instr.lhs));
        self.fun
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

    fn branch(&mut self, val: Value, src: BlockId, yes: BlockId, no: BlockId) {
        self.append_instruction(Instruction::branch(src, val)); 
        self.fun.blocks[src.0].filled = true;
        self.fun.blocks[src.0].target = yes;
        self.fun.blocks[src.0].branch = no;
        self.fun.blocks[yes.0].predecessors.push(src);
        self.fun.blocks[no.0].predecessors.push(src);
    }

    fn jump(&mut self, src: BlockId, target: BlockId) {
        self.append_instruction(Instruction::jump(src)); // Todo Check
        self.fun.blocks[src.0].filled = true;
        self.fun.blocks[src.0].target = target;
        self.fun.blocks[target.0].predecessors.push(src);
    }

    fn alloc(&mut self, ident: Ident) -> usize{
        self.idents.push(ident);
        for blk in &mut self.fun.blocks{
            blk.current_defs.push(Value::default());
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

        if val != Value::default() {
            return val 
        }
        val = self.new_phi(blk, var_n);

        if self.fun.blocks[blk.0].sealed{
            self.write(blk, var_n, val); // To avoid loops
            val = self.build_phi(blk, val);                
        }
        self.write(blk, var_n, val);
        return val;
    }    

    fn new_phi(&mut self, blk: BlockId, var_n: usize) -> Value{
        let phi = self.new_temp();
        self.fun.blocks[blk.0].incomplete_phis.push(phi);
        self.fun.instructions.push(Instruction::phi(blk, phi, var_n));
        return phi;
    }

    fn build_phi(&mut self, blk: BlockId, mut phi: Value) -> Value{
        let var_n = self.fun.instructions[*phi.num()].const_val;

        let mut same = Value::Top;
        for i in 0..self.fun.blocks[blk.0].predecessors.len(){
            let pred = self.fun.blocks[blk.0].predecessors[i];
            let val = self.read(pred, var_n);
            if val != phi{
                same = same.join(val);
                if same == Value::Bottom{break}
            }
        }
        if same.is_norm(){
            self.cancel_phi(blk, phi, same);
            same
        }else{
            phi
        }
    }

    fn cancel_phi(&mut self, blk: BlockId, mut phi: Value, val: Value){
        let instr = &mut self.fun.instructions[*phi.num()];
        *instr = Instruction::mov(blk, phi, val);
    }

    fn append_instruction(&mut self, instr: Instruction){
        assert!(!self.fun.blocks[instr.blk.0].filled);
        self.fun.instructions.push(instr); 
    }

    //Block related methods-----------------------------------------

    fn ret(&mut self, blk: BlockId, v: Value) {
        self.append_instruction(Instruction::ret(blk, v));
    }

    fn call(&mut self, blk: BlockId, id: Ident, args: Vec<Value>) -> Value{
        let r = self.new_temp();
        self.append_instruction(Instruction::call(blk, id, r, args[0]));
        return r;
    }

    fn neg(&mut self, blk: BlockId, a: Value) -> Value{
        let v = self.new_temp();
        let z = self.constant(0);
        self.append_instruction(Instruction::sub(blk, v, z, a));
        return v;
    }

    fn cmp(&mut self, blk: BlockId, rel: RelOp, a: Value, b: Value) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::cmp(blk, v, rel as usize, a, b));
        return v;
    }

    fn add(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::add(blk, v, a, b));
        return v;
    }

    fn sub(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::sub(blk, v, a, b));
        return v;
    }

    fn mul(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::mul(blk, v, a, b));
        return v;
    }

    fn modulo(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::modulo(blk, v, a, b));
        return v;
    }

    fn div(&mut self, blk: BlockId, a: Value, b: Value) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::div(blk, v , a, b));
        return v;
    }

    fn constant(&mut self, c: usize) -> Value{
        let v = self.new_temp();
        self.append_instruction(Instruction::constant(BlockId(0), v, c));
        return v;
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
    let a = build_expr(fb, blk, &rel.lhs);
    let b = build_expr(fb, blk, &rel.rhs);
    fb.cmp(blk, rel.op, a, b)
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
