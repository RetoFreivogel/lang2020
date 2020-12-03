/*	TODO
	expand syntax
	test test test
    less fields in the Parser
    use Result?
    use combinators?
*/


use crate::module::{Module, Type, Kind, Symbol};
use std::io::Read;
use crate::builder::Builder;
use crate::lexer::{LexPos, Lexer, TokenSet, Token};
use std::fmt;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError{
    ParseErr{
    	position: LexPos,
        tried: TokenSet,
        found: Token,
    },
    TypeErr{
        position: LexPos,
        left: Type,
        right: Type,
    },
    UndefErr{
        position: LexPos,
        sym: String
    },
    GeneralErr{
        position: LexPos,
        msg: String
    },
}

type Parsing<T> = Result<T, CompileError>;

impl fmt::Display for CompileError{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        match self{
            CompileError::ParseErr{position, tried, found} => {
                writeln!(f, "{} Syntax error:\n Expected: {}\n Found: {}",
                    position, tried, found)                
            },
            CompileError::TypeErr{position, left, right} => {
                writeln!(f, "{} Type mismatch: Left side is {}. Right side is {}.",
                    position, left, right)
            }
            CompileError::UndefErr{position, sym} => {
                writeln!(f, "{} Symbol Undefined: '{}'",
                    position, sym)
            }
            CompileError::GeneralErr{position, msg} => {
                writeln!(f, "{} {}",
                    position, msg)
            }
        }

    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parser<I:Read>{
	lexer: Lexer<I>,
    current: Token,
    tried: TokenSet,
    module: Module,
    builder: Builder
}
impl<I: std::io::Read> Parser<I>{
    pub fn new_file(filename: String, input: I) -> Parser<I>{
        let mut p = Parser{
            lexer: Lexer::new(filename, input),
            current: Token::Error,
            tried: TokenSet::new(),
            module: Module::new(),
            builder: Builder::new(),
        };
        p.current = p.lexer.next();
        p
    }

    pub fn done(self) -> (Module, Builder){
        (self.module, self.builder)
    }

//error handling and parser utilities-------------------------------------------




    fn syntax_err<T>(&mut self) -> Parsing<T>{
        self.current = Token::Error;
        Err(CompileError::ParseErr{
        	position: self.lexer.pos(),
            tried: self.tried,
            found: self.current,
        })
    }

    fn mark<T>(&mut self, text: &str) -> Parsing<T>{
        self.current = Token::Error;
        Err(CompileError::GeneralErr{
            position: self.lexer.pos(),
            msg: text.to_string(),
        })
    }

    fn typecheck(&mut self, a: &Type, b: &Type) -> Parsing<Type>{
        let c = a.unify(b);
        if c.is_some(){
            return Ok(c.unwrap())
        }
        self.current = Token::Error;
        Err(CompileError::TypeErr{
            position: self.lexer.pos(),
            left: a.clone(),
            right: b.clone(),
        })
    }

    fn lookup(&mut self, id: &str) -> Parsing<Symbol>{
        let sym = self.module.get_sym(id);
        if sym.is_none(){
            Err(CompileError::UndefErr{
                position: self.lexer.pos(),
                sym: id.to_string(),
            })
        }else{
            Ok(sym.unwrap())
        }
    }

    fn check(&self, t: Token) -> bool{
        self.current == t
    }

    fn accept(&mut self, t: Token) -> bool{
        if self.check(t) {
            self.current = self.lexer.next();
            self.tried = TokenSet::new();
            true
        }else{
            self.tried.set(t);
            false           
        }
    }

    fn expect(&mut self, t: Token) -> Parsing<()>{
        if self.check(t) {
            self.current = self.lexer.next();
            self.tried = TokenSet::new();
            Ok(())
        }else{
            self.tried.set(t);
            self.syntax_err()
        }
    }

//parser rules------------------------------------------------------------------




// module -> {item} EOF
    pub fn module(&mut self) -> Vec<Parsing<()>>{
        let mut items = Vec::new();
        while !self.accept(Token::Eof){
            if self.check(Token::Fun){ 
                items.push(self.item());
            }else{
                self.current = self.lexer.next();
            }  
        }
        items
    }

//item -> 'fun' funhead funbody
    fn item(&mut self) -> Parsing<()>{
        self.expect(Token::Fun)?;
        self.funhead()?;
        self.funbody()
    }

// funhead -> ID '(' [ ID { ',' ID } ] ')' ':' typedec
    fn funhead(&mut self) -> Parsing<()>{
        let id = self.ident()?;
        let mut args_id = Vec::new();

        self.expect(Token::LParen)?;
        if !self.check(Token::RParen){
            args_id.push(self.ident()?);
            while self.accept(Token::Comma){
                args_id.push(self.ident()?);
            }               
        }
        self.expect(Token::RParen)?;
        self.expect(Token::Colon)?;
        let typ = self.typedec()?;

        let start = self.builder.new_function(id.clone());
        self.builder.select(start);
        self.module.declare_fun(id, typ.clone(), start);

        let args = self.module.get_last_fun().typ.get_args();
        if args.len() != args_id.len(){
            self.mark("Arity mismatch.")?;
        }
        for i in 0..args.len(){
            self.module.declare_arg(args_id[i].clone(), args[i].clone(), i);
        }
        Ok(())
    }

// funbody  '=' expr ';'
// funbody -> 'is' vardec block 'end'
    fn funbody(&mut self) -> Parsing<()>{
        if self.accept(Token::Equal){
            let typ1 = self.expr()?;
            let typ2 = self.module.get_last_fun().typ.get_ret();
            self.typecheck(&typ1, &typ2)?;
            self.expect(Token::Semi)
        }else{
            self.expect(Token::Is)?;
            self.vardec()?;
            self.block()?;
            self.expect(Token::End)
        }
    }

// vardec -> { 'var' ID { ',' ID } ':' typedec ';' }
    fn vardec(&mut self) -> Parsing<()>{
        while self.accept(Token::Var){
            let mut ids = Vec::new();
            ids.push(self.ident()?);
            while self.accept(Token::Comma){
                ids.push(self.ident()?);
            }
            self.expect(Token::Colon)?;
            let typ = self.typedec()?;
            for id in ids{
                self.module.declare_var(id, typ.clone());            
            }
            self.expect(Token::Semi)?;
        }
        Ok(())
    }

// typedec <- '(' [ typedec { ',' typedec } ] ')' typedec
// typedec <- Id
    fn typedec(&mut self) -> Parsing<Type>{
        if self.accept(Token::LParen){
            let mut args = Vec::new();

            if !self.check(Token::RParen){
                args.push(self.typedec()?);
                while self.accept(Token::Comma){
                    args.push(self.typedec()?);
                }
            }
            self.expect(Token::RParen)?;
            let typ = self.typedec()?;
            Ok(Type::Function{args, ret: Box::new(typ)})
        }else{
            let id = self.ident()?;
            let sym = self.lookup(&id)?;
            if sym.kind == Kind::Type{
                Ok(sym.typ)
            }else{
                self.mark("Symbol isnt Type")
            }
        }
    }

// block -> { stmt }
    fn block(&mut self) -> Parsing<()>{
        while !self.check(Token::End) && 
            !self.check(Token::Elsif) && 
            !self.check(Token::Else) && 
            !self.check(Token::Error)
        {
            self.stmt()?;
        }
        Ok(())
    }

// stmt = ID ':=' expr ';'
//      | 'return' expr ';'
//      | 'if' expr 'then' block {'elsif' expr 'then' block} ['else' block] 'end'
//      | 'while' expr 'do' block 'end'
    fn stmt(&mut self) -> Parsing<()>{
        if self.accept(Token::Return){
            let typ1 = self.expr()?;
            let typ2 = self.module.get_last_fun().typ.get_ret();
            self.typecheck(&typ1, &typ2)?;
            self.expect(Token::Semi)?;
            Ok(())
        }else if self.accept(Token::If){
            let if_typ = self.expr()?;
            self.typecheck(&Type::Bool, &if_typ)?;
            self.expect(Token::Then)?;
            let mut then_block = self.builder.new_block();
            let mut else_block = self.builder.new_block();
            let end_block = self.builder.new_block();
            self.builder.jump_if(then_block, else_block);
            self.builder.select(then_block);
            self.block()?;
            self.builder.jump(end_block);
            self.builder.select(else_block);
            while self.accept(Token::Elsif){
                let elsif_typ = self.expr()?;
                self.typecheck(&Type::Bool, &elsif_typ)?;
                self.expect(Token::Then)?;
                then_block = self.builder.new_block();
                else_block = self.builder.new_block();
                self.builder.jump_if(then_block, else_block);
                self.builder.select(then_block);
                self.block()?;
                self.builder.jump(end_block);
                self.builder.select(else_block);
            }
            if self.accept(Token::Else){
                self.block()?;
            }
            self.expect(Token::End)?;
            self.builder.jump(end_block);
            self.builder.select(end_block);
            Ok(())
        }else if self.accept(Token::While){
            let cond_block = self.builder.new_block();
            self.builder.jump(cond_block);
            self.builder.select(cond_block);
            let while_typ = self.expr()?;
            self.typecheck(&Type::Bool, &while_typ)?;
            self.expect(Token::Do)?;
            let loop_block = self.builder.new_block();
            let end_block = self.builder.new_block();
            self.builder.jump_if(loop_block, end_block);
            self.builder.select(loop_block);
            self.block()?;
            self.expect(Token::End)?;
            self.builder.jump(cond_block);
            self.builder.select(end_block);
            Ok(())
        }else{
            let id = self.ident()?;
            let sym = self.lookup(&id)?;
            self.expect(Token::Assign)?; 
            let expr_typ = self.expr()?;            
            self.typecheck(&sym.typ, &expr_typ)?;
            self.expect(Token::Semi)?;
            self.builder.store(sym);
            Ok(())
        }        
    }

// expr -> conjunction {'or' conjunction}
    fn expr(&mut self) -> Parsing<Type>{
        let typ = self.conjunction()?;
        while self.accept(Token::Or){
            let conj_typ = self.conjunction()?;
            self.typecheck(&typ, &conj_typ)?;
            self.builder.or();
        }
        Ok(typ)
    }

// conjunction -> relation { 'and' relation }
    fn conjunction(&mut self) -> Parsing<Type>{
        let typ = self.relation()?;
        while self.accept(Token::And){
            let rel_typ = self.relation()?;
            self.typecheck(&typ, &rel_typ)?;
            self.builder.and();
        }
        Ok(typ)
    }

// relation -> [ 'not' ] sum [ ( '==' | '<' | '>' | '<=' | '>=' ) sum ]
    fn relation(&mut self) -> Parsing<Type>{
        let is_negated = self.accept(Token::Not);
        let mut typ = self.sum()?;

        if self.accept(Token::EqualEqual){
                let sum_typ = self.sum()?;
                self.typecheck(&typ, &sum_typ)?;
                typ = Type::Bool;
                self.builder.comp_eq();
        }else if self.accept(Token::LessThan){
                let sum_typ = self.sum()?;
                self.typecheck(&typ, &sum_typ)?;
                typ = Type::Bool;
                self.builder.comp_lt();
        }else if self.accept(Token::GreaterThan){
                let sum_typ = self.sum()?;
                self.typecheck(&typ, &sum_typ)?;
                typ = Type::Bool;
                self.builder.comp_gt();
        }else if self.accept(Token::LessEqual){ 
                let sum_typ = self.sum()?;
                self.typecheck(&typ, &sum_typ)?;
                typ = Type::Bool;
                self.builder.comp_le();
        }else if self.accept(Token::GreaterEqual){
                let sum_typ = self.sum()?;
                self.typecheck(&typ, &sum_typ)?;
                typ = Type::Bool;
                self.builder.comp_ge();
        }
        if is_negated{
            self.typecheck(&Type::Bool, &typ)?;
            self.builder.not();
        }
        Ok(typ)
    }

// sum -> ['-'] term { ('+' | '-') term }
    fn sum(&mut self) -> Parsing<Type>{
        let has_minus = self.accept(Token::Minus);
        let typ = self.term()?;
        loop{
        	if self.accept(Token::Plus){
                let term_typ = self.term()?;
                self.typecheck(&typ, &term_typ)?;
        		self.builder.add();
        	}else if self.accept(Token::Minus){
                let term_typ = self.term()?;
                self.typecheck(&typ, &term_typ)?;
            	self.builder.sub(); 
        	}else{
        		break;
        	}
        }
        if has_minus{
            self.builder.negate();
        }
        Ok(typ)
    }

// term <- atom { ('*' | '/' | '%') atom }
    fn term(&mut self) -> Parsing<Type>{
        let typ = self.designator()?;
        loop{
        	if self.accept(Token::Times){
                let des_typ = self.designator()?;
                self.typecheck(&typ, &des_typ)?;
        		self.builder.mul();
        	}else if self.accept(Token::Over){
                let des_typ = self.designator()?;
                self.typecheck(&typ, &des_typ)?;
        		self.builder.div();
        	}else if self.accept(Token::Modulo){
                let des_typ = self.designator()?;
                self.typecheck(&typ, &des_typ)?;
        		self.builder.modulo();
        	}else{
        		break Ok(typ);
        	}
        }
    }

//  designator <- atom { '(' [ expr {',' expr } ] ')' }
    fn designator(&mut self) -> Parsing<Type>{
        let mut sym = self.atom()?;
        while self.accept(Token::LParen){
            let mut args = Vec::new();
            if !self.check(Token::RParen){
                loop{
                    args.push(self.expr()?);
                    if !self.accept(Token::Comma){
                        break
                    }
                }
            }
            self.expect(Token::RParen)?;
            let mut fun_typ = Type::Function{args, ret: Box::new(Type::Unknown)};
            fun_typ = self.typecheck(&sym.typ, &fun_typ)?;
            self.builder.call(sym.clone());
            sym = Symbol::temporary(fun_typ.get_ret());
        }
        if let Kind::Function{..} = sym.kind{
            self.builder.load(sym.clone());
            sym = Symbol::temporary(sym.typ);
        }
        Ok(sym.typ)
    }

//  atom <- '(' expr ')'
//  atom <- Integer 
//  atom <- Id 
    fn atom(&mut self) -> Parsing<Symbol>{
        if self.accept(Token::LParen){
            let typ = self.expr()?;
            let sym = Symbol::temporary(typ);
            self.expect(Token::RParen)?;
            Ok(sym) 
        }else if self.accept(Token::Integer){
            let value = self.lexer.number;
            let sym = Symbol::constant(Type::Integer, value);
            self.builder.constant(sym.clone());
            Ok(sym)
        }else{
            let id = self.ident()?;
            let sym = self.lookup(&id)?;
            match sym.kind{
                Kind::Function{..} => {}
                _ => self.builder.load(sym.clone()),
            }
            Ok(sym)
        }
    }

//ident <- ID
    fn ident(&mut self) -> Parsing<String>{
        let word = self.lexer.word.to_string();
        self.expect(Token::Ident)?;
        Ok(word)
    }
}
