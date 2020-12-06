/*	TODO
    expand syntax
    test test test
    less fields in the Parser
    use Result?
    use combinators?
*/

use crate::builder::Builder;
use crate::lexer::{LexPos, Lexer, Token, TokenSet};
use crate::module::{FuncType, Ident, Module, Symbol, Type};
use std::fmt;
use std::io::Read;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    ParseErr {
        position: LexPos,
        tried: TokenSet,
        found: Token,
    },
    TypeErr {
        position: LexPos,
        left: Type,
        right: Type,
    },
    UndefErr {
        position: LexPos,
        id: Ident,
    },
    GeneralErr {
        position: LexPos,
        msg: String,
    },
}

type Parsing<T> = Result<T, CompileError>;

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::ParseErr {
                position,
                tried,
                found,
            } => writeln!(
                f,
                "{} Syntax error:\n Expected: {}\n Found: {}",
                position, tried, found
            ),
            CompileError::TypeErr {
                position,
                left,
                right,
            } => writeln!(
                f,
                "{} Type mismatch: Left side is {}. Right side is {}.",
                position, left, right
            ),
            CompileError::UndefErr { position, id } => {
                writeln!(f, "{} Symbol Undefined: '{}'", position, id)
            }
            CompileError::GeneralErr { position, msg } => writeln!(f, "{} {}", position, msg),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parser<I: Read> {
    lexer: Lexer<I>,
    current: Token,
    tried: TokenSet,
    module: Module,
    current_fun: FuncType,
    builder: Builder,
}
impl<I: std::io::Read> Parser<I> {
    pub fn new_file(filename: String, input: I) -> Parser<I> {
        let mut p = Parser {
            lexer: Lexer::new(filename, input),
            current: Token::Error,
            tried: TokenSet::new(),
            module: Module::new(),
            current_fun: FuncType {
                args: Vec::new(),
                ret: Type::Unknown,
            },
            builder: Builder::new(),
        };
        p.current = p.lexer.next();
        p
    }

    pub fn done(self) -> (Module, Builder) {
        (self.module, self.builder)
    }

    //error handling and parser utilities-------------------------------------------

    fn syntax_err<T>(&mut self) -> Parsing<T> {
        self.current = Token::Error;
        Err(CompileError::ParseErr {
            position: self.lexer.pos(),
            tried: self.tried,
            found: self.current,
        })
    }

    fn mark<T>(&mut self, text: &str) -> Parsing<T> {
        self.current = Token::Error;
        Err(CompileError::GeneralErr {
            position: self.lexer.pos(),
            msg: text.to_string(),
        })
    }

    fn typecheck(&mut self, a: &Type, b: &Type) -> Parsing<Type> {
        let c = a.unify(b);
        if c.is_some() {
            return Ok(c.unwrap());
        }
        self.current = Token::Error;
        Err(CompileError::TypeErr {
            position: self.lexer.pos(),
            left: a.clone(),
            right: b.clone(),
        })
    }

    fn lookup(&mut self, id: Ident) -> Parsing<Symbol> {
        let sym = self.module.get_sym(&id);
        if sym.is_none() {
            Err(CompileError::UndefErr {
                position: self.lexer.pos(),
                id: id.clone(),
            })
        } else {
            Ok(sym.unwrap())
        }
    }

    fn check(&self, t: Token) -> bool {
        self.current == t
    }

    fn accept(&mut self, t: Token) -> bool {
        if self.check(t) {
            self.current = self.lexer.next();
            self.tried = TokenSet::new();
            true
        } else {
            self.tried.set(t);
            false
        }
    }

    fn expect(&mut self, t: Token) -> Parsing<()> {
        if self.check(t) {
            self.current = self.lexer.next();
            self.tried = TokenSet::new();
            Ok(())
        } else {
            self.tried.set(t);
            self.syntax_err()
        }
    }

    //parser rules------------------------------------------------------------------

    // module -> {item} EOF
    pub fn module(&mut self) -> Vec<Parsing<()>> {
        let mut items = Vec::new();
        while !self.accept(Token::Eof) {
            if self.check(Token::Fun) {
                items.push(self.item());
            } else {
                self.current = self.lexer.next();
            }
        }
        items
    }

    //item -> 'fun' funhead funbody
    fn item(&mut self) -> Parsing<()> {
        self.expect(Token::Fun)?;
        self.funhead()?;
        self.funbody()
    }

    // funhead -> ID '(' [ ID { ',' ID } ] ')' ':' funtype
    fn funhead(&mut self) -> Parsing<()> {
        let id = self.ident()?;
        let mut args_id = Vec::new();

        self.expect(Token::LParen)?;
        if !self.check(Token::RParen) {
            args_id.push(self.ident()?);
            while self.accept(Token::Comma) {
                args_id.push(self.ident()?);
            }
        }
        self.expect(Token::RParen)?;
        self.expect(Token::Colon)?;
        let typ = self.funtype()?;

        let start = self.builder.new_function(id.clone());
        self.builder.select(start);
        self.module.declare_fun(id, typ.clone(), start);
        self.current_fun = typ.clone();
        if typ.args.len() != args_id.len() {
            self.mark("Arity mismatch.")?;
        }
        for i in 0..args_id.len() {
            self.module
                .declare_arg(args_id[i].clone(), typ.args[i].clone());
        }
        Ok(())
    }

    // funbody  '=' expr ';'
    // funbody -> 'is' vardec block 'end'
    fn funbody(&mut self) -> Parsing<()> {
        if self.accept(Token::Equal) {
            self.builder.enter(self.current_fun.args.len(), 0);
            let typ1 = self.expr()?;
            let typ2 = self.current_fun.ret.clone();
            self.typecheck(&typ1, &typ2)?;
            self.expect(Token::Semi)?;
            Ok(())
        } else {
            self.expect(Token::Is)?;
            let n_vars = self.vardec()?;
            self.builder.enter(self.current_fun.args.len(), n_vars);
            self.block()?;
            self.expect(Token::End)?;
            Ok(())
        }
    }

    // vardec -> { 'var' ID { ',' ID } ':' typedec ';' }
    fn vardec(&mut self) -> Parsing<usize> {
        let mut count = 0;
        while self.accept(Token::Var) {
            let mut ids = Vec::new();
            ids.push(self.ident()?);
            count += 1;
            while self.accept(Token::Comma) {
                ids.push(self.ident()?);
                count += 1;
            }
            self.expect(Token::Colon)?;
            let typ = self.typedec()?;
            for id in ids {
                self.module.declare_var(id, typ.clone());
            }
            self.expect(Token::Semi)?;
        }
        Ok(count)
    }

    // typedec <- funtype | Id
    fn typedec(&mut self) -> Parsing<Type> {
        if self.check(Token::LParen) {
            Ok(self.funtype()?.into_type())
        } else {
            let id = self.ident()?;
            let sym = self.lookup(id)?;
            if sym.is_type() {
                Ok(sym.get_typ())
            } else {
                self.mark("Symbol isnt Type")
            }
        }
    }

    //funtype -> '(' [ typedec { ',' typedec } ] ')' typedec
    fn funtype(&mut self) -> Parsing<FuncType> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.check(Token::RParen) {
            args.push(self.typedec()?);
            while self.accept(Token::Comma) {
                args.push(self.typedec()?);
            }
        }
        self.expect(Token::RParen)?;
        let typ = self.typedec()?;
        Ok(FuncType { args, ret: typ })
    }

    // block -> { stmt }
    fn block(&mut self) -> Parsing<()> {
        while !self.check(Token::End)
            && !self.check(Token::Elsif)
            && !self.check(Token::Else)
            && !self.check(Token::Error)
        {
            self.stmt()?;
        }
        Ok(())
    }

    // stmt = ID ':=' expr ';'
    //      | 'return' expr ';'
    //      | 'if' clause 'then' block {'elsif' clause 'then' block} ['else' block] 'end'
    //      | 'while' clause 'do' block 'end'
    fn stmt(&mut self) -> Parsing<()> {
        if self.accept(Token::Return) {
            let typ1 = self.expr()?;
            let typ2 = self.current_fun.ret.clone();
            self.typecheck(&typ1, &typ2)?;
            self.expect(Token::Semi)?;
            self.builder.ret();
            Ok(())
        } else if self.accept(Token::If) {
            self.clause()?;
            self.expect(Token::Then)?;
            let mut then_block = self.builder.new_block();
            let mut else_block = self.builder.new_block();
            let end_block = self.builder.new_block();
            self.builder.jump_if(then_block, else_block);
            self.builder.select(then_block);
            self.block()?;
            self.builder.jump(end_block);
            self.builder.select(else_block);
            while self.accept(Token::Elsif) {
                self.clause()?;
                self.expect(Token::Then)?;
                then_block = self.builder.new_block();
                else_block = self.builder.new_block();
                self.builder.jump_if(then_block, else_block);
                self.builder.select(then_block);
                self.block()?;
                self.builder.jump(end_block);
                self.builder.select(else_block);
            }
            if self.accept(Token::Else) {
                self.block()?;
            }
            self.expect(Token::End)?;
            self.builder.jump(end_block);
            self.builder.select(end_block);
            Ok(())
        } else if self.accept(Token::While) {
            let cond_block = self.builder.new_block();
            self.builder.jump(cond_block);
            self.builder.select(cond_block);
            self.clause()?;
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
        } else {
            let id = self.ident()?;
            let sym = self.lookup(id)?;
            self.expect(Token::Assign)?;
            let expr_typ = self.expr()?;
            self.typecheck(&sym.get_typ(), &expr_typ)?;
            self.expect(Token::Semi)?;
            self.builder.store(sym);
            Ok(())
        }
    }
    /*
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
    */

    // clause -> [ 'not' ] expr ( '==' | '<' | '<=' | '>=' | '>' ) expr
    fn clause(&mut self) -> Parsing<()> {
        let negate = self.accept(Token::Not);
        let typ = self.expr()?;

        let op = if self.accept(Token::EqualEqual) {
            if negate {
                Builder::comp_neq
            } else {
                Builder::comp_eq
            }
        } else if self.accept(Token::LessThan) {
            if negate {
                Builder::comp_ge
            } else {
                Builder::comp_lt
            }
        } else if self.accept(Token::GreaterThan) {
            if negate {
                Builder::comp_le
            } else {
                Builder::comp_gt
            }
        } else if self.accept(Token::LessEqual) {
            if negate {
                Builder::comp_gt
            } else {
                Builder::comp_le
            }
        } else {
            self.expect(Token::GreaterEqual)?;
            if negate {
                Builder::comp_lt
            } else {
                Builder::comp_ge
            }
        };
        let sum_typ = self.expr()?;
        self.typecheck(&typ, &sum_typ)?;
        op(&mut self.builder);
        Ok(())
    }

    // expr -> ['-'] term { ('+' | '-') term }
    fn expr(&mut self) -> Parsing<Type> {
        let has_minus = self.accept(Token::Minus);
        let typ = self.term()?;
        if has_minus {
            self.builder.negate();
        }
        loop {
            if self.accept(Token::Plus) {
                let term_typ = self.term()?;
                self.typecheck(&typ, &term_typ)?;
                self.builder.add();
            } else if self.accept(Token::Minus) {
                let term_typ = self.term()?;
                self.typecheck(&typ, &term_typ)?;
                self.builder.sub();
            } else {
                break;
            }
        }

        Ok(typ)
    }

    // term <- atom { ('*' | '/' | '%') atom }
    fn term(&mut self) -> Parsing<Type> {
        let typ = self.designator()?;
        loop {
            if self.accept(Token::Times) {
                let des_typ = self.designator()?;
                self.typecheck(&typ, &des_typ)?;
                self.builder.mul();
            } else if self.accept(Token::Over) {
                let des_typ = self.designator()?;
                self.typecheck(&typ, &des_typ)?;
                self.builder.div();
            } else if self.accept(Token::Modulo) {
                let des_typ = self.designator()?;
                self.typecheck(&typ, &des_typ)?;
                self.builder.modulo();
            } else {
                break Ok(typ);
            }
        }
    }

    //  designator <- atom { '(' [ expr {',' expr } ] ')' }

    //  designator <- atom
    fn designator(&mut self) -> Parsing<Type> {
        let typ = self.atom()?;
        /*
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
            let mut fun_typ = FuncType::from_args(args).into_type();
            fun_typ = self.typecheck(&typ, &fun_typ)?;
            self.builder.icall(typ);
            typ = fun_typ.get_function().ret.clone();
        }
        */
        Ok(typ)
    }

    //  atom <- '(' expr ')'
    //  atom <- Integer
    //  atom <- Id [ '(' [ expr {',' expr } ] ')' ]
    fn atom(&mut self) -> Parsing<Type> {
        if self.accept(Token::LParen) {
            let typ = self.expr()?;
            self.expect(Token::RParen)?;
            Ok(typ)
        } else if self.accept(Token::Integer) {
            let value = self.lexer.number;
            self.builder.constant(value);
            Ok(Type::Integer)
        } else {
            let id = self.ident()?;
            let sym = self.lookup(id)?;
            if sym.is_function() && self.accept(Token::LParen) {
                let mut args = Vec::new();
                if !self.check(Token::RParen) {
                    loop {
                        args.push(self.expr()?);
                        if !self.accept(Token::Comma) {
                            break;
                        }
                    }
                }
                self.expect(Token::RParen)?;
                let mut fun_typ = FuncType::from_args(args).into_type();
                fun_typ = self.typecheck(&sym.get_typ(), &fun_typ)?;
                self.builder.call(sym.clone());
                Ok(fun_typ.get_function().ret.clone())
            } else {
                self.builder.load(sym.clone());
                Ok(sym.get_typ())
            }
        }
    }

    //ident <- ID
    fn ident(&mut self) -> Parsing<Ident> {
        let id = self.module.make_id(&self.lexer.word).clone();
        self.expect(Token::Ident)?;
        Ok(id)
    }
}
