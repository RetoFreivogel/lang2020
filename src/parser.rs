/*	TODO
    expand syntax
    test test test
    less fields in the Parser
    use Result?
    use combinators?
*/

use crate::builder::{Function, FunctionBuilder, Relation, Type, Value};
use crate::lexer::{LexPos, Lexer, Token, TokenSet};
//use crate::module::{FuncType, Module, Symbol, Type};
use crate::strtab::Ident;
use std::fmt;

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

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current: Token,
    tried: TokenSet,
    //module: Module,
    //current_fun: FuncType,
    //builder: Builder,
}
impl Parser {
    pub fn new_file(filename: String) -> Parser {
        let mut p = Parser {
            lexer: Lexer::new(filename),
            current: Token::Error,
            tried: TokenSet::new(),
            //module: Module::new(),
            //current_fun: FuncType {
            //    args: Vec::new(),
            //    ret: Type::Unknown,
            //},
            //builder: Builder::new(),
        };
        p.current = p.lexer.next();
        p
    }

    /*
    pub fn done(self) -> (Module, Builder) {
        (self.module, self.builder)
    }
    */

    //error handling and parser utilities-------------------------------------------

    fn syntax_err<T>(&mut self) -> Parsing<T> {
        let err = Err(CompileError::ParseErr {
            position: self.lexer.pos(),
            tried: self.tried,
            found: self.current,
        });
        self.current = Token::Error;
        err
    }

    fn mark<T>(&mut self, text: &str) -> Parsing<T> {
        self.current = Token::Error;
        Err(CompileError::GeneralErr {
            position: self.lexer.pos(),
            msg: text.to_string(),
        })
    }

    /*
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
    */

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
}
//parser rules------------------------------------------------------------------

// module -> {item} EOF
pub fn module(parser: &mut Parser) -> crate::builder::Module {
    let mut items = Vec::new();
    while !parser.accept(Token::Eof) {
        if parser.check(Token::Fun) {
            match item(parser) {
                Ok(fun) => items.push(fun),
                Err(e) => eprintln!("{:?}", e),
            }
        } else {
            parser.current = parser.lexer.next();
        }
    }
    crate::builder::Module { items }
}

//item -> 'fun' ID '(' [ ID { ',' ID } ] ')' ':' type_dec funbody
fn item(parser: &mut Parser) -> Parsing<Function> {
    parser.expect(Token::Fun)?;
    let id = ident(parser)?;

    let mut args = Vec::new();
    parser.expect(Token::LParen)?;
    if !parser.check(Token::RParen) {
        args.push(ident(parser)?);
        while parser.accept(Token::Comma) {
            args.push(ident(parser)?);
        }
    }
    parser.expect(Token::RParen)?;
    parser.expect(Token::Colon)?;
    let typ = type_dec(parser)?;

    let mut fun = FunctionBuilder::new(id, args, typ);

    funbody(parser, &mut fun)?;
    let fun = fun.done();
    Ok(fun)
}

// funbody  '=' expr ';'
// funbody -> 'is' vardec block 'end'
fn funbody(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    if parser.accept(Token::Equal) {
        let a = expr(parser, fun)?;
        parser.expect(Token::Semi)?;
        fun.term_return(a);
        Ok(())
    } else {
        parser.expect(Token::Is)?;
        vardec(parser, fun)?;
        let start = fun.new_block();
        fun.begin_block(start);
        block(parser, fun)?;
        parser.expect(Token::End)?;
        Ok(())
    }
}

// vardec -> { 'var' ID { ',' ID } ':' type_dec ';' }
fn vardec(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    while parser.accept(Token::Var) {
        fun.add_var(ident(parser)?);
        while parser.accept(Token::Comma) {
            fun.add_var(ident(parser)?);
        }
        parser.expect(Token::Colon)?;
        let _typ = type_dec(parser)?;
        parser.expect(Token::Semi)?;
    }
    Ok(())
}

// type_dec <- '(' [ Id { ',' Id } ] ')' Id | Id
fn type_dec(parser: &mut Parser) -> Parsing<Type> {
    if parser.accept(Token::LParen) {
        let mut args = Vec::new();
        if !parser.check(Token::RParen) {
            args.push(ident(parser)?);
            while parser.accept(Token::Comma) {
                args.push(ident(parser)?);
            }
        }
        parser.expect(Token::RParen)?;
        let ret = ident(parser)?;
        Ok(Type::Func { args, ret })
    } else {
        let id = ident(parser)?;
        Ok(Type::Id(id))
    }
}

// block -> { stmt } [ "return" expr ]
fn block(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    while parser.check(Token::If)
        || parser.check(Token::While)
        || parser.check(Token::Ident)
        || parser.check(Token::Return)
    {
        stmt(parser, fun)?;
    }
    Ok(())
}

// stmt = ID ':=' expr ';'
//      | 'return' expr ';'
//      | 'if' clause 'then' block {'elsif' clause 'then' block} ['else' block] 'end'
//      | 'while' clause 'do' block 'end'
fn stmt(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    if parser.accept(Token::Return) {
        let a = expr(parser, fun)?;
        fun.term_return(a);
        parser.expect(Token::Semi)?;
        Ok(())
    } else if parser.accept(Token::If) {
        let end = fun.new_block();
        loop {
            let els = fun.new_block();
            let then = fun.new_block();
            clause(parser, fun)?;
            parser.expect(Token::Then)?;
            fun.term_branch(then, els);
            fun.begin_block(then);
            block(parser, fun)?;
            fun.term_jump(end);
            fun.begin_block(els);
            if !parser.accept(Token::Elsif) {
                break;
            }
        }
        if parser.accept(Token::Else) {
            block(parser, fun)?;
        }
        parser.expect(Token::End)?;
        fun.term_jump(end);
        fun.begin_block(end);
        Ok(())
    } else if parser.accept(Token::While) {
        let cond = fun.new_block();
        let lop = fun.new_block();
        let end = fun.new_block();

        fun.term_jump(cond);
        fun.begin_block(cond);
        clause(parser, fun)?;
        parser.expect(Token::Do)?;
        fun.term_branch(lop, end);
        fun.begin_block(lop);
        block(parser, fun)?;
        parser.expect(Token::End)?;
        fun.term_jump(cond);
        fun.begin_block(end);
        Ok(())
    } else {
        let id = ident(parser)?;
        parser.expect(Token::Assign)?;
        let a = expr(parser, fun)?;
        parser.expect(Token::Semi)?;
        fun.add_store(&id, a);
        Ok(())
    }
}
/*
// expr -> conjunction {'or' conjunction}
    fn expr(&mut self) -> Parsing<Type>{
        let typ = self.conjunction()?;
        while parser.accept(Token::Or){
            let conj_typ = self.conjunction()?;
            self.typecheck(&typ, &conj_typ)?;
            self.builder.or();
        }
        Ok(typ)
    }

// conjunction -> relation { 'and' relation }
    fn conjunction(&mut self) -> Parsing<Type>{
        let typ = self.relation()?;
        while parser.accept(Token::And){
            let rel_typ = self.relation()?;
            self.typecheck(&typ, &rel_typ)?;
            self.builder.and();
        }
        Ok(typ)
    }
*/

// clause -> [ 'not' ] expr ( '==' | '<' | '<=' | '>=' | '>' ) expr
fn clause(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    let negate = parser.accept(Token::Not);
    let a = expr(parser, fun)?;

    let mut op = if parser.accept(Token::EqualEqual) {
        Relation::Eq
    } else if parser.accept(Token::LessThan) {
        Relation::Lt
    } else if parser.accept(Token::GreaterThan) {
        Relation::Gt
    } else if parser.accept(Token::LessEqual) {
        Relation::Le
    } else {
        parser.expect(Token::GreaterEqual)?;
        Relation::Ge
    };
    if negate {
        op = op.negate()
    }

    let b = expr(parser, fun)?;
    fun.add_cmp(op, a, b);
    Ok(())
}

// expr -> ['-'] term { ('+' | '-') term }
fn expr(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<Value> {
    let has_minus = parser.accept(Token::Minus);
    let mut a = term(parser, fun)?;
    if has_minus {
        a = fun.add_neg(a);
    }
    loop {
        if parser.accept(Token::Plus) {
            let b = term(parser, fun)?;
            a = fun.add_add(a, b);
        } else if parser.accept(Token::Minus) {
            let b = term(parser, fun)?;
            a = fun.add_sub(a, b);
        } else {
            break Ok(a);
        }
    }
}

// term <- atom { ('*' | '/' | '%') atom }
fn term(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<Value> {
    let mut a = designator(parser, fun)?;
    loop {
        if parser.accept(Token::Times) {
            let b = designator(parser, fun)?;
            a = fun.add_mul(a, b);
        } else if parser.accept(Token::Over) {
            let b = designator(parser, fun)?;
            a = fun.add_div(a, b);
        } else if parser.accept(Token::Modulo) {
            let b = designator(parser, fun)?;
            a = fun.add_mod(a, b);
        } else {
            break Ok(a);
        }
    }
}

//  designator <- atom { '(' [ expr {',' expr } ] ')' }

//  designator <- atom
fn designator(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<Value> {
    atom(parser, fun)
}
/*
while parser.accept(Token::LParen){
    let mut args = Vec::new();
    if !parser.check(Token::RParen){
        loop{
            args.push(self.expr()?);
            if !parser.accept(Token::Comma){
                break
            }
        }
    }
    parser.expect(Token::RParen)?;
    let mut fun_typ = FuncType::from_args(args).into_type();
    fun_typ = self.typecheck(&typ, &fun_typ)?;
    self.builder.icall(typ);
    typ = fun_typ.get_function().ret.clone();
}
*/

//  atom <- '(' expr ')'
//  atom <- Integer
//  atom <- Id [ '(' [ expr {',' expr } ] ')' ]
fn atom(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<Value> {
    if parser.accept(Token::LParen) {
        let val = expr(parser, fun)?;
        parser.expect(Token::RParen)?;
        Ok(val)
    } else if parser.accept(Token::Integer) {
        let value = parser.lexer.number;
        Ok(fun.add_const(value))
    } else {
        let id = ident(parser)?;
        let mut args = Vec::new();
        if parser.accept(Token::LParen) {
            if !parser.check(Token::RParen) {
                args.push(expr(parser, fun)?);
                while parser.accept(Token::Comma) {
                    args.push(expr(parser, fun)?);
                }
            }
            parser.expect(Token::RParen)?;
            Ok(fun.add_call(id, args))
        } else {
            Ok(fun.add_load(&id))
        }
    }
}

//ident <- ID
fn ident(parser: &mut Parser) -> Parsing<Ident> {
    let id = parser.lexer.word.clone();
    parser.expect(Token::Ident)?;
    Ok(id)
}
