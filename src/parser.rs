/*	TODO
    expand syntax
    test test test
    less fields in the Parser
    use Result?
    use combinators?
*/

use crate::builder::{Function, FunctionBuilder, Type, Relation};
use crate::lexer::{LexPos, Lexer, Token, TokenSet};
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
}
impl Parser {
    pub fn new_file(filename: String) -> Parser {
        let mut p = Parser {
            lexer: Lexer::new(filename),
            current: Token::Error,
            tried: TokenSet::new(),
        };
        p.current = p.lexer.next();
        p
    }

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
                Err(e) => eprintln!("{}", e),
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
        expr(parser, fun)?;
        parser.expect(Token::Semi)?;
        fun.ret();
        Ok(())
    } else {
        parser.expect(Token::Is)?;
        vardec(parser, fun)?;
        let start = fun.new_block();
        fun.select_block(start);
        block(parser, fun)?;
        parser.expect(Token::End)?;
        Ok(())
    }
}

// vardec -> { 'var' ID { ',' ID } ':' type_dec ';' }
fn vardec(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    while parser.accept(Token::Var) {
        fun.alloc(&ident(parser)?);
        while parser.accept(Token::Comma) {
            fun.alloc(&ident(parser)?);
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
        expr(parser, fun)?;
        fun.ret();
        parser.expect(Token::Semi)?;
        Ok(())
    } else if parser.accept(Token::If) {
        let end = fun.new_block();
        loop {
            let els = fun.new_block();
            let then = fun.new_block();
            clause(parser, fun)?;
            parser.expect(Token::Then)?;
            fun.branch(then, els);
            fun.select_block(then);
            block(parser, fun)?;
            fun.jump(end);
            fun.select_block(els);
            if !parser.accept(Token::Elsif) {
                break;
            }
        }
        if parser.accept(Token::Else) {
            block(parser, fun)?;
        }
        parser.expect(Token::End)?;
        fun.jump(end);
        fun.select_block(end);
        Ok(())
    } else if parser.accept(Token::While) {
        let cond = fun.new_block();
        let lop = fun.new_block();
        let end = fun.new_block();

        fun.jump(cond);
        fun.select_block(cond);
        clause(parser, fun)?;
        parser.expect(Token::Do)?;
        fun.branch(lop, end);
        fun.select_block(lop);
        block(parser, fun)?;
        parser.expect(Token::End)?;
        fun.jump(cond);
        fun.select_block(end);
        Ok(())
    } else {
        let id = ident(parser)?;
        parser.expect(Token::Assign)?;
        expr(parser, fun)?;
        parser.expect(Token::Semi)?;
        fun.store(&id);
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
    expr(parser, fun)?;

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

    expr(parser, fun)?;
    fun.cmp(op);
    Ok(())
}

// expr -> ['-'] term { ('+' | '-') term }
fn expr(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    let has_minus = parser.accept(Token::Minus);
    let mut a = term(parser, fun)?;
    if has_minus {
        a = fun.neg();
    }
    loop {
        if parser.accept(Token::Plus) {
            term(parser, fun)?;
            a = fun.add();
        } else if parser.accept(Token::Minus) {
            term(parser, fun)?;
            a = fun.sub();
        } else {
            break Ok(a);
        }
    }
}

// term <- atom { ('*' | '/' | '%') atom }
fn term(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    let mut a = designator(parser, fun)?;
    loop {
        if parser.accept(Token::Times) {
            designator(parser, fun)?;
            a = fun.mul();
        } else if parser.accept(Token::Over) {
            designator(parser, fun)?;
            a = fun.div();
        } else if parser.accept(Token::Modulo) {
            designator(parser, fun)?;
            a = fun.modulo();
        } else {
            break Ok(a);
        }
    }
}

//  designator <- atom { '(' [ expr {',' expr } ] ')' }

//  designator <- atom
fn designator(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
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
fn atom(parser: &mut Parser, fun: &mut FunctionBuilder) -> Parsing<()> {
    if parser.accept(Token::LParen) {
        let val = expr(parser, fun)?;
        parser.expect(Token::RParen)?;
        Ok(val)
    } else if parser.accept(Token::Integer) {
        let value = parser.lexer.number;
        Ok(fun.constant(value))
    } else {
        let id = ident(parser)?;
        if parser.accept(Token::LParen) {
            if !parser.check(Token::RParen) {
                expr(parser, fun)?;
                while parser.accept(Token::Comma) {
                    expr(parser, fun)?;
                }
            }
            parser.expect(Token::RParen)?;
            Ok(fun.call(id))
        } else {
            Ok(fun.load(&id))
        }
    }
}

//ident <- ID
fn ident(parser: &mut Parser) -> Parsing<Ident> {
    let id = parser.lexer.word.clone();
    parser.expect(Token::Ident)?;
    Ok(id)
}
