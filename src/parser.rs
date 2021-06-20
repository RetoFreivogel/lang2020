/*	TODO
    expand syntax
    test test test
    less fields in the Parser
    use Result?
    use combinators?
*/

use crate::lexer::{LexPos, Lexer, Token, TokenSet};
use crate::strtab::{Ident, Type};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    ParseErr {
        position: LexPos,
        tried: TokenSet,
        found: Token,
    },
    /*
    TypeErr {
        position: LexPos,
        left: Type,
        right: Type,
    },
    MissingReturnErr {
        position: LexPos,
    },
    */
    ArityErr {
        position: LexPos,
        typ: Type,
        arglen: usize,
    },
    KindErr {
        position: LexPos,
        typ: Type,
    },
    UndefErr {
        position: LexPos,
        id: Ident,
    }
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
            /*
            CompileError::TypeErr {
                position,
                left,
                right,
            } => writeln!(
                f,
                "{} Type mismatch: Left side is {}. Right side is {}.",
                position, left, right
            ),
            CompileError::MissingReturnErr {
                position,
            } => writeln!(f, "{} Expected a return statement.", position),
            */
            CompileError::ArityErr {
                position,
                typ,
                arglen,
            } => writeln!(
                f,
                "{} Arity mismatch: Type is {}. But there are {} arguments.",
                position, typ, arglen
            ),
            CompileError::KindErr {
                position,
                typ,
            } => writeln!(
                f,
                "{} Kind mismatch: Expected a function type. Found {}.",
                position, typ
            ),
            CompileError::UndefErr { position, id } => {
                writeln!(f, "{} Symbol Undefined: '{}'", position, id)
            }
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

    //error handling and self utilities-------------------------------------------

    fn mark_syntax<T>(&mut self) -> Parsing<T> {
        let err = Err(CompileError::ParseErr {
            position: self.lexer.pos(),
            tried: self.tried,
            found: self.current,
        });
        self.current = Token::Error;
        err
    }

    fn mark_undef<T>(&mut self, id: Ident) -> Parsing<T> {
        let err = Err(CompileError::UndefErr {
            position: self.lexer.pos(),
            id,
        });
        self.current = Token::Error;
        err
    }

    fn mark_arity<T>(&mut self, typ: Type, arglen: usize) -> Parsing<T> {
        let err = Err(CompileError::ArityErr {
            position: self.lexer.pos(),
            typ,
            arglen,
        });
        self.current = Token::Error;
        err
    }

    fn mark_kind<T>(&mut self, typ: Type) -> Parsing<T> {
        let err = Err(CompileError::KindErr {
            position: self.lexer.pos(),
            typ,
        });
        self.current = Token::Error;
        err
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
        if self.accept(t) {
            Ok(())
        } else {
            self.mark_syntax()
        }
    }
}
//self rules------------------------------------------------------------------

impl Parser{
    // module -> {item} EOF
    pub fn module(&mut self) -> AstMod {
        let mut items = Vec::new();
        while !self.accept(Token::Eof) {
            if self.check(Token::Fun) {
                match self.item() {
                    Ok(fun) => items.push(fun),
                    Err(e) => eprintln!("{}", e),
                }
            } else {
                self.current = self.lexer.next();
            }
        }
        AstMod{items}
    }

    // type_dec <- '(' [ Id { ',' Id } ] ')' Id | Id
    fn type_dec(&mut self) -> Parsing<Type> {
        if self.accept(Token::LParen) {
            let mut args = Type::int(); // TODO create void type
            if !self.check(Token::RParen) {
                args = self.type_dec()?;
                while self.accept(Token::Comma) {
                    args = Type::cons(args, self.type_dec()?);
                }
            }
            self.expect(Token::RParen)?;
            let ret = self.type_dec()?;
            Ok(Type::func(args, ret))
        } else {
            let id = self.ident()?;
            if id == Ident::ident("int"){
                Ok(Type::int())
            }else{
                self.mark_undef(id)
            }
        }
    }

    //item -> 'fun' ID '(' [ ID { ',' ID } ] ')' ':' type_dec funbody
    // funbody -> '=' expr ';'
    // funbody -> 'is' {'var' declaration} block 'return' expr 'end'
    fn item(&mut self) -> Parsing<AstItem> {
        self.expect(Token::Fun)?;
        let id = self.ident()?;

        let mut args = Vec::new();
        self.expect(Token::LParen)?;
        if !self.check(Token::RParen) {
            args.push(self.ident()?);
            while self.accept(Token::Comma) {
                args.push(self.ident()?);
            }
        }
        self.expect(Token::RParen)?;
        self.expect(Token::Colon)?;
        let typ = self.type_dec()?;
 
        if let Some(count) = typ.arg_count(){
            if count != args.len(){
                self.mark_arity(typ.clone(), args.len())?;
            }
        }else{
            self.mark_kind(typ.clone())?;
        }

        let mut stmts = Vec::new();
        let ret;
        if self.accept(Token::Equal) {
            ret = self.expr()?;
            self.expect(Token::Semi)?;
        } else {
            self.expect(Token::Is)?;

            stmts = self.block()?;

            self.expect(Token::Return)?;
            ret = self.expr()?;
            self.expect(Token::Semi)?;
            self.expect(Token::End)?;
        };
        let fun = AstFun{
            id,
            typ,
            args,
            stmts,
            ret,
        };
        Ok(AstItem::Fun(fun))
    }

    // block -> { stmt }
    fn block(&mut self) -> Parsing<Vec<AstStmt>> {
        let mut stmts = Vec::new();
        while self.accept(Token::Var){
            stmts.push(self.declaration()?);
        }
        while self.check(Token::If)
            || self.check(Token::While)
            || self.check(Token::Ident)
        {
            stmts.push(self.stmt()?);
        }
        Ok(stmts)
    }

    // vardec -> ID [ ':' type_dec ] ':=' expr ';' 
    fn declaration(&mut self) -> Parsing<AstStmt> {
        let id = self.ident()?;
        if self.accept(Token::Colon){
            let _typ = self.type_dec()?;
        }
        self.expect(Token::Assign)?;
        let expr = self.expr()?;
        self.expect(Token::Semi)?;
        Ok(AstStmt::Declaration(id, expr))
    }

    // stmt = ID ':=' expr ';'
    //      | 'if' relation 'then' block {'elsif' relation 'then' block} ['else' block] 'end'
    //      | 'while' relation 'do' block 'end'
    fn stmt(&mut self) -> Parsing<AstStmt> {
        if self.accept(Token::If) {
            let cond = self.relation()?;
            self.expect(Token::Then)?;
            let then = self.block()?;
            let mut elifs = Vec::new();
            while self.accept(Token::Elsif){
                let cond = self.relation()?;
                self.expect(Token::Then)?;
                let then = self.block()?;
                elifs.push((cond, then));
            }
            let els = if self.accept(Token::Else){
                self.block()?
            }else{
                Vec::new()
            };
            self.expect(Token::End)?;
            Ok(AstStmt::If(cond, then, elifs, els))
        } else if self.accept(Token::While) {
            let rel = self.relation()?;
            self.expect(Token::Do)?;
            let lop = self.block()?;
            self.expect(Token::End)?;
            Ok(AstStmt::While(rel, lop))
        } else {
            let pos = self.lexer.pos();
            let id = self.ident()?;
            self.expect(Token::Assign)?;
            let expr = self.expr()?;
            self.expect(Token::Semi)?;
            Ok(AstStmt::Assign(id, pos, expr))
        }
    }

    // clause -> [ 'not' ] expr ( '==' | '<' | '<=' | '>=' | '>' ) expr
    fn relation(&mut self) -> Parsing<AstRel> {
        let has_not = self.accept(Token::Not);
        let lhs = self.expr()?;
        let mut op = if self.accept(Token::EqualEqual) {
            RelOp::Eq
        } else if self.accept(Token::LessThan) {
            RelOp::Lt
        } else if self.accept(Token::GreaterThan) {
            RelOp::Gt
        } else if self.accept(Token::LessEqual) {
            RelOp::Le
        } else {
            self.expect(Token::GreaterEqual)?;
            RelOp::Ge
        };
        if has_not {
            op = op.not()
        }
        let rhs = self.expr()?;
        Ok(AstRel{rhs, lhs, op})
    }

    // expr -> ['-'] term { ('+' | '-') term }
    fn expr(&mut self) -> Parsing<AstExpr> {
        let has_minus = self.accept(Token::Minus);
        let mut lhs = self.term()?;
        if has_minus {
            lhs = AstExpr::Uminus(Box::new(lhs));
        }
        loop {
            if self.accept(Token::Plus) {
                let rhs = self.term()?;
                lhs = AstExpr::Plus(Box::new(lhs), Box::new(rhs));
            } else if self.accept(Token::Minus) {
                let rhs = self.term()?;
                lhs = AstExpr::Minus(Box::new(lhs), Box::new(rhs));
            } else {
                break Ok(lhs);
            }
        }
    }

    // term <- atom { ('*' | '/' | '%') atom }
    fn term(&mut self) -> Parsing<AstExpr> {
        let mut lhs = self.designator()?;
        loop {
            if self.accept(Token::Times) {
                let rhs = self.term()?;
                lhs = AstExpr::Times(Box::new(lhs), Box::new(rhs));
            } else if self.accept(Token::Over) {
                let rhs = self.term()?;
                lhs = AstExpr::Divide(Box::new(lhs), Box::new(rhs));
            } else if self.accept(Token::Modulo) {
                let rhs = self.term()?;
                lhs = AstExpr::Modulo(Box::new(lhs), Box::new(rhs));
            } else {
                break Ok(lhs);
            }
        }
    }

    //  designator <- atom
    fn designator(&mut self) -> Parsing<AstExpr> {
        self.atom()
    }

    //  atom <- '(' expr ')'
    //  atom <- Integer
    //  atom <- Id [ '(' [ expr {',' expr } ] ')' ]
    fn atom(&mut self) -> Parsing<AstExpr> {
        if self.accept(Token::LParen) {
            let val = self.expr()?;
            self.expect(Token::RParen)?;
            Ok(val)
        } else if self.accept(Token::Integer) {
            let value = self.lexer.number;
            Ok(AstExpr::Integer(value))
        } else {
            let id = self.ident()?;

            if self.accept(Token::LParen) {
                let mut args = Vec::new();
                if !self.check(Token::RParen) {
                    let a = self.expr()?;
                    args.push(a);
                    while self.accept(Token::Comma) {
                        let a = self.expr()?;
                        args.push(a);
                    }
                }
                self.expect(Token::RParen)?;
                Ok(AstExpr::Call(id, args))
            } else {
                Ok(AstExpr::Id(id))
            }
        }
    }

    //ident <- ID
    fn ident(&mut self) -> Parsing<Ident> {
        let id = self.lexer.word.clone();
        self.expect(Token::Ident)?;
        Ok(id)
    }
}

#[derive(Debug)]
pub struct AstMod{
    pub items: Vec<AstItem>,
}

#[derive(Debug)]
pub enum AstItem{
    Fun(AstFun),
}

#[derive(Debug)]
pub struct AstFun{
    pub id: Ident,
    pub typ: Type,
    pub args: Vec<Ident>,
    pub stmts: Vec<AstStmt>,
    pub ret: AstExpr,
}

#[derive(Debug)]
pub enum AstStmt{
    While(AstRel, Vec<AstStmt>),
    If(AstRel, Vec<AstStmt>, Vec<(AstRel, Vec<AstStmt>)>, Vec<AstStmt>),
    Declaration(Ident, AstExpr),
    Assign(Ident, LexPos, AstExpr),
}

#[derive(Debug)]
pub struct AstRel{
    pub lhs: AstExpr,
    pub rhs: AstExpr,
    pub op: RelOp
}

#[derive(Debug)]
pub enum AstExpr{
    Integer(usize),
    Id(Ident),
    Call(Ident, Vec<AstExpr>),
    Uminus(Box<AstExpr>),
    Plus(Box<AstExpr>, Box<AstExpr>),
    Minus(Box<AstExpr>, Box<AstExpr>),
    Times(Box<AstExpr>, Box<AstExpr>),
    Divide(Box<AstExpr>, Box<AstExpr>),
    Modulo(Box<AstExpr>, Box<AstExpr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}
pub const RELOP_CHARS: [&'static str; 6] = ["==", "<>", "<", ">", "<=", ">="];

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RELOP_CHARS[*self as usize].fmt(f)
    }
}

impl RelOp {
    pub fn not(self) -> RelOp {
        match self {
            RelOp::Eq => RelOp::Neq,
            RelOp::Neq => RelOp::Eq,
            RelOp::Lt => RelOp::Ge,
            RelOp::Gt => RelOp::Le,
            RelOp::Le => RelOp::Gt,
            RelOp::Ge => RelOp::Lt,
        }
    }
}

