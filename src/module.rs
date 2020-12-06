/*  TODO
    deduplicate Types
    externalise the offset
    nested scopes?
*/

use crate::strtab::{Ident};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    context: Vec<Symbol>,
    pos: usize, // move
}
impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for sym in &self.context {
            let kind = match sym {
                Symbol::Type { .. } => "Typ",
                Symbol::Func { .. } => "Fun",
                Symbol::Var { .. } => "Var",
                Symbol::Arg { .. } => "Arg",
            };
            writeln!(f, "{}  {}", kind, sym)?;
        }
        Ok(())
    }
}

impl Module {
    pub fn new() -> Module {
        let mut m = Module {
            context: Vec::new(),
            pos: 0,
        };
        let id = Ident::ident("Int");
        m.declare_type(id, Type::Integer);
        let id = Ident::ident("Bool");
        m.declare_type(id, Type::Bool);
        return m;
    }

    pub fn get_sym(&self, id: &Ident) -> Option<Symbol> {
        let mut must_be_global = false;
        for sym in self.context.iter().rev() {
            if must_be_global != sym.is_global() {
                if must_be_global {
                    continue;
                } else {
                    must_be_global = true;
                }
            }
            if &sym.get_id() == id {
                return Some(sym.clone());
            }
        }
        None
    }

    pub fn declare_type(&mut self, id: Ident, typ: Type) -> Symbol {
        let sym = Symbol::Type { id, typ };
        self.context.push(sym.clone());
        return sym;
    }

    pub fn declare_fun(&mut self, id: Ident, typ: FuncType, start: usize) -> Symbol {
        self.pos = 0;
        let sym = Symbol::Func { id, typ, start };
        self.context.push(sym.clone());
        return sym;
    }

    pub fn declare_arg(&mut self, id: Ident, typ: Type) -> Symbol {
        self.pos += 1;
        let sym = Symbol::Arg {
            id,
            typ,
            pos: self.pos,
        };
        self.context.push(sym.clone());
        return sym;
    }

    pub fn declare_var(&mut self, id: Ident, typ: Type) -> Symbol {
        self.pos += 1;
        let sym = Symbol::Var {
            id,
            typ,
            pos: self.pos,
        };
        self.context.push(sym.clone());
        return sym;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Type {
        id: Ident,
        typ: Type,
    },
    Func {
        id: Ident,
        typ: FuncType,
        start: usize,
    },
    Var {
        id: Ident,
        typ: Type,
        pos: usize,
    },
    Arg {
        id: Ident,
        typ: Type,
        pos: usize,
    },
}
impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Type { typ, .. } => write!(f, "{}", typ),
            Symbol::Func { id, .. } => write!(f, "{}", id),
            Symbol::Var { id, .. } => write!(f, "{}", id),
            Symbol::Arg { id, .. } => write!(f, "{}", id),
        }
    }
}

impl Symbol {
    pub fn get_typ(&self) -> Type {
        match self {
            Symbol::Type { typ, .. } | Symbol::Var { typ, .. } | Symbol::Arg { typ, .. } => {
                typ.clone()
            }
            Symbol::Func { typ, .. } => typ.clone().into_type(),
        }
    }
    pub fn get_id(&self) -> Ident {
        match self {
            Symbol::Type { id, .. }
            | Symbol::Var { id, .. }
            | Symbol::Arg { id, .. }
            | Symbol::Func { id, .. } => id.clone(),
        }
    }
    pub fn is_function(&self) -> bool {
        match self {
            Symbol::Func { .. } => true,
            _ => false,
        }
    }
    pub fn is_type(&self) -> bool {
        match self {
            Symbol::Type { .. } => true,
            _ => false,
        }
    }
    fn is_global(&self) -> bool {
        match self {
            Symbol::Type { .. } | Symbol::Func { .. } => true,
            _ => false,
        }
    }
}

//types--------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub args: Vec<Type>,
    pub ret: Type,
}
impl FuncType {
    fn unify(&self, other: &FuncType) -> Option<FuncType> {
        if self.args.len() != other.args.len() {
            return None;
        }
        let args = self
            .args
            .iter()
            .zip(other.args.clone())
            .map(|(t1, t2)| t1.unify(&t2))
            .collect::<Option<Vec<Type>>>()?;
        let ret = self.ret.unify(&other.ret)?;
        Some(FuncType { args, ret })
    }
    pub fn from_args(args: Vec<Type>) -> FuncType {
        FuncType {
            args,
            ret: Type::Unknown,
        }
    }
    pub fn into_type(self) -> Type {
        Type::Function(Box::new(self))
    }
}
impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for n in 0..self.args.len() {
            write!(f, "{}", self.args[n])?;
            if n < self.args.len() - 1 {
                write!(f, ",")?;
            }
        }
        write!(f, "){}", self.ret)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Integer,
    Bool,
    Function(Box<FuncType>),
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "?"),
            Type::Integer => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Function(fun) => write!(f, "{}", fun),
        }
    }
}
impl Type {
    pub fn unify(&self, other: &Type) -> Option<Type> {
        use Type::*;
        match (self, other) {
            (t1, t2) if t1 == t2 => Some(t1.clone()),
            (Unknown, t) => Some(t.clone()),
            (t, Unknown) => Some(t.clone()),
            (Function(f1), Function(f2)) => Some(Function(Box::new(f1.unify(f2)?))),
            _ => None,
        }
    }

    pub fn get_function(&self) -> &FuncType {
        if let Type::Function(f) = self {
            f
        } else {
            panic!("Not a function type!");
        }
    }
}
