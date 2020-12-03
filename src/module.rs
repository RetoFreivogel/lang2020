/*  TODO
    deduplicate Types
    externalise the offset
    nested scopes?
*/

use std::fmt;
use std::rc::Rc;

pub type Ident = Rc<String>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module{
    words: Vec<Ident>,
    context: Vec<Symbol>,
    offset: usize, // move
}
impl fmt::Display for Module{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        for sym in &self.context{
            match sym.kind{
                Kind::Type => write!(f, "Typ "),
                Kind::Function{..} => write!(f, "Fun "),
                Kind::Variable{..} => write!(f, "Var "),
                Kind::Argument{..} => write!(f, "Arg "),
            }?;
            writeln!(f, "{:>8}: {}", sym.id, sym.typ)?;
        }
        Ok(())
    }
}

impl Module{
    pub fn new() -> Module{
        let mut m = Module{
            words: Vec::new(),
            context: Vec::new(),
            offset: 0,
        };
        let id = m.make_id("Int");
        m.declare_type(id, Type::Integer);
        let id = m.make_id("Bool");
        m.declare_type(id, Type::Bool);
        return m;
    }

    pub fn make_id(&mut self, s: &str) -> Ident{
        for word in self.words.iter(){
            if **word == s{
                return word.clone();
            }
        }
        let word = Rc::new(s.to_string());
        self.words.push(word.clone());
        return word;
    }

    pub fn get_sym(&self, id: &Ident) -> Option<Symbol>{
        let mut must_be_global = false;
        for sym in self.context.iter().rev(){
            if must_be_global != sym.kind.is_global(){
                if must_be_global{
                    continue;
                }else{
                    must_be_global = true;
                }
            }
            if Rc::ptr_eq(&sym.id, &id){
                return Some(sym.clone());
            }
        }
        None
    }

    pub fn declare_type(&mut self, id: Ident, typ: Type){
        let kind = Kind::Type;
        self.context.push(Symbol::new(id, typ, kind));
    }

    pub fn declare_fun(&mut self, id: Ident, typ: Type, start: usize) -> Symbol{
        let kind = Kind::Function{start_block: start}; //TODO 
        self.offset = 0;
        let sym = Symbol::new(id, typ, kind);
        self.context.push(sym.clone());
        return sym;
    }

    pub fn declare_arg(&mut self, id: Ident, typ: Type, pos: usize){
        let kind = Kind::Argument{pos};
        self.context.push(Symbol::new(id, typ, kind));
    }

    pub fn declare_var(&mut self, id: Ident, typ: Type){
        self.offset += typ.sizeof();
        let kind = Kind::Variable{offset: self.offset};
        self.context.push(Symbol::new(id, typ, kind));
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol{
    id: Ident,
    pub typ: Type,
    kind: Kind,
}
impl fmt::Display for Symbol{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        match self.kind{
            Kind::Type         => write!(f, "{}", self.typ),
            Kind::Function{..} => write!(f, "{}", self.id),
            Kind::Variable{..} => write!(f, "{}", self.id),
            Kind::Argument{..} => write!(f, "{}", self.id),
        }
    }
}

impl Symbol{
    fn new(id: Ident, typ: Type, kind: Kind) -> Symbol{
        Symbol{id: id.clone(), typ, kind}
    }

    pub fn is_function(&self) -> bool{
        match self.kind{
            Kind::Function{..} => true,
            _ => false,
        }
    }
    pub fn is_type(&self) -> bool{
        match self.kind{
            Kind::Type{..} => true,
            _ => false,
        }        
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Kind{
    Type,
    Function{start_block: usize},
    Variable{offset: usize},
    Argument{pos: usize},    
}
impl Kind{
    fn is_global(&self) -> bool{
        match self {
            Kind::Type | Kind::Function{..} => true,
            _ => false,
        }
    }
}


//types--------------------------------------------------------




#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type{
    Unknown,
    Integer,
    Bool,
    Function{args: Vec<Type>, ret: Box<Type>},
}
impl fmt::Display for Type{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        match self{
            Type::Unknown => write!(f, "?"),
            Type::Integer => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Function{args, ret} => {
                write!(f, "(")?;
                for n in 0..args.len(){
                    write!(f, "{}", args[n])?;
                    if n < args.len() - 1{
                        write!(f, ",")?;
                    }
                }
                write!(f, "){}", ret)
            }
        }
    }
}
impl Type{
    fn sizeof(&self) -> usize{
        match self{
            Type::Unknown => 0,
            Type::Integer => 8,
            Type::Bool => 1,
            Type::Function{..} => 8,
        }
    }

    pub fn unify(&self, other: &Type) -> Option<Type>{
        use Type::*;
        match (self, other){
            (t1, t2) if t1 == t2 => Some(t1.clone()),
            (Unknown, t) => Some(t.clone()),
            (t, Unknown) => Some(t.clone()),
            (Function{args: a1, ret: r1}, Function{args: a2, ret: r2})
                if a1.len() == a2.len() => {
                Some(Function{
                    args: a1.iter().zip(a2)
                        .map(|(t,u)|t.unify(u))
                        .collect::<Option<Vec<Type>>>()?,
                    ret: Box::new(r1.unify(r2)?)
                })
            }
            _ => None,
        }
    }

    pub fn from_args(args:Vec<Type>) -> Type{
        Type::Function{args, ret: Box::new(Type::Unknown)}
    }

    pub fn get_ret(&self) -> Type{
        if let Type::Function{args:_, ret} = self {
            *ret.clone()
        }else{
            panic!("Not a function type!");
        }
    }

    pub fn get_args(&self) -> Vec<Type>{
        if let Type::Function{args, ret:_} = self {
            args.clone()
        }else{
            panic!("Not a function type!");
        }
    }
}

