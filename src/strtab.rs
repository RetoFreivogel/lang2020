use lazy_static::lazy_static;
use std::sync::RwLock;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident(pub usize);
lazy_static! {
    static ref WORDS: RwLock<Vec<String>> = RwLock::new(Vec::new());
}
impl Display for Ident {
    fn fmt(&self, f: &mut Formatter) -> Result {
        WORDS.read().unwrap()[self.0].fmt(f)
    }
}
impl Ident {
    pub fn ident(s: &str) -> Ident {
        for (i, word) in WORDS.read().unwrap().iter().enumerate() {
            if word == s {
                return Ident(i);
            }
        }
        let word = Ident(WORDS.read().unwrap().len());
        WORDS.write().unwrap().push(s.to_string());
        return word;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Type(usize);
lazy_static! {
    static ref TYPES: RwLock<Vec<TypeDesc>> = RwLock::new(Vec::new());
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let typ = Type::get(self);
        typ.fmt(f)
    }
}
impl Type{
    fn def(typ: &TypeDesc) -> Type{
        for (i, t) in TYPES.read().unwrap().iter().enumerate() {
            if t == typ {
                return Type(i);
            }
        }
        let i = TYPES.read().unwrap().len();
        TYPES.write().unwrap().push(typ.clone());
        return Type(i);
    }
    fn get(&self) -> TypeDesc{
        TYPES.read().unwrap()[self.0]
    }
    pub fn arg_count(&self) -> Option<usize>{
        if let TypeDesc::Func{mut args, ..} = self.get(){
            let mut count = 0;
            while let TypeDesc::Cons{left, ..} = args.get(){
                args = left;
                count += 1;
            }
            count += 1; // TODO void type
            Some(count)
        }else{
            None
        }
    }      
    pub fn int() -> Type{
        Type::def(&TypeDesc::Int)
    }
    pub fn cons(left: Type, right: Type) -> Type{
        Type::def(&TypeDesc::Cons{left, right})
    }
    pub fn func(args: Type, ret: Type) -> Type{
        Type::def(&TypeDesc::Func{args, ret})
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TypeDesc{
    Int,
    Cons { left: Type, right: Type},
    Func { args: Type, ret: Type },
}
impl Display for TypeDesc {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TypeDesc::Int => write!(f, "int"),
            TypeDesc::Cons { left, right } => write!(f, "{}, {}", left, right),
            TypeDesc::Func { args, ret } => write!(f, "({}){}", args, ret),
        }
    }
}
