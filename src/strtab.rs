use std::sync::RwLock;
use lazy_static::lazy_static;
use std::sync::Arc;
use std::fmt::{Display, Result, Formatter};

#[derive(Debug, Clone)]
pub struct Ident(Arc<String>);

lazy_static!{
    static ref WORDS: RwLock<Vec<Ident>> = RwLock::new(Vec::new());
}

impl PartialEq for Ident{
    fn eq(&self, other: &Self) -> bool{
        Arc::eq(&self.0, &other.0)
    }
}
impl Eq for Ident{}
impl Display for Ident{
    fn fmt(&self, f: &mut Formatter) -> Result{
        self.0.fmt(f)
    }
}
impl Ident {
    pub fn ident(s: &str) -> Ident {
        for word in WORDS.read().unwrap().iter() {
            if *word.0 == s {
                return word.clone();
            }
        }
        let word = Ident(Arc::new(s.to_string()));
        WORDS.write().unwrap().push(word.clone());
        return word;
    }
}   