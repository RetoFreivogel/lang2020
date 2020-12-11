use lazy_static::lazy_static;
use std::sync::RwLock;

use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident(usize);

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
