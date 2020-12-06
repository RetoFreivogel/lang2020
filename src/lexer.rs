/*	TODO
    handle tab size
    use macro for the symbol structures
    make symbolset generic?
    separate Lexpos?
    use UIDs for filenames
    return Errors, dont print to stderr
    parse all 'reserved characters'
*/

//Token-------------------------------------------------------------------------
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    //Keywords
    //Module,
    Fun,
    //Struct,
    //Union,
    //Enum,
    //Import,
    Var,
    //Use,
    //As,
    //Match,
    If,
    Then,
    Else,
    Elsif,
    While,
    //Continue,
    Break,
    //For,
    //In,
    Do,
    Is,
    Return,
    Not,
    And,
    Or,
    End,
    //Identifiers
    Ident,
    //Symbols
    Colon,
    Semi,
    LParen,
    RParen,
    //LCurly,
    //RCurly,
    //LBracket,
    //RBracket,
    Comma,
    Equal,
    //Dot,
    //Operators
    Assign,
    EqualEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    //BitOr,
    //BitAnd,
    Plus,
    Minus,
    Times,
    Over,
    Modulo,
    //Literals
    Integer,
    //Float,
    //Text,
    //Underscore,
    Eof,
    Error,
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Token::*;
        write!(
            f,
            "{}",
            match self {
                If => "if",
                Then => "then",
                Else => "else",
                Elsif => "elsif",
                While => "while",
                Do => "do",
                Break => "break",
                Fun => "fun",
                Var => "var",
                Is => "is",
                Return => "return",
                Not => "not",
                And => "and",
                Or => "or",
                End => "end",
                Eof => "EOF",
                Ident => "an identifer",
                Colon => ":",
                Semi => ";",
                LParen => "(",
                RParen => ")",
                Comma => ",",
                Equal => "=",
                Assign => ":=",
                EqualEqual => "==",
                LessThan => "<",
                GreaterThan => ">",
                LessEqual => "<=",
                GreaterEqual => ">=",
                Plus => "+",
                Minus => "-",
                Times => "*",
                Over => "/",
                Modulo => "%",
                Integer => "an integer",
                Error => "",
            }
        )
    }
}
const TOKENS: [Token; Token::Error as usize] = [
    Token::If,
    Token::Then,
    Token::Else,
    Token::Elsif,
    Token::While,
    Token::Do,
    Token::Break,
    Token::Fun,
    Token::Var,
    Token::Is,
    Token::Return,
    Token::End,
    Token::Semi,
    Token::Colon,
    Token::LParen,
    Token::RParen,
    Token::Comma,
    Token::Equal,
    Token::Assign,
    Token::And,
    Token::Or,
    Token::Not,
    Token::EqualEqual,
    Token::LessThan,
    Token::GreaterThan,
    Token::LessEqual,
    Token::GreaterEqual,
    Token::Plus,
    Token::Minus,
    Token::Times,
    Token::Over,
    Token::Modulo,
    Token::Integer,
    Token::Ident,
    Token::Eof,
];
const KEYWORDS: [(&'static str, Token); 15] = [
    ("fun", Token::Fun),
    ("if", Token::If),
    ("then", Token::Then),
    ("else", Token::Else),
    ("elsif", Token::Elsif),
    ("while", Token::While),
    ("do", Token::Do),
    ("and", Token::And),
    ("or", Token::Or),
    ("not", Token::Not),
    ("break", Token::Break),
    ("var", Token::Var),
    ("is", Token::Is),
    ("return", Token::Return),
    ("end", Token::End),
];

//TokenSet----------------------------------------------------------------------
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct TokenSet(u64);
impl TokenSet {
    pub fn new() -> TokenSet {
        TokenSet(0)
    }
    pub fn set(&mut self, s: Token) {
        self.0 |= 1 << (s as u64);
    }
    fn iter(&self) -> impl Iterator<Item = &Token> {
        TOKENS
            .iter()
            .filter(move |s| (self.0 & 1 << **s as u64) != 0)
    }
}
impl std::fmt::Display for TokenSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for t in self.iter() {
            write!(f, "{} ", t)?;
        }
        Ok(())
    }
}

//Lexpos----------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexPos {
    filename: String,
    line: usize,
    column: usize,
}
impl std::fmt::Display for LexPos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(
            f,
            "In '{}' line {} column {}:",
            self.filename, self.line, self.column,
        )
    }
}

use crate::strtab::Ident;
use std::io::Read;

//Lexer----------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer<I: Read> {
    stream: I,
    linecount: usize,
    column: usize,
    pos: usize,
    pub word: Ident,
    pub number: usize,
    filename: String,
    current: u8,
}
impl<I: Read> Lexer<I> {
    pub fn new(filename: String, stream: I) -> Lexer<I> {
        let mut lex = Lexer {
            stream,
            linecount: 1,
            column: 0,
            pos: 0,
            word: Ident::ident(""),
            number: 0,
            filename,
            current: 0,
        };
        lex.get_char();
        return lex;
    }

    pub fn pos(&self) -> LexPos {
        LexPos {
            filename: self.filename.clone(),
            line: self.linecount,
            column: self.column - 1, // UGLY
        }
    }

    fn error(&self, msg: &str) {
        eprintln!("   ERROR: {}", msg);
    }

    fn get_char(&mut self) {
        let mut c = [0u8];
        if self.stream.read(&mut c).is_ok() {
            self.current = c[0];
            self.column += 1;
        } else {
            self.current = 0;
        }
    }

    //Lexer Rules----------------------------------------------------------------------
    pub fn next(&mut self) -> Token {
        loop {
            match self.current {
                0 => return Token::Eof,
                b'\n' | b'\r' | b'\t' | b' ' => self.lex_whitespace(),
                1..=31 | 127 => self.lex_special(),
                b'0'..=b'9' => return self.lex_number(),
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => return self.lex_word(),
                b'#' => self.lex_comment(),
                b'!'..=b'~' => return self.lex_oparator(),
                128..=255 => self.lex_utf8(),
            }
        }
    }

    fn lex_oparator(&mut self) -> Token {
        let last = self.current;
        self.get_char();
        let sym = match (last, self.current) {
            (b':', b'=') => Token::Assign,
            (b'<', b'=') => Token::LessEqual,
            (b'>', b'=') => Token::GreaterEqual,
            (b'=', b'=') => Token::EqualEqual,
            _ => Token::Eof,
        };
        if sym != Token::Eof {
            self.get_char();
            return sym;
        }
        let sym = match last {
            b'%' => Token::Modulo,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'*' => Token::Times,
            b'+' => Token::Plus,
            b',' => Token::Comma,
            b'-' => Token::Minus,
            b'/' => Token::Over,
            b':' => Token::Colon,
            b';' => Token::Semi,
            b'<' => Token::LessThan,
            b'=' => Token::Equal,
            b'>' => Token::GreaterThan,
            _ => {
                self.error("reserved character");
                Token::Eof
            }
        };
        return sym;
    }

    fn lex_special(&mut self) {
        self.get_char();
        self.error("Unsupported special character. Skipping.");
    }

    fn lex_whitespace(&mut self) {
        /* CR-only newlines won't bump the linecount,
        but i'm not targeting those systems anyway. */
        loop {
            match self.current {
                b' ' | b'\r' => {}
                b'\t' => {
                    self.column += 3; // TODO make this configurable
                }
                b'\n' => {
                    self.linecount += 1;
                    self.column = 0;
                }
                _ => break,
            }
            self.get_char();
        }
    }

    fn lex_utf8(&mut self) {
        while 128 <= self.current {
            self.get_char();
        }
        self.error("Utf8 codepoints are only accepted in comments and strings");
    }

    fn lex_number(&mut self) -> Token {
        let mut n = 0usize;
        while b'0' <= self.current && self.current <= b'9' {
            n *= 10;
            n += (self.current - b'0') as usize;
            self.get_char();
        }
        self.number = n;
        return Token::Integer;
    }

    fn lex_word(&mut self) -> Token {
        let mut word = String::new();
        loop {
            match self.current {
                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                    word.push(self.current as char);
                    self.get_char();
                }
                _ => break,
            }
        }
        if let Some(&(_, token)) = KEYWORDS.iter().find(|(kw, _)| kw == &word) {
            return token;
        } else {
            self.word = Ident::ident(&word);
            return Token::Ident;
        }
    }

    fn lex_comment(&mut self) {
        while self.current != b'\n' && self.current != 0 {
            self.get_char();
        }
    }
}
