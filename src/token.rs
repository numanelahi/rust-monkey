use std::fmt::{Result, Formatter, Display, Debug};

#[derive(Debug)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    
    // Identifiers and Literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    EQ,
    NOTEQ,
    PLUS,
    MINUS,
    BANG, // !
    ASTERISK, // *
    SLASH,

    LT, // <
    GT, // >
    LTEQ,
    GTEQ,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self)
    }
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token{
            token_type,
            literal,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "[  TokenType: {}, Literal: {}  ]", self.token_type.to_string(), self.literal)
    }
}