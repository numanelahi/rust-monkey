use std::io;
use crate::lexer::Lexer;
use crate::token::TokenType;

pub fn start() {
    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Err(error) => {
                println!("{}", error);
            },
            _ => {
                let mut l = Lexer::new(line);
                let mut tok = l.next_token();
                while tok.token_type != TokenType::EOF {
                    println!("{}", tok);
                    tok = l.next_token();
                }
            },
        }
    }
}