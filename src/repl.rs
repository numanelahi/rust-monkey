use std::io;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub fn start() {
    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Err(error) => {
                println!("{}", error);
            },
            _ => {
                let l = Lexer::new(line);
                let mut parse = Parser::new(l);
                let program = parse.parse_program();
                if parse.errors.len() != 0 {
                    print_parse_errors(&parse.errors);
                }
                println!("{}", program.to_string());
            },
        }
    }
}

fn print_parse_errors(errors: &Vec<String>) {
    for error in errors.iter() {
        println!("\t{}\n", error);
    }
}