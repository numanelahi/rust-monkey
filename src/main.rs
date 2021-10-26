mod token;
mod lexer;
mod utils;
mod repl;
mod ast;
mod parser;

use users;

fn main() {
    match users::get_current_username() {
        Some(uname) => {
            println!("Hello {:?}! This is the Monkey programming language!", uname);
        },
        None => {
            panic!("No USER!!!");
        }
    }
    repl::start();
}