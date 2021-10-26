use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::ast::{Program, Statement, Identifier, LetStatement, Expression, Node};

#[derive(Default)]
struct Parser {
    l: Lexer,
    current_token: Token,
    peek_token: Token, 
}

impl Parser {
    fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            ..Default::default()
        };

        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn current_token_is(&self, expected_type: TokenType) -> bool {
        self.current_token.token_type == expected_type
    }

    fn peek_token_is(&self, expected_type: TokenType) -> bool {
        self.peek_token.token_type == expected_type
    }

    fn expect_peek(&mut self, expected_token: TokenType) -> bool {
        if expected_token == self.peek_token.token_type {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt_token: Token = self.current_token.clone();
        if !self.expect_peek(TokenType::IDENT) {
            return None
        }
        let identifier: Identifier = Identifier{
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };
        if !self.expect_peek(TokenType::ASSIGN) {
            return None
        }
        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let expression = Identifier{
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        Some(Box::new(LetStatement{
           token: stmt_token,
           name: identifier,
           value: Box::new(expression), 
        }))
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            _ => None,
        }
    }


    pub fn parse_program(&mut self) -> Program {
        let statements: Vec<Box<dyn Statement>> = Vec::new();
        let mut program = Program{statements};

        while self.current_token.token_type != TokenType::EOF {
            if let Some(box_stmt) = self.parse_statement() {
                program.statements.push(box_stmt);
            }
            self.next_token();
        } 
        program
    }
}


#[cfg(test)]
mod test {
    use super::*;

    struct ExpectedIdentifier {
        identifier: String,
    }

    impl ExpectedIdentifier {
        fn new(s: &str) -> Self {
            Self {
                identifier: s.to_string()
            }
        }
    }

    #[test]
    fn test_parser() {
        let input = String::from("
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ");

        let lex = Lexer::new(input);
        let mut p = Parser::new(lex);
        let program = p.parse_program();

        let tests = [
            ExpectedIdentifier::new("x"), 
            ExpectedIdentifier::new("y"),
            ExpectedIdentifier::new("foobar")
            ];

        for (i, test) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            let pass = test_let_statement(&test.identifier, stmt);
            assert_eq!(pass, true);
        }
    }

    fn test_let_statement(name: &String, stmt: &Box<dyn Statement>) -> bool {
        if stmt.token_literal() != "let".to_string() {
            return false
        }
        match stmt.as_any().downcast_ref::<LetStatement>() {
            Some(let_stmt) => {
                let_stmt.name.value == *name && let_stmt.name.token_literal() == *name
            },
            None => false
        }
    }
}