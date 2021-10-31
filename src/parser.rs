use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::ast::{Program, Statement, Identifier, LetStatement, ReturnStatement, 
    ExpressionStatement, IntegerLiteral, PrefixExpression, ExpressionType, Expression, Node};
use std::collections::HashMap;


// type prefix_parser_fn = Fn() -> Box<dyn Expression>;
// type infix_parser_fn = Fn(dyn Expression) -> Box<dyn Expression>;

#[derive(Default)]
struct Parser {
    l: Lexer,
    errors: Vec<String>,
    current_token: Token,
    peek_token: Token, 
    //prefix_parse_fns: HashMap<TokenType, Box<prefix_parser_fn>>,
    //infix_parse_fns: HashMap<TokenType, Box<infix_parser_fn>>,
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

    // fn register_prefix(&mut self, token_type: TokenType, func: Box<prefix_parser_fn>) {
    //     self.prefix_parse_fns.insert(token_type, func);
    // }

    // fn register_infix(&mut self, token_type: TokenType, func: Box<infix_parser_fn>) {
    //     self.infix_parse_fns.insert(token_type, func);
    // }

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

    fn peek_error(&mut self, expected_type: TokenType) {
        self.errors.push(format!("Expected next token to be {}, got {} instead", expected_type.to_string(), self.peek_token.token_type.to_string()));
    }

    fn expect_peek(&mut self, expected_token: TokenType) -> bool {
        if expected_token == self.peek_token.token_type {
            self.next_token();
            true
        } else {
            self.peek_error(expected_token);
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

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt_token: Token = self.current_token.clone();

        self.next_token();
        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        let expression = Identifier{
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };
        Some(Box::new(ReturnStatement{
            token: stmt_token,
            value: Box::new(expression),
        }))
    }

    fn other_parse_errors(&mut self, error: String) {
        self.errors.push(error);
    } 

    fn parse_prefix_fn_error(&mut self, token_type: TokenType) {
        self.errors.push(format!("No prefix function found for token {}", token_type));
    }

    fn parse_identifier(&self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier{
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        match self.current_token.literal.parse::<i64>() {
            Ok(int) => {
                Some(Box::new(IntegerLiteral{
                    token: self.current_token.clone(),
                    value: int,
                }))
            },
            Err(err) => {
                self.other_parse_errors(format!("{}", err));
                None
            },
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.next_token();
        match self.parse_expression(ExpressionType::PREFIX) {
            Some(right) => {
                Some(Box::new(PrefixExpression{
                    token,
                    operator,
                    right
                }))
            },
            None => None,
        }       
    }

    fn parse_expression(&mut self, expression_type: ExpressionType) -> Option<Box<dyn Expression>> {
        match self.current_token.token_type {
            TokenType::IDENT => self.parse_identifier(),
            TokenType::INT => self.parse_integer_literal(),
            TokenType::MINUS | TokenType::BANG => self.parse_prefix_expression(),
            _ => {
                self.parse_prefix_fn_error(self.current_token.token_type.clone());
                None
            },
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let expression = self.parse_expression(ExpressionType::LOWEST);
        match expression {
            Some(exp) => {
                let stmt: Option<Box<dyn Statement>> = Some(Box::new(ExpressionStatement{
                    token: self.current_token.clone(),
                    expression: exp,
                }));
                if self.peek_token_is(TokenType::SEMICOLON) {
                    self.next_token();
                }
                stmt
            },
            None => None
        }
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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

    struct PrefixTest {
        input: String,
        operator: String,
        integer_value: i64,
    }

    impl PrefixTest {
        fn new(input: &str, operator: &str, integer_value: i64) -> Self {
            Self {
                input: input.to_string(),
                operator: operator.to_string(),
                integer_value
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
        check_parser_errors(&p);

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

    #[test]
    fn test_return_statement() {
        let input = String::from("
        return 5;
        return 10;
        return 10101010;
        ");

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            match stmt.as_any().downcast_ref::<ReturnStatement>() {
                Some(return_stmt) => {
                    assert_eq!(return_stmt.token_literal(), "return");
                },
                None => {panic!("failed to downcast return statement");},
            }
        }
    }

    fn check_parser_errors(p: &Parser) {
        //assert_eq!(p.errors.len(), 0);
        println!("Parser has {} errors.", p.errors.len());
        for err in &p.errors {
            println!("parser error: {}", *err);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_ne!(program.statements.len(), 0);

        for stmt in program.statements {
            match stmt.as_any().downcast_ref::<ExpressionStatement>() {
                Some(expr_stmt) => {
                    match expr_stmt.expression.as_any().downcast_ref::<Identifier>() {
                        Some(ident) => {
                            assert_eq!(ident.token_literal(), "foobar".to_string());
                            assert_eq!(ident.value, "foobar".to_string());
                        },
                        None => {panic!("Failed to downcase indentifier");},
                    }
                },
                None => {panic!("No expression inside the expression statement");}
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5");

        let lex = Lexer::new(input);
        let mut parse = Parser::new(lex);
        let program = parse.parse_program();
        check_parser_errors(&parse);

        assert_ne!(program.statements.len(), 0);
        for stmt in program.statements {
            match stmt.as_any().downcast_ref::<ExpressionStatement>() {
                Some(expr_stmt) => {
                    assert_eq!(test_integer_literal(&expr_stmt.expression, 5), true);
                },
                None => panic!("Failed to downcast expression statement"),
            }
        }
    }

    fn test_integer_literal(expression: &Box<dyn Expression>, int: i64) -> bool {
        match expression.as_any().downcast_ref::<IntegerLiteral>() {
            Some(integer_literal) => {
                integer_literal.value == int && integer_literal.token_literal() == int.to_string()
            }
            None => false,
        }
    }

    #[test]
    fn test_prefix_expresssion_parsing() {
        let tests = [
            PrefixTest::new("-5;", "-", 5),
            PrefixTest::new("!5", "!", 5),
        ];

        for test in tests.iter() {
            let lex = Lexer::new(test.input.clone());
            let mut parse = Parser::new(lex);
            let program = parse.parse_program();
            check_parser_errors(&parse);

            assert_eq!(program.statements.len(), 1);
            match program.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
                Some(exp_stmt) => {
                    match exp_stmt.expression.as_any().downcast_ref::<PrefixExpression>() {
                        Some(pre_exp) => {
                            assert_eq!(pre_exp.operator, test.operator);
                            assert_eq!(test_integer_literal(&pre_exp.right, test.integer_value), true);
                        },
                        None => panic!("Failed to parse prefix expression"),
                    }
                },
                None => panic!("Failed to parse expression statement"),
            }
        }
    }
}