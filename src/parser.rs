use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::ast::{Program, Statement, Identifier, LetStatement, ReturnStatement, 
    ExpressionStatement, IntegerLiteral, PrefixExpression, InfixExpression,
    IfExpression, BlockStatement, FunctionLiteral, CallExpression,
    Boolean, ExpressionType, Expression, Node};



#[derive(Default)]
pub struct Parser {
    pub l: Lexer,
    pub errors: Vec<String>,
    pub current_token: Token,
    pub peek_token: Token,
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
        self.next_token();

        let expression = self.parse_expression(ExpressionType::LOWEST.get_value());
        if expression.is_none() {
            return None
        }
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(LetStatement{
           token: stmt_token,
           name: identifier,
           value: expression.unwrap(), 
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt_token: Token = self.current_token.clone();

        self.next_token();

        let value = self.parse_expression(ExpressionType::LOWEST.get_value());
        if value.is_none() {
            return None
        }
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(ReturnStatement{
            token: stmt_token,
            value: value.unwrap(),
        }))
    }

    fn other_parse_errors(&mut self, error: String) {
        self.errors.push(error);
    } 

    fn parse_fn_error(&mut self, token_type: &TokenType, exp_type: &str) {
        self.errors.push(format!("No {} function found for token {}", exp_type, token_type));
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

    fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Boolean{
            token: self.current_token.clone(),
            value: self.current_token_is(TokenType::TRUE),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();
        let exp = self.parse_expression(ExpressionType::LOWEST.get_value());
        if !self.expect_peek(TokenType::RPAREN) {
            None
        } else {
            exp
        } 
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.next_token();
        match self.parse_expression(ExpressionType::PREFIX.get_value()) {
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

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> (Option<Box<dyn Expression>>, bool) {
        self.next_token();
        let precedence = self.current_precedence();
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.next_token();
        let right: Option<Box<dyn Expression>> = self.parse_expression(precedence);
        if right.is_none() {
            return (Some(left), true)
        }
        (Some(Box::new(InfixExpression{
            token,
            operator,
            left,
            right: right.unwrap(),
        })), false)
    }

    fn parse_if_expresssion(&mut self) -> Option<Box<dyn Expression>> {
        let current_token = self.current_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None
        }
        self.next_token();
        let condition: Box<dyn Expression> = self.parse_expression(ExpressionType::LOWEST.get_value()).unwrap();
        if !self.expect_peek(TokenType::RPAREN) {
            return None
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None
        }

        let consequence = self.parse_block_statement();

        let mut alternative: Option<BlockStatement> = None;

        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            
            if !self.expect_peek(TokenType::LBRACE) {
                return None
            }

            alternative = Some(self.parse_block_statement());
        }

        Some(Box::new(IfExpression{
            token: current_token,
            condition,
            consequence,
            alternative
        }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.current_token.clone();
        let mut stmts: Vec<Box<dyn Statement>> = Vec::new();
        self.next_token();

        while !self.current_token_is(TokenType::RBRACE) && !self.current_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                stmts.push(stmt.unwrap());
            }
            self.next_token();
        }
        BlockStatement{
            token,
            statements: stmts
        }
    }

    fn parse_function_params(&mut self) -> Option<Vec<Identifier>> {
        let mut idents: Vec<Identifier> = Vec::new();
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(idents)
        }
        self.next_token();
        idents.push(Identifier{
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            idents.push(Identifier{
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            });
        }
        if !self.expect_peek(TokenType::RPAREN) {
            return None
        }
        Some(idents)
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();

        if !self.expect_peek(TokenType::LPAREN) {
            return None
        }

        let parameters: Vec<Identifier>;
        if let Some(p) = self.parse_function_params() {
            parameters = p;
        } else {
            return None
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None
        }

        let body = self.parse_block_statement();

        Some(Box::new(FunctionLiteral{
            token, 
            parameters,
            body
        }))
    }

    fn parse_call_expression(&mut self, left: Box<dyn Expression>) -> (Option<Box<dyn Expression>>, bool) {
        self.next_token();
        let token = self.current_token.clone();
        match self.parse_call_arguments() {
            Some(arguments) => {
                (Some(Box::new(CallExpression{
                    token,
                    function: left,
                    arguments
                })), false)
            },
            None => (None, true),
        }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut args: Vec<Box<dyn Expression>> = Vec::new();
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(args);
        }
        self.next_token();
        args.push(self.parse_expression(ExpressionType::LOWEST.get_value()).unwrap());

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let arg = self.parse_expression(ExpressionType::LOWEST.get_value());
            if arg.is_some() {
                args.push(arg.unwrap());
            }
        }
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        Some(args)
    }

    fn parse_prefix_fn_expressions(&mut self) -> Option<Box<dyn Expression>> {
        match self.current_token.token_type {
            TokenType::IDENT => self.parse_identifier(),
            TokenType::INT => self.parse_integer_literal(),
            TokenType::MINUS | TokenType::BANG => self.parse_prefix_expression(),
            TokenType::TRUE | TokenType::FALSE => self.parse_boolean(),
            TokenType::LPAREN => self.parse_grouped_expression(),
            TokenType::IF => self.parse_if_expresssion(),
            TokenType::FUNCTION => self.parse_function_literal(),
            _ => {
                self.parse_fn_error(&self.current_token.token_type.clone(), "prefix");
                None
            },
        }
    }

    fn parse_infix_fn_expressions(&mut self, left: Box<dyn Expression>) -> (Option<Box<dyn Expression>>, bool) {
        match self.peek_token.token_type {
            TokenType::PLUS | TokenType::MINUS | TokenType::GT | TokenType::LT |
            TokenType::SLASH | TokenType::ASTERISK | TokenType::EQ | 
            TokenType::NOTEQ => self.parse_infix_expression(left),
            TokenType::LPAREN => self.parse_call_expression(left),
            _ => {
                self.parse_fn_error(&self.peek_token.token_type.clone(), "infix");
                (Some(left), true)
            }
        }
    }

    fn parse_expression(&mut self, precedence: usize) -> Option<Box<dyn Expression>> {
        let mut lft_exp: Option<Box<dyn Expression>> = self.parse_prefix_fn_expressions();
        if lft_exp.is_none() {
            return None
        }
        
        while !self.peek_token_is(TokenType::SEMICOLON) && self.peek_precendence() > precedence {
            let (exp, should_stop) = self.parse_infix_fn_expressions(lft_exp.unwrap());
            if should_stop {
                return exp
            }
            lft_exp = exp
        } 
        lft_exp
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let expression = self.parse_expression(ExpressionType::LOWEST.get_value());
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

    pub fn peek_precendence(&self) -> usize {
        ExpressionType::get_token_precedence(self.peek_token.token_type.clone())
    }

    pub fn current_precedence(&self) -> usize {
        ExpressionType::get_token_precedence(self.current_token.token_type.clone())
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
                    assert_eq!(test_identifier(&expr_stmt.expression, "foobar".to_string()), true);
                },
                None => {panic!("No expression inside the expression statement");}
            }
        }
    }

    fn test_identifier(exp: &Box<dyn Expression>, value: String) -> bool {
        match exp.as_any().downcast_ref::<Identifier>() {
            Some(iden) => {
                iden.token_literal() == value && iden.value == value
            }
            None => false,
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

    fn test_boolean_literal(expression: &Box<dyn Expression>, boolean: bool) -> bool {
        match expression.as_any().downcast_ref::<Boolean>() {
            Some(bool_exp) => {
                bool_exp.value == boolean && bool_exp.token_literal() == boolean.to_string()
            },
            None => false,
        }
    }

    struct PrefixTest<T> {
        input: String,
        operator: String,
        value: T,
    }

    impl<T> PrefixTest<T> {
        fn new(input: &str, operator: &str, value: T) -> Self {
            Self {
                input: input.to_string(),
                operator: operator.to_string(),
                value
            }
        }
    }

    #[test]
    fn test_int_prefix_expresssion_parsing() {
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
                            assert_eq!(test_integer_literal(&pre_exp.right, test.value), true);
                        },
                        None => panic!("Failed to parse prefix expression"),
                    }
                },
                None => panic!("Failed to parse expression statement"),
            }
        }
    }

    #[test]
    fn test_bool_prefix_expresssion_parsing() {
        let tests = [
            PrefixTest::new("!true;", "!", true),
            PrefixTest::new("!false", "!", false),
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
                            assert_eq!(test_boolean_literal(&pre_exp.right, test.value), true);
                        },
                        None => panic!("Failed to parse prefix expression"),
                    }
                },
                None => panic!("Failed to parse expression statement"),
            }
        }
    }

    struct InfixTest<T> {
        input: String,
        operator: String,
        left_value: T,
        right_value: T,
    }

    impl<T> InfixTest<T> {
        fn new(input: &str, operator: &str, left_value: T, right_value: T) -> Self {
            Self{
                input: input.to_string(),
                operator: operator.to_string(),
                left_value, 
                right_value
            }
        }
    }

    #[test]
    fn test_int_infix_expression_parsing() {
        let tests = [
            InfixTest::new("5 + 5;", "+", 5, 5),
            InfixTest::new("5 - 5;", "-", 5, 5),
            InfixTest::new("5 / 5;", "/", 5, 5),
            InfixTest::new("5 * 5;", "*", 5, 5),
            InfixTest::new("5 > 5;", ">", 5, 5),
            InfixTest::new("5 < 5;", "<", 5, 5),
            InfixTest::new("5 == 5;", "==", 5, 5),
            InfixTest::new("5 != 5;", "!=", 5, 5),
        ];

        for test in tests.iter() {
            let lex = Lexer::new(test.input.clone());
            let mut parse = Parser::new(lex);
            let program = parse.parse_program();

            check_parser_errors(&parse);
            match program.statements[0].as_any().downcast_ref::<ExpressionStatement> () {
                Some(exp_stmt) => {
                    match exp_stmt.expression.as_any().downcast_ref::<InfixExpression>() {
                        Some(infix_exp) => {
                            assert_eq!(infix_exp.operator, test.operator);
                            assert_eq!(test_integer_literal(&infix_exp.right, test.right_value), true);
                            assert_eq!(test_integer_literal(&infix_exp.left, test.left_value), true);
                        },
                        None => panic!("failed to downcast an infix expression"),
                    }
                },
                None => panic!("failed to downcast the expression statement"),
            }
        }
    }

    #[test]
    fn test_bool_infix_expression_parsing() {
        let tests = [
            InfixTest::new("true == true", "==", true, true),
            InfixTest::new("true != false", "!=", true, false),
            InfixTest::new("false == false", "==", false, false),
        ];

        for test in tests.iter() {
            let lex = Lexer::new(test.input.clone());
            let mut parse = Parser::new(lex);
            let program = parse.parse_program();

            check_parser_errors(&parse);
            match program.statements[0].as_any().downcast_ref::<ExpressionStatement> () {
                Some(exp_stmt) => {
                    match exp_stmt.expression.as_any().downcast_ref::<InfixExpression>() {
                        Some(infix_exp) => {
                            assert_eq!(infix_exp.operator, test.operator);
                            assert_eq!(test_boolean_literal(&infix_exp.right, test.right_value), true);
                            assert_eq!(test_boolean_literal(&infix_exp.left, test.left_value), true);
                        },
                        None => panic!("failed to downcast an infix expression"),
                    }
                },
                None => panic!("failed to downcast the expression statement"),
            }
        }
    }

    struct ComplexExpressions(String, String);

    impl ComplexExpressions {
        fn new(input: &str, expected: &str) -> Self {
            ComplexExpressions(input.to_string(), expected.to_string())
        }
    }

    #[test]
    fn test_complex_expressions() {
        let tests = [
            ComplexExpressions::new("-a * b", "((-a) * b)"),
            ComplexExpressions::new("!-a", "(!(-a))"),
            ComplexExpressions::new("a + b + c", "((a + b) + c)"),
            ComplexExpressions::new("a + b - c", "((a + b) - c)"),
            ComplexExpressions::new("a * b * c", "((a * b) * c)"),
            ComplexExpressions::new("a * b / c", "((a * b) / c)"),
            ComplexExpressions::new("a + b / c", "(a + (b / c))"),
            ComplexExpressions::new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ComplexExpressions::new("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ComplexExpressions::new("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ComplexExpressions::new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ComplexExpressions::new("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ComplexExpressions::new("true", "true"),
            ComplexExpressions::new("false", "false"),
            ComplexExpressions::new("3 > 2 == true", "((3 > 2) == true)"),
            ComplexExpressions::new("3 < 5 == true", "((3 < 5) == true)"),
            ComplexExpressions::new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ComplexExpressions::new("(5 + 5) * 2", "((5 + 5) * 2)"),
            ComplexExpressions::new("2 / (5 + 5)", "(2 / (5 + 5))"),
            ComplexExpressions::new("-(5 + 5)", "(-(5 + 5))"),
            ComplexExpressions::new("!(true == true)", "(!(true == true))"),
            ComplexExpressions::new("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ComplexExpressions::new("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ComplexExpressions::new("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
        ];

        for test in tests.iter() {
            let lex = Lexer::new(test.0.clone());
            let mut parse = Parser::new(lex);
            let program = parse.parse_program();
            check_parser_errors(&parse);

            let actual = program.to_string();
            assert_eq!(actual, test.1);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = String::from("true;");
        let lex = Lexer::new(input);
        let mut parse = Parser::new(lex);
        let program = parse.parse_program();
        check_parser_errors(&parse);

        match program.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
            Some(exp_stmt) => {
                match exp_stmt.expression.as_any().downcast_ref::<Boolean>() {
                    Some(boolean) => {
                        assert_eq!(boolean.token_literal(), "true");
                        assert_eq!(boolean.value, true);
                    },
                    None => panic!("Failed to downcast boolean expression"),
                }
            },
            None => panic!("Failed to downcast statement expression"),
        }
    }

    #[test]
    fn test_if_expression() {
        let input = String::from("if (x < y) { x } else { y }");
        let lex = Lexer::new(input);
        let mut parse = Parser::new(lex);
        let program = parse.parse_program();
        check_parser_errors(&parse);

        if program.statements.len() != 1 {
            panic!("Expected there to be one statement");
        }
        assert_eq!(program.to_string(), "if (x < y) x else y");
        match program.statements[0].as_any().downcast_ref::<ExpressionStatement>() {
            Some(exp_stmt) => {
                match exp_stmt.expression.as_any().downcast_ref::<IfExpression>() {
                    Some(if_exp) => {
                        
                        assert_eq!(if_exp.condition.to_string(), "(x < y)");
                        
                        if if_exp.consequence.statements.len() != 1 {
                            panic!("Expected there to be just one statement");
                        }

                        if let Some(exp_stmt) = if_exp.consequence.statements[0].as_any().downcast_ref::<ExpressionStatement> () {
                            assert_eq!(test_identifier(&exp_stmt.expression, "x".to_string()), true);
                        }

                        assert_eq!(if_exp.alternative.is_some(), true);
                    },
                    None => panic!("Failed to downcast if expression"),
                }
            },
            None => panic!("failed to downcast expression statement"),
        }
    }


    #[test]
    fn test_function_literal() {
        let input = String::from("fn (x, y) { x + y; }");

        let lex = Lexer::new(input);
        let mut parse = Parser::new(lex);
        let program = parse.parse_program();
        check_parser_errors(&parse);

        assert_eq!(program.statements.len(), 1);


        let exp_stmt = program.statements[0].as_any().downcast_ref::<ExpressionStatement>().unwrap();

        let function = exp_stmt.expression.as_any().downcast_ref::<FunctionLiteral>().unwrap();

        assert_eq!(function.parameters.len(), 2);
        assert_eq!(function.body.statements.len(), 1);
    }

    #[test]
    fn test_call_expression() {
        let input = String::from("add(1, 2 * 3, 4 + 5);");

        let lex = Lexer::new(input);
        let mut parse = Parser::new(lex);
        let program = parse.parse_program();
        check_parser_errors(&parse);

        assert_eq!(program.statements.len(), 1);

        let exp_stmt = program.statements[0].as_any().downcast_ref::<ExpressionStatement>().unwrap();
        let call_exp = exp_stmt.expression.as_any().downcast_ref::<CallExpression>().unwrap();

        assert_eq!(test_identifier(&call_exp.function, "add".to_string()), true);
        assert_eq!(call_exp.arguments.len(), 3);
        assert_eq!(call_exp.arguments[0].to_string(), "1");
        assert_eq!(call_exp.arguments[1].to_string(), "(2 * 3)");
        assert_eq!(call_exp.arguments[2].to_string(), "(4 + 5)");
    }
}