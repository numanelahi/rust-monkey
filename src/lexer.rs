use crate::token::{Token, TokenType};
use crate::utils;

#[derive(Default, Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char, 
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer{
            input: input,
            ..Default::default()
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.input.len() <= self.read_position {
            self.ch = 0 as char;
        } else {
            self.ch = self.input.as_bytes()[self.read_position] as char
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let token = self.parse_token();
        self.read_char();
        token
    }

    fn peek_next_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while utils::is_letter(self.peek_next_char()) {
            self.read_char();
        }
        String::from(&self.input[pos..self.read_position])
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while utils::is_digit(self.peek_next_char()) {
            self.read_char();
        }
        String::from(&self.input[pos..self.read_position])
    }

    fn read_token_type(&self, ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }

    fn eat_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\r' || self.ch == '\n' || self.ch == '\t' {
            self.read_char();
        }
    }

    fn parse_token(&mut self) -> Token {
        self.eat_whitespace();
        match self.ch {
            '=' => {
                if self.peek_next_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(TokenType::EQ, format!("{}{}", ch, self.ch))
                } else {
                    Token::new(TokenType::ASSIGN, String::from(self.ch))
                }
            },
            ';' => Token::new(TokenType::SEMICOLON, String::from(self.ch)),
            '(' => Token::new(TokenType::LPAREN, String::from(self.ch)),
            ')' => Token::new(TokenType::RPAREN, String::from(self.ch)),
            ',' => Token::new(TokenType::COMMA, String::from(self.ch)),
            '+' => Token::new(TokenType::PLUS, String::from(self.ch)),
            '-' => Token::new(TokenType::MINUS, String::from(self.ch)),
            '!' => {
                if self.peek_next_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(TokenType::NOTEQ, format!("{}{}", ch, self.ch))
                } else {
                    Token::new(TokenType::BANG, String::from(self.ch))
                }
            },
            '*' => Token::new(TokenType::ASTERISK, String::from(self.ch)),
            '/' => Token::new(TokenType::SLASH, String::from(self.ch)),
            '<' => {
                if self.peek_next_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(TokenType::LTEQ, format!("{}{}", ch, self.ch))
                } else {
                    Token::new(TokenType::LT, String::from(self.ch))
                }
            },
            '>' => {
                if self.peek_next_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(TokenType::GTEQ, format!("{}{}", ch, self.ch))
                } else {
                    Token::new(TokenType::GT, String::from(self.ch))
                }
            },
            '{' => Token::new(TokenType::LBRACE, String::from(self.ch)),
            '}' => Token::new(TokenType::RBRACE, String::from(self.ch)),
            '\0' => Token::new(TokenType::EOF, String::from(self.ch)),
            _ => {
                if utils::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    Token::new(self.read_token_type(&literal), literal)
                } else if utils::is_digit(self.ch) {
                    Token::new(TokenType::INT, self.read_number())
                } else {
                    Token::new(TokenType::ILLEGAL, String::from(self.ch))
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {

    use super::*;
    use crate::token::TokenType;

    struct ExpectedToken {
        expected_token_type: TokenType,
        expected_literal: String,
    }

    impl ExpectedToken {
        fn new(expected_token_type: TokenType, expected_literal: &str) -> ExpectedToken {
            ExpectedToken{
                expected_token_type,
                expected_literal: String::from(expected_literal),
            }
        }
    }

    #[test]
    fn simple_next_token_test() {
        let input: String = String::from("=+{},;");
        
        let tests = [
            ExpectedToken::new(TokenType::ASSIGN, "="),
            ExpectedToken::new(TokenType::PLUS, "+"),
            ExpectedToken::new(TokenType::LBRACE, "{"),
            ExpectedToken::new(TokenType::RBRACE, "}"),
            ExpectedToken::new(TokenType::COMMA, ","),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
        ];

        let mut l = Lexer::new(input);

        for test in tests.iter() {
            let tok = l.next_token();
            assert_eq!(tok.token_type, test.expected_token_type);
            assert_eq!(tok.literal, test.expected_literal);
        }
    }

    #[test]
    fn monkey_next_token_test() {
        let s = String::from("let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        10 >= 9;
        9 <= 10;
        ");

        let tests = [
            ExpectedToken::new(TokenType::LET, "let"),
            ExpectedToken::new(TokenType::IDENT, "five"),
            ExpectedToken::new(TokenType::ASSIGN, "="),
            ExpectedToken::new(TokenType::INT, "5"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::LET, "let"),
            ExpectedToken::new(TokenType::IDENT, "ten"),
            ExpectedToken::new(TokenType::ASSIGN, "="),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::LET, "let"),
            ExpectedToken::new(TokenType::IDENT, "add"),
            ExpectedToken::new(TokenType::ASSIGN, "="),
            ExpectedToken::new(TokenType::FUNCTION, "fn"),
            ExpectedToken::new(TokenType::LPAREN, "("),
            ExpectedToken::new(TokenType::IDENT, "x"),
            ExpectedToken::new(TokenType::COMMA, ","),
            ExpectedToken::new(TokenType::IDENT, "y"),
            ExpectedToken::new(TokenType::RPAREN, ")"),
            ExpectedToken::new(TokenType::LBRACE, "{"),
            ExpectedToken::new(TokenType::IDENT, "x"),
            ExpectedToken::new(TokenType::PLUS, "+"),
            ExpectedToken::new(TokenType::IDENT, "y"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::RBRACE, "}"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::LET, "let"),
            ExpectedToken::new(TokenType::IDENT, "result"),
            ExpectedToken::new(TokenType::ASSIGN, "="),
            ExpectedToken::new(TokenType::IDENT, "add"),
            ExpectedToken::new(TokenType::LPAREN, "("),
            ExpectedToken::new(TokenType::IDENT, "five"),
            ExpectedToken::new(TokenType::COMMA, ","),
            ExpectedToken::new(TokenType::IDENT, "ten"),
            ExpectedToken::new(TokenType::RPAREN, ")"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::BANG, "!"),
            ExpectedToken::new(TokenType::MINUS, "-"),
            ExpectedToken::new(TokenType::SLASH, "/"),
            ExpectedToken::new(TokenType::ASTERISK, "*"),
            ExpectedToken::new(TokenType::INT, "5"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::INT, "5"),
            ExpectedToken::new(TokenType::LT, "<"),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::GT, ">"),
            ExpectedToken::new(TokenType::INT, "5"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::IF, "if"),
            ExpectedToken::new(TokenType::LPAREN, "("),
            ExpectedToken::new(TokenType::INT, "5"),
            ExpectedToken::new(TokenType::LT, "<"),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::RPAREN, ")"),
            ExpectedToken::new(TokenType::LBRACE, "{"),
            ExpectedToken::new(TokenType::RETURN, "return"),
            ExpectedToken::new(TokenType::TRUE, "true"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::RBRACE, "}"),
            ExpectedToken::new(TokenType::ELSE, "else"),
            ExpectedToken::new(TokenType::LBRACE, "{"),
            ExpectedToken::new(TokenType::RETURN, "return"),
            ExpectedToken::new(TokenType::FALSE, "false"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::RBRACE, "}"),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::EQ, "=="),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::NOTEQ, "!="),
            ExpectedToken::new(TokenType::INT, "9"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::GTEQ, ">="),
            ExpectedToken::new(TokenType::INT, "9"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::INT, "9"),
            ExpectedToken::new(TokenType::LTEQ, "<="),
            ExpectedToken::new(TokenType::INT, "10"),
            ExpectedToken::new(TokenType::SEMICOLON, ";"),
            ExpectedToken::new(TokenType::EOF, "\0")
        ];

        let mut l = Lexer::new(s);

        for test in tests.iter() {
            let tok = l.next_token();
            assert_eq!(test.expected_literal, tok.literal);
            assert_eq!(test.expected_token_type, tok.token_type);
        }
    }
}