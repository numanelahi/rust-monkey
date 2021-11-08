use std::any::Any;
use crate::token::{Token, TokenType};
use std::fmt::{self, Display};

pub enum ExpressionType {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

impl ExpressionType {
    pub fn get_value(&self) -> usize {
        match self {
            Self::LOWEST => 1,
            Self::EQUALS => 2,
            Self::LESSGREATER => 3,
            Self::SUM => 4,
            Self::PRODUCT => 5,
            Self::PREFIX => 6,
            Self::CALL => 7
        }
    }

    pub fn get_token_precedence(token_type: TokenType) -> usize {
        let val = match token_type {
            TokenType::EQ | TokenType::NOTEQ => Self::EQUALS.get_value(),
            TokenType::LT | TokenType::GT => Self::LESSGREATER.get_value(),
            TokenType::PLUS | TokenType::MINUS => Self::SUM.get_value(),
            TokenType::ASTERISK | TokenType::SLASH => Self::PRODUCT.get_value(),
            _ => Self::LOWEST.get_value(),
        };
        val
    }
}

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

pub trait Downcast: Node {
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Downcast {}

pub trait Expression: Downcast {}

#[derive(Default)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    fn new() -> Self {
        Self{
            statements: Vec::new(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut program_str: String = String::new();
        for stmt in &self.statements {
            program_str.push_str(stmt.to_string().as_str());
        }
        write!(f, "{}", program_str)
    } 
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() == 0 {
            "".to_string()
        } else {
            self.statements[0].token_literal()
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} = {};", self.token_literal(), self.name, self.value)
    } 
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for LetStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement{}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {};", self.token_literal(), self.value)
    } 
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for ReturnStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement{}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for ExpressionStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement{}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for Identifier {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {}


pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for IntegerLiteral {
    fn as_any(&self) -> &dyn Any {
        self
    } 
}

impl Expression for IntegerLiteral {}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for PrefixExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {}

pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
    pub left: Box<dyn Expression>
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    } 
}

impl Downcast for InfixExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {}

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for Boolean {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Boolean {}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("if {} {}", self.condition.to_string(), self.consequence.to_string());
        if let Some(alt) = &self.alternative {
            s.push_str(" else ");
            s.push_str(alt.to_string().as_str());
        }
        write!(f, "{}", s)
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for IfExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for stmt in &self.statements {
            s.push_str(stmt.to_string().as_str())
        }
        write!(f, "{}", s)
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Downcast for BlockStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for BlockStatement {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_program_string() {
        let mut program = Program::new();
        program.statements.push(Box::new(LetStatement{
            token: Token::new(TokenType::LET, "let".to_string()),
            name: Identifier{
                token: Token::new(TokenType::IDENT, "my_var".to_string()),
                value: "my_var".to_string(),
            },
            value: Box::new(Identifier{
                token: Token::new(TokenType::IDENT, "another_var".to_string()),
                value: "another_var".to_string(),
            })
        }));
        assert_eq!(program.to_string(), "let my_var = another_var;")
    }
}