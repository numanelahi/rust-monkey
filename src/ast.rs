use std::any::Any;
use crate::token::Token;
use std::fmt::{self, Display};

pub trait Node {
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

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.token, self.value)
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

