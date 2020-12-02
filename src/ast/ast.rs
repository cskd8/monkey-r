use crate::lexer::lexer::*;
use crate::parser::parser::*;
use crate::token::token::*;

#[derive(Debug)]
pub struct Identifier {
    pub Value: String,
}

#[derive(Debug)]
pub enum Prefix {
    IDENT,
    INT,
    BANG,
    MINUS,
    TRUE,
    FALSE,
    LPAREN,
    IF,
    FUNCTION,
    STRING,
    LBRACKET,
    LBRACE,
    MACRO,
}

#[derive(Debug)]
pub enum Infix {
    PLUS,
    MINUS,
    SLASH,
    ASTERISK,
    EQ,
    NOT_EQ,
    LT,
    GT,
    LPAREN,
    LBRACKET,
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If {
        Condition: Box<Expression>,
        Consequence: BlockStatement,
        Alternative: BlockStatement,
    },
    Call {
        Function: Box<Expression>,
        Arguments: Vec<Expression>,
    },
    Index {
        Left: Box<Expression>,
        Index: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Function {
        Parameters: Vec<Identifier>,
        Body: BlockStatement,
    },
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    Hash(Vec<(Expression, Expression)>),
    Macro {
        Parameters: Vec<Identifier>,
        Body: BlockStatement,
    },
}

#[derive(Debug)]
pub struct BlockStatement {
    pub Statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Program {
    pub Statements: Vec<Statement>,
}
