use crate::lexer::lexer::*;
use crate::parser::parser::*;
use crate::token::token::*;

pub struct Identifier {
    Value: String,
}

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

pub enum Statement {
    Let(Identifier, Expression),
    Return(Identifier, Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

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

pub struct BlockStatement {
    Statements: Vec<Statement>,
}

pub struct Program {
    pub Statements: Vec<Statement>,
}
