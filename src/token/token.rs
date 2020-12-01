pub type TokenType = String;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,

    IDENT(String),
    INT(i64),
    STRING(String),
    BOOL(bool),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,
    COMMA,
    SEMICOLON,
    COLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    MACRO,
}

pub fn LookupIdent(ident: String) -> Token {
    match &*ident {
        "fn" => Token::FUNCTION,
        "let" => Token::LET,
        "true" => Token::TRUE,
        "false" => Token::FALSE,
        "if" => Token::IF,
        "else" => Token::ELSE,
        "return" => Token::RETURN,
        "macro" => Token::MACRO,
        _ => Token::IDENT(ident),
    }
}
