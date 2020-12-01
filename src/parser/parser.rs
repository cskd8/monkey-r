use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;

pub enum Exp {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
}

pub struct Parser<'a> {
    l: lexer::Lexer<'a>,
    errors: Vec<String>,
    curToken: token::Token,
    peekToken: token::Token,
}

impl<'a> Parser<'a> {
    pub fn New(l: lexer::Lexer) -> parser::Parser {
        Parser {
            l: l,
            errors: Vec::new(),
            curToken: token::Token::EOF,
            peekToken: token::Token::EOF,
        }
    }

    pub fn Errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    pub fn nextToken(&mut self) {
        self.curToken = self.peekToken.clone();
        self.peekToken = self.l.NextToken();
    }

    pub fn ParseProgram(&mut self) -> ast::Program {
        let mut program = ast::Program { Statements: vec![] };

        while !self.curTokenIs(token::Token::EOF) {
            let mut stmt = self.parseStatement();
            if let Some(s) = stmt {
                program.Statements.push(s);
            }
            self.nextToken();
        }
        return program;
    }

    pub fn parseStatement(&self) -> Option<ast::Statement> {
        match self.curToken {
            token::Token::LET => self.parseLetStatement(),
            token::Token::RETURN => self.parseReturnStatement(),
            _ => self.parseExpressionStatement(),
        }
    }

    pub fn parseLetStatement(&self) -> Option<ast::Statement> {
        match self.peekToken {
            token::Token::IDENT(_) => {}
            _ => return None,
        }
        let mut n;
        match self.curToken {
            token::Token::IDENT(name) => n = ast::Identifier { Value: name },
            _ => return None,
        }
        let mut v = self.parseExpression(Exp::LOWEST);
        if self.peekTokenIs(token::Token::SEMICOLON) {
            self.nextToken();
        }

        let stmt = ast::Statement::Let(n, v);
        return Some(stmt);
    }

    pub fn curTokenIs(&self, t: token::Token) -> bool {
        self.curToken == t
    }

    pub fn peekTokenIs(&self, t: token::Token) -> bool {
        self.peekToken == t
    }

    pub fn expectPeek(&mut self, t: token::Token) -> bool {
        if self.peekTokenIs(t) {
            self.nextToken();
            return true;
        } else {
            self.peekError(t);
            return false;
        }
    }

    pub fn peekError(&mut self, t: token::Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peekToken
        );
        println!("{}", msg);
        self.errors.push(msg);
    }

    pub fn parseReturnStatement(&mut self) -> Option<ast::Statement> {
        let mut n;
        match self.curToken {
            token::Token::IDENT(name) => n = ast::Identifier { Value: name },
            _ => return None,
        }
        let mut v = self.parseExpression(Exp::LOWEST);
        if self.peekTokenIs(token::Token::SEMICOLON) {
            self.nextToken();
        }

        let stmt = ast::Statement::Let(n, v);
        return Some(stmt);
    }

    pub fn parseExpressionStatement(&self) -> Option<ast::Statement> {
        let mut v = self.parseExpression(Exp::LOWEST);
        if self.peekTokenIs(token::Token::SEMICOLON) {
            self.nextToken();
        }

        let stmt = ast::Statement::Expression(v);
        return Some(stmt);
    }

    pub fn noPrefixParseFnError(&mut self, t: token::Token) {
        let msg = format!("no prefix parse function for {:?} found", t);
        println!("{}", msg);
        self.errors.push(msg);
    }

    pub fn parseExpression(&mut self, precedence: Exp) -> Option<ast::Expression> {
        let mut leftExp = match self.curToken {
            token::Token::IDENT(_) => self.parseIdentifier(),
            token::Token::INT(_) => self.parseIntegerLiteral(),
            token::Token::BANG | token::Token::MINUS | token::Token::PLUS => {
                self.parsePrefixExpression()
            }
            token::Token::TRUE | token::Token::FALSE => self.parseBoolean(),
            token::Token::LPAREN => self.parseGroupedExpression(),
            token::Token::IF => self.parseIfExpression(),
            token::Token::FUNCTION => self.parseFunctionLiteral(),
            token::Token::STRING(_) => self.parseStringLiteral(),
            token::Token::LBRACKET => self.parseArrayLiteral(),
            token::Token::LBRACE => self.parseHashLiteral(),
            token::Token::MACRO => self.parseMacroLiteral(),
            _ => {
                self.noPrefixParseFnError(self.curToken);
                return None;
            }
        };

        while !self.peekTokenIs(token::Token::SEMICOLON) && precedence < self.peekPrecedence() {
            leftExp = match self.peekToken {
                token::Token::PLUS
                | token::Token::MINUS
                | token::Token::SLASH
                | token::Token::ASTERISK
                | token::Token::EQ
                | token::Token::NOT_EQ
                | token::Token::LT
                | token::Token::GT => self.parseInfixExpression(),
                token::Token::LPAREN => self.parseCallExpression(),
                token::Token::LBRACKET => self.parseIndexExpression(),
                _ => return leftExp,
            };
            self.nextToken();
        }
        return leftExp;
    }
}
