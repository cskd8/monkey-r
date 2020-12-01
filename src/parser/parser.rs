use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;

pub enum Exp {
    LOWEST = 0,
    EQUALS = 1,
    LESSGREATER = 2,
    SUM = 3,
    PRODUCT = 4,
    PREFIX = 5,
    CALL = 6,
    INDEX = 7,
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
        let mut program = ast::Program {
            Statements: Vec::new(),
        };

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
        let mut v = match self.parseExpression(Exp::LOWEST as i32) {
            Some(expr) => expr,
            None => return None,
        };
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
        let mut v = match self.parseExpression(Exp::LOWEST as i32) {
            Some(expr) => expr,
            None => return None,
        };
        if self.peekTokenIs(token::Token::SEMICOLON) {
            self.nextToken();
        }

        let stmt = ast::Statement::Let(n, v);
        return Some(stmt);
    }

    pub fn parseExpressionStatement(&self) -> Option<ast::Statement> {
        let mut v = match self.parseExpression(Exp::LOWEST as i32) {
            Some(expr) => expr,
            None => return None,
        };
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

    pub fn parseExpression(&mut self, precedence: i32) -> Option<ast::Expression> {
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

        while !self.peekTokenIs(token::Token::SEMICOLON)
            && precedence < (self.peekPrecedence() as i32)
        {
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

    pub fn parseIdentifier(&self) -> Option<ast::Expression> {
        match self.curToken {
            token::Token::IDENT(ident) => Some(ast::Expression::Identifier(ast::Identifier {
                Value: ident,
            })),
            _ => None,
        }
    }

    pub fn parseIntegerLiteral(&self) -> Option<ast::Expression> {
        let v = match self.curToken {
            token::Token::INT(lit) => lit as i64,
            _ => {
                let msg = format!("could not parse {:?} as integer", self.curToken);
                println!("{}", msg);
                self.errors.push(msg);
                return None;
            }
        };

        let lit = Some(ast::Expression::Literal(ast::Literal::Int(v)));

        return lit;
    }

    pub fn parsePrefixExpression(&self) -> Option<ast::Expression> {
        let prefix = match self.curToken {
            token::Token::IDENT(_) => ast::Prefix::IDENT,
            token::Token::INT(_) => ast::Prefix::INT,
            token::Token::BANG => ast::Prefix::BANG,
            token::Token::MINUS => ast::Prefix::MINUS,
            token::Token::TRUE => ast::Prefix::TRUE,
            token::Token::FALSE => ast::Prefix::FALSE,
            token::Token::LPAREN => ast::Prefix::LPAREN,
            token::Token::IF => ast::Prefix::IF,
            token::Token::FUNCTION => ast::Prefix::FUNCTION,
            token::Token::STRING(_) => ast::Prefix::STRING,
            token::Token::LBRACKET => ast::Prefix::LBRACKET,
            token::Token::LBRACE => ast::Prefix::LBRACE,
            token::Token::MACRO => ast::Prefix::MACRO,
            _ => return None,
        };

        self.nextToken();

        match self.parseExpression(Exp::PREFIX as i32) {
            Some(expr) => Some(ast::Expression::Prefix(prefix, Box::new(expr))),
            None => None,
        }
    }

    pub fn peekPrecedence(&self) -> i32 {
        let p = match self.peekToken {
            token::Token::EQ => Exp::EQUALS,
            token::Token::NOT_EQ => Exp::EQUALS,
            token::Token::LT => Exp::LESSGREATER,
            token::Token::GT => Exp::LESSGREATER,
            token::Token::PLUS => Exp::SUM,
            token::Token::MINUS => Exp::SUM,
            token::Token::SLASH => Exp::PRODUCT,
            token::Token::ASTERISK => Exp::PRODUCT,
            token::Token::LPAREN => Exp::CALL,
            token::Token::LBRACKET => Exp::INDEX,
            _ => Exp::LOWEST,
        };

        return p as i32;
    }

    pub fn curPrecedence(&self) -> i32 {
        let p = match self.curToken {
            token::Token::EQ => Exp::EQUALS,
            token::Token::NOT_EQ => Exp::EQUALS,
            token::Token::LT => Exp::LESSGREATER,
            token::Token::GT => Exp::LESSGREATER,
            token::Token::PLUS => Exp::SUM,
            token::Token::MINUS => Exp::SUM,
            token::Token::SLASH => Exp::PRODUCT,
            token::Token::ASTERISK => Exp::PRODUCT,
            token::Token::LPAREN => Exp::CALL,
            token::Token::LBRACKET => Exp::INDEX,
            _ => Exp::LOWEST,
        };

        return p as i32;
    }

    pub fn parseInfixExpression(&self, left: ast::Expression) -> Option<ast::Expression> {
        let infix = match self.curToken {
            token::Token::PLUS => ast::Infix::PLUS,
            token::Token::MINUS => ast::Infix::MINUS,
            token::Token::SLASH => ast::Infix::SLASH,
            token::Token::ASTERISK => ast::Infix::ASTERISK,
            token::Token::EQ => ast::Infix::EQ,
            token::Token::NOT_EQ => ast::Infix::NOT_EQ,
            token::Token::LT => ast::Infix::LT,
            token::Token::GT => ast::Infix::GT,
            token::Token::LPAREN => ast::Infix::LPAREN,
            token::Token::LBRACKET => ast::Infix::LBRACKET,
            _ => return None,
        };

        let precedence = self.curPrecedence();
        self.nextToken();
        match self.parseExpression(precedence) {
            Some(expr) => Some(ast::Expression::Infix(
                infix,
                Box::new(left),
                Box::new(expr),
            )),
            None => None,
        }
    }

    pub fn parseBoolean(&self) -> Option<ast::Expression> {
        match self.curToken {
            token::Token::BOOL(value) => {
                Some(ast::Expression::Literal(ast::Literal::Boolean(value)))
            }
            _ => None,
        }
    }

    pub fn parseGroupedExpression(&self) -> Option<ast::Expression> {
        self.nextToken();

        let exp = self.parseExpression(Exp::LOWEST as i32);
        if !self.expectPeek(token::Token::RPAREN) {
            return None;
        }

        return exp;
    }

    pub fn parseIfExpression(&self) -> Option<ast::Expression> {
        if !self.expectPeek(token::Token::LPAREN) {
            return None;
        }

        self.nextToken();

        let condition = match self.parseExpression(Exp::LOWEST as i32) {
            Some(cond) => cond,
            _ => return None,
        };

        if !self.expectPeek(token::Token::RPAREN) {
            return None;
        }
        if !self.expectPeek(token::Token::LBRACE) {
            return None;
        }
        let consequence = match self.parseBlockStatement() {
            Some(value) => value,
            _ => return None,
        };

        let mut alternative;
        if self.peekTokenIs(token::Token::ELSE) {
            self.nextToken();

            if !self.expectPeek(token::Token::LBRACE) {
                return None;
            }

            alternative = match self.parseBlockStatement() {
                Some(value) => value,
                _ => return None,
            };
        }

        return Some(ast::Expression::If {
            Condition: Box::new(condition),
            Consequence: consequence,
            Alternative: alternative,
        });
    }

    pub fn parseBlockStatement(&self) -> Option<ast::BlockStatement> {
        let mut statements = Vec::new();
        self.nextToken();
        while !self.curTokenIs(token::Token::RBRACE) && !self.curTokenIs(token::Token::EOF) {
            let stmt = self.parseStatement();
            if let Some(s) = stmt {
                statements.push(s);
            }
            self.nextToken();
        }
        return Some(ast::BlockStatement {
            Statements: statements,
        });
    }

    pub fn parseFunctionLiteral(&self) -> Option<ast::Expression> {
        if !self.expectPeek(token::Token::LPAREN) {
            return None;
        }

        let parameters = match self.parseFunctionParameters() {
            Some(value) => value,
            _ => return None,
        };

        if !self.expectPeek(token::Token::LBRACE) {
            return None;
        }

        let body = match self.parseBlockStatement() {
            Some(value) => value,
            _ => return None,
        };

        let lit = ast::Expression::Literal(ast::Literal::Function {
            Parameters: parameters,
            Body: body,
        });

        return Some(lit);
    }

    pub fn parseFunctionParameters(&self) -> Option<Vec<ast::Identifier>> {
        let identifiers = Vec::new();
        if self.peekTokenIs(token::Token::RPAREN) {
            self.nextToken();
            return Some(identifiers);
        }

        self.nextToken();

        let ident = match self.curToken {
            token::Token::IDENT(name) => ast::Identifier { Value: name },
            _ => return None,
        };
        identifiers.push(ident);

        while !self.peekTokenIs(token::Token::COMMA) {
            self.nextToken();
            self.nextToken();
            let ident = match self.curToken {
                token::Token::IDENT(name) => ast::Identifier { Value: name },
                _ => return None,
            };
            identifiers.push(ident);
        }

        if !self.expectPeek(token::Token::RPAREN) {
            return None;
        }

        return Some(identifiers);
    }

    pub fn parseCallExpression(&self, function: ast::Expression) -> Option<ast::Expression> {
        let exp = ast::Expression::Call {
            Function: Box::new(function),
            Arguments: match self.parseExpressionList(token::Token::RPAREN) {
                Some(value) => value,
                _ => return None,
            },
        };
        return Some(exp);
    }
}
