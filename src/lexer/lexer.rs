use crate::token::token::*;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn New(input: &'a str) -> Self {
        let mut l = Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.readChar();
        return l;
    }

    pub fn readChar(&mut self) {
        self.ch = {
            if self.read_position >= self.input.len() {
                0
            } else {
                self.input.as_bytes()[self.read_position]
            }
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn NextToken(&mut self) -> Token {
        self.skipWhitespace();
        let tok = match self.ch {
            b'=' => {
                if self.peekChar() == b'=' {
                    let ch = self.ch;
                    self.readChar();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            b'+' => Token::PLUS,
            b'-' => Token::MINUS,
            b'!' => {
                if self.peekChar() == b'=' {
                    let ch = self.ch;
                    self.readChar();
                    Token::NOT_EQ
                } else {
                    Token::BANG
                }
            }
            b'/' => Token::SLASH,
            b'*' => Token::ASTERISK,
            b'<' => Token::LT,
            b'>' => Token::GT,
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b',' => Token::COMMA,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b'"' => Token::STRING(self.readString()),
            b'[' => Token::LBRACKET,
            b']' => Token::RBRACKET,
            b':' => Token::COLON,
            0 => Token::EOF,
            b'a'...b'z' | b'A'...b'Z' | b'_' => {
                return LookupIdent(self.readIdentifier());
            }
            b'0'...b'9' => {
                return Token::INT(self.readNumber());
            }
            _ => Token::ILLEGAL,
        };
        self.readChar();
        return tok;
    }
    pub fn readNumber(&mut self) -> i64 {
        let position = self.position;
        loop {
            match self.ch {
                b'0'...b'9' => self.readChar(),
                _ => {
                    break;
                }
            }
        }
        return std::str::from_utf8(&self.input.as_bytes()[position..self.position])
            .unwrap()
            .to_string()
            .parse::<i64>()
            .unwrap();
    }
    pub fn readIdentifier(&mut self) -> String {
        let position = self.position;
        loop {
            match self.ch {
                b'a'...b'z' | b'A'...b'Z' | b'_' => {
                    self.readChar();
                }
                _ => {
                    break;
                }
            }
        }
        std::str::from_utf8(&self.input.as_bytes()[position..self.position])
            .unwrap()
            .to_string()
    }
    pub fn skipWhitespace(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' | b'\r' => {
                    self.readChar();
                }
                _ => {
                    break;
                }
            }
        }
    }
    pub fn peekChar(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    pub fn readString(&mut self) -> String {
        let position = self.position + 1;
        self.readChar();
        loop {
            match self.ch {
                b'"' | 0 => {
                    let literal = &self.input[position..self.position];
                    self.readChar();
                    return literal.to_string();
                }
                _ => {
                    self.readChar();
                }
            }
        }
    }
}
