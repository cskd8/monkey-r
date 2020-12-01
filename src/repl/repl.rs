use crate::lexer::lexer::*;
use crate::token::token::*;

const PROMPT: &str = ">> ";

pub fn Start() {
    let mut rl = rustyline::Editor::<()>::new();
    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(&line);
                let mut l = Lexer::New(&line);
                let mut tok = l.NextToken();
                while tok != Token::EOF {
                    println!("{:?}", tok);
                    tok = l.NextToken();
                }
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    }
}
