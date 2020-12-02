use crate::lexer::lexer::*;
use crate::parser::parser::*;
use crate::token::token::*;

const PROMPT: &str = ">> ";

pub fn Start() {
    let mut rl = rustyline::Editor::<()>::new();
    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(&line);
                if (line == "exit") {
                    println!("\nBye :)");
                    break;
                }
                let mut l = Lexer::New(&line);
                let mut p = Parser::New(l);

                let program = p.ParseProgram();
                if p.Errors().len() != 0 {
                    printParserErrors(p.Errors());
                    continue;
                }
                println!("{:#?}", program);
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("\nBye :)");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
            }
        }
    }
}

pub fn printParserErrors(errors: Vec<String>) {
    for err in errors {
        println!("{}", err);
    }
}
