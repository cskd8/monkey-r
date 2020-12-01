mod lexer;
mod repl;
use repl::repl::*;
mod ast;
mod parser;
mod token;

fn main() {
    println!(
        "Hello {}! This is the Monkey programming language!",
        whoami::username()
    );
    println!("Feel free to type in commands");
    Start();
}
