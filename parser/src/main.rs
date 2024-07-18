mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::expression::Expression;
use crate::parser::Parse;
use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Expression { path: String },
}

impl Command {
    fn path(&self) -> &str {
        match self {
            Command::Expression { path } => path,
        }
    }

    fn run(&self) {
        let path = self.path();
        let content = std::fs::read_to_string(path).unwrap();
        let tokens = Lexer::new(content.chars().peekable())
            .into_iter()
            .collect::<Vec<_>>();

        println!("{:?}", tokens);

        let mut tokens = tokens.into_iter().peekable();

        let str = match self {
            Command::Expression { .. } => {
                let expression = Expression::parse(&mut tokens).unwrap();
                format!("{:?}", expression)
            }
        };

        println!("{}", str);
    }
}

fn main() {
    let args = Args::parse();
    args.command.run();
}
