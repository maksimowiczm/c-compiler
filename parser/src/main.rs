mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::declaration::declaration::Declaration;
use crate::parser::expression::Expression;
use crate::parser::Parse;
use clap::{Parser, Subcommand};
use std::fmt::Debug;

#[derive(Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Expression { path: String },
    Declaration { path: String },
}

impl Command {
    fn path(&self) -> &str {
        match self {
            Command::Expression { path } | Command::Declaration { path } => path,
        }
    }

    fn run(&self) {
        match self {
            Command::Expression { .. } => self.parse::<Expression>(),
            Command::Declaration { .. } => self.parse::<Declaration>(),
        }
    }

    fn parse<P: Parse + Debug>(&self) {
        let path = self.path();
        let content = std::fs::read_to_string(path).unwrap();
        let tokens = Lexer::new(content.chars().peekable())
            .into_iter()
            .collect::<Vec<_>>();

        println!("{:?}", tokens);

        let mut tokens = tokens.into_iter().peekable();
        let result = P::parse(&mut tokens).unwrap();
        println!("{:?}", result);
    }
}

fn main() {
    let args = Args::parse();
    args.command.run();
}
