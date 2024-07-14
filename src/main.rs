use crate::code_generator::CodeGenerator;
use crate::parser::Parser as CodeParser;
use clap::{Parser, Subcommand};
use code_generator::asm64::StringyAssembly64CodeGenerator;
use code_generator::ir::IR;
use std::error::Error;

mod code_generator;
mod lexer;
mod parser;

#[derive(Parser)]
struct Args {
    #[clap(subcommand)]
    command: SubCommand,

    #[clap(long, global = true)]
    output: Option<String>,
}

#[derive(Subcommand)]
enum SubCommand {
    ASM64 { path: String },
    IR { path: String },
}

impl SubCommand {
    fn path(&self) -> &str {
        match self {
            SubCommand::ASM64 { path } | SubCommand::IR { path } => path,
        }
    }

    fn run(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let path = self.path();
        let source = std::fs::read_to_string(&path).expect("could not read file");
        let lexer = lexer::Lexer::new(source.chars().peekable());
        let tokens = lexer.into_iter().peekable();
        let ast = CodeParser::parse(tokens).expect("could not parse program");
        let mut buffer = Vec::new();

        match self {
            SubCommand::ASM64 { .. } => {
                let generator = StringyAssembly64CodeGenerator;
                generator.generate(ast, &mut buffer)?;
            }
            SubCommand::IR { .. } => {
                let generator = IR;
                generator.generate(ast, &mut buffer)?;
            }
        }

        Ok(buffer)
    }
}

fn main() {
    let args = Args::parse();

    let buffer = match args.command.run() {
        Ok(buffer) => buffer,
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    };

    if let Some(output) = args.output {
        std::fs::write(output, &buffer).expect("could not write to file");
    }
}
