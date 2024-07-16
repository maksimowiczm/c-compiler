use crate::code_generator::CodeGenerator;
use clap::{Parser, Subcommand};
use code_generator::asm64::StringyAssembly64CodeGenerator;
use code_generator::ir::IR;
use parser::lexer::Lexer;
use parser::parser::Parser as CodeParser;
use std::error::Error;

mod code_generator;

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
    Tokenize { path: String },
    Ast { path: String },
}

impl SubCommand {
    fn path(&self) -> &str {
        match self {
            SubCommand::ASM64 { path }
            | SubCommand::IR { path }
            | SubCommand::Tokenize { path }
            | SubCommand::Ast { path } => path,
        }
    }

    fn run(&self) -> Result<Option<Vec<u8>>, Box<dyn Error>> {
        let path = self.path();
        let source = std::fs::read_to_string(path).expect("could not read file");
        let lexer = Lexer::new(source.chars().peekable());
        let tokens = lexer.into_iter().peekable();

        if let SubCommand::Tokenize { .. } = self {
            for token in tokens {
                println!("{:?}", token);
            }
            return Ok(None);
        }

        let ast = CodeParser::parse(tokens).expect("could not parse program");

        if let SubCommand::Ast { .. } = self {
            println!("{:#?}", ast);
            return Ok(None);
        }

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
            _ => unreachable!(),
        }

        Ok(Some(buffer))
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
        if let Some(buffer) = buffer {
            std::fs::write(output, buffer).expect("could not write to file");
        }
    }
}
