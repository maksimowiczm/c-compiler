use crate::code_generator::CodeGenerator;
use crate::parser::Parser;

mod lexer;
mod parser;

mod code_generator;

fn main() {
    let path = std::env::args().nth(1).expect("no file path provided");
    let source = std::fs::read_to_string(&path).expect("could not read file");
    let lexer = lexer::Lexer::new(source.chars().peekable());
    let tokens = lexer.into_iter().peekable();
    let ast = Parser::parse(tokens).expect("could not parse program");
    let code_generator = code_generator::asm64::StringyAssembly64CodeGenerator;
    let mut buffer = Vec::new();
    code_generator
        .generate(ast, &mut buffer)
        .expect("could not generate code");
    let output_path = path.replace(".c", ".s");
    let str = std::str::from_utf8(&buffer).expect("could not convert to string");
    println!("Generated code written to {}", str);
    std::fs::write(output_path, buffer).expect("could not write to file");
}
