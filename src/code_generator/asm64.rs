use crate::code_generator::CodeGenerator;
use crate::parser::{Expression, Function, Program, Statement};
use derive_more::{Display, Error};
use std::io::Write;

pub struct StringyAssembly64CodeGenerator;

#[derive(Error, Debug, Display)]
pub enum Asm64CodeGenerationError {
    BufferError(std::io::Error),
}

impl<B> CodeGenerator<B, Asm64CodeGenerationError> for StringyAssembly64CodeGenerator
where
    B: Write,
{
    fn generate(self, node: Program, buffer: &mut B) -> Result<(), Asm64CodeGenerationError> {
        for function in node.functions {
            let instructions = generate_function(function);
            for instruction in instructions {
                writeln!(buffer, "{}", instruction)
                    .map_err(Asm64CodeGenerationError::BufferError)?;
            }
        }

        Ok(())
    }
}

#[derive(Display)]
#[cfg_attr(test, derive(Debug, PartialEq))]
/// x86-64 assembly instructions
/// AT&T syntax
enum Instruction {
    #[display("{}:", _0)]
    Label(String),
    #[display("mov {}, {}", _0, _1)]
    Mov(String, String),
    #[display("syscall")]
    Syscall,
    #[display(".globl {}", _0)]
    Globl(String),
}

enum FunctionContext {
    Main,
}

fn generate_function(function: Function) -> Vec<Instruction> {
    let Function { name, body } = function;
    let context = match name.as_str() {
        "main" => FunctionContext::Main,
        _ => todo!(),
    };
    let mut instructions = match context {
        FunctionContext::Main => {
            vec![
                Instruction::Globl("_start".to_string()),
                Instruction::Label("_start".to_string()),
            ]
        }
    };
    instructions.extend(generate_statement(body, context));
    instructions
}

fn generate_statement(statement: Statement, context: FunctionContext) -> Vec<Instruction> {
    match statement {
        Statement::Return { expression } => match expression {
            Expression::Integer(value) => match context {
                FunctionContext::Main => {
                    vec![
                        Instruction::Mov("$60".to_string(), "%rax".to_string()),
                        Instruction::Mov(format!("${}", value.to_string()), "%rdi".to_string()),
                        Instruction::Syscall,
                    ]
                }
            },
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::main_return_integer(
        Statement::Return {
            expression: Expression::Integer(10)
        },
        vec![
            Instruction::Mov("$60".to_string(), "%rax".to_string()),
            Instruction::Mov("$10".to_string(), "%rdi".to_string()),
            Instruction::Syscall,
        ]
    )]
    fn test_generate_statement(#[case] statement: Statement, #[case] expected: Vec<Instruction>) {
        assert_eq!(
            generate_statement(statement, FunctionContext::Main),
            expected
        );
    }

    #[rstest]
    #[case::main_function(
        Function {
            name: "main".to_string(),
            body: Statement::Return {
                expression: Expression::Integer(10)
            }
        },
        vec![
            Instruction::Globl("_start".to_string()),
            Instruction::Label("_start".to_string()),
            Instruction::Mov("$60".to_string(), "%rax".to_string()),
            Instruction::Mov("$10".to_string(), "%rdi".to_string()),
            Instruction::Syscall,
        ]
    )]
    fn test_generate_function(#[case] function: Function, #[case] expected: Vec<Instruction>) {
        assert_eq!(generate_function(function), expected);
    }
}
