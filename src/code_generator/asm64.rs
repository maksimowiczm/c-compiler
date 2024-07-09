use crate::code_generator::CodeGenerator;
use crate::parser::{Expression, Function, Program, Statement, UnaryOperator};
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

#[derive(Display, Clone)]
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
    #[display("neg {}", _0)]
    Neg(String),
    #[display("cmp {}, {}", _0, _1)]
    Cmp(String, String),
    #[display("sete {}", _0)]
    Sete(String),
}

enum FunctionContext {
    Main,
}

#[derive(Display, Clone, Copy)]
enum Register64 {
    #[display("%rax")]
    Rax,
    #[display("%rdi")]
    Rdi,
}

#[derive(Display, Clone, Copy)]
enum Register8 {
    #[display("%al")]
    Al,
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
        Statement::Return { expression } => {
            let expression = generate_expression(expression);
            match context {
                // It is stupid that it moves RAX to RDI, but it is what it is :)
                FunctionContext::Main => expression
                    .iter()
                    .chain(&[
                        Instruction::Mov(Register64::Rax.to_string(), Register64::Rdi.to_string()),
                        Instruction::Mov("$60".to_string(), Register64::Rax.to_string()),
                        Instruction::Syscall,
                    ])
                    .cloned()
                    .collect(),
            }
        }
    }
}

/// Generates expression instructions and places the result in RAX
fn generate_expression(expression: Expression) -> Vec<Instruction> {
    match expression {
        Expression::Integer(value) => vec![Instruction::Mov(
            format!("${}", value.to_string()),
            Register64::Rax.to_string(),
        )],
        Expression::UnaryOperation { operator, operand } => {
            let operand = generate_expression(*operand);
            let operator = generate_unary_operator(operator);
            operand.iter().chain(&operator).cloned().collect()
        }
    }
}

/// Generates unary operator instructions and places the result in RAX
fn generate_unary_operator(operator: UnaryOperator) -> Vec<Instruction> {
    match operator {
        UnaryOperator::Negation | UnaryOperator::BitwiseNot => {
            vec![Instruction::Neg(Register64::Rax.to_string())]
        }
        UnaryOperator::LogicalNot => {
            vec![
                Instruction::Cmp("$0".to_string(), Register64::Rax.to_string()),
                Instruction::Mov("$0".to_string(), Register64::Rax.to_string()),
                Instruction::Sete(Register8::Al.to_string()),
            ]
        }
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
            Instruction::Mov("$10".to_string(), "%rax".to_string()),
            Instruction::Mov("%rax".to_string(), "%rdi".to_string()),
            Instruction::Mov("$60".to_string(), "%rax".to_string()),
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
    #[case::negation(
        Expression::UnaryOperation {
            operator: UnaryOperator::Negation,
            operand: Box::new(Expression::Integer(10))
        },
        vec![
            Instruction::Mov("$10".to_string(), "%rax".to_string()),
            Instruction::Neg("%rax".to_string())
        ]
    )]
    #[case::bitwise_not(
        Expression::UnaryOperation {
            operator: UnaryOperator::BitwiseNot,
            operand: Box::new(Expression::Integer(10))
        },
        vec![
            Instruction::Mov("$10".to_string(), "%rax".to_string()),
            Instruction::Neg("%rax".to_string())
        ]
    )]
    #[case::logical_not(
        Expression::UnaryOperation {
            operator: UnaryOperator::LogicalNot,
            operand: Box::new(Expression::Integer(10))
        },
        vec![
            Instruction::Mov("$10".to_string(), "%rax".to_string()),
            Instruction::Cmp("$0".to_string(), "%rax".to_string()),
            Instruction::Mov("$0".to_string(), "%rax".to_string()),
            Instruction::Sete("%al".to_string())
        ]
    )]
    #[case::double_logical_not(
        Expression::UnaryOperation {
            operator: UnaryOperator::LogicalNot,
            operand: Box::new(Expression::UnaryOperation {
                operator: UnaryOperator::LogicalNot,
                operand: Box::new(Expression::Integer(10))
            })
        },
        vec![
            Instruction::Mov("$10".to_string(), "%rax".to_string()),
            Instruction::Cmp("$0".to_string(), "%rax".to_string()),
            Instruction::Mov("$0".to_string(), "%rax".to_string()),
            Instruction::Sete("%al".to_string()),
            Instruction::Cmp("$0".to_string(), "%rax".to_string()),
            Instruction::Mov("$0".to_string(), "%rax".to_string()),
            Instruction::Sete("%al".to_string())
        ]
    )]
    fn test_expression_with_unary_operator(
        #[case] operator: Expression,
        #[case] expected: Vec<Instruction>,
    ) {
        assert_eq!(generate_expression(operator), expected);
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
            Instruction::Mov("$10".to_string(), "%rax".to_string()),
            Instruction::Mov("%rax".to_string(), "%rdi".to_string()),
            Instruction::Mov("$60".to_string(), "%rax".to_string()),
            Instruction::Syscall,
        ]
    )]
    fn test_generate_function(#[case] function: Function, #[case] expected: Vec<Instruction>) {
        assert_eq!(generate_function(function), expected);
    }
}
