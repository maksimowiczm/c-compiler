use crate::code_generator::CodeGenerator;
use crate::parser::{
    Expression, Function, LogicalOperator, MathOperator, Program, RelationalOperator, Statement,
    UnaryOperator,
};
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
                let line = match instruction {
                    Instruction::Label(_) | Instruction::Globl(_) => format!("{}", instruction),
                    _ => format!("\t{}", instruction),
                };
                writeln!(buffer, "{}", line).map_err(Asm64CodeGenerationError::BufferError)?;
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
    /// mov src, dest
    #[display("mov {}, {}", _0, _1)]
    Mov(String, String),
    #[display("syscall")]
    Syscall,
    #[display(".globl {}", _0)]
    Globl(String),
    #[display("neg {}", _0)]
    Neg(String),
    #[display("not {}", _0)]
    Not(String),
    #[display("cmp {}, {}", _0, _1)]
    Cmp(String, String),
    #[display("sete {}", _0)]
    Sete(String),
    #[display("setne {}", _0)]
    Setne(String),
    #[display("setl {}", _0)]
    Setl(String),
    #[display("setle {}", _0)]
    Setle(String),
    #[display("setg {}", _0)]
    Setg(String),
    #[display("setge {}", _0)]
    Setge(String),
    #[display("push {}", _0)]
    Push(String),
    #[display("pop {}", _0)]
    Pop(String),
    #[display("add {}, {}", _0, _1)]
    Add(String, String),
    #[display("mul {}", _0)]
    Mul(String),
    #[display("div {}", _0)]
    Div(String),
    #[display("je {}", _0)]
    Je(String),
    #[display("jmp {}", _0)]
    Jmp(String),
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
    #[display("%rdx")]
    Rdx,
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
        Expression::MathOperation {
            operator,
            left,
            right,
        } => generate_math_operator(operator, *left, *right),
        Expression::LogicalOperation {
            operator,
            left,
            right,
        } => generate_logical_operator(operator, *left, *right),
        Expression::RelationalOperation {
            operator,
            left,
            right,
        } => generate_relational_operator(operator, *left, *right),
    }
}

fn generate_relational_operator(
    operator: RelationalOperator,
    left: Expression,
    right: Expression,
) -> Vec<Instruction> {
    let mut instructions = vec![];
    // Generate left expression instructions and push the result to the stack
    instructions.extend(generate_expression(left));
    instructions.push(Instruction::Push(Register64::Rax.to_string()));
    // Generate right expression instructions and store the result in RAX
    instructions.extend(generate_expression(right));
    instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
    // Compare the two values
    instructions.push(Instruction::Cmp(
        Register64::Rax.to_string(),
        Register64::Rdi.to_string(),
    ));
    // Zero out RAX
    instructions.push(Instruction::Mov(
        "$0".to_string(),
        Register64::Rax.to_string(),
    ));

    // Set AL as result of the comparison
    match operator {
        RelationalOperator::Equal => {
            instructions.push(Instruction::Sete(Register8::Al.to_string()));
        }
        RelationalOperator::NotEqual => {
            instructions.push(Instruction::Setne(Register8::Al.to_string()));
        }
        RelationalOperator::LessThan => {
            instructions.push(Instruction::Setl(Register8::Al.to_string()));
        }
        RelationalOperator::LessThanOrEqual => {
            instructions.push(Instruction::Setle(Register8::Al.to_string()));
        }
        RelationalOperator::GreaterThan => {
            instructions.push(Instruction::Setg(Register8::Al.to_string()));
        }
        RelationalOperator::GreaterThanOrEqual => {
            instructions.push(Instruction::Setge(Register8::Al.to_string()));
        }
    }

    instructions
}

static mut LABEL_COUNTER: u64 = 0;

fn generate_logical_operator(
    operator: LogicalOperator,
    left: Expression,
    right: Expression,
) -> Vec<Instruction> {
    let mut instructions = vec![];
    // Generate left expression instructions and store it in RAX
    instructions.extend(generate_expression(left));

    match operator {
        LogicalOperator::And => todo!(),
        LogicalOperator::Or => {
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            let label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let exit_label = format!(".E{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            instructions.push(Instruction::Je(label.clone()));
            instructions.push(Instruction::Mov(
                "$1".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Jmp(exit_label.clone()));
            instructions.push(Instruction::Label(label));
            instructions.extend(generate_expression(right));
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Mov(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Setne(Register8::Al.to_string()));
            instructions.push(Instruction::Label(exit_label));
        }
    }

    instructions
}

fn generate_math_operator(
    operator: MathOperator,
    left: Expression,
    right: Expression,
) -> Vec<Instruction> {
    let mut instructions = vec![];
    let left = generate_expression(left);
    instructions.extend(left);
    instructions.push(Instruction::Push(Register64::Rax.to_string()));
    let right = generate_expression(right);
    instructions.extend(right);

    match operator {
        MathOperator::Addition => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::Add(
                Register64::Rdi.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        MathOperator::Multiplication => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::Mul(Register64::Rdi.to_string()))
        }
        MathOperator::Division => {
            instructions.push(Instruction::Mov(
                Register64::Rax.to_string(),
                Register64::Rdi.to_string(),
            ));
            instructions.push(Instruction::Pop(Register64::Rax.to_string()));
            instructions.push(Instruction::Mov(
                "$0".to_string(),
                Register64::Rdx.to_string(),
            ));
            instructions.push(Instruction::Div(Register64::Rdi.to_string()));
        }
    }

    instructions
}

/// Generates unary operator instructions and places the result in RAX
fn generate_unary_operator(operator: UnaryOperator) -> Vec<Instruction> {
    match operator {
        UnaryOperator::BitwiseNot => {
            vec![Instruction::Not(Register8::Al.to_string())]
        }
        UnaryOperator::Negation => {
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
