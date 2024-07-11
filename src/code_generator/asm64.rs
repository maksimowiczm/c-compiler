use crate::code_generator::CodeGenerator;
use crate::parser::{
    Expression, Function, LogicalOperator, Operator, Program, RelationalOperator, Statement,
    UnaryOperator,
};
use derive_more::{Display, Error};
use std::collections::HashMap;
use std::io::Write;

pub struct StringyAssembly64CodeGenerator;

#[derive(Error, Debug, Display)]
pub enum Asm64CodeGenerationError {
    BufferError(std::io::Error),
    #[display("Variable {} already declared", variable)]
    VariableAlreadyDeclared {
        variable: String,
    },
    #[display("Variable {} not declared", variable)]
    VariableNotDeclared {
        variable: String,
    },
}

impl<B> CodeGenerator<B, Asm64CodeGenerationError> for StringyAssembly64CodeGenerator
where
    B: Write,
{
    fn generate(self, node: Program, buffer: &mut B) -> Result<(), Asm64CodeGenerationError> {
        for function in node.functions {
            let instructions = generate_function(function)?;
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
    #[display("jne {}", _0)]
    Jne(String),
    #[display("jmp {}", _0)]
    Jmp(String),
}

#[derive(Default)]
struct Context {
    variables: HashMap<String, u64>,
    stack_index: u64,
}

impl Context {
    fn epilogue(&self) -> Vec<Instruction> {
        vec![
            Instruction::Mov(Register64::Rbp.to_string(), Register64::Rsp.to_string()),
            Instruction::Pop(Register64::Rbp.to_string()),
        ]
    }
}

#[derive(Display, Clone, Copy)]
enum Register64 {
    #[display("%rax")]
    Rax,
    #[display("%rdi")]
    Rdi,
    #[display("%rdx")]
    Rdx,
    #[display("%rbp")]
    Rbp,
    #[display("%rsp")]
    Rsp,
}

#[derive(Display, Clone, Copy)]
enum Register8 {
    #[display("%al")]
    Al,
}

fn generate_function(function: Function) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let Function { body, .. } = function;
    let mut context = Default::default();
    let mut instructions = vec![
        Instruction::Globl("_start".to_string()),
        Instruction::Label("_start".to_string()),
    ];
    // insert prologue
    instructions.push(Instruction::Push(Register64::Rbp.to_string()));
    instructions.push(Instruction::Mov(
        Register64::Rsp.to_string(),
        Register64::Rbp.to_string(),
    ));
    instructions.extend(generate_statement(body, &mut context)?);

    // brute force return 0 for main, c standard, I will fix it later :) clueless
    instructions.extend(context.epilogue());
    instructions.push(Instruction::Mov(
        "$0".to_string(),
        Register64::Rdi.to_string(),
    ));
    instructions.push(Instruction::Mov(
        "$60".to_string(),
        Register64::Rax.to_string(),
    ));
    instructions.push(Instruction::Syscall);
    Ok(instructions)
}

fn generate_statement(
    statement: Statement,
    context: &mut Context,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    match statement {
        Statement::Return { expression } => {
            let expression = generate_expression(expression, &context.variables)?;
            // It is stupid that it moves RAX to RDI, but it is what it is :)
            Ok(expression
                .iter()
                .chain(context.epilogue().iter())
                .chain(&[
                    Instruction::Mov(Register64::Rax.to_string(), Register64::Rdi.to_string()),
                    Instruction::Mov("$60".to_string(), Register64::Rax.to_string()),
                    Instruction::Syscall,
                ])
                .cloned()
                .collect())
        }
        Statement::Declaration {
            variable,
            expression,
        } => {
            if context.variables.contains_key(&variable) {
                return Err(Asm64CodeGenerationError::VariableAlreadyDeclared { variable });
            }
            let mut instructions = vec![];
            if let Some(expression) = expression {
                instructions.extend(generate_expression(expression, &context.variables)?);
            }
            instructions.push(Instruction::Push(Register64::Rax.to_string()));
            context.stack_index += 8;
            context.variables.insert(variable, context.stack_index);
            Ok(instructions)
        }
        Statement::StatementList(statements) => Ok(statements
            .into_iter()
            .map(|statement| generate_statement(statement, context))
            .collect::<Result<Vec<Vec<Instruction>>, Asm64CodeGenerationError>>()?
            .into_iter()
            .flatten()
            .collect()),
        Statement::Expression(expression) => generate_expression(expression, &context.variables),
    }
}

/// Generates expression instructions and places the result in RAX
fn generate_expression(
    expression: Expression,
    variables: &HashMap<String, u64>,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let result = match expression {
        Expression::Integer(value) => Ok(vec![Instruction::Mov(
            format!("${}", value.to_string()),
            Register64::Rax.to_string(),
        )]),
        Expression::UnaryOperation { operator, operand } => {
            let operand = generate_expression(*operand, variables)?;
            let operator = generate_unary_operator(operator);
            Ok(operand.iter().chain(&operator).cloned().collect())
        }
        Expression::Operation {
            operator,
            left,
            right,
        } => generate_math_operator(operator, *left, *right, variables),
        Expression::LogicalOperation {
            operator,
            left,
            right,
        } => generate_logical_operator(operator, *left, *right, variables),
        Expression::RelationalOperation {
            operator,
            left,
            right,
        } => generate_relational_operator(operator, *left, *right, variables),
        Expression::Variable(name) => {
            let offset = variables
                .get(&name)
                .ok_or(Asm64CodeGenerationError::VariableNotDeclared { variable: name })?;
            Ok(vec![Instruction::Mov(
                format!("-{}(%rbp)", offset),
                Register64::Rax.to_string(),
            )])
        }
        Expression::Assignment {
            variable,
            expression,
        } => {
            let mut instructions = vec![];
            instructions.extend(generate_expression(*expression, variables)?);
            instructions.push(Instruction::Mov(
                Register64::Rax.to_string(),
                format!(
                    "-{}(%rbp)",
                    variables
                        .get(&variable)
                        .ok_or(Asm64CodeGenerationError::VariableNotDeclared { variable })?
                ),
            ));
            Ok(instructions)
        }
    };

    result
}

fn generate_relational_operator(
    operator: RelationalOperator,
    left: Expression,
    right: Expression,
    variables: &HashMap<String, u64>,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let mut instructions = vec![];
    // Generate left expression instructions and push the result to the stack
    instructions.extend(generate_expression(left, variables)?);
    instructions.push(Instruction::Push(Register64::Rax.to_string()));
    // Generate right expression instructions and store the result in RAX
    instructions.extend(generate_expression(right, variables)?);
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

    Ok(instructions)
}

static mut LABEL_COUNTER: u64 = 0;

fn generate_logical_operator(
    operator: LogicalOperator,
    left: Expression,
    right: Expression,
    variables: &HashMap<String, u64>,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let mut instructions = vec![];
    // Generate left expression instructions and store it in RAX
    instructions.extend(generate_expression(left, variables)?);

    let label = format!(".L{}", unsafe {
        LABEL_COUNTER += 1;
        LABEL_COUNTER
    });
    instructions.push(Instruction::Cmp(
        "$0".to_string(),
        Register64::Rax.to_string(),
    ));

    match operator {
        LogicalOperator::And => {
            instructions.push(Instruction::Jne(label.clone()));
        }
        LogicalOperator::Or => {
            instructions.push(Instruction::Je(label.clone()));
            instructions.push(Instruction::Mov(
                "$1".to_string(),
                Register64::Rax.to_string(),
            ));
        }
    }

    let exit_label = format!(".E{}", unsafe {
        LABEL_COUNTER += 1;
        LABEL_COUNTER
    });
    instructions.push(Instruction::Jmp(exit_label.clone()));
    instructions.push(Instruction::Label(label));
    instructions.extend(generate_expression(right, variables)?);
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

    Ok(instructions)
}

fn generate_math_operator(
    operator: Operator,
    left: Expression,
    right: Expression,
    variables: &HashMap<String, u64>,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let mut instructions = vec![];
    let left = generate_expression(left, variables)?;
    instructions.extend(left);
    instructions.push(Instruction::Push(Register64::Rax.to_string()));
    let right = generate_expression(right, variables)?;
    instructions.extend(right);

    match operator {
        Operator::Addition => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::Add(
                Register64::Rdi.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        Operator::Multiplication => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::Mul(Register64::Rdi.to_string()))
        }
        Operator::Division => {
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
        Operator::Modulo => todo!(),
        Operator::BitwiseOr => todo!(),
        Operator::BitwiseAnd => todo!(),
        Operator::BitwiseXor => todo!(),
        Operator::ShiftLeft => todo!(),
        Operator::ShiftRight => todo!(),
    }

    Ok(instructions)
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
        UnaryOperator::Increment => todo!(),
        UnaryOperator::Decrement => todo!(),
    }
}
