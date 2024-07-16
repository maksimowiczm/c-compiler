use crate::code_generator::CodeGenerator;
use derive_more::{Display, Error};
use parser::parser_old::{
    Block, Declaration, Expression, Function, LogicalOperator, Operator, Program,
    RelationalOperator, Statement, UnaryOperator,
};
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
    ContinueNotInLoop,
    BreakNotInLoop,
    TooManyArguments,
}

impl CodeGenerator for StringyAssembly64CodeGenerator {
    type Err = Asm64CodeGenerationError;
    fn generate(
        self,
        node: Program,
        buffer: &mut impl Write,
    ) -> Result<(), Asm64CodeGenerationError> {
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
    // #[display("syscall")]
    // Syscall,
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
    #[display("or {}, {}", _0, _1)]
    Or(String, String),
    #[display("and {}, {}", _0, _1)]
    And(String, String),
    #[display("xor {}, {}", _0, _1)]
    Xor(String, String),
    #[display("shl {}, {}", _0, _1)]
    Shl(String, String),
    #[display("shr {}, {}", _0, _1)]
    Shr(String, String),
    #[display("// {}", _0)]
    Comment(String),
    #[display("ret")]
    Ret,
    #[display("call {}", _0)]
    Call(String),
    #[display("sub {}, {}", _0, _1)]
    Sub(String, String),
}

#[derive(Default)]
struct Context {
    /// variable name, (offset, is_current_scope)
    variables: HashMap<String, (i64, bool)>,
    stack_index: u32,
    loop_context: Option<LoopContext>,
}

#[derive(Clone)]
struct LoopContext {
    pre_post_expression: String,
    end_label: String,
}

impl Context {
    fn epilogue(&self) -> Vec<Instruction> {
        vec![
            Instruction::Mov(Register64::Rbp.to_string(), Register64::Rsp.to_string()),
            Instruction::Pop(Register64::Rbp.to_string()),
        ]
    }

    fn inner_scope(&mut self) -> Self {
        Context {
            variables: self
                .variables
                .iter()
                .map(|(k, v)| (k.clone(), (v.0, false)))
                .collect(),
            stack_index: self.stack_index,
            loop_context: self.loop_context.clone(),
        }
    }

    fn loop_scope(&mut self, pre_post_expression: String, end_label: String) -> Self {
        Context {
            variables: self
                .variables
                .iter()
                .map(|(k, v)| (k.clone(), (v.0, false)))
                .collect(),
            stack_index: self.stack_index,
            loop_context: Some(LoopContext {
                pre_post_expression,
                end_label,
            }),
        }
    }

    fn insert_variable(&mut self, name: String) -> Result<(), Asm64CodeGenerationError> {
        self.stack_index += 8;

        match self.variables.get_mut(&name) {
            Some((stack, scope)) => {
                if *scope {
                    return Err(Asm64CodeGenerationError::VariableAlreadyDeclared {
                        variable: name,
                    });
                }
                *stack = self.stack_index as i64 * -1;
                *scope = true;
            }
            None => {
                self.variables
                    .insert(name, (self.stack_index as i64 * -1, true));
            }
        }

        Ok(())
    }

    fn stack_size(&self) -> u32 {
        self.stack_index
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
    #[display("%rcx")]
    Rcx,
    #[display("%rsi")]
    Rsi,
    #[display("%r8")]
    R8,
    #[display("%r9")]
    R9,
}

#[derive(Display, Clone, Copy)]
enum Register8 {
    #[display("%al")]
    Al,
    #[display("%cl")]
    Cl,
}

fn generate_function(function: Function) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let Function {
        body,
        name,
        arguments,
        declaration,
    } = function;

    if declaration {
        return Ok(vec![]);
    }

    let mut context = Context::default();
    let mut instructions = vec![
        Instruction::Globl(name.clone()),
        Instruction::Label(name.clone()),
    ];

    // insert prologue
    instructions.push(Instruction::Push(Register64::Rbp.to_string()));
    instructions.push(Instruction::Mov(
        Register64::Rsp.to_string(),
        Register64::Rbp.to_string(),
    ));

    if arguments.len() > 6 {
        return Err(Asm64CodeGenerationError::TooManyArguments);
    }

    instructions.push(Instruction::Comment(
        "allocate space for arguments".to_string(),
    ));
    instructions.push(Instruction::Sub(
        format!("${}", arguments.len() as i64 * 8),
        Register64::Rsp.to_string(),
    ));

    let registers = [
        Register64::Rdi,
        Register64::Rsi,
        Register64::Rdx,
        Register64::Rcx,
        Register64::R8,
        Register64::R9,
    ];

    let iter = arguments.into_iter();

    iter.take(6)
        .zip(registers.iter())
        .for_each(|(argument, register)| {
            context.insert_variable(argument.clone()).unwrap();
            let (bias, _) = context.variables.get(&argument).unwrap();

            instructions.push(Instruction::Mov(
                register.to_string(),
                format!("{}(%rbp)", bias),
            ));
        });

    for block in body {
        instructions.extend(generate_block(block, &mut context)?);
    }

    if name == "main" {
        // brute force return 0 for main, c standard, I will fix it later :) clueless
        instructions.push(Instruction::Comment("auto generated exit".to_string()));
        instructions.extend(context.epilogue());
        instructions.push(Instruction::Mov(
            "$0".to_string(),
            Register64::Rax.to_string(),
        ));
        instructions.push(Instruction::Ret);
    }

    Ok(instructions)
}

fn generate_block(
    block: Block,
    context: &mut Context,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    match block {
        Block::Statement(statement) => generate_statement(statement, context),
        Block::Declaration(declaration) => generate_declaration(declaration, context),
    }
}

fn generate_declaration(
    declaration: Declaration,
    context: &mut Context,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let Declaration {
        variable,
        expression,
    } = declaration;
    let mut instructions = vec![];
    if let Some(expression) = expression {
        instructions.extend(generate_expression(expression, context)?);
    }
    instructions.push(Instruction::Push(Register64::Rax.to_string()));
    context.insert_variable(variable)?;
    Ok(instructions)
}

fn generate_statement(
    statement: Statement,
    context: &mut Context,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    match statement {
        Statement::Return { expression } => {
            let expression = generate_expression(expression, context)?;
            // It is stupid that it moves RAX to RDI, but it is what it is :)
            Ok(expression
                .iter()
                .chain(context.epilogue().iter())
                .chain(&[Instruction::Ret])
                .cloned()
                .collect())
        }
        Statement::Expression(expression) => generate_expression(expression, context),
        Statement::Conditional {
            condition,
            then,
            otherwise,
        } => {
            let mut instructions = vec![];
            instructions.extend(generate_expression(condition, context)?);
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            let end_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let else_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            if let Some(_) = otherwise {
                instructions.push(Instruction::Je(else_label.clone()));
            } else {
                instructions.push(Instruction::Je(end_label.clone()));
            }

            instructions.extend(generate_statement(*then, context)?);

            if let Some(otherwise) = otherwise {
                instructions.push(Instruction::Jmp(end_label.clone()));
                instructions.push(Instruction::Label(else_label));
                instructions.extend(generate_statement(*otherwise, context)?);
            }

            instructions.push(Instruction::Label(end_label));
            Ok(instructions)
        }
        Statement::Compound(blocks) => {
            let mut inner_context = context.inner_scope();
            let mut instructions = blocks
                .into_iter()
                .map(|block| generate_block(block, &mut inner_context))
                .collect::<Result<Vec<Vec<Instruction>>, Asm64CodeGenerationError>>()?
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();
            let to_deallocate = inner_context.stack_size() - context.stack_size();
            if to_deallocate > 0 {
                instructions.push(Instruction::Add(
                    format!("${}", to_deallocate),
                    Register64::Rsp.to_string(),
                ));
            };

            Ok(instructions)
        }
        Statement::NullExpression => Ok(vec![]),
        Statement::For {
            initializer,
            condition,
            post_expression,
            body,
        } => {
            let mut instructions = vec![];
            if let Some(initializer) = initializer {
                instructions.extend(generate_expression(initializer, context)?);
            }
            let start_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let pre_post = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let end_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            instructions.push(Instruction::Label(start_label.clone()));
            instructions.extend(generate_expression(condition, context)?);
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Je(end_label.clone()));
            let mut inner_context = context.loop_scope(pre_post.clone(), end_label.clone());
            instructions.extend(generate_statement(*body, &mut inner_context)?);
            instructions.push(Instruction::Label(pre_post.clone()));
            if let Some(post_expression) = post_expression {
                instructions.extend(generate_expression(post_expression, &context)?);
            }
            instructions.push(Instruction::Jmp(start_label.clone()));
            instructions.push(Instruction::Label(end_label));

            Ok(instructions)
        }
        Statement::ForDeclaration {
            declaration,
            condition,
            post_expression,
            body,
        } => {
            let mut instructions = vec![];
            let start_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let pre_post = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let end_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let mut header_context = context.loop_scope(pre_post.clone(), end_label.clone());
            instructions.extend(generate_declaration(declaration, &mut header_context)?);
            instructions.push(Instruction::Label(start_label.clone()));
            instructions.extend(generate_expression(condition, &header_context)?);
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Je(end_label.clone()));
            let mut inner_context = header_context.inner_scope();
            instructions.extend(generate_statement(*body, &mut inner_context)?);
            instructions.push(Instruction::Label(pre_post.clone()));
            if let Some(post_expression) = post_expression {
                instructions.extend(generate_expression(post_expression, &header_context)?);
            }
            instructions.push(Instruction::Jmp(start_label.clone()));
            instructions.push(Instruction::Label(end_label));

            Ok(instructions)
        }
        Statement::While { condition, body } => {
            let mut instructions = vec![];
            let start_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let end_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            instructions.push(Instruction::Label(start_label.clone()));
            instructions.extend(generate_expression(condition, context)?);
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Je(end_label.clone()));
            let mut inner_context = context.loop_scope(start_label.clone(), end_label.clone());
            instructions.extend(generate_statement(*body, &mut inner_context)?);
            instructions.push(Instruction::Jmp(start_label.clone()));
            instructions.push(Instruction::Label(end_label));
            Ok(instructions)
        }
        Statement::Do { body, condition } => {
            let mut instructions = vec![];
            let start_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });

            let end_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            instructions.push(Instruction::Label(start_label.clone()));
            let mut inner_context = context.loop_scope(start_label.clone(), end_label.clone());
            instructions.extend(generate_statement(*body, &mut inner_context)?);
            instructions.extend(generate_expression(condition, context)?);
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            instructions.push(Instruction::Je(end_label.clone()));
            instructions.push(Instruction::Jmp(start_label.clone()));
            instructions.push(Instruction::Label(end_label));
            Ok(instructions)
        }
        Statement::Continue => {
            if let Some(LoopContext {
                pre_post_expression,
                ..
            }) = &context.loop_context
            {
                Ok(vec![Instruction::Jmp(pre_post_expression.clone())])
            } else {
                Err(Asm64CodeGenerationError::ContinueNotInLoop)
            }
        }
        Statement::Break => {
            if let Some(LoopContext { end_label, .. }) = &context.loop_context {
                Ok(vec![Instruction::Jmp(end_label.clone())])
            } else {
                Err(Asm64CodeGenerationError::BreakNotInLoop)
            }
        }
    }
}

/// Generates expression instructions and places the result in RAX
fn generate_expression(
    expression: Expression,
    context: &Context,
) -> Result<Vec<Instruction>, Asm64CodeGenerationError> {
    let result = match expression {
        Expression::Integer(value) => Ok(vec![Instruction::Mov(
            format!("${}", value.to_string()),
            Register64::Rax.to_string(),
        )]),
        Expression::UnaryOperation { operator, operand } => {
            let operand = generate_expression(*operand, context)?;
            let operator = generate_unary_operator(operator);
            Ok(operand.iter().chain(&operator).cloned().collect())
        }
        Expression::Operation {
            operator,
            left,
            right,
        } => generate_math_operator(operator, *left, *right, context),
        Expression::LogicalOperation {
            operator,
            left,
            right,
        } => generate_logical_operator(operator, *left, *right, context),
        Expression::RelationalOperation {
            operator,
            left,
            right,
        } => generate_relational_operator(operator, *left, *right, context),
        Expression::Variable(name) => {
            let offset = context
                .variables
                .get(&name)
                .ok_or(Asm64CodeGenerationError::VariableNotDeclared { variable: name })?;
            Ok(vec![Instruction::Mov(
                format!("{}(%rbp)", offset.0),
                Register64::Rax.to_string(),
            )])
        }
        Expression::Assignment {
            variable,
            expression,
        } => {
            let mut instructions = vec![];
            instructions.extend(generate_expression(*expression, context)?);
            instructions.push(Instruction::Mov(
                Register64::Rax.to_string(),
                format!(
                    "-{}(%rbp)",
                    context
                        .variables
                        .get(&variable)
                        .ok_or(Asm64CodeGenerationError::VariableNotDeclared { variable })?
                        .0
                ),
            ));
            Ok(instructions)
        }
        Expression::Ternary {
            condition,
            then,
            otherwise,
        } => {
            let mut instructions = generate_expression(*condition, context)?;
            instructions.push(Instruction::Cmp(
                "$0".to_string(),
                Register64::Rax.to_string(),
            ));
            let else_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            let end_label = format!(".L{}", unsafe {
                LABEL_COUNTER += 1;
                LABEL_COUNTER
            });
            instructions.push(Instruction::Je(else_label.clone()));
            instructions.extend(generate_expression(*then, context)?);
            instructions.push(Instruction::Jmp(end_label.clone()));
            instructions.push(Instruction::Label(else_label));
            instructions.extend(generate_expression(*otherwise, context)?);
            instructions.push(Instruction::Label(end_label));
            Ok(instructions)
        }
        Expression::Call {
            function,
            arguments,
        } => {
            let mut instructions = vec![];
            if arguments.len() > 6 {
                return Err(Asm64CodeGenerationError::TooManyArguments);
            }

            let registers = [
                Register64::Rdi,
                Register64::Rsi,
                Register64::Rdx,
                Register64::Rcx,
                Register64::R8,
                Register64::R9,
            ];

            let iter = arguments.into_iter();

            iter.take(6)
                .zip(registers.iter())
                .for_each(|(argument, register)| {
                    instructions.extend(generate_expression(argument, context).unwrap());
                    instructions.push(Instruction::Mov(
                        Register64::Rax.to_string(),
                        register.to_string(),
                    ));
                });

            instructions.push(Instruction::Call(function));
            Ok(instructions)
        }
    };

    result
}

fn generate_relational_operator(
    operator: RelationalOperator,
    left: Expression,
    right: Expression,
    variables: &Context,
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
    variables: &Context,
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
    variables: &Context,
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
        Operator::Modulo => {
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
            instructions.push(Instruction::Mov(
                Register64::Rdx.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        Operator::BitwiseOr => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::Or(
                Register64::Rdi.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        Operator::BitwiseAnd => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::And(
                Register64::Rdi.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        Operator::BitwiseXor => {
            instructions.push(Instruction::Pop(Register64::Rdi.to_string()));
            instructions.push(Instruction::Xor(
                Register64::Rdi.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        Operator::ShiftLeft => {
            instructions.push(Instruction::Mov(
                Register64::Rax.to_string(),
                Register64::Rcx.to_string(),
            ));
            instructions.push(Instruction::Pop(Register64::Rax.to_string()));
            instructions.push(Instruction::Shl(
                Register8::Cl.to_string(),
                Register64::Rax.to_string(),
            ));
        }
        Operator::ShiftRight => {
            instructions.push(Instruction::Mov(
                Register64::Rax.to_string(),
                Register64::Rcx.to_string(),
            ));
            instructions.push(Instruction::Pop(Register64::Rax.to_string()));
            instructions.push(Instruction::Shr(
                Register8::Cl.to_string(),
                Register64::Rax.to_string(),
            ));
        }
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
    }
}
