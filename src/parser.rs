use crate::lexer::{Assignment, Keyword, Token};
use derive_more::{Display, Error};
use std::iter::Peekable;

pub struct Parser;

#[derive(Error, Display, Debug)]
pub enum ParserError {
    UnexpectedEndOfInput,
    #[display(
        "Unexpected token: {:?}, expected: {:?}, near tokens: {:?}",
        unexpected,
        expected,
        near_tokens
    )]
    UnexpectedToken {
        unexpected: Token,
        expected: Vec<Token>,
        near_tokens: Vec<Token>,
    },
    #[display("Two definitions of function {:?}", name)]
    TwoDefinitionsOfFunction {
        name: String,
    },
    #[display("Two different definitions of function {:?}", name)]
    TwoDifferentDeclarationsOfFunction {
        name: String,
    },
    #[display("Function definition doesn't match declaration {:?}", name)]
    DefinitionDoesntMatchDeclaration {
        name: String,
    },
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Program {
    pub(crate) functions: Vec<Function>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Function {
    pub(crate) declaration: bool,
    pub(crate) name: String,
    pub(crate) body: Vec<Block>,
    pub(crate) arguments: Vec<String>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Block {
    Statement(Statement),
    Declaration(Declaration),
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Declaration {
    pub(crate) variable: String,
    pub(crate) expression: Option<Expression>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Statement {
    Return {
        expression: Expression,
    },
    Expression(Expression),
    NullExpression,
    Conditional {
        condition: Expression,
        then: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
    Compound(Vec<Block>),
    For {
        initializer: Option<Expression>,
        condition: Expression,
        post_expression: Option<Expression>,
        body: Box<Statement>,
    },
    ForDeclaration {
        declaration: Declaration,
        condition: Expression,
        post_expression: Option<Expression>,
        body: Box<Statement>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    Do {
        body: Box<Statement>,
        condition: Expression,
    },
    Continue,
    Break,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Expression {
    Integer(u64),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Operation {
        operator: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    LogicalOperation {
        operator: LogicalOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    RelationalOperation {
        operator: RelationalOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment {
        variable: String,
        expression: Box<Expression>,
    },
    Variable(String),
    Ternary {
        condition: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
    },
    Call {
        function: String,
        arguments: Vec<Expression>,
    },
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum UnaryOperator {
    Negation,
    BitwiseNot,
    LogicalNot,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Operator {
    Addition,
    Multiplication,
    Division,
    Modulo,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum LogicalOperator {
    And,
    Or,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum RelationalOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl Parser {
    pub fn parse(
        mut tokens: Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Program, ParserError> {
        let mut functions = vec![];

        while let Some(token) = tokens.peek() {
            if let Token::EndOfFile = token {
                break;
            }

            functions.push(Function::parse(&mut tokens)?);
        }

        let program = Program { functions };

        Self::validate_functions(&program)?;

        Ok(program)
    }

    fn validate_functions(program: &Program) -> Result<(), ParserError> {
        let declarations = program
            .functions
            .iter()
            .filter(|function| function.declaration)
            .collect::<Vec<_>>();

        let definitions = program
            .functions
            .iter()
            .filter(|function| !function.declaration)
            .collect::<Vec<_>>();

        for definition in &definitions {
            if definitions
                .iter()
                .filter(|f| f.name == definition.name)
                .count()
                > 1
            {
                return Err(ParserError::TwoDefinitionsOfFunction {
                    name: definition.name.clone(),
                });
            }
        }

        for declaration in &declarations {
            if declarations
                .iter()
                .filter(|f| f.name == declaration.name)
                .any(|f| f.arguments.len() != declaration.arguments.len())
            {
                return Err(ParserError::TwoDifferentDeclarationsOfFunction {
                    name: declaration.name.clone(),
                });
            }

            if definitions.iter().any(|f| {
                f.name == declaration.name && f.arguments.len() != declaration.arguments.len()
            }) {
                return Err(ParserError::DefinitionDoesntMatchDeclaration {
                    name: declaration.name.clone(),
                });
            }
        }

        Ok(())
    }
}

fn expect_token(
    tokens: &mut impl Iterator<Item = Token>,
    expected: Token,
) -> Result<(), ParserError> {
    let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

    if token == expected {
        Ok(())
    } else {
        Err(ParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![expected],
            near_tokens: tokens.take(6).collect(),
        })
    }
}

impl Function {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Function, ParserError> {
        expect_token(tokens, Token::Keyword(Keyword::Int))?;
        let name = match tokens.next() {
            Some(Token::Word(name)) => Ok(name),
            _ => Err(ParserError::UnexpectedToken {
                unexpected: Token::EndOfFile,
                expected: vec![Token::Word("<function_name>".to_string())],
                near_tokens: tokens.take(6).collect(),
            }),
        }?;

        expect_token(tokens, Token::OpenParenthesis)?;

        let mut arguments = vec![];
        while let Some(token) = tokens.next() {
            if let Token::CloseParenthesis = token {
                break;
            }
            if let Token::Keyword(Keyword::Int) = token {
                let next = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
                if let Token::Word(argument) = next {
                    arguments.push(argument);
                } else {
                    return Err(ParserError::UnexpectedToken {
                        unexpected: next,
                        expected: vec![Token::Word("<variable_name>".to_string())],
                        near_tokens: tokens.take(6).collect(),
                    });
                }
            } else {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token.clone(),
                    expected: vec![Token::Keyword(Keyword::Int)],
                    near_tokens: tokens.take(6).collect(),
                });
            }

            if let Some(Token::Comma) = tokens.peek() {
                tokens.next();
            }
        }

        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        if let Token::SemiColon = peek {
            tokens.next();
            return Ok(Function {
                declaration: true,
                name,
                body: vec![],
                arguments,
            });
        }

        expect_token(tokens, Token::OpenBrace)?;

        let mut blocks = vec![];
        while let Some(token) = tokens.peek() {
            if let Token::CloseBrace = token {
                break;
            }
            blocks.push(Block::parse(tokens)?);
        }
        expect_token(tokens, Token::CloseBrace)?;

        Ok(Function {
            declaration: false,
            name,
            body: blocks,
            arguments,
        })
    }
}

impl Block {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        match peek {
            Token::Keyword(Keyword::Int) => {
                let declaration = Declaration::parse(tokens)?;
                Ok(Block::Declaration(declaration))
            }
            _ => {
                let statement = Statement::parse(tokens)?;
                Ok(Block::Statement(statement))
            }
        }
    }
}

impl Statement {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        match peek {
            Token::Word(_)
            | Token::Integer(_)
            | Token::Increment
            | Token::Decrement
            | Token::Negation
            | Token::BitwiseNot
            | Token::LogicalNot => {
                let expression = Expression::parse(tokens)?;
                expect_token(tokens, Token::SemiColon)?;
                Ok(Statement::Expression(expression))
            }
            Token::OpenBrace => {
                expect_token(tokens, Token::OpenBrace)?;
                let mut blocks = vec![];
                while let Some(token) = tokens.peek() {
                    if let Token::CloseBrace = token {
                        break;
                    }
                    blocks.push(Block::parse(tokens)?);
                }
                expect_token(tokens, Token::CloseBrace)?;

                Ok(Statement::Compound(blocks))
            }
            Token::Keyword(Keyword::Return) => {
                expect_token(tokens, Token::Keyword(Keyword::Return))?;
                let expression = Expression::parse(tokens)?;
                expect_token(tokens, Token::SemiColon)?;

                Ok(Statement::Return { expression })
            }
            Token::Keyword(Keyword::If) => {
                expect_token(tokens, Token::Keyword(Keyword::If))?;
                expect_token(tokens, Token::OpenParenthesis)?;
                let condition = Expression::parse(tokens)?;
                expect_token(tokens, Token::CloseParenthesis)?;
                let then = Box::new(Statement::parse(tokens)?);
                let otherwise = if let Some(Token::Keyword(Keyword::Else)) = tokens.peek() {
                    tokens.next();
                    Some(Box::new(Statement::parse(tokens)?))
                } else {
                    None
                };

                Ok(Statement::Conditional {
                    condition,
                    then,
                    otherwise,
                })
            }
            Token::Keyword(Keyword::Continue) => {
                expect_token(tokens, Token::Keyword(Keyword::Continue))?;
                expect_token(tokens, Token::SemiColon)?;

                Ok(Statement::Continue)
            }
            Token::Keyword(Keyword::Break) => {
                expect_token(tokens, Token::Keyword(Keyword::Break))?;
                expect_token(tokens, Token::SemiColon)?;

                Ok(Statement::Break)
            }
            Token::SemiColon => {
                expect_token(tokens, Token::SemiColon)?;
                Ok(Statement::NullExpression)
            }
            Token::Keyword(Keyword::For) => parse_for(tokens),
            Token::Keyword(Keyword::While) => parse_while(tokens),
            Token::Keyword(Keyword::Do) => parse_do_while(tokens),
            _ => Err(ParserError::UnexpectedToken {
                unexpected: peek.clone(),
                expected: vec![Token::Word("<variable_name>".to_string())],
                near_tokens: tokens.take(6).collect(),
            }),
        }
    }
}

fn parse_for(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Statement, ParserError> {
    fn condition(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Expression, ParserError> {
        match tokens.peek() {
            Some(Token::SemiColon) => Ok(Expression::Integer(1)),
            _ => Expression::parse(tokens),
        }
    }
    fn post_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Option<Expression>, ParserError> {
        match tokens.peek() {
            Some(Token::CloseParenthesis) => Ok(None),
            _ => {
                let expression = Expression::parse(tokens)?;
                Ok(Some(expression))
            }
        }
    }

    expect_token(tokens, Token::Keyword(Keyword::For))?;
    expect_token(tokens, Token::OpenParenthesis)?;

    let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

    if let Token::Keyword(Keyword::Int) = peek {
        let declaration = Declaration::parse(tokens)?;
        let condition = condition(tokens)?;
        expect_token(tokens, Token::SemiColon)?;
        let post_expression = post_expression(tokens)?;
        expect_token(tokens, Token::CloseParenthesis)?;
        let body = Box::new(Statement::parse(tokens)?);
        Ok(Statement::ForDeclaration {
            declaration,
            condition,
            post_expression,
            body,
        })
    } else {
        let initializer = if let Token::SemiColon = peek {
            None
        } else {
            Some(Expression::parse(tokens)?)
        };

        expect_token(tokens, Token::SemiColon)?;
        let condition = condition(tokens)?;
        expect_token(tokens, Token::SemiColon)?;
        let post_expression = post_expression(tokens)?;
        expect_token(tokens, Token::CloseParenthesis)?;
        let body = Box::new(Statement::parse(tokens)?);
        Ok(Statement::For {
            initializer,
            condition,
            post_expression,
            body,
        })
    }
}

fn parse_while(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Statement, ParserError> {
    expect_token(tokens, Token::Keyword(Keyword::While))?;
    expect_token(tokens, Token::OpenParenthesis)?;
    let condition = Expression::parse(tokens)?;
    expect_token(tokens, Token::CloseParenthesis)?;
    let body = Box::new(Statement::parse(tokens)?);

    Ok(Statement::While { condition, body })
}

fn parse_do_while(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Statement, ParserError> {
    expect_token(tokens, Token::Keyword(Keyword::Do))?;
    let body = Box::new(Statement::parse(tokens)?);
    expect_token(tokens, Token::Keyword(Keyword::While))?;
    expect_token(tokens, Token::OpenParenthesis)?;
    let condition = Expression::parse(tokens)?;
    expect_token(tokens, Token::CloseParenthesis)?;
    expect_token(tokens, Token::SemiColon)?;

    Ok(Statement::Do { body, condition })
}

impl Declaration {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        expect_token(tokens, Token::Keyword(Keyword::Int))?;

        let variable = match tokens.next() {
            Some(Token::Word(variable)) => Ok(variable),
            Some(token) => Err(ParserError::UnexpectedToken {
                unexpected: token,
                expected: vec![Token::Word("<variable_name>".to_string())],
                near_tokens: tokens.take(6).collect(),
            }),
            _ => Err(ParserError::UnexpectedEndOfInput),
        }?;

        let expression = match tokens.peek() {
            Some(Token::Assignment(Assignment::Equal)) => {
                tokens.next();
                Some(Expression::parse(tokens)?)
            }
            _ => None,
        };

        expect_token(tokens, Token::SemiColon)?;

        Ok(Declaration {
            variable,
            expression,
        })
    }
}

impl Expression {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        match Expression::ternary(tokens)? {
            // <id> <assignment> <exp>
            Expression::Variable(id) => {
                if !matches!(tokens.peek(), Some(Token::Assignment(_))) {
                    return Ok(Expression::Variable(id));
                }
                if let Some(Token::Assignment(assignment)) = tokens.next() {
                    let right = Box::new(Expression::parse(tokens)?);
                    match assignment {
                        Assignment::Equal => Ok(Expression::Assignment {
                            variable: id,
                            expression: right,
                        }),
                        Assignment::PlusEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::Addition,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::MinusEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::Addition,
                                left: Box::new(Expression::Variable(id)),
                                right: Box::new(Expression::UnaryOperation {
                                    operator: UnaryOperator::Negation,
                                    operand: right,
                                }),
                            }),
                        }),
                        Assignment::MultiplyEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::Multiplication,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::DivideEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::Division,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::AndEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::BitwiseAnd,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::OrEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::BitwiseOr,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::ShiftLeftEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::ShiftLeft,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::ShiftRightEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::ShiftRight,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::ModuloEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::Modulo,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::XorEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::Operation {
                                operator: Operator::BitwiseXor,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                    }
                } else {
                    unreachable!()
                }
            }
            // logical_or
            expression => Ok(expression),
        }
    }

    fn ternary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let condition = Expression::logical_or(tokens)?;

        if let Some(Token::QuestionMark) = tokens.peek() {
            tokens.next();
            let then = Box::new(Expression::parse(tokens)?);
            expect_token(tokens, Token::Colon)?;
            let otherwise = Box::new(Expression::parse(tokens)?);

            Ok(Expression::Ternary {
                condition: Box::new(condition),
                then,
                otherwise,
            })
        } else {
            Ok(condition)
        }
    }

    fn logical_or(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::logical_and(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::LogicalOr => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::LogicalOperation {
                        operator: LogicalOperator::Or,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn logical_and(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Self, ParserError> {
        let mut node = Expression::bitwise_or(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::LogicalAnd => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::LogicalOperation {
                        operator: LogicalOperator::And,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn bitwise_or(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::bitwise_xor(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::BitwiseOr => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::BitwiseOr,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn bitwise_xor(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Self, ParserError> {
        let mut node = Expression::bitwise_and(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::BitwiseXor => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::BitwiseXor,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn bitwise_and(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Self, ParserError> {
        let mut node = Expression::equality(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::BitwiseAnd => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::BitwiseAnd,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn equality(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::relational(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Equal => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::RelationalOperation {
                        operator: RelationalOperator::Equal,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::NotEqual => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::RelationalOperation {
                        operator: RelationalOperator::NotEqual,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn relational(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::shifting(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::LessThan => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::RelationalOperation {
                        operator: RelationalOperator::LessThan,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::LessThanOrEqual => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::RelationalOperation {
                        operator: RelationalOperator::LessThanOrEqual,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::GreaterThan => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::RelationalOperation {
                        operator: RelationalOperator::GreaterThan,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::GreaterThanOrEqual => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::RelationalOperation {
                        operator: RelationalOperator::GreaterThanOrEqual,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn shifting(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::additive(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::ShiftLeft => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::ShiftLeft,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::ShiftRight => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::ShiftRight,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn additive(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::term(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Addition => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::Addition,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Negation => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::Addition,
                        left: Box::new(node),
                        right: Box::new(Expression::UnaryOperation {
                            operator: UnaryOperator::Negation,
                            operand: right,
                        }),
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, ParserError> {
        let mut node = Expression::factor(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Multiplication => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::Multiplication,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Division => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::Division,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Modulo => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::Operation {
                        operator: Operator::Modulo,
                        left: Box::new(node),
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn factor(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Expression, ParserError> {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        match token {
            Token::Word(id) => {
                let peek = tokens.peek();

                if let Some(Token::OpenParenthesis) = peek {
                    tokens.next();
                    let mut arguments = vec![];
                    while let Some(token) = tokens.peek() {
                        if let Token::CloseParenthesis = token {
                            break;
                        }
                        arguments.push(Expression::parse(tokens)?);
                        // allows for trailing commas
                        if let Some(Token::Comma) = tokens.peek() {
                            tokens.next();
                        }
                    }
                    expect_token(tokens, Token::CloseParenthesis)?;

                    return Ok(Expression::Call {
                        function: id,
                        arguments,
                    });
                } else if let Some(Token::Increment) = peek {
                    tokens.next();
                    return Ok(Expression::Assignment {
                        variable: id.clone(),
                        expression: Box::new(Expression::Operation {
                            operator: Operator::Addition,
                            left: Box::new(Expression::Variable(id)),
                            right: Box::new(Expression::Integer(1)),
                        }),
                    });
                } else if let Some(Token::Decrement) = peek {
                    tokens.next();
                    return Ok(Expression::Assignment {
                        variable: id.clone(),
                        expression: Box::new(Expression::Operation {
                            operator: Operator::Addition,
                            left: Box::new(Expression::Variable(id)),
                            right: Box::new(Expression::UnaryOperation {
                                operator: UnaryOperator::Negation,
                                operand: Box::new(Expression::Integer(1)),
                            }),
                        }),
                    });
                }

                Ok(Expression::Variable(id))
            }
            Token::Integer(value) => Ok(Expression::Integer(value as u64)),
            Token::OpenParenthesis => {
                let node = Expression::parse(tokens)?;
                expect_token(tokens, Token::CloseParenthesis)?;
                Ok(node)
            }
            // unary
            Token::Negation => {
                let factor = Expression::factor(tokens)?;
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand: Box::new(factor),
                })
            }
            Token::BitwiseNot => {
                let factor = Expression::factor(tokens)?;
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::BitwiseNot,
                    operand: Box::new(factor),
                })
            }
            Token::LogicalNot => {
                let factor = Expression::factor(tokens)?;
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::LogicalNot,
                    operand: Box::new(factor),
                })
            }
            Token::Increment => {
                let factor = Expression::factor(tokens)?;
                if let Expression::Variable(variable) = &factor {
                    Ok(Expression::Assignment {
                        variable: variable.clone(),
                        expression: Box::new(Expression::Operation {
                            operator: Operator::Addition,
                            left: Box::new(factor),
                            right: Box::new(Expression::Integer(1)),
                        }),
                    })
                } else {
                    Err(ParserError::UnexpectedToken {
                        unexpected: token,
                        expected: vec![Token::Word("<variable_name>".to_string())],
                        near_tokens: tokens.take(6).collect(),
                    })
                }
            }
            Token::Decrement => {
                let factor = Expression::factor(tokens)?;
                if let Expression::Variable(variable) = &factor {
                    Ok(Expression::Assignment {
                        variable: variable.clone(),
                        expression: Box::new(Expression::Operation {
                            operator: Operator::Addition,
                            left: Box::new(factor),
                            right: Box::new(Expression::UnaryOperation {
                                operator: UnaryOperator::Negation,
                                operand: Box::new(Expression::Integer(1)),
                            }),
                        }),
                    })
                } else {
                    Err(ParserError::UnexpectedToken {
                        unexpected: token,
                        expected: vec![Token::Word("<variable_name>".to_string())],
                        near_tokens: tokens.take(6).collect(),
                    })
                }
            }
            _ => Err(ParserError::UnexpectedToken {
                unexpected: token,
                expected: vec![Token::Integer(0), Token::OpenParenthesis],
                near_tokens: tokens.take(6).collect(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::return_integer(
        &[Token::Keyword(Keyword::Return), Token::Integer(1234), Token::SemiColon],
        Statement::Return { expression: Expression::Integer(1234) }
    )]
    #[case::expression(
        &[Token::Integer(1), Token::SemiColon],
        Statement::Expression(Expression::Integer(1))
    )]
    #[case::null_expression(
        &[Token::SemiColon],
        Statement::NullExpression
    )]
    #[case::continue_statement(
        &[Token::Keyword(Keyword::Continue), Token::SemiColon],
        Statement::Continue
    )]
    #[case::break_statement(
        &[Token::Keyword(Keyword::Break), Token::SemiColon],
        Statement::Break
    )]
    fn test_statement(#[case] tokens: &[Token], #[case] expected: Statement) {
        let mut tokens = tokens.iter().cloned().peekable();
        let statement = Statement::parse(&mut tokens).unwrap();

        assert_eq!(statement, expected);
    }

    #[rstest]
    #[case::do_while(
        &[
            Token::Keyword(Keyword::Do),
            Token::OpenBrace,
            Token::Keyword(Keyword::Continue),
            Token::SemiColon,
            Token::CloseBrace,
            Token::Keyword(Keyword::While),
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::CloseParenthesis,
            Token::SemiColon,
        ],
        Statement::Do {
            body: Box::new(Statement::Compound(vec![Block::Statement(Statement::Continue)])),
            condition: Expression::Integer(1),
        }
    )]
    #[case::while_statement(
        &[
            Token::Keyword(Keyword::While),
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Continue),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::While {
            condition: Expression::Integer(1),
            body: Box::new(Statement::Compound(vec![Block::Statement(Statement::Continue)])),
        }
    )]
    #[case::for_statement(
        &[
            Token::Keyword(Keyword::For),
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::SemiColon,
            Token::Integer(2),
            Token::SemiColon,
            Token::Integer(3),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Continue),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::For {
            initializer: Some(Expression::Integer(1)),
            condition: Expression::Integer(2),
            post_expression: Some(Expression::Integer(3)),
            body: Box::new(Statement::Compound(vec![Block::Statement(Statement::Continue)])),
        }
    )]
    #[case::for_with_declaration_statement(
        &[
            Token::Keyword(Keyword::For),
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::SemiColon,
            Token::Integer(2),
            Token::SemiColon,
            Token::Integer(3),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Continue),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::ForDeclaration {
            declaration: Declaration {
                variable: "variable".to_string(),
                expression: None,
            },
            condition: Expression::Integer(2),
            post_expression: Some(Expression::Integer(3)),
            body: Box::new(Statement::Compound(vec![Block::Statement(Statement::Continue)])),
        }
    )]
    #[case::empty_for_statement(
        &[
            Token::Keyword(Keyword::For),
            Token::OpenParenthesis,
            Token::SemiColon,
            Token::SemiColon,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Continue),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::For {
            initializer: None,
            condition: Expression::Integer(1),
            post_expression: None,
            body: Box::new(Statement::Compound(vec![Block::Statement(Statement::Continue)])),
        }
    )]
    fn test_loop(#[case] tokens: &[Token], #[case] expected: Statement) {
        let mut tokens = tokens.iter().cloned().peekable();
        let statement = Statement::parse(&mut tokens).unwrap();

        assert_eq!(statement, expected);
    }

    #[rstest]
    #[case::declaration(
        &[Token::Keyword(Keyword::Int), Token::Word("variable".to_string()), Token::SemiColon],
        Block::Declaration(Declaration { variable: "variable".to_string(), expression: None })
    )]
    fn test_block(#[case] tokens: &[Token], #[case] expected: Block) {
        let mut tokens = tokens.iter().cloned().peekable();
        let block = Block::parse(&mut tokens).unwrap();

        assert_eq!(block, expected);
    }

    #[rstest]
    #[case::integer(&[Token::Integer(1234)], Expression::Integer(1234))]
    // negation
    #[case::integer_negation(
        &[Token::Negation, Token::Integer(1234)],
        Expression::UnaryOperation {
            operator: UnaryOperator::Negation,
            operand: Box::new(Expression::Integer(1234))
        }
    )]
    #[case::bitwise_not_integer(
        &[Token::BitwiseNot, Token::Integer(1234)],
        Expression::UnaryOperation {
            operator: UnaryOperator::BitwiseNot,
            operand: Box::new(Expression::Integer(1234))
        }
    )]
    #[case::logical_not_integer(
        &[Token::LogicalNot, Token::Integer(1234)],
        Expression::UnaryOperation {
            operator: UnaryOperator::LogicalNot,
            operand: Box::new(Expression::Integer(1234))
        }
    )]
    #[case::negation_bitwise_not_logical_not_integer(
        &[Token::Negation, Token::BitwiseNot, Token::LogicalNot, Token::Integer(1234)],
        Expression::UnaryOperation {
            operator: UnaryOperator::Negation,
            operand: Box::new(Expression::UnaryOperation {
                operator: UnaryOperator::BitwiseNot,
                operand: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::LogicalNot,
                    operand: Box::new(Expression::Integer(1234))
                })
            })
        })
    ]
    #[case::addition(
        &[
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::subtraction(
        &[
            Token::Integer(1),
            Token::Negation,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::UnaryOperation {
                operator: UnaryOperator::Negation,
                operand: Box::new(Expression::Integer(2))
            })
        }
    )]
    #[case::multiplication(
        &[
            Token::Integer(1),
            Token::Multiplication,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::Multiplication,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::division(
        &[
            Token::Integer(1),
            Token::Division,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::Division,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    // operation combination
    #[case::addition_multiplication(
        &[
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::Multiplication,
            Token::Integer(3),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::Integer(3))
            })
        }
    )]
    #[case::multiplication_addition(
        &[
            Token::Integer(1),
            Token::Multiplication,
            Token::Integer(2),
            Token::Addition,
            Token::Integer(3),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::Integer(3))
        }
    )]
    #[case::multiplication_division(
        &[
            Token::Integer(1),
            Token::Multiplication,
            Token::Integer(2),
            Token::Division,
            Token::Integer(3),
        ],
        Expression::Operation {
            operator: Operator::Multiplication,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Operation {
                operator: Operator::Division,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::Integer(3))
            })
        }
    )]
    #[case::division_multiplication(
        &[
            Token::Integer(1),
            Token::Division,
            Token::Integer(2),
            Token::Multiplication,
            Token::Integer(3),
        ],
        Expression::Operation {
            operator: Operator::Division,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::Integer(3))
            })
        }
    )]
    #[case::addition_multiplication_division(
        &[
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::Multiplication,
            Token::Integer(3),
            Token::Division,
            Token::Integer(4),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::Operation {
                    operator: Operator::Division,
                    left: Box::new(Expression::Integer(3)),
                    right: Box::new(Expression::Integer(4))
                })
            })
        }
    )]
    #[case::addition_division_multiplication(
        &[
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::Division,
            Token::Integer(3),
            Token::Multiplication,
            Token::Integer(4),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Operation {
                operator: Operator::Division,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::Operation {
                    operator: Operator::Multiplication,
                    left: Box::new(Expression::Integer(3)),
                    right: Box::new(Expression::Integer(4))
                })
            })
        }
    )]
    #[case::multiplication_addition_division(
        &[
            Token::Integer(1),
            Token::Multiplication,
            Token::Integer(2),
            Token::Addition,
            Token::Integer(3),
            Token::Division,
            Token::Integer(4),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::Operation {
                operator: Operator::Division,
                left: Box::new(Expression::Integer(3)),
                right: Box::new(Expression::Integer(4))
            })
        }
    )]
    #[case::multiplication_division_addition(
        &[
            Token::Integer(1),
            Token::Multiplication,
            Token::Integer(2),
            Token::Division,
            Token::Integer(3),
            Token::Addition,
            Token::Integer(4),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Operation {
                    operator: Operator::Division,
                    left: Box::new(Expression::Integer(2)),
                    right: Box::new(Expression::Integer(3))
                })
            }),
            right: Box::new(Expression::Integer(4))
        }
    )]
    // parenthesis
    #[case::parenthesis(
        &[
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::CloseParenthesis,
        ],
        Expression::Integer(1)
    )]
    #[case::parenthesis_addition(
        &[
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::CloseParenthesis,
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::parenthesis_addition_multiplication(
        &[
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::CloseParenthesis,
            Token::Multiplication,
            Token::Integer(3),
        ],
        Expression::Operation {
            operator: Operator::Multiplication,
            left: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::Integer(3))
        }
    )]
    #[case::double_negation(
        &[
            Token::Integer(1),
            Token::Negation,
            Token::Negation,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::UnaryOperation {
                operator: UnaryOperator::Negation,
                operand: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand: Box::new(Expression::Integer(2))
                })
            })
        }
    )]
    #[case::bitwise_not_parenthesis_negation_logical_not(
        &[
            Token::BitwiseNot,
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::Negation,
            Token::LogicalNot,
            Token::Integer(2),
            Token::CloseParenthesis,
        ],
        Expression::UnaryOperation {
            operator: UnaryOperator::BitwiseNot,
            operand: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand: Box::new(Expression::UnaryOperation {
                        operator: UnaryOperator::LogicalNot,
                        operand: Box::new(Expression::Integer(2))
                    })
                })
            })
        }
    )]
    // 1 + ~(2 == 3) + 1
    #[case::addition_bitwise_not_parenthesis_equal(
        &[
            Token::Integer(1),
            Token::Addition,
            Token::BitwiseNot,
            Token::OpenParenthesis,
            Token::Integer(2),
            Token::Equal,
            Token::Integer(3),
            Token::CloseParenthesis,
            Token::Addition,
            Token::Integer(1),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::BitwiseNot,
                    operand: Box::new(Expression::RelationalOperation {
                        operator: RelationalOperator::Equal,
                        left: Box::new(Expression::Integer(2)),
                        right: Box::new(Expression::Integer(3))
                    })
                })
            }),
            right: Box::new(Expression::Integer(1))
        }
    )]
    #[case::variable(
        &[Token::Word("variable".to_string())],
        Expression::Variable("variable".to_string())
    )]
    #[case::bitwise_xor(
        &[
            Token::Integer(1),
            Token::BitwiseXor,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::BitwiseXor,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::bitwise_and(
        &[
            Token::Integer(1),
            Token::BitwiseAnd,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::BitwiseAnd,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::bitwise_or(
        &[
            Token::Integer(1),
            Token::BitwiseOr,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::BitwiseOr,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::shift_left(
        &[
            Token::Integer(1),
            Token::ShiftLeft,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::ShiftLeft,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::shift_right(
        &[
            Token::Integer(1),
            Token::ShiftRight,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::ShiftRight,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::modulo(
        &[
            Token::Integer(1),
            Token::Modulo,
            Token::Integer(2),
        ],
        Expression::Operation {
            operator: Operator::Modulo,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::increment_addition_decrement(
        &[
            Token::Increment,
            Token::Word("variable".to_string()),
            Token::Addition,
            Token::Decrement,
            Token::Word("other".to_string()),
        ],
        Expression::Operation {
            operator: Operator::Addition,
            left: Box::new(Expression::Assignment {
                variable: "variable".to_string(),
                expression: Box::new(Expression::Operation {
                    operator: Operator::Addition,
                    left: Box::new(Expression::Variable("variable".to_string())),
                    right: Box::new(Expression::Integer(1))
                })
            }),
            right: Box::new(Expression::Assignment {
                variable: "other".to_string(),
                expression: Box::new(Expression::Operation {
                    operator: Operator::Addition,
                    left: Box::new(Expression::Variable("other".to_string())),
                    right: Box::new(Expression::UnaryOperation {
                        operator: UnaryOperator::Negation,
                        operand: Box::new(Expression::Integer(1))
                    })
                })
            })
        }
    )]
    #[case::left_increment(
        &[
            Token::Increment,
            Token::Word("variable".to_string()),
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::left_decrement(
        &[
            Token::Decrement,
            Token::Word("variable".to_string()),
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand: Box::new(Expression::Integer(1))
                })
            })
        }
    )]
    #[case::right_increment(
        &[
            Token::Word("variable".to_string()),
            Token::Increment,
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::right_decrement(
        &[
            Token::Word("variable".to_string()),
            Token::Decrement,
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand: Box::new(Expression::Integer(1))
                })
            })
        }
    )]
    #[case::ternary(
        &[
            Token::Integer(1),
            Token::QuestionMark,
            Token::Integer(2),
            Token::Colon,
            Token::Integer(3),
        ],
        Expression::Ternary {
            condition: Box::new(Expression::Integer(1)),
            then: Box::new(Expression::Integer(2)),
            otherwise: Box::new(Expression::Integer(3))
        }
    )]
    fn test_expression(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case::logical_and(
        &[
            Token::Integer(1),
            Token::LogicalAnd,
            Token::Integer(2),
        ],
        Expression::LogicalOperation {
            operator: LogicalOperator::And,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::logical_or(
        &[
            Token::Integer(1),
            Token::LogicalOr,
            Token::Integer(2),
        ],
        Expression::LogicalOperation {
            operator: LogicalOperator::Or,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    fn test_logical_expression(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case::equal(
        &[
            Token::Integer(1),
            Token::Equal,
            Token::Integer(2),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::Equal,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::not_equal(
        &[
            Token::Integer(1),
            Token::NotEqual,
            Token::Integer(2),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::NotEqual,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::equal_combined(
        &[
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::Equal,
            Token::Integer(3),
            Token::Multiplication,
            Token::Integer(4),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::Equal,
            left: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Integer(3)),
                right: Box::new(Expression::Integer(4))
            })
        }
    )]
    fn test_equality_expression(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case::less_than(
        &[
            Token::Integer(1),
            Token::LessThan,
            Token::Integer(2),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::LessThan,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::less_than_or_equal(
        &[
            Token::Integer(1),
            Token::LessThanOrEqual,
            Token::Integer(2),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::LessThanOrEqual,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::greater_than(
        &[
            Token::Integer(1),
            Token::GreaterThan,
            Token::Integer(2),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::GreaterThan,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    #[case::greater_than_or_equal(
        &[
            Token::Integer(1),
            Token::GreaterThanOrEqual,
            Token::Integer(2),
        ],
        Expression::RelationalOperation {
            operator: RelationalOperator::GreaterThanOrEqual,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::Integer(2))
        }
    )]
    fn test_relational_expression(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case::return_integer(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("main".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Return),
            Token::Integer(1234),
            Token::SemiColon,
            Token::CloseBrace
        ],
        Function {
            name: "main".to_string(),
            body: vec![
                Block::Statement(Statement::Return { expression: Expression::Integer(1234) })
            ],
            arguments: vec![],
            declaration: false
        }
    )]
    #[case::multiple_statements(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("main".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::SemiColon,
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Word("variable".to_string()),
            Token::Addition,
            Token::Integer(1234),
            Token::SemiColon,
            Token::Keyword(Keyword::Return),
            Token::Integer(1234),
            Token::SemiColon,
            Token::CloseBrace
        ],
        Function {
            name: "main".to_string(),
            body: vec![
                Block::Declaration(Declaration { variable: "variable".to_string(), expression: None }),
                Block::Statement(Statement::Expression(Expression::Assignment {
                    variable: "variable".to_string(),
                    expression: Box::new(Expression::Operation {
                        operator: Operator::Addition,
                        left: Box::new(Expression::Variable("variable".to_string())),
                        right: Box::new(Expression::Integer(1234))
                    })
                })),
                Block::Statement(Statement::Return { expression: Expression::Integer(1234) })
            ],
            arguments: vec![],
            declaration: false
        }
    )]
    fn test_function(#[case] tokens: &[Token], #[case] expected: Function) {
        let mut tokens = tokens.iter().cloned().peekable();
        let function = Function::parse(&mut tokens).unwrap();

        assert_eq!(function, expected);
    }

    #[rstest]
    #[case::no_expression(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::SemiColon
        ],
        Declaration {
            variable: "variable".to_string(),
            expression: None
        }
    )]
    #[case::with_integer(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::SemiColon
        ],
        Declaration {
            variable: "variable".to_string(),
            expression: Some(Expression::Integer(1))
        }
    )]
    #[case::with_expression(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::SemiColon
        ],
        Declaration {
            variable: "variable".to_string(),
            expression: Some(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            })
        }
    )]
    fn test_declaration(#[case] tokens: &[Token], #[case] expected: Declaration) {
        let mut tokens = tokens.iter().cloned().peekable();
        let declaration = Declaration::parse(&mut tokens).unwrap();

        assert_eq!(declaration, expected);
    }

    #[rstest]
    #[case::integer_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Integer(1))
        }
    )]
    #[case::expression_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            })
        }
    )]
    #[case::assigment_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::OpenParenthesis,
            Token::Word("other".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::CloseParenthesis,
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Assignment {
                variable: "other".to_string(),
                expression: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::plus_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::PlusEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::minus_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::MinusEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Addition,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand: Box::new(Expression::Integer(1))
                })
            })
        }
    )]
    #[case::multiplication_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::MultiplyEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Multiplication,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::division_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::DivideEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Division,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::shift_left_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::ShiftLeftEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::ShiftLeft,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::shift_right_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::ShiftRightEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::ShiftRight,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::modulo_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::ModuloEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::Modulo,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    #[case::xor_assigment(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::XorEqual),
            Token::Integer(1),
            Token::SemiColon
        ],
        Expression::Assignment {
            variable: "variable".to_string(),
            expression: Box::new(Expression::Operation {
                operator: Operator::BitwiseXor,
                left: Box::new(Expression::Variable("variable".to_string())),
                right: Box::new(Expression::Integer(1))
            })
        }
    )]
    fn test_assignment(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let assignment = Expression::parse(&mut tokens).unwrap();

        assert_eq!(assignment, expected);
    }

    #[rstest]
    #[case::if_statement(
        &[
            Token::Keyword(Keyword::If),
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::CloseParenthesis,
            Token::Keyword(Keyword::Return),
            Token::Integer(1),
            Token::SemiColon,
        ],
        Statement::Conditional{
            condition: Expression::Integer(1),
            then: Box::new(Statement::Return { expression: Expression::Integer(1) }),
            otherwise: None
        }
    )]
    #[case::if_else_statement(
        &[
            Token::Keyword(Keyword::If),
            Token::OpenParenthesis,
            Token::Integer(1),
            Token::CloseParenthesis,
            Token::Keyword(Keyword::Return),
            Token::Integer(1),
            Token::SemiColon,
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Return),
            Token::Integer(2),
            Token::SemiColon,
        ],
        Statement::Conditional{
            condition: Expression::Integer(1),
            then: Box::new(Statement::Return { expression: Expression::Integer(1) }),
            otherwise: Some(Box::new(Statement::Return { expression: Expression::Integer(2) }))
        }
    )]
    fn test_conditional(#[case] tokens: &[Token], #[case] expected: Statement) {
        let mut tokens = tokens.iter().cloned().peekable();
        let statement = Statement::parse(&mut tokens).unwrap();

        assert_eq!(statement, expected);
    }

    #[rstest]
    #[case(
        &[
            Token::Word("variable".to_string()),
            Token::QuestionMark,
            Token::Integer(1),
            Token::Colon,
            Token::Word("other".to_string()),
            Token::QuestionMark,
            Token::Integer(2),
            Token::Colon,
            Token::Integer(3),
        ],
        Expression::Ternary {
            condition: Box::new(Expression::Variable("variable".to_string())),
            then: Box::new(Expression::Integer(1)),
            otherwise: Box::new(Expression::Ternary {
                condition: Box::new(Expression::Variable("other".to_string())),
                then: Box::new(Expression::Integer(2)),
                otherwise: Box::new(Expression::Integer(3))
            })
        }
    )]
    fn test_expression_with_ternary(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case(
        &[
            Token::OpenBrace,
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::Compound(vec![
            Block::Declaration(Declaration { variable: "variable".to_string(), expression: None })
        ])
    )]
    fn test_compound_block(#[case] tokens: &[Token], #[case] expected: Statement) {
        let mut tokens = tokens.iter().cloned().peekable();
        let blocks = Statement::parse(&mut tokens).unwrap();

        assert_eq!(blocks, expected);
    }

    #[rstest]
    #[case::no_parameters(
        &[
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
        ],
        Expression::Call {
            function: "func".to_string(),
            arguments: vec![]
        }
    )]
    #[case::with_parameter(
        &[
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::Word("variable".to_string()),
            Token::CloseParenthesis,
        ],
        Expression::Call {
            function: "func".to_string(),
            arguments: vec![Expression::Variable("variable".to_string())]
        }
    )]
    #[case::with_parameters(
        &[
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::Word("variable".to_string()),
            Token::Comma,
            Token::Integer(1),
            Token::CloseParenthesis,
        ],
        Expression::Call {
            function: "func".to_string(),
            arguments: vec![
                Expression::Variable("variable".to_string()),
                Expression::Integer(1)
            ]
        }
    )]
    fn test_call(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case::no_parameters(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::SemiColon,
        ],
        Function {
            name: "func".to_string(),
            body: vec![],
            arguments: vec![],
            declaration: true
        }
    )]
    #[case::with_parameter(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::CloseParenthesis,
            Token::SemiColon,
        ],
        Function {
            name: "func".to_string(),
            body: vec![],
            arguments: vec![ "variable".to_string() ],
            declaration: true
        }
    )]
    #[case::with_parameters(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::Comma,
            Token::Keyword(Keyword::Int),
            Token::Word("other".to_string()),
            Token::CloseParenthesis,
            Token::SemiColon,
        ],
        Function {
            name: "func".to_string(),
            body: vec![],
            arguments: vec![ "variable".to_string(), "other".to_string() ],
            declaration: true
        }
    )]
    #[case::with_argument_as_return(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::Word("variable".to_string()),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Return),
            Token::Word("variable".to_string()),
            Token::SemiColon,
            Token::CloseBrace
        ],
        Function {
            name: "func".to_string(),
            body: vec![
                Block::Statement(Statement::Return { expression: Expression::Variable("variable".to_string()) })
            ],
            arguments: vec![ "variable".to_string() ],
            declaration: false
        }
    )]
    fn test_function_declaration(#[case] tokens: &[Token], #[case] expected: Function) {
        let mut tokens = tokens.iter().cloned().peekable();
        let function = Function::parse(&mut tokens).unwrap();

        assert_eq!(function, expected);
    }

    #[rstest]
    #[case::multiple_functions_with_body(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::CloseBrace,
            Token::Keyword(Keyword::Int),
            Token::Word("main".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::CloseBrace,
        ],
        Program {
            functions: vec![
                Function {
                    name: "func".to_string(),
                    body: vec![],
                    arguments: vec![],
                    declaration: false,
                },
                Function {
                    name: "main".to_string(),
                    body: vec![],
                    arguments: vec![],
                    declaration: false,
                }
            ]
        }
    )]
    #[case::multiple_fucntions_without_body(
        &[
            Token::Keyword(Keyword::Int),
            Token::Word("func".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::SemiColon,
            Token::Keyword(Keyword::Int),
            Token::Word("main".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::SemiColon,
        ],
        Program {
            functions: vec![
                Function {
                    name: "func".to_string(),
                    body: vec![],
                    arguments: vec![],
                    declaration: true
                },
                Function {
                    name: "main".to_string(),
                    body: vec![],
                    arguments: vec![],
                    declaration: true
                }
            ]
        }
    )]
    fn test_parser(#[case] tokens: &[Token], #[case] expected: Program) {
        let tokens = tokens.iter().cloned().peekable();
        let program = Parser::parse(tokens.into_iter().peekable()).unwrap();

        assert_eq!(program, expected);
    }
}
