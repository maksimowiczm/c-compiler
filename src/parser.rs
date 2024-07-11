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
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Program {
    pub(crate) functions: Vec<Function>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Function {
    pub(crate) name: String,
    pub(crate) body: Statement,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Statement {
    StatementList(Vec<Statement>),
    Return {
        expression: Expression,
    },
    Expression(Expression),
    Declaration {
        variable: String,
        expression: Option<Expression>,
    },
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Expression {
    Integer(u64),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    MathOperation {
        operator: MathOperator,
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
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum UnaryOperator {
    Negation,
    BitwiseNot,
    LogicalNot,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum MathOperator {
    Addition,
    Multiplication,
    Division,
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

            functions.push(function(&mut tokens)?);
        }

        let program = Program { functions };

        Ok(program)
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

fn function(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Function, ParserError> {
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
    expect_token(tokens, Token::CloseParenthesis)?;
    expect_token(tokens, Token::OpenBrace)?;

    let mut statements = vec![];
    while let Some(token) = tokens.peek() {
        if let Token::CloseBrace = token {
            break;
        }
        statements.push(Statement::parse(tokens)?);
    }
    expect_token(tokens, Token::CloseBrace)?;

    let body = if statements.len() == 1 {
        statements.remove(0)
    } else {
        Statement::StatementList(statements)
    };

    Ok(Function { name, body })
}

impl Statement {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        match peek {
            Token::Word(_) | Token::Integer(_) => {
                let expression = Expression::parse(tokens)?;
                expect_token(tokens, Token::SemiColon)?;
                return Ok(Statement::Expression(expression));
            }
            Token::Keyword(_) => parse_keyword(tokens),
            _ => Err(ParserError::UnexpectedToken {
                unexpected: peek.clone(),
                expected: vec![Token::Word("<variable_name>".to_string())],
                near_tokens: tokens.take(6).collect(),
            }),
        }
    }
}

fn parse_keyword(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Statement, ParserError> {
    let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
    match token {
        Token::Keyword(Keyword::Return) => {
            let expression = Expression::parse(tokens)?;
            expect_token(tokens, Token::SemiColon)?;

            Ok(Statement::Return { expression })
        }
        Token::Keyword(Keyword::Int) => {
            let declaration = declaration(tokens)?;
            Ok(declaration)
        }
        _ => Err(ParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![
                Token::Keyword(Keyword::Int),
                Token::Keyword(Keyword::Return),
            ],
            near_tokens: tokens.take(6).collect(),
        }),
    }
}

fn declaration(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Statement, ParserError> {
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

    Ok(Statement::Declaration {
        variable,
        expression,
    })
}

impl Expression {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        match Expression::logical_or(tokens)? {
            // <id> "=" <exp>
            Expression::Variable(id) => {
                if !matches!(tokens.peek(), Some(Token::Assignment(_))) {
                    return Ok(Expression::Variable(id));
                }
                return if let Some(Token::Assignment(assignment)) = tokens.next() {
                    let right = Box::new(Expression::parse(tokens)?);
                    match assignment {
                        Assignment::Equal => Ok(Expression::Assignment {
                            variable: id,
                            expression: right,
                        }),
                        Assignment::PlusEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::MathOperation {
                                operator: MathOperator::Addition,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::MinusEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::MathOperation {
                                operator: MathOperator::Addition,
                                left: Box::new(Expression::Variable(id)),
                                right: Box::new(Expression::UnaryOperation {
                                    operator: UnaryOperator::Negation,
                                    operand: right,
                                }),
                            }),
                        }),
                        Assignment::MultiplyEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::MathOperation {
                                operator: MathOperator::Multiplication,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::DivideEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::MathOperation {
                                operator: MathOperator::Division,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::AndEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::LogicalOperation {
                                operator: LogicalOperator::And,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                        Assignment::OrEqual => Ok(Expression::Assignment {
                            variable: id.clone(),
                            expression: Box::new(Expression::LogicalOperation {
                                operator: LogicalOperator::Or,
                                left: Box::new(Expression::Variable(id)),
                                right,
                            }),
                        }),
                    }
                } else {
                    unreachable!()
                };
            }
            // logical_or
            expression => Ok(expression),
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
        let mut node = Expression::equality(tokens)?;

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
        let mut node = Expression::additive(tokens)?;

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

    fn additive(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::term(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Addition => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::MathOperation {
                        operator: MathOperator::Addition,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Negation => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::MathOperation {
                        operator: MathOperator::Addition,
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

        return Ok(node);
    }

    fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, ParserError> {
        let mut node = Expression::factor(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Multiplication => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::MathOperation {
                        operator: MathOperator::Multiplication,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Division => {
                    tokens.next();
                    let right = Box::new(Expression::term(tokens)?);
                    node = Expression::MathOperation {
                        operator: MathOperator::Division,
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
            Token::Word(id) => Ok(Expression::Variable(id)),
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
    #[case::declaration(
        &[Token::Keyword(Keyword::Int), Token::Word("variable".to_string()), Token::SemiColon],
        Statement::Declaration { variable: "variable".to_string(), expression: None }
    )]
    fn test_statement(#[case] tokens: &[Token], #[case] expected: Statement) {
        let mut tokens = tokens.iter().cloned().peekable();
        let statement = Statement::parse(&mut tokens).unwrap();

        assert_eq!(statement, expected);
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
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
        Expression::MathOperation {
            operator: MathOperator::Multiplication,
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
        Expression::MathOperation {
            operator: MathOperator::Division,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
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
        Expression::MathOperation {
            operator: MathOperator::Multiplication,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Division,
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
        Expression::MathOperation {
            operator: MathOperator::Division,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::MathOperation {
                    operator: MathOperator::Division,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Division,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::MathOperation {
                    operator: MathOperator::Multiplication,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Division,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::MathOperation {
                    operator: MathOperator::Division,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
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
        Expression::MathOperation {
            operator: MathOperator::Multiplication,
            left: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
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
            operand: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
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
        Expression::MathOperation {
            operator: MathOperator::Addition,
            left: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
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
            left: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
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
            body: Statement::Return {
                expression: Expression::Integer(1234)
            }
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
            body: Statement::StatementList(vec![
                Statement::Declaration {
                    variable: "variable".to_string(),
                    expression: None
                },
                Statement::Expression(Expression::Assignment {
                    variable: "variable".to_string(),
                    expression: Box::new(Expression::MathOperation {
                        operator: MathOperator::Addition,
                        left: Box::new(Expression::Variable("variable".to_string())),
                        right: Box::new(Expression::Integer(1234))
                    })
                }),
                Statement::Return {
                    expression: Expression::Integer(1234)
                }
            ])
        }
    )]
    fn test_function(#[case] tokens: &[Token], #[case] expected: Function) {
        let mut tokens = tokens.iter().cloned().peekable();
        let function = function(&mut tokens).unwrap();

        assert_eq!(function, expected);
    }

    #[rstest]
    #[case::no_expression(
        &[
            Token::Word("variable".to_string()),
            Token::SemiColon
        ],
        Statement::Declaration {
            variable: "variable".to_string(),
            expression: None
        }
    )]
    #[case::with_integer(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::SemiColon
        ],
        Statement::Declaration {
            variable: "variable".to_string(),
            expression: Some(Expression::Integer(1))
        }
    )]
    #[case::with_expression(
        &[
            Token::Word("variable".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Integer(1),
            Token::Addition,
            Token::Integer(2),
            Token::SemiColon
        ],
        Statement::Declaration {
            variable: "variable".to_string(),
            expression: Some(Expression::MathOperation {
                operator: MathOperator::Addition,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            })
        }
    )]
    fn test_declaration(#[case] tokens: &[Token], #[case] expected: Statement) {
        let mut tokens = tokens.iter().cloned().peekable();
        let declaration = declaration(&mut tokens).unwrap();

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
            expression: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
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
            expression: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
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
            expression: Box::new(Expression::MathOperation {
                operator: MathOperator::Addition,
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
            expression: Box::new(Expression::MathOperation {
                operator: MathOperator::Multiplication,
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
            expression: Box::new(Expression::MathOperation {
                operator: MathOperator::Division,
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
}
