use crate::lexer::Token;
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
    Return { expression: Expression },
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Expression {
    Integer(u64),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum UnaryOperator {
    Negation,
    BitwiseNot,
    LogicalNot,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum BinaryOperator {
    Addition,
    Multiplication,
    Division,
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
    expect_token(tokens, Token::Word("int".to_string()))?;
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
    let body = Statement::parse(tokens)?;
    expect_token(tokens, Token::CloseBrace)?;

    Ok(Function { name, body })
}

impl Statement {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        match token {
            Token::Word(word) if word == "return" => {
                let expression = Expression::parse(tokens)?;
                expect_token(tokens, Token::SemiColon)?;
                Ok(Statement::Return { expression })
            }
            _ => Err(ParserError::UnexpectedToken {
                unexpected: token,
                expected: vec![Token::Word("return".to_string())],
                near_tokens: tokens.take(6).collect(),
            }),
        }
    }
}

impl Expression {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError> {
        let mut node = Expression::term(tokens)?;

        while let Some(token) = tokens.peek() {
            match token {
                Token::Addition => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::BinaryOperation {
                        operator: BinaryOperator::Addition,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Negation => {
                    tokens.next();
                    let right = Box::new(Expression::parse(tokens)?);
                    node = Expression::BinaryOperation {
                        operator: BinaryOperator::Addition,
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
                    let right = Box::new(Self::term(tokens)?);
                    node = Expression::BinaryOperation {
                        operator: BinaryOperator::Multiplication,
                        left: Box::new(node),
                        right,
                    };
                }
                Token::Division => {
                    tokens.next();
                    let right = Box::new(Self::term(tokens)?);
                    node = Expression::BinaryOperation {
                        operator: BinaryOperator::Division,
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
        &[Token::Word("return".to_string()), Token::Integer(1234), Token::SemiColon],
        Statement::Return { expression: Expression::Integer(1234) }
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Multiplication,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Division,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Multiplication,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Multiplication,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Multiplication,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Division,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Division,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Multiplication,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Multiplication,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::BinaryOperation {
                    operator: BinaryOperator::Division,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::Integer(1)),
            right: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Division,
                left: Box::new(Expression::Integer(2)),
                right: Box::new(Expression::BinaryOperation {
                    operator: BinaryOperator::Multiplication,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::Integer(2))
            }),
            right: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Division,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Multiplication,
                left: Box::new(Expression::Integer(1)),
                right: Box::new(Expression::BinaryOperation {
                    operator: BinaryOperator::Division,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Multiplication,
            left: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Addition,
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
        Expression::BinaryOperation {
            operator: BinaryOperator::Addition,
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
            operand: Box::new(Expression::BinaryOperation {
                operator: BinaryOperator::Addition,
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
    fn test_expression(#[case] tokens: &[Token], #[case] expected: Expression) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = Expression::parse(&mut tokens).unwrap();

        assert_eq!(expression, expected);
    }

    #[rstest]
    #[case(
        &[
            Token::Word("int".to_string()),
            Token::Word("main".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Word("return".to_string()),
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
    fn test_function(#[case] tokens: &[Token], #[case] expected: Function) {
        let mut tokens = tokens.iter().cloned().peekable();
        let function = function(&mut tokens).unwrap();

        assert_eq!(function, expected);
    }
}
