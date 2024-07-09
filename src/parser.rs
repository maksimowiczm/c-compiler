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
    Integer(i128),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum UnaryOperator {
    Negation,
    BitwiseNot,
    LogicalNot,
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
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
        match token {
            Token::Integer(value) => Ok(Expression::Integer(value as i128)),
            Token::Negation => {
                let operand = Box::new(Expression::parse(tokens)?);
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::Negation,
                    operand,
                })
            }
            Token::BitwiseNot => {
                let operand = Box::new(Expression::parse(tokens)?);
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::BitwiseNot,
                    operand,
                })
            }
            Token::LogicalNot => {
                let operand = Box::new(Expression::parse(tokens)?);
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::LogicalNot,
                    operand,
                })
            }
            _ => Err(ParserError::UnexpectedToken {
                unexpected: token,
                expected: vec![Token::Integer(0)],
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
    #[case(&[Token::Integer(1234)], Expression::Integer(1234))]
    #[case(&[Token::Negation, Token::Integer(1234)], Expression::UnaryOperation {
        operator: UnaryOperator::Negation,
        operand: Box::new(Expression::Integer(1234))
    })]
    #[case(&[Token::BitwiseNot, Token::Integer(1234)], Expression::UnaryOperation {
        operator: UnaryOperator::BitwiseNot,
        operand: Box::new(Expression::Integer(1234))
    })]
    #[case(&[Token::LogicalNot, Token::Integer(1234)], Expression::UnaryOperation {
        operator: UnaryOperator::LogicalNot,
        operand: Box::new(Expression::Integer(1234))
    })]
    #[case(&[Token::Negation, Token::BitwiseNot, Token::LogicalNot, Token::Integer(1234)], Expression::UnaryOperation {
        operator: UnaryOperator::Negation,
        operand: Box::new(Expression::UnaryOperation {
            operator: UnaryOperator::BitwiseNot,
            operand: Box::new(Expression::UnaryOperation {
                operator: UnaryOperator::LogicalNot,
                operand: Box::new(Expression::Integer(1234))
            })
        })
    })]
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
