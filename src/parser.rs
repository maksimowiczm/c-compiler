use crate::lexer::Token;
use derive_more::{Display, Error};
use std::iter::Peekable;

pub struct Parser;

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Node {
    Program { functions: Vec<Node> },
    Function { name: String, body: Box<Node> },
    Return { expression: Box<Node> },
    Expression { value: Box<Node> },
    Integer { value: i32 },
}

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

impl Parser {
    pub fn parse(mut tokens: Peekable<impl Iterator<Item = Token>>) -> Result<Node, ParserError> {
        let mut functions = vec![];

        while let Some(token) = tokens.peek() {
            if let Token::EndOfFile = token {
                break;
            }

            functions.push(function(&mut tokens)?);
        }

        let program = Node::Program { functions };

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

fn function(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Node, ParserError> {
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
    let body = Box::new(statement(tokens)?);
    expect_token(tokens, Token::CloseBrace)?;

    Ok(Node::Function { name, body })
}

fn expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Node, ParserError> {
    let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
    match token {
        Token::Integer(value) => Ok(Node::Integer { value }),
        _ => Err(ParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![Token::Integer(0)],
            near_tokens: tokens.take(6).collect(),
        }),
    }
}

fn statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Node, ParserError> {
    return_statement(tokens)
}

fn return_statement(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Node, ParserError> {
    expect_token(tokens, Token::Word("return".to_string()))?;
    let expression = expression(tokens)?;
    expect_token(tokens, Token::SemiColon)?;

    Ok(Node::Return {
        expression: Box::new(expression),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::return_integer(
        &[Token::Word("return".to_string()), Token::Integer(1234), Token::SemiColon],
        Node::Return { expression: Box::new(Node::Integer { value: 1234 }) }
    )]
    fn test_statement(#[case] tokens: &[Token], #[case] expected: Node) {
        let mut tokens = tokens.iter().cloned().peekable();
        let statement = statement(&mut tokens).unwrap();

        assert_eq!(statement, expected);
    }

    #[rstest]
    #[case(&[Token::Integer(1234)], Node::Integer { value: 1234 })]
    fn test_expression(#[case] tokens: &[Token], #[case] expected: Node) {
        let mut tokens = tokens.iter().cloned().peekable();
        let expression = expression(&mut tokens).unwrap();

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
        Node::Function {
            name: "main".to_string(),
            body: Box::new(Node::Return {
                expression: Box::new(Node::Integer { value: 1234 })
            })
        }
    )]
    fn test_function(#[case] tokens: &[Token], #[case] expected: Node) {
        let mut tokens = tokens.iter().cloned().peekable();
        let function = function(&mut tokens).unwrap();

        assert_eq!(function, expected);
    }
}
