pub mod constant;
pub mod expression;
pub mod statement;
pub mod type_info;

use crate::lexer::Token;
use derive_more::{Display, Error};
use std::iter::Peekable;

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
    #[display(
        "Unexpected token {:?}. Expected type, near tokens: {:?}",
        unexpected,
        near_tokens
    )]
    ExpectedType {
        unexpected: Token,
        near_tokens: Vec<Token>,
    },
    #[display(
        "Unexpected token {:?}. Expected identifier, near tokens: {:?}",
        unexpected,
        near_tokens
    )]
    ExpectedIdentifier {
        unexpected: Token,
        near_tokens: Vec<Token>,
    },
}

pub type Result<T> = std::result::Result<T, ParserError>;

trait Parse {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized;

    fn expect_token(tokens: &mut impl Iterator<Item = Token>, expected: Token) -> Result<()> {
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

    fn allow_token(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        expected: Token,
    ) -> Result<()> {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        if token.clone() == expected {
            tokens.next();
        }

        Ok(())
    }
}
