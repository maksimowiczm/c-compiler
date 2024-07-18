pub mod constant;
pub mod declaration;
pub mod expression;
pub mod statement;

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
    #[display(
        "Unexpected token {:?}. Expected type specifier, near tokens: {:?}",
        unexpected,
        near_tokens
    )]
    ExpectedTypeSpecifier {
        unexpected: Token,
        near_tokens: Vec<Token>,
    },
    #[display("Expected initializer, near tokens: {:?}", near_tokens)]
    ExpectedInitializer {
        near_tokens: Vec<Token>,
    },
    #[display("Expected declarator, near tokens: {:?}", near_tokens)]
    ExpectedDeclarator {
        near_tokens: Vec<Token>,
    },
    #[display("Expected constant, near tokens: {:?}", near_tokens)]
    ExpectedConstant {
        near_tokens: Vec<Token>,
    },
    #[display("Expected declaration specifier, near tokens: {:?}", near_tokens)]
    ExpectedDeclarationSpecifier {
        near_tokens: Vec<Token>,
    },
    #[display("Expected expression, near tokens: {:?}", near_tokens)]
    ExpectedExpression {
        near_tokens: Vec<Token>,
    },
}

pub type Result<T> = std::result::Result<T, ParserError>;

pub trait Parse {
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
        if let Some(token) = tokens.peek() {
            if token.clone() == expected {
                tokens.next();
            }
        }

        Ok(())
    }
}

pub trait TryParse {
    /// Try to parse the current token stream into the current type.
    /// If the current token stream does not match the current type, return None.
    /// Do not consume the tokens if the current token stream does not match the current type.
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized;
}
