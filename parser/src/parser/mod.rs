pub mod constant;

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

pub(crate) trait Parse {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError>
    where
        Self: Sized;
}
