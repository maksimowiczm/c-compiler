pub mod constant;
pub mod expression;

use crate::lexer::Token;
use derive_more::{Display, Error};
use std::collections::HashMap;
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

trait Parse {
    fn parse(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result<Self, ParserError>
    where
        Self: Sized;

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

    fn allow_token(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        expected: Token,
    ) -> Result<(), ParserError> {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        if token.clone() == expected {
            tokens.next();
        }

        Ok(())
    }
}

struct Context {
    enums: HashMap<String, Vec<String>>,
}

impl Context {
    fn contains_enumeration_constant(&self, constant: &str) -> bool {
        self.enums
            .values()
            .any(|values| values.contains(&constant.to_string()))
    }
}
