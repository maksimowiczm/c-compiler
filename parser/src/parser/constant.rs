use crate::lexer::{Constant as TokenConstant, Token};
use crate::parser::{Parse, ParserError, Result};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Constant {
    Integer(i128),
    Character(char),
    Float(f64),
    Enumeration(String),
}

impl Parse for Constant {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
        let out = match token {
            Token::Constant(constant) => match constant {
                TokenConstant::SignedInteger(value) => Constant::Integer(value as i128),
                TokenConstant::UnsignedInteger(value) => Constant::Integer(value as i128),
                TokenConstant::Decimal(value) => Constant::Float(value),
                TokenConstant::Character(value) => Constant::Character(value),
            },
            Token::Word(value) => Constant::Enumeration(value),
            _ => {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token,
                    expected: vec![
                        Token::Constant(TokenConstant::SignedInteger(0)),
                        Token::Constant(TokenConstant::UnsignedInteger(0)),
                        Token::Constant(TokenConstant::Decimal(0.0)),
                        Token::Constant(TokenConstant::Character('*')),
                        Token::Word("<identifier>".to_string()),
                        Token::Word("<enumeration-constant>".to_string()),
                    ],
                    near_tokens: vec![],
                })
            }
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Constant as TokenConstant, Token};
    use crate::parser::constant::Constant;
    use crate::parser::Parse;
    use rstest::rstest;

    #[rstest]
    #[case::signed_integer(vec![Token::Constant(TokenConstant::SignedInteger(42))], Constant::Integer(42))]
    #[case::unsigned_integer(vec![Token::Constant(TokenConstant::UnsignedInteger(42))], Constant::Integer(42))]
    #[case::float(vec![Token::Constant(TokenConstant::Decimal(42.0))], Constant::Float(42.0))]
    #[case::character(vec![Token::Constant(TokenConstant::Character('a'))], Constant::Character('a'))]
    #[case::enumeration(vec![Token::Word("foo".to_string())], Constant::Enumeration("foo".to_string()))]
    fn test_constant(#[case] input: Vec<Token>, #[case] expected: Constant) {
        let result = Constant::parse(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }
}
