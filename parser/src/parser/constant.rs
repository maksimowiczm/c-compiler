use crate::lexer::{Constant as TokenConstant, Token};
use crate::parser::{Parse, ParserError, Result, TryParse};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Constant {
    Integer(i128),
    Character(char),
    Float(f64),
    Enumeration(String),
}

impl TryParse for Constant {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(None),
        };

        let out = match peek {
            Token::Constant(constant) => match *constant {
                TokenConstant::SignedInteger(value) => Constant::Integer(value as i128),
                TokenConstant::UnsignedInteger(value) => Constant::Integer(value as i128),
                TokenConstant::Decimal(value) => Constant::Float(value),
                TokenConstant::Character(value) => Constant::Character(value),
            },
            Token::Word(value) => Constant::Enumeration(value.to_string()),
            _ => return Ok(None),
        };

        tokens.next();

        Ok(Some(out))
    }
}

impl Parse for Constant {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        match Self::try_parse(tokens)? {
            Some(constant) => Ok(constant),
            None => Err(ParserError::ExpectedConstant {
                near_tokens: tokens.take(6).collect(),
            }),
        }
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
