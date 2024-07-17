use crate::lexer::{Keyword, Token};
use crate::parser::{Parse, ParserError};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeQualifier {
    Const,
    Volatile,
}

impl Parse for TypeQualifier {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
        let out = match token {
            Token::Keyword(Keyword::Const) => TypeQualifier::Const,
            Token::Keyword(Keyword::Volatile) => TypeQualifier::Volatile,
            _ => unreachable!(),
        };

        Ok(out)
    }
}

impl TypeQualifier {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parse;
    use rstest::rstest;

    #[rstest]
    #[case::const_keyword(
        vec![Token::Keyword(Keyword::Const)],
        TypeQualifier::Const
    )]
    #[case::volatile_keyword(
        vec![Token::Keyword(Keyword::Volatile)],
        TypeQualifier::Volatile
    )]
    fn test_parse_type_qualifier(#[case] input: Vec<Token>, #[case] expected: TypeQualifier) {
        let mut tokens = input.into_iter().peekable();
        let result = TypeQualifier::parse(&mut tokens).unwrap();
        assert_eq!(result, expected);
    }
}
