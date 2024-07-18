use crate::lexer::{Keyword, Token};
use crate::parser::{Result, TryParse};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeQualifier {
    Const,
    Volatile,
}

impl TryParse for TypeQualifier {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let out = match tokens.peek() {
            Some(Token::Keyword(Keyword::Const)) => Some(TypeQualifier::Const),
            Some(Token::Keyword(Keyword::Volatile)) => Some(TypeQualifier::Volatile),
            _ => return Ok(None),
        };

        tokens.next();

        Ok(out)
    }
}

impl TypeQualifier {}

#[cfg(test)]
mod tests {
    use super::*;
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
        let result = TypeQualifier::try_parse(&mut tokens).unwrap().unwrap();
        assert_eq!(result, expected);
    }
}
